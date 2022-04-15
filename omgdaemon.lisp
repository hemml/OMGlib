(defpackage :omgdaemon
  (:use cl omg inferior-shell)
  (:export *omg-version* ;; Current version name (string)
           *omg-last-version* ;; Last production version name (string)
           omg-init      ;; Init function, called after version startup.
                         ;;     Can be redefined, but must call (setf omg::*port* port) and (start-server)
                         ;;     The port is supplied as a parameter
           commit-devel  ;; Save development version image
           commit-production ;; Commit the image as a production version
           make-omg-daemon ;; Dump daemon image, specify proxy port
           commit-notify ;; The fucntion called on versions when new production version commited
           version-set-sym ;; Set symbol value in server (version) context
           steal-swank     ;; Redirect all standard/error output and debugger hooks to the current swank session
           +omg-version-cookie+ ;; cookie name
           +devel-version+))    ;; development version name

(in-package :omgdaemon)

(if (not (find-package :hunchentoot))
    (ql:quickload :hunchentoot))

(defvar *main-st-i* nil) ;; Streams and lock for devel<->daemon communications
(defvar *main-st-o* nil)
(defvar *main-lock* nil)

(defvar +app_prefix+ "omg_app_") ;; Prefix for dumped version images
(defvar +devel-version+ "devel") ;; Name of development version
(defvar +omg-images-path+ (merge-pathnames (make-pathname :directory '(:relative ".omg")))) ;; path to store version images

(defvar +proxy-chunk-size+ 65536) ;; Buffer size for internal proxy
(defvar +omg-version-cookie+ "OMGVERSION") ;; Internal proxy cookie name

(defvar +version-cookie-prefix+ (concatenate 'string +omg-version-cookie+ "=")) ;; Cookie prefix, just to speed-up cookie extraction process

(defvar *forks* (make-hash-table :test #'equal)) ;; Hash to store started versions data: I/O streams and fds, pid, etc

(defvar *proxy-sock* nil) ;; Listening proxy socket. Setf to nil to kill proxy and all proxy threads.

(defvar *omg-version* +devel-version+) ;; Current version name (string), default to devel
(defvar *omg-last-version* +devel-version+) ;; Current version name (string), default to devel

(defvar +nl-code+ (char-code #\Newline))
(defvar +cr-lf+ (trivial-utf-8:string-to-utf-8-bytes (format nil "~C~C" #\Return #\Newline)))

(defvar *prevent-devel-startup* nil) ;; Setf to T to temporary prevent development version startup, use to avoid race condition while new version commit

(defun omg-init (port)     ;; Called at version startup
  (setf omg::*port* port)
  (restart-server :address "127.0.0.1"))

(defvar +keyword-pkg+ (find-package "KEYWORD"))
(defvar +nil-str+ (write-to-string (list nil)))

(defun get-cmd-res (cmd) ;; Eval cmd and return results as a readable string
  (let* ((r1 (ignore-errors (multiple-value-list (eval cmd))))
         (res (if r1 r1 (list nil)))
         (str (let ((*package* +keyword-pkg+)) (write-to-string res)))
         (re1 (ignore-errors
                (multiple-value-bind (r l) (read-from-string str)
                  (declare (ignore r))
                  (if (= l (length str)) str +nil-str+)))))
    (if re1 re1 +nil-str+)))

(defparameter *server-set-list* nil)
(defparameter *server-set-sem* nil)

(defun version-set-sym (var val) ;; Set symbol value in server (version) context
  (push (cons var val) *server-set-list*)
  (bt:signal-semaphore *server-set-sem*))

(defun steal-swank () ;; Redirect all standard/error output and debugger hooks to the current swank session
  (mapcar
    (lambda (s)
      (push (cons s (symbol-value s)) *server-set-list*))
    '(*standard-output* *error-output* *debugger-hook* sb-ext:*invoke-debugger-hook*))
  (bt:signal-semaphore *server-set-sem*))


(defun run-main () ;; toplevel function, called after image startup. Waiting and executing commands from proxy
  (let* ((args (uiop:command-line-arguments))
         (fds (mapcar #'parse-integer args))
         (st-i (swank/backend:make-fd-stream (parse-integer (car args)) :UTF-8))  ;; I/O FDs are supplied as parameters
         (st-o (swank/backend:make-fd-stream (parse-integer (cadr args)) :UTF-8))
         (eofv (gensym)))
    (loop for i from 4 to 2048 when (and (not (position i fds)) (osicat-posix:fd-open-p i)) do
        (osicat-posix:close i)) ;; For security reasons closing all unneeded file descriptors
    (if (caddr args) ;; For devel version two additional FDs are supplied, to execute commands in proxy process
        (setf *main-st-i* (swank/backend:make-fd-stream (parse-integer (caddr args)) :UTF-8)))
    (if (cadddr args)
        (setf *main-st-o* (swank/backend:make-fd-stream (parse-integer (cadddr args)) :UTF-8)))
    (if (cddr args)
        (progn
          (setf *main-lock* (bt:make-lock))
          (sb-debug::enable-debugger)
          (setf sb-impl::*descriptor-handlers* nil)
          (setf *server-set-sem* (bt:make-semaphore))
          (bt:make-thread ;; process symbol-set requests to set symbol values in the server (version) context
            (lambda ()
              (loop do
                (progn
                  (bt:wait-on-semaphore *server-set-sem* :timeout 1)
                  (if *server-set-list*
                      (ignore-errors
                        (progn
                          (map nil
                               (lambda (kv)
                                 (set (car kv) (cdr kv)))
                               *server-set-list*)
                          (setf *server-set-list* nil))))))))))
    (loop while (and (open-stream-p st-i) (open-stream-p st-o)) do
      (let* ((cmd (ignore-errors (read st-i nil eofv)))
             (res (if (not (equal cmd eofv)) (get-cmd-res (eval cmd)))))
        (format st-o "~A~%" res)
        (force-output st-o)))))

(defun version-file-path (version) ;; Return a pathname for version image
  (if (or (position #\/ version) ;; Version name included into filename, so it cannot include "/" symbol
          (= 0 (length version)))
      (error (format nil "Invalid version name: \"~A\"" version)))
  (merge-pathnames (make-pathname :name (format nil "~A~A" +app_prefix+ version)) +omg-images-path+))

(defun version-path-p (path) ;; Check is a path a version image path
  (and (equal (directory-namestring path) (directory-namestring +omg-images-path+))
       (equal (ignore-errors (subseq (file-namestring path) 0 (length +app_prefix+)))
              +app_prefix+)))

(defun kill-all-threads ()
  (loop while (cdr (bt:all-threads)) do
    (progn
      (map nil
        (lambda (thr)
          (if (not (equal thr (bt:current-thread)))
              (bt:destroy-thread thr)))
        (bt:all-threads))
      (if (cdr (bt:all-threads))
          (sleep 1)))))

(defun makimg (path) ;; save lisp image. FIXME: SBCL only for now
  (ensure-directories-exist path)
  (ignore-errors (kill-server))
  (swank:stop-server 4008)
  (loop for i below 10 ;; Wait for swank shutdown
    while (position-if (lambda (s) (search "swank" s :test #'char-equal)) (mapcar #'bt:thread-name (bt:all-threads)))
    do (progn
         (format t "Waiting for swank shutdown. Please, close all connections!~%")
         (sleep 1)))
  (if (position-if (lambda (s) (search "swank" s :test #'char-equal)) (mapcar #'bt:thread-name (bt:all-threads)))
      (format t "SWANK NOT DEAD!!1111~%"))
  (map nil
    (lambda (conn)
      (ignore-errors (close (swank::connection.socket-io conn))))
    swank::*connections*)
  (loop for i below 10 ;; Wait for swank shutdown
    while (position-if (lambda (s) (search "swank" s :test #'char-equal)) (mapcar #'bt:thread-name (bt:all-threads)))
    do (progn
         (format t "Waiting for swank shutdown. Please, close all connections!~%")
         ;;;(swank:stop-server 4008)
         (sleep 1)))
  (kill-all-threads)
  ;;(sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die path :executable t :save-runtime-options t :purify nil :toplevel #'run-main))

(defun make-tmp-version (version) ;; construct a temporary version name (used while image dump, just to prevent current image damage)
  (format nil "tmp_~A" version))

(defun tmp-version-p (version)
  (equal "tmp_" (subseq version 0 4)))

(defun get-version-info (version) ;; Return an alist with version info -- streams, fds, pid, etc. Returns NIL if process not alive.
  (let* ((frk (omg::gethash-lock version *forks*))
         (pid (if frk (cdr (assoc :pid frk)))))
    (if (and pid (not (ignore-errors (osicat-posix:kill pid 0)))) ;; Check process alive, using signal 0
        (progn
          (format t "Process ~A of version ~A died~%" pid version)
          (osicat-posix:waitpid pid)
          (return-from get-version-info nil)))
    frk))

(defun send-cmd-to (version cmd) ;; Send command to the version (or to proxy if version is NIL) and return result(s)
  (let ((frk (if version (get-version-info version))))
    (if (or (not version) frk)
        (let ((st-i (if frk (cdr (assoc :in frk)) *main-st-i*))
              (st-o (if frk (cdr (assoc :out frk)) *main-st-o*))
              (lock (if frk (cdr (assoc :lock frk)) *main-lock*)))
          (bt:with-lock-held (lock)
            ;;(format t "CMD TO: ~A ~A~%" version cmd)
            (format st-o "~A~%" (let ((*package* (find-package "KEYWORD")))
                                  (write-to-string cmd)))
            (force-output st-o)
            (apply #'values (ignore-errors (read st-i)))))
        (error (format nil "Cannot find forked version ~A" version)))))

(defun commit (version) ;; Save image as a version
  (if (not (equal *omg-version* +devel-version+))
      (error "Only development version can be commited!"))
  (let ((tmpv (make-tmp-version version)))
    (send-cmd-to nil `(wait-for-commit-version ,tmpv ,version ,(osicat-posix:getpid)))
    (send-cmd-to nil `(send-cmd-to ,+devel-version+ '(progn
                                                       (setf omgdaemon::*omg-version* ,version)
                                                       (makimg (version-file-path ,tmpv)))))))

(defun commit-devel () ;; Save development version image
  (if (not (equal *omg-version* +devel-version+))
      (error "Only development version can be commited!"))
  (commit +devel-version+))

(defun get-top-version () ;; Return latest (by write time) production version, or "devel" if no production versions yet
  (let* ((files (remove-if
                  (lambda (path)
                    (let ((fname (file-namestring path)))
                      (or  (equal (subseq fname (length +app_prefix+))
                                  +devel-version+)
                           (tmp-version-p fname))))
                  (remove-if-not #'version-path-p
                                 (directory (merge-pathnames (make-pathname :name :wild :type :wild) +omg-images-path+)))))
         (tvf (car (sort (mapcar #'cons files (mapcar #'file-write-date files)) #'> :key #'cdr))))
    (if tvf
        (subseq (file-namestring (car tvf)) (length +app_prefix+))
        +devel-version+)))

(defun commit-production () ;; Commit the image as a production version
  (if (not (equal *omg-version* +devel-version+))
      (error "Only development version can be commited!"))
  (loop for version = (omg::random-string 8) ;; Generate random string as a version name
        for path = (version-file-path version)
        when (not (probe-file path)) return (commit version)))

(defun kill-version (version) ;; Kill version process
  (let* ((inf (get-version-info version))
         (pid (cdr (assoc :pid inf)))
         (fds (cdr (assoc :fds inf)))
         (st-i (cdr (assoc :in inf)))
         (st-o (cdr (assoc :out inf))))
    (if inf
        (progn
          (format t "Killing version ~A~%" version)
          (bt:make-thread
            (lambda ()
              (if (and (equal version +devel-version+) *prevent-devel-startup*) ;; Don't try to restart development version while image saved
                  (loop for i below 60 while *prevent-devel-startup* do (sleep 1)))
              (loop for i below 5 while (ignore-errors (osicat-posix:kill pid 0)) do (sleep 1))
              (if (ignore-errors (osicat-posix:kill pid 0))
                  (progn
                    (format t "Version ~A not died, sending SIGTERM!~%" version)
                    (ignore-errors (osicat-posix:kill pid osicat-posix:sigterm))
                    (loop for i below 5 while (ignore-errors (osicat-posix:kill pid 0)) do (sleep 1))
                    (if (get-version-info version)
                        (progn
                          (format t "Version ~A not died, sending SIGKILL!~%" version)
                          (ignore-errors (osicat-posix:kill pid osicat-posix:sigkill))))))
              (osicat-posix:waitpid pid)
              (format t "Version ~A killed!~%" version))
            :name "kill-version")
          (format t "Trying to quit version ~A...~%" version)
          (send-cmd-to version '(bt:make-thread (lambda () (sleep 1) (osicat-posix:exit 0))))
          (map nil #'osicat-posix:close fds)
          (remhash version *forks*)
          (format t "Version ~A unregistered~%" version)))))

(defun posix-fork () ;; Adopted from https://gitlab.common-lisp.net/qitab/poiu/blob/master/fork.lisp
  #+(and allegro os-unix)
  (excl.osi:fork)
  #+(and clisp os-unix)
  (funcall (find-symbol* 'fork "LINUX"))
  #+(and clozure os-unix)
  (ccl:external-call "fork" :int)
  #+(and sbcl os-unix)
  (progn
    (sb-ext:gc)
    (sb-posix:fork))
  #-(and os-unix (or allegro clisp clozure sbcl))
  (error "Fork not implemented, sorry"))

;; Adopded from swank/sbcl.lisp
(sb-alien:define-alien-routine ("execv" sys-execv) sb-alien:int
    (program sb-alien:c-string)
    (argv (* sb-alien:c-string)))

;; Adopded from swank/sbcl.lisp
(defun execv (program args)
    "Replace current executable with another one."
    (let ((a-args (sb-alien:make-alien sb-alien:c-string
                                       (+ 1 (length args)))))
      (unwind-protect
           (progn
             (loop for index from 0 by 1
                   and item in (append args '(nil))
                   do (setf (sb-alien:deref a-args index)
                            item))
             (when (minusp
                    (sys-execv program a-args))
               (error "execv(3) returned.")))
        (sb-alien:free-alien a-args))))

(defun version-alive-p (version &key no-cmd) ;; Check if the version up and works. If no-cmd is T, don't try to send test command
  (let* ((frk (get-version-info version))
         (port (cdr (assoc :port frk))))
    (and port
         (not (find-port:port-open-p port))
         (or no-cmd
             (equal 3 (send-cmd-to version '(+ 1 2)))))))

(defun wait-for-version-startup (version &optional (timeout 30)) ;; Wait up to 30 seconds (default) for version startup
  (loop for i below timeout while (not (version-alive-p version))
    do (sleep 1)))

(defun run-version (version) ;; Start a version
  (if (get-version-info version)
      (format t "The version ~A is already alive!~%" version)
      (let* ((path (version-file-path version))
             (is-dev (equal version +devel-version+)))
         (if (and is-dev *prevent-devel-startup*) ;; Don't try to restart development version while image saved
             (loop for i below 60 while *prevent-devel-startup* do (sleep 1)))
         (if (probe-file path)
             (let* ((pip1 (multiple-value-list (osicat-posix:pipe)))
                    (pip2 (multiple-value-list (osicat-posix:pipe)))
                    (dpip1 (if is-dev (multiple-value-list (osicat-posix:pipe))))
                    (dpip2 (if is-dev (multiple-value-list (osicat-posix:pipe))))
                    (st-i (swank/backend:make-fd-stream (car pip1) :UTF-8))
                    (st-o (swank/backend:make-fd-stream (cadr pip2) :UTF-8))
                    (st-di (if is-dev (swank/backend:make-fd-stream (car dpip1) :UTF-8)))
                    (st-do (if is-dev (swank/backend:make-fd-stream (cadr dpip2) :UTF-8)))
                    (fds `(,@pip1 ,@pip2 ,@(if is-dev `(,@dpip1 ,@dpip2)))) ;; Collect all FDs which we need to close after process termination)
                    (name (namestring path))
                    (port (find-port:find-port)))
               (let ((pid (osicat-posix:fork)))  ;; fork-exec, to preserve open FDs
                 (if (= pid 0)
                     (execv name `(,name
                                   ,(format nil "~A" (car pip2))
                                   ,(format nil "~A" (cadr pip1))
                                   ,@(if is-dev
                                         `(,(format nil "~A" (car dpip2))
                                           ,(format nil "~A" (cadr dpip1))))))
                     (let ((dev-thr (if is-dev  ;; for development version we need to start sawnk server and process incoming commands
                                        (bt:make-thread
                                          (lambda ()
                                            (wait-for-version-startup version)
                                            (send-cmd-to version
                                              '(progn
                                                (setf swank::*loopback-interface* "0.0.0.0")
                                                (setf swank:*globally-redirect-io* t)
                                                (swank:create-server :port 4008 :dont-close t)))
                                            (loop
                                              while (and (open-stream-p st-di)
                                                         (open-stream-p st-do))
                                              do (let* ((eofv (gensym))
                                                        (cmd (ignore-errors (read st-di nil eofv)))
                                                        (res (if (not (equal cmd eofv)) (get-cmd-res (eval cmd)))))
                                                   (format st-do "~A~%" res)
                                                   (force-output st-do))))
                                          :name "devel processing"))))
                       (unwind-protect
                         (progn
                           (setf (omg::gethash-lock version *forks*)
                                 `((:out . ,st-o)
                                   (:in . ,st-i)
                                   (:fds . ,fds)
                                   (:pid . ,pid)
                                   (:port . ,port)
                                   (:lock . ,(bt:make-lock)))) ;; The lock needed to prevent parallel command processing (because streams are sequental)
                           (send-cmd-to version `(progn
                                                   (setf omgdaemon::*omg-version* ,version) ;; Allow the process to know its version
                                                   (setf omgdaemon::*omg-last-version* ,(get-top-version))
                                                   (omg-init ,port)))
                           (if is-dev (send-cmd-to version `(setf hunchentoot:*catch-errors-p* nil)))
                           (wait-for-version-startup version)
                           (if (version-alive-p version)
                               (format t "Version [~A] spawned!~%" version)
                               (error (format nil "Cannot spawn version [~A]" version))))
                         (if (not (version-alive-p version))
                             (progn
                               (bt:destroy-thread dev-thr)
                               (kill-version version))))))))
             (error (format nil "Cannot find image file for version ~A" version))))))

(defun restart-version (version)
  (format t "Restarting version ~A~%" version)
  (kill-version version)
  (run-version version))

(defun ensure-version-working (version &key no-cmd) ;; If the version not spawned, start it
  (if (not (version-alive-p version :no-cmd no-cmd))
      (progn
        (format t "Version ~A not responding~%" version)
        (if (and (equal version +devel-version+) *prevent-devel-startup*)
            (loop for i below 60 while *prevent-devel-startup* do (sleep 1)))
        (restart-version version)
        (loop for i below 10
              while (not (version-alive-p version))
              do (sleep 1))
        (if (not (version-alive-p version))
            (error (format t "Cannot spawn version ~A" version))))))

(defun commit-notify (version)
  (declare (ignore version)))

(defun wait-for-commit-version (tv v pid) ;; The function called (on proxy) by development version just before image dump
  (bt:make-thread                         ;; Wait, process with PID termination and tename tv image to v
    (lambda ()
      (setf *prevent-devel-startup* t)
      (unwind-protect
        (let ((tv-path (version-file-path tv))
              (v-path (version-file-path v))
              (link-ok nil))
          (osicat-posix:waitpid pid)
          (ensure-version-working tv)
          (kill-version tv)
          (if (probe-file v-path) (delete-file v-path))
          (rename-file tv-path v-path)
          (if (not (equal v +devel-version+)) ;; For production commit, we also have to replace development version
              (let ((dev-path (version-file-path +devel-version+)))
                (unwind-protect
                  (progn
                    (rename-file dev-path tv-path)
                    (osicat-posix:link v-path dev-path)
                    (setf link-ok t)
                    (delete-file tv-path)
                    (map nil
                      (lambda (vers)
                        (bt:make-thread
                          (lambda ()
                            (format t "Sending cmd to: ~A ~A~%" vers v)
                            (send-cmd-to vers `(progn
                                                 (setf omgdaemon::*omg-last-version* ,v)
                                                 (commit-notify ,v)))
                            (format t "Cmd to ~A sent!~%" vers)))
                        (sleep 1))
                      (bt:with-lock-held (omg::*giant-hash-lock*)
                        (loop for vers being each hash-key of *forks* when vers collect vers))))
                  (if (not link-ok)
                      (rename-file tv-path dev-path))))))
        (setf *prevent-devel-startup* nil)))
    :name "wait-for-commit")
  nil)

(defun stop-proxy ()
  (if *proxy-sock*
      (usocket:socket-close *proxy-sock*))
  (setf *proxy-sock* nil))

(defmacro buf-append (b1 b2)
  (let ((bl1 (gensym))
        (b1t (gensym))
        (b2t (gensym)))
    `(let* ((,b1t ,b1)
            (,b2t ,b2)
            (,bl1 (array-dimension ,b1t 0)))
       (adjust-array ,b1t (list (+ ,bl1 (array-dimension ,b2t 0))))
       (replace ,b1t ,b2t :start1 ,bl1)
       ,b1t)))

(defun version-available (version) ;; Check if version image exists
  (probe-file (version-file-path version)))

(defun proxy-job (cs) ;; Proxy connection cs to the specific version image. FIXME: SBCL only
  (let* ((c-rd-buf (make-array '(0) :adjustable t :element-type '(unsigned-byte 8)))
         (s-rd-buf (make-array '(0) :adjustable t :element-type '(unsigned-byte 8))))
    (labels ((read-lin (soc buf) ;; Just more efficient read-line, by using buffered readings. Returns line and unprocessed input in buf
               (let* ((nlp (position +nl-code+ buf))
                      (nb (array-dimension buf 0)))
                 (if nlp
                     (let ((s (if (= 0 nlp)
                                  ""
                                  (string-right-trim '(#\Return) (trivial-utf-8:utf-8-bytes-to-string buf :end nlp)))))
                       (replace buf buf :start2 (+ 1 nlp))
                       (adjust-array buf (list (- nb nlp 1)))
                       s)
                     (let ((tmp-buf (make-array `(,+proxy-chunk-size+) :element-type '(unsigned-byte 8)))
                           (bl0 (array-dimension buf 0)))
                       (multiple-value-bind (b nb s) (sb-bsd-sockets:socket-receive (usocket:socket soc) tmp-buf nil :element-type '(unsigned-byte 8))
                         (declare (ignore b s))
                         (if (and nb (> nb 0))
                             (progn
                               (adjust-array buf (list (+ bl0 nb)))
                               (replace buf tmp-buf :start1 bl0)
                               (read-lin soc buf))))))))
             (extract-cookie (s) ;; If the line is a Cookie: header, try to extract version cookie value
               (let* ((cp (position #\: s))
                      (ccp (string-equal s "cookie" :end1 cp)))
                 (if (and cp ccp)
                     (let* ((p (search +version-cookie-prefix+ s :start2 cp))
                            (ps (if p (+ p (length +version-cookie-prefix+))))
                            (pe (if p (position #\; s :start ps)))
                            (pe2 (if pe pe (length s)))
                            (val (if p (subseq s ps))))
                       (values val ccp p pe2)))))
             (tee (s1 s2) ;; Just transfer all data from s1 to s2, using buffered I/O to speedup
               (let ((buf (make-array `(,+proxy-chunk-size+) :element-type '(unsigned-byte 8))))
                 (loop for (b nb p) = (multiple-value-list
                                        (sb-bsd-sockets:socket-receive
                                          (usocket:socket s1) buf nil :element-type '(unsigned-byte 8)))
                       while (and *proxy-sock* nb (> nb 0))
                       for rs = (sb-bsd-sockets:socket-send (usocket:socket s2) buf nb)
                       until (not rs)))))
      (unwind-protect
        (multiple-value-bind (vers sb sv) ;; Process input, before version cookie will be found, or before end of headers reached
                             (loop for s = (read-lin cs c-rd-buf)
                                   for v = (if s (extract-cookie s))
                                   when s collect (format nil "~A~C~C" s #\return #\newline) into hdrs
                                   when (or (not s) v (= (length s) 0))
                                        return (values (if (and v (version-available v))
                                                           v
                                                           (get-top-version))
                                                       (trivial-utf-8:string-to-utf-8-bytes
                                                         (format nil "~{~A~}" hdrs))
                                                       (not v)))
          (ensure-version-working vers :no-cmd t)
          (let* ((frk (get-version-info vers))
                 (ss (usocket:socket-connect "127.0.0.1" (cdr (assoc :port frk)) :element-type '(unsigned-byte 8)))
                 (tee-thr nil))
            (unwind-protect
              (progn
                (sb-bsd-sockets:socket-send (usocket:socket ss) sb nil) ;; Send all client headers to server
                (sb-bsd-sockets:socket-send (usocket:socket ss) c-rd-buf nil) ;; Send all data remaining in the buffer
                (setf tee-thr (bt:make-thread (lambda () (tee cs ss)) :name "tee cs ss")) ;; Connect client to server
                (if sv ;; If we need to set a new version cookie, read server headers and add/replace apropriate Set-cookie: line
                    (progn
                      (loop for s = (read-lin ss s-rd-buf)
                            when (or (not s) (= 0 (length s))) return
                                 (sb-bsd-sockets:socket-send
                                    (usocket:socket cs)
                                    (trivial-utf-8:string-to-utf-8-bytes
                                      (format nil "~{~A~}"
                                              `(,@hdrs
                                                ,@(if sv `(,(format nil "Set-cookie: ~A~A~C~C"
                                                                    +version-cookie-prefix+
                                                                    vers
                                                                    #\return #\newline)))
                                                ,(format nil "~C~C" #\return #\newline))))
                                    nil)
                            when (let ((cp (position #\: s)))
                                   (not (and cp
                                             (string-equal s "set-cookie" :end1 cp)
                                             (not (search +version-cookie-prefix+ s :start2 cp)))))
                                 collect (format nil "~A~C~C" s #\return #\newline) into hdrs)
                      (sb-bsd-sockets:socket-send (usocket:socket cs) s-rd-buf nil))) ;; Send data remaining in line buffer
                (tee ss cs)) ;; Connect server to client
              (progn
                (ignore-errors (usocket:socket-close ss))
                (ignore-errors (usocket:socket-close cs))
                (if tee-thr (ignore-errors (bt:destroy-thread tee-thr)))))))
        (usocket:socket-close cs)))))

(defun proxy (port) ;; Start a proxy server
  (let ((psock (usocket:socket-listen "0.0.0.0" port :reuse-address t :element-type '(unsigned-byte 8))))
    (setf *proxy-sock* psock)
    (unwind-protect
      (loop while *proxy-sock* do
        (let ((s (usocket:socket-accept *proxy-sock*)))
          (bt:make-thread (lambda () (proxy-job s)) :name "proxy-job")))
      (progn
        (usocket:socket-close psock)
        (setf *proxy-sock* nil)))))

(defvar *proxy-port* 80)

(defun run-daemon () ;; Toplevel function for daemon startup
  (if (not omg::*giant-hash-lock*)
      (setf omg::*giant-hash-lock* (bt:make-lock)))
  (let ((devel-path (version-file-path +devel-version+)))
    (if (not (probe-file devel-path)) ;; If this is a first start, dump development version image
        (let ((pid (posix-fork)))
          (if (> pid 0)
              (osicat-posix:waitpid pid)
              (makimg devel-path))))
    (bt:start-multiprocessing)
    (setf *main-lock* (bt:make-lock))
    (ensure-version-working +devel-version+)
    (proxy *proxy-port*)))

(defun make-omg-daemon (port) ;; Dump daemon image, specify proxy port
  (swank-loader:init
    :delete nil         ; delete any existing SWANK packages
    :reload nil         ; reload SWANK, even if the SWANK package already exists
    :load-contribs nil)
  (setf *proxy-port* port)
  (setf swank:*globally-redirect-io* t)
  (loop for i below 20 ;; Wait for swank shutdown
    while (position-if (lambda (s) (search "swank" s :test #'char-equal)) (mapcar #'bt:thread-name (bt:all-threads)))
    do (progn
         (format t "Waiting for swank shutdown...~%")
         (sleep 1)))
  (if (position-if (lambda (s) (search "swank" s :test #'char-equal)) (mapcar #'bt:thread-name (bt:all-threads)))
      (format t "SWANK NOT DEAD!!1111~%"))
  (kill-all-threads)
  ;;(sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die (merge-pathnames (make-pathname :name "omgdaemon"))
                            :executable t :save-runtime-options t :purify nil :toplevel #'run-daemon))

(defun make-docker-image ()
  (with-input-from-string
    (fd (format nil "~{~A~%~}"
          `("FROM debian"
            "RUN apt-get update && apt-get dist-upgrade -y"
            "RUN apt-get install -y sbcl cl-quicklisp git build-essential"
            "RUN adduser omg"
            "RUN su -l omg -c 'sbcl --load \"/usr/share/common-lisp/source/quicklisp/quicklisp.lisp\" --eval \"(quicklisp-quickstart:install)\" --disable-debugger'"
            "RUN su -l omg -c 'mkdir -p /home/omg/quicklisp/local-projects && cd /home/omg/quicklisp/local-projects && git clone --recurse-submodules https://github.com/hemml/OMGlib.git'"
            "RUN su -l omg -c 'cd /home/omg && sbcl --eval \"(load \\\"/home/omg/quicklisp/setup.lisp\\\")\" --eval \"(ql:quickload :hunchentoot)\" --eval \"(ql:quickload :omg)\" --eval \"(omgdaemon:make-omg-daemon 8080)\" --disable-debugger'"
            "EXPOSE 8080 4008"
            "CMD while true; do su -l omg -c 'cd /home/omg && ./omgdaemon' ; sleep 1 ; done")))
    (run '(docker build :tag omgdaemon :pull :no-cache -) :input fd)))

(defun update-omg ()
  (run '(and (cd "/home/omg/quicklisp/local-projects/OMGlib")
             (git pull :rebase)))
  (require :omg))
