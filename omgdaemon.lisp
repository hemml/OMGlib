(defpackage :omgdaemon
  (:use cl omg inferior-shell iolib log4cl trivial-dump-core)
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
           +omg-version-cookie+ ;; cookie name
           +devel-version+))    ;; development version name

(in-package :omgdaemon)

(if (not (find-package :hunchentoot))
    (require :hunchentoot))

(defvar *main-st-i* nil) ;; Streams and lock for devel<->daemon communications
(defvar *main-st-o* nil)
(defvar *main-lock* nil)
(defvar *start-lock* nil)

(defvar +app_prefix+ "omg_app_") ;; Prefix for dumped version images
(defvar +devel-version+ "devel") ;; Name of development version
(defvar +omg-images-path+ (merge-pathnames (make-pathname :directory '(:relative ".omg")))) ;; path to store version images

(defvar +proxy-chunk-size+ 65536) ;; Buffer size for internal proxy
(defvar +omg-version-cookie+ "OMGVERSION") ;; Internal proxy cookie name

(defvar +version-cookie-prefix+ (concatenate 'string +omg-version-cookie+ "=")) ;; Cookie prefix, just to speed-up cookie extraction process

(defvar *forks* (make-hash-table :test #'equal)) ;; Hash to store started versions data: I/O streams and fds, pid, etc
(defvar *start-locks* (make-hash-table :test #'equal)) ;; Hash to store locks, preventing multiple starups of a version

(defvar *omg-version* +devel-version+) ;; Current version name (string), default to devel
(defvar *omg-last-version* +devel-version+) ;; Current version name (string), default to devel

(defvar +nl-code+ (char-code #\Newline))
(defvar +cr-lf+ (trivial-utf-8:string-to-utf-8-bytes (format nil "~C~C" #\Return #\Newline)))

(defvar *prevent-devel-startup* nil) ;; Setf to T to temporary prevent development version startup, use to avoid race condition while new version commit

(defun disable-debugger ()
  (setf *debugger-hook*
        (lambda (c h)
          (declare (ignore h))
          (log:error "Error found, while debugger is disabled: ~A" c)
          (uiop:quit))))

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

(defun run-main () ;; toplevel function, called after image startup. Waiting and executing commands from proxy
  (setf omg::*giant-hash-lock* (bt:make-lock))
  (let* ((args (uiop:command-line-arguments))
         (fds (mapcar #'parse-integer args))
         (st-i (swank/backend:make-fd-stream (parse-integer (car args)) :UTF-8))  ;; I/O FDs are supplied as parameters
         (st-o (swank/backend:make-fd-stream (parse-integer (cadr args)) :UTF-8))
         (eofv (gensym)))
    (loop for i from 4 to 2048 when (and (not (position i fds)) (osicat-posix:fd-open-p i)) do
        (log:debug "Closing FD ~A" i)
        (osicat-posix:close i)) ;; For security reasons closing all unneeded file descriptors
    (if (caddr args) ;; For devel version two additional FDs are supplied, to execute commands in proxy process
        (setf *main-st-i* (swank/backend:make-fd-stream (parse-integer (caddr args)) :UTF-8)))
    (if (cadddr args)
        (setf *main-st-o* (swank/backend:make-fd-stream (parse-integer (cadddr args)) :UTF-8)))
    (if (cddr args)
        (progn
          (setf *main-lock* (bt:make-lock))
          (setf *debugger-hook* #'swank:swank-debugger-hook)
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
                          (setf *server-set-list* nil)))))))))
        (disable-debugger))
    (loop while (and (open-stream-p st-i) (open-stream-p st-o)) do
      (let* ((cmd (ignore-errors (read st-i nil eofv)))
             (res (if (and cmd (not (equal cmd eofv))) (get-cmd-res cmd))))
        (if (or (not cmd) (equal cmd eofv))
            (uiop:quit))
        (format st-o "~A~%" res)
        (force-output st-o)))
    (uiop:quit)))

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

(defun makimg (path) ;; save lisp image
  (ensure-directories-exist path)
  (ignore-errors (kill-server))
  (let* ((ostream (make-string-output-stream :element-type 'extended-char))
         (istream (make-string-input-stream ""))
         (*standard-output* ostream)
         (*error-output* ostream)
         (*standard-input* istream))
    (kill-all-threads)
    (save-executable path #'run-main)))

(defun make-tmp-version (version) ;; construct a temporary version name (used while image dump, just to prevent current image damage)
  (format nil "tmp_~A" version))

(defun tmp-version-p (version)
  (equal "tmp_" (subseq version 0 4)))

(defun get-version-info (version) ;; Return an alist with version info -- streams, fds, pid, etc. Returns NIL if process not alive.
  (let* ((frk (omg::gethash-lock version *forks*))
         (pid (if frk (cdr (assoc :pid frk)))))
    (if (and pid (not (ignore-errors (osicat-posix:kill pid 0)))) ;; Check process alive, using signal 0
        (progn
          (log:info "Process ~A of version ~A died" pid version)
          (osicat-posix:waitpid pid)
          (return-from get-version-info nil)))
    frk))

(defun send-cmd-to (version cmd &key no-wait) ;; Send command to the version (or to proxy if version is NIL) and return result(s)
  (log:debug "CMD to ~A: ~A" version cmd)
  (let ((frk (if version (get-version-info version))))
    (if (or (not version) frk)
        (let ((st-i (if frk (cdr (assoc :in frk)) *main-st-i*))
              (st-o (if frk (cdr (assoc :out frk)) *main-st-o*))
              (lock (if frk (cdr (assoc :lock frk)) *main-lock*)))
          (bt:with-lock-held (lock)
            (format st-o "~A~%" (let ((*package* (find-package "KEYWORD")))
                                  (write-to-string cmd)))
            (force-output st-o)
            (if (not no-wait)
                (apply #'values (ignore-errors (read st-i))))))
        (error (format nil "Cannot find forked version ~A" version)))))

(defun commit (version) ;; Save image as a version
  (if (not (equal *omg-version* +devel-version+))
      (error "Only development version can be commited!"))
  (let ((tmpv (make-tmp-version version)))
    (send-cmd-to nil `(wait-for-commit-version ,tmpv ,version ,(osicat-posix:getpid)))
    (send-cmd-to nil `(send-cmd-to ,+devel-version+
                                   '(progn
                                      (setf omgdaemon::*omg-version* ,version)
                                      (makimg (version-file-path ,tmpv)))
                                   :no-wait t))
    (swank:stop-server 4008)
    (map nil
      (lambda (conn)
        (ignore-errors (close (swank::connection.socket-io conn))))
      swank::*connections*)))

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
         (fds (cdr (assoc :fds inf))))
    (if inf
        (progn
          (log:info "Killing version ~A" version)
          (bt:make-thread
            (lambda ()
              (if (and (equal version +devel-version+) *prevent-devel-startup*) ;; Don't try to restart development version while image saved
                  (loop for i below 60 while *prevent-devel-startup* do (sleep 1)))
              (loop for i below 5 while (ignore-errors (osicat-posix:kill pid 0)) do (sleep 1))
              (if (ignore-errors (osicat-posix:kill pid 0))
                  (progn
                    (log:info "Version ~A not died, sending SIGTERM!" version)
                    (ignore-errors (osicat-posix:kill pid osicat-posix:sigterm))
                    (loop for i below 5 while (ignore-errors (osicat-posix:kill pid 0)) do (sleep 1))
                    (if (get-version-info version)
                        (progn
                          (log:info "Version ~A not died, sending SIGKILL!" version)
                          (ignore-errors (osicat-posix:kill pid osicat-posix:sigkill))))))
              (osicat-posix:waitpid pid)
              (log:info "Version ~A killed!" version))
            :name "kill-version")
          (log:info "Trying to quit version ~A..." version)
          (send-cmd-to version '(bt:make-thread (lambda () (sleep 1) (osicat-posix:exit 0))))
          (map nil #'osicat-posix:close fds)
          (remhash version *forks*)
          (log:info "Version ~A unregistered" version)))))

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

(defparameter *swank-comm-style* :spawn)

(defun run-version (version) ;; Start a version
  (if (get-version-info version)
      (log:info "The version ~A is already alive!" version)
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
                     (let* ((args `(,name
                                    ,(format nil "~A" (car pip2))
                                    ,(format nil "~A" (cadr pip1))
                                    ,@(if is-dev
                                          `(,(format nil "~A" (car dpip2))
                                            ,(format nil "~A" (cadr dpip1))))))
                            (argc (+ 2 (length args))))
                       (cffi:with-foreign-object (argv :pointer argc)
                         (iolib/syscalls:bzero argv (* argc (iolib/syscalls:sizeof :pointer)))
                         (loop for s in args and i below (length args) do (setf (cffi:mem-aref argv :pointer i) (cffi:foreign-string-alloc s)))
                         (iolib/syscalls:execv name argv)))
                     (let ((dev-thr (if is-dev  ;; for development version we need to start sawnk server and process incoming commands
                                        (bt:make-thread
                                          (lambda ()
                                            (wait-for-version-startup version)
                                            (send-cmd-to version
                                              `(progn
                                                (setf swank::*loopback-interface* "0.0.0.0")
                                                (setf swank::*globally-redirect-io* t)
                                                (bt:make-thread
                                                  (lambda ()
                                                    (swank:create-server :port 4008 :dont-close t :style ,*swank-comm-style*)))))
                                            (loop
                                              while (and (open-stream-p st-di)
                                                         (open-stream-p st-do))
                                              do (let* ((eofv (gensym))
                                                        (cmd (ignore-errors (read st-di nil eofv)))
                                                        (res (if (and cmd (not (equal cmd eofv))) (get-cmd-res cmd))))
                                                   (if (or (not cmd) (equal cmd eofv))
                                                       (return))
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
                                   (:lock . ,(bt:make-lock)) ;; The lock needed to prevent parallel command processing (because streams are sequental)
                                   (:fresh . t)))
                           (send-cmd-to version `(progn
                                                   (setf omgdaemon::*omg-version* ,version) ;; Allow the process to know its version
                                                   (setf omgdaemon::*omg-last-version* ,(get-top-version))
                                                   (omg-init ,port)))
                           (if is-dev (send-cmd-to version `(setf hunchentoot:*catch-errors-p* nil)))
                           (wait-for-version-startup version)
                           (if (version-alive-p version)
                               (log:info "Version [~A] spawned!" version)
                               (error (format nil "Cannot spawn version [~A]" version))))
                         (if (not (version-alive-p version))
                             (progn
                               (if dev-thr (bt:destroy-thread dev-thr))
                               (kill-version version))))))))
             (error (format nil "Cannot find image file for version ~A" version))))))

(defun restart-version (version)
  (log:info "Restarting version ~A" version)
  (ignore-errors (kill-version version))
  (run-version version))

(defun ensure-version-working (version &key no-cmd) ;; If the version not spawned, start it
  (bt:with-lock-held ((bt:with-lock-held (*start-lock*)
                        (let ((l (gethash version *start-locks*)))
                          (if l l (setf (gethash version *start-locks*) (bt:make-lock))))))
    (let ((tim0 (get-universal-time)))
      (loop while (and (not (version-alive-p version :no-cmd no-cmd))
                       (< (- (get-universal-time) tim0) 120)) do
        (log:info "Version ~A not responding" version)
        (when (and (equal version +devel-version+) *prevent-devel-startup*)
            (loop for i below 60 while *prevent-devel-startup* do (sleep 1)))
        (restart-version version)
        (loop for i below 10
           while (not (version-alive-p version))
           do (sleep 1)))
      (version-alive-p version))))

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
          (when (ensure-version-working tv)
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
                              (log:debug "Sending cmd to: ~A ~A" vers v)
                              (ignore-errors
                                (send-cmd-to vers `(ignore-errors
                                                     (setf omgdaemon::*omg-last-version* ,v)
                                                     (commit-notify ,v)))
                                (log:debug "Cmd to ~A sent!" vers))))
                          (sleep 1))
                        (bt:with-lock-held (omg::*giant-hash-lock*)
                          (loop for vers being each hash-key of *forks* when vers collect vers))))
                    (if (not link-ok)
                        (rename-file tv-path dev-path)))))))
        (setf *prevent-devel-startup* nil)))
    :name "wait-for-commit")
  nil)

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

(defvar *proxy-port* 80)

(defclass ring-buffer ()
  (buf
   (buf-size :initform +proxy-chunk-size+
             :initarg :size)
   (start :initform 0)
   (end :initform 0)))

(defmethod initialize-instance :after ((b ring-buffer) &key &allow-other-keys)
  (with-slots (buf buf-size) b
    (setf buf (make-array (list buf-size) :element-type '(unsigned-byte 8)))))

(defmethod get-space ((b ring-buffer) &optional size)
  (with-slots (start end buf buf-size) b
    (let ((end1 (if (and (= end buf-size)
                         (> start 0))
                    0
                    end)))
      (if size
          (values buf end1 (min (+ end1 size) (if (<= start end1) buf-size (1- start))))
          (values buf end1 (if (<= start end1) buf-size (1- start)))))))

(defmethod allocate-space ((b ring-buffer) size)
  (with-slots (start end buf buf-size) b
    (when (< size 0)
      (error (format nil "Trying to allocate ~A btyes" size)))
    (let ((end1 (if (and (= end buf-size)
                         (> start 0))
                    0
                    end)))
      (when (> (+ end1 size) (if (<= start end1) buf-size (1- start)))
        (error "Buffer overrrun!"))
      (setf end (+ end1 size))
      (when (and (= end buf-size) (> start 0))
        (setf end 0)))))

(defmethod data-available ((b ring-buffer))
  (with-slots (start end buf buf-size) b
    (if (<= start end)
        (values buf start end)
        (values buf start buf-size))))

(defmethod deallocate-space ((b ring-buffer) size)
  (with-slots (start end buf buf-size) b
    (when (< size 0)
      (error (format nil "Trying to deallocate ~A btyes" size)))
    (when (> (+ start size) (if (<= start end) end buf-size))
      (error "Buffer underrun!"))
    (incf start size)
    (when (= start buf-size)
      (setf start 0)
      (when (= end buf-size)
        (setf end 0)))))

(defmethod empty-p ((b ring-buffer))
  (with-slots (start end) b
    (= start end)))

(defmethod full-p ((b ring-buffer))
  (with-slots (start end buf-size) b
    (not (if (<= start end)
             (or (< end buf-size)
                 (> start 0))
             (< end start)))))

; (defmethod extend-buf ((b ring-buffer))
;   (with-))

(defvar *active-connections* (make-hash-table))

(defclass async-conn ()
  ((write-buf :initform (make-instance 'ring-buffer))
   (read-buf :initform (make-instance 'ring-buffer))
   (base :initarg :base
         :initform (error "IOLIB base must be specified!"))
   (conn :initarg :conn)
   (fin :initform nil)
   (connected :initform nil)
   (readed :initform 0)
   (written :initform 0)
   (write-registered :initform nil)
   (conn-id :initform "[unknown ip:port]")))

(defclass selector-conn (async-conn)
  ((last-pos :initform 0)
   (cookie-found :initform nil)
   (cookie-sent-state :initform :waiting-end-of-hdrs)
   (cur-str-len :initform 0)
   (version-id)))

(defmacro wait-for (base pred &rest code)
  (let ((fn (gensym))
        (dt (gensym)))
    `(let ((,dt 0.01))
       (labels ((,fn ()
                  (if ,pred
                      (progn ,@code)
                      (progn
                        (setf ,dt (min 5.0 (* 1.1 ,dt)))
                        (add-timer ,base #',fn ,dt :one-shot t)))))
         (,fn)))))

(defvar *version-spawn-threads* (make-hash-table :test #'equalp))

(defmethod connect-to-version ((c selector-conn) &optional (version (get-top-version)))
  (with-slots (cookie-found base last-pos version-id) c
    (log:debug "Connecting to version ~A" version)
    (setf cookie-found t)
    (let ((frk (get-version-info version)))
      (when (and (not frk)
                 (not (gethash version *version-spawn-threads*)))
        (setf (gethash version *version-spawn-threads*)
              (bt:make-thread
                (lambda ()
                  (unwind-protect
                    (loop for i below 5 until (ignore-errors (ensure-version-working version))
                      when (not (version-alive-p version))
                      do (let ((v1 (get-top-version)))
                            (when (not (gethash v1 *version-spawn-threads*)))
                            (when (not (equal v1 version))
                              (log:info "Cannot start version ~A, trying with ~A" version v1)
                              (remhash version *version-spawn-threads*)
                              (setf version v1)
                              (if (gethash v1 *version-spawn-threads*)
                                  (return)
                                  (setf (gethash v1 *version-spawn-threads*) (bt:current-thread))))))
                    (remhash version *version-spawn-threads*)))
                :name (format nil "starting version ~A" version)))))
    (let ((t0 (get-universal-time)))
      (wait-for base (or (> (- (get-universal-time) t0) 60)
                         (get-version-info version))
        (let ((frk (get-version-info version)))
          (if frk
              (let ((vrs (make-socket :connect :active :address-family :internet :type :stream :ipv6 nil)))
                (when (cdr (assoc :fresh frk))
                  (sleep 1)
                  (setf (cdr (assoc :fresh frk)) nil))
                (connect vrs +ipv4-loopback+ :port (cdr (assoc :port frk)) :wait nil)
                (wait-for base (socket-connected-p vrs)
                  (let ((vrs-conn (make-instance 'async-conn :conn vrs :base base)))
                    (setf version-id version)
                    (log:debug "Connected to version ~A" version)
                    (attach c vrs-conn)
                    (attach vrs-conn c))))
              (log:debug "Cannot wait more for version ~A" version)))))))

(defmethod push-to ((c selector-conn) buf &key (start 0) (end (array-dimension buf 0)))
  (with-slots (cookie-sent-state cur-str-len write-buf version-id) c
    (cond ((eql cookie-sent-state :cookie-sent)
           (call-next-method))
          ((eql cookie-sent-state :send-a-cookie)
           (let* ((cookie-hdr (trivial-utf-8:string-to-utf-8-bytes
                                 (format nil "Set-cookie: ~A~A~C~C"
                                   +version-cookie-prefix+
                                   version-id
                                   #\return #\newline)))
                  (cookie-len (array-dimension cookie-hdr 0)))
              (multiple-value-bind (buf1 start1 end1) (get-space write-buf cookie-len)
                (when (>= (- end1 start1) cookie-len)
                  (replace buf1 cookie-hdr :start1 start1 :start2 0 :end1 (+ start1 cookie-len) :end2 cookie-len)
                  (allocate-space write-buf cookie-len)
                  (register-write c)
                  (setf cookie-sent-state :cookie-sent)))
              (return-from push-to 0)))
          ((eql cookie-sent-state :waiting-end-of-hdrs)
           (let ((lpos start))
             (multiple-value-bind (buf1 start1 end1) (get-space write-buf (- end start))
               (let ((cw (min (- end start) (- end1 start1))))
                 (when (> cw 0)
                   (let ((end (+ start cw)))
                     (loop for pos = (position +nl-code+ buf :start lpos :end end)
                       when (and pos (= pos (1+ lpos))) do
                         (log:debug "End of headers found, injecting a cookie header")
                         (replace buf1 buf :start1 start1 :start2 start :end1 (+ start1 (- lpos start)) :end2 (+ start (- lpos start)))
                         (allocate-space write-buf (- lpos start))
                         (register-write c)
                         (setf cookie-sent-state :send-a-cookie)
                         (return-from push-to (- lpos start))
                       when pos do
                        (setf lpos (1+ pos))
                        (setf cur-str-len 0)
                       when (not pos) do
                        (incf cur-str-len (- end lpos))
                        (return-from push-to (call-next-method))))))))))))

(defmethod try-read :after ((c selector-conn))
  (with-slots (connected last-pos read-buf cookie-found) c
    (when (and (not connected) (not cookie-found))
      (if (full-p read-buf)
          (close-conn c)
          (multiple-value-bind (buf start end) (data-available read-buf)
            (declare (ignore start))
            (loop for pos = (position +nl-code+ buf :start last-pos :end end) while pos do
              (let* ((s (trivial-utf-8:utf-8-bytes-to-string buf :start last-pos :end (1- pos)))
                     (cp (position #\: s)))
                (when (and cp (string-equal s "cookie" :end1 cp))
                  (let* ((p (search +version-cookie-prefix+ s :start2 cp))
                         (ps (if p (+ p (length +version-cookie-prefix+))))
                         (pe (if p (position #\; s :start ps)))
                         (pe2 (if pe pe (length s)))
                         (val (if p (subseq s ps pe2))))
                    (when val
                      (log:debug "Version cookie found: [~A]" val)
                      (return (connect-to-version c val)))))
                (when (= 0 (length s))
                  (return (connect-to-version c)))
                (setf last-pos (1+ pos)))))))))

(defmethod attach ((from async-conn) (to async-conn))
  (with-slots (connected) from
    (setf connected to)
    (feed-connected from)))

(defmethod close-conn ((c async-conn) &key close abort)
  (with-slots (conn base connected written readed conn-id) c
    (let ((fd (socket-os-fd conn)))
      (ignore-errors (remove-fd-handlers base fd :read t :write t :error t))
      (log:debug "Closing connection to ~A (abort: ~A close: ~A) W: ~A R: ~A" conn-id abort close written readed)
      (if abort
          (close conn :abort t)
          (if close
              (close conn))))
    (remhash c *active-connections*)))

(defmethod try-write ((c async-conn))
  (with-slots (conn base fin write-registered write-buf written conn-id) c
    (handler-case
      (progn
        (if (not (empty-p write-buf))
            (multiple-value-bind (buf start end) (data-available write-buf)
              (let ((wb (send-to conn buf :start start :end end)))
                (log:debug "~A Written ~A bytes" conn-id wb)
                (incf written wb)
                (deallocate-space write-buf wb)))
            (progn
              (ignore-errors (remove-fd-handlers base (socket-os-fd conn) :write t))
              (setf write-registered nil)))
        (when (and fin (empty-p write-buf))
          (log:debug "Finalized connection ~A, closing" conn-id)
          (close-conn c :close t)))
      (socket-connection-reset-error ()
        (log:debug "Client ~A: connection reset by peer" conn-id)
        (close-conn conn))
      (isys:ewouldblock ()
        (log:debug "write-some-bytes: ewouldblock"))
      (isys:epipe ()
        (log:debug "Client ~A got hangup on write" conn-id)
        (close-conn conn)))))

(defmethod fin ((c async-conn) &optional err)
  (with-slots (fin base conn connected read-buf) c
    (when (not fin)
      (when err (log:debug "~A" err))
      (ignore-errors (remove-fd-handlers base (socket-os-fd conn) :read t))
      (setf fin t)
      (try-write c))))

(defmethod register-write ((c async-conn))
  (with-slots (conn base write-registered) c
    (when (not write-registered)
      (when (socket-connected-p conn)
        (set-io-handler base (socket-os-fd conn) :write (lambda (&rest args) (declare (ignore args)) (try-write c)))
        (setf write-registered t)))))

(defmethod push-to ((c async-conn) buf &key (start 0) (end (array-dimension buf 0)))
  (with-slots (write-buf) c
    (multiple-value-bind (buf1 start1 end1) (get-space write-buf (- end start))
      (let ((cw (min (- end start) (- end1 start1))))
        (when (> cw 0)
          (replace buf1 buf :start1 start1 :start2 start :end1 (+ start1 cw) :end2 (+ start cw))
          (allocate-space write-buf cw)
          (register-write c))
        cw))))

(defmethod feed-connected ((c async-conn))
  (with-slots (connected read-buf base fin conn) c
    (when (not (empty-p read-buf))
      (let ((conn-dt 0.01))7
        (labels ((schedule-push ()
                   (setf conn-dt (min 5.0 (* conn-dt 1.1)))
                   (add-timer base #'try-push conn-dt :one-shot t))
                 (try-push ()
                   (when connected
                     (with-slots (write-buf) connected
                       (if (empty-p read-buf)
                           (when fin
                             (fin connected))
                           (multiple-value-bind (buf start end) (data-available read-buf)
                             (when (not (full-p write-buf))
                               (let ((cw (push-to connected buf :start start :end end)))
                                 (when (> cw 0)
                                   (deallocate-space read-buf cw))))
                             (schedule-push)))))))
          (try-push))))))

(defmethod try-read ((c async-conn))
  (with-slots (fin conn base read-buf connected readed conn-id) c
    (when (and (not fin) (not (full-p read-buf)))
      (handler-case
        (multiple-value-bind (buf start end) (get-space read-buf)
          (when (> end start)
            (multiple-value-bind (buf rb) (receive-from conn :buffer buf :start start :end end)
              (declare (ignore buf))
              (when (= 0 rb)
                (error 'end-of-file))
              (log:debug "~A Readed ~A bytes" conn-id rb)
              (incf readed rb)
              (allocate-space read-buf rb)
              (feed-connected c))))
        (socket-connection-reset-error ()
          (fin c (format nil "Client ~A: connection reset by peer" conn-id)))
        (end-of-file ()
          (fin c (format nil "Client ~A produced end-of-file on a read" conn-id)))))))

(defmethod initialize-instance :after ((c async-conn) &key &allow-other-keys)
  (when (not (slot-boundp c 'conn))
    (error "Connection must be specified!"))
  (with-slots (conn base conn-id) c
    (set-io-handler base (socket-os-fd conn) :read  (lambda (&rest args) (declare (ignore args)) (try-read c)))
    (ignore-errors
      (multiple-value-bind (ip port) (remote-name conn)
        (setf conn-id (format nil "~A:~A" ip port)))))
  (setf (omg::gethash-lock c *active-connections*) (get-universal-time)))

(defun proxy (port) ;; Start a proxy server
  (let ((ebase (make-instance 'event-base)))
    (unwind-protect
      (handler-case
        (with-open-socket (srv :connect :passive
                               :address-family :internet
                               :type :stream
                               :ipv6 nil)
                               ;;:external-format 'unsigned-byte)
          (log:debug "Created socket: ~A[fd=~A]" srv (socket-os-fd srv))
          (bind-address srv +ipv4-unspecified+ :port port :reuse-addr t)
          (log:debug "Bound socket: ~A" srv)
          (listen-on srv :backlog 5)
          (log:info "Listening on socket bound to: ~A:~A" (local-host srv) (local-port srv))
          (set-io-handler ebase (socket-os-fd srv) :read
            (lambda (fd ev exc)
              (declare (ignorable ev exc))
              (let ((conn (accept-connection srv :wait t)))
                (log:debug "Connection accepted:" conn)
                (wait-for ebase (socket-connected-p conn)
                  (with-slots (conn-id) (make-instance 'selector-conn :conn conn :base ebase)
                    (log:info "Connection from ~A" conn-id))))))
          (log:info "Entering loop!")
          (loop do (ignore-errors (event-dispatch ebase)) (sleep 1)))
        (socket-address-in-use-error ()
          (log:info "Cannot start omgdaemon serever, address already in use!")))
      (progn
        (log:debug "Finalize!")
        (loop for c being each hash-key of *active-connections* do (close-conn c :abort t))
        (close ebase)))))

(defun run-daemon () ;; Toplevel function for daemon startup
  (setf omg::*giant-hash-lock* (bt:make-lock))
  (let ((devel-path (version-file-path +devel-version+)))
    (if (not (probe-file devel-path)) ;; If this is a first start, dump development version image
        (let ((pid (posix-fork)))
          (if (> pid 0)
              (osicat-posix:waitpid pid)
              (makimg devel-path))))
    (bt:start-multiprocessing)
    (setf *main-lock* (bt:make-lock))
    (setf *start-lock* (bt:make-lock))
    (loop while (not (ensure-version-working +devel-version+)))
    (disable-debugger)
    (loop do (proxy *proxy-port*) do
      (log:info "Proxy exited, restarting...")
      (sleep 1))))

(defun make-omg-daemon (port &key (swank-comm-style :spawn)) ;; Dump daemon image, specify proxy port
  (swank-loader:init
    :delete nil         ; delete any existing SWANK packages
    :reload nil         ; reload SWANK, even if the SWANK package already exists
    :load-contribs nil)
  (setf *proxy-port* port)
  (setf swank::*globally-redirect-io* t)
  (setf *swank-comm-style* swank-comm-style)
  (loop for i below 20 ;; Wait for swank shutdown
    while (position-if (lambda (s) (search "swank" s :test #'char-equal)) (mapcar #'bt:thread-name (bt:all-threads)))
    do (progn
         (log:info "Waiting for swank shutdown...")
         (sleep 1)))
  (if (position-if (lambda (s) (search "swank" s :test #'char-equal)) (mapcar #'bt:thread-name (bt:all-threads)))
      (log:info "SWANK NOT DEAD!!1111"))
  (kill-all-threads)
  (disable-debugger)
  (save-executable (merge-pathnames (make-pathname :name "omgdaemon")) #'run-daemon))

(defun make-docker-image (&key (tag 'omgdaemon) (sbcl-version "2.4.7"))
  (with-input-from-string
    (fd (format nil "FROM debian
RUN apt-get update && apt-get dist-upgrade -y
RUN apt-get install -y sbcl cl-quicklisp git build-essential ziproxy libzstd-dev libfixposix-dev
RUN cd /root && git clone -b sbcl-~A https://git.code.sf.net/p/sbcl/sbcl sbcl-sbcl &&\\
    cd sbcl-sbcl && ./make.sh --fancy --prefix=/usr && ./install.sh && cd / &&\\
    rm -fr /root/sbcl-sbcl
RUN adduser omg
RUN su -l omg -c 'ziproxy -d ; sbcl --load \"/usr/share/common-lisp/source/quicklisp/quicklisp.lisp\" \\
      --eval \"(quicklisp-quickstart:install :proxy \\\"http://127.0.0.1:8080/\\\")\" --non-interactive'
RUN su -l omg -c 'mkdir -p /home/omg/quicklisp/local-projects && cd /home/omg/quicklisp/local-projects &&\\
      git clone --recurse-submodules https://github.com/hemml/OMGlib.git'
RUN su -l omg -c 'ziproxy -d ; cd /home/omg && sbcl --eval \"(load \\\"/home/omg/quicklisp/setup.lisp\\\")\"\\
      --eval \"(ql:quickload :omgdaemon)\" --eval \"(omgdaemon:make-omg-daemon 8081)\" --non-interactive'
EXPOSE 8081 4008
CMD ziproxy -d ; while true; do su -l omg -c 'cd /home/omg && ./omgdaemon' ; sleep 1 ; done" sbcl-version))
    (run `(docker build :tag ,tag :pull :no-cache -) :input fd)))

(defun update-omg ()
  (run '(and (cd "/home/omg/quicklisp/local-projects/OMGlib")
             (git pull :rebase)))
  (require :omg))
