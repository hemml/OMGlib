(defpackage :omgui
  (:use cl omg jscl bordeaux-threads)
  (:export add-event-handler
           add-event-listener
           add-style
           add-youtube-player
           append-element
           async-bind
           allow-page-close
           bind-exit-values-for
           browser-case
           cache-vars
           check-element
           classic-worker
           close-current-dialog
           create-element
           dialog-ok
           disable-back-button
           disable-scroll
           dont-transfer
           dragabble-list
           dragabble-list-elements
           dragabble-list-insert
           dragabble-list-insert-position
           element-width
           element-height
           enable-back-button
           enable-scroll
           ensure-element
           execute-after
           find-widget
           gensym2
           get-dialog-data
           get-element-id
           idb-object-store
           if-idb-key
           in-service-worker
           indexed-db-add
           indexed-db-put
           indexed-db-get
           indexed-db-delete
           indexed-db-get-all-keys
           is-nan
           js-get-element-by-id
           jsceil
           jsfloor
           jstrunc
           jsln
           jslog
           jsmax
           jsmin
           js-parse-float
           jssin
           jscos
           jstan
           jsasin
           jsacos
           jsatan
           jsatan2
           jsrandom
           jssort
           kill
           load-js-script
           local-storage
           make-dialog
           make-dragabble-list
           make-js-function
           make-js-object
           make-pwa
           make-svg
           make-tab-form
           modal-dialog
           now
           oget-bind
           on-element-remove
           page-width
           page-height
           parent-element
           prevent-page-close
           register-hash-cb
           register-main-lambda
           remove-element
           rm-event-handler
           run-in-web-worker
           service-worker
           session-storage
           set-service-worker-uri-handler
           setup-indexed-db
           show-notification
           visible-width
           visible-height
           visible-left
           visible-top
           when-worker-free
           when-worker-ready
           winref
           with-promise
           with-self
           set-service-worker-uri-handler
           respond-with
           default-action
           uri-path
           store-to-buffer
           load-from-buffer))


(in-package :omgui)

(defvar *default-icon* nil)

(defun-f winref (name)
  (jscl::oget (jscl::%js-vref "self") name))

(defun-f is-nan (n)
  (funcall (winref "isNaN") n))

(defun-f js-parse-float (s)
  (funcall (winref "Number") (jscl::lisp-to-js s)))

(defun-f local-storage (key &optional def)
  (let ((vl ((jscl::oget (winref "localStorage") "getItem") (jscl::lisp-to-js key))))
    (if (jscl::js-null-p vl) def vl)))

(defun-f (setf local-storage) (val key)
  ((jscl::oget (winref "localStorage") "setItem") (jscl::lisp-to-js key) val))

(defun-f session-storage (key &optional def)
  (let ((vl ((jscl::oget (winref "sessionStorage") "getItem") (jscl::lisp-to-js key))))
    (if (jscl::js-null-p vl) def vl)))

(defun-f (setf session-storage) (val key)
  ((jscl::oget (winref "sessionStorage") "setItem") (jscl::lisp-to-js key) val))


(def-local-macro-f async-bind (vcmd &rest cod)
  (let* ((res (gensym))
         (rst (gensym))
         (vrs (car vcmd))
         (ll (if (listp vrs)
                `(,(car vrs) &optional ,@(cdr vrs) &rest ,rst)
                `(,vrs &rest ,rst))))
    `(progn
       (funcall (jscl::oget (jscl::%js-vref "OMG") "AsyncRPC2")
                (jscl::omg-write-to-string ',(omg::get-id omg::*current-session*)
                                           ',(intern (package-name *package*))
                                           (list ',(caadr vcmd) (list ,@(cdadr vcmd))))
                (lambda (,res)
                  (destructuring-bind ,ll ,res
                    ,@cod)))
       nil)))


(defun-f make-svg (&rest cod)
  (labels ((process-cmd (cmd args)
             (let* ((symname (symbol-name cmd))
                    (symdown (string-downcase symname))
                    (el ((jscl::oget (jscl::%js-vref "document") "createElementNS")
                         "http://www.w3.org/2000/svg"
                         (if (equal symname (string-upcase symname))
                             symdown
                             symname))))
               ((jscl::oget el "setAttribute") "xmlns" "http://www.w3.org/2000/svg")
               (process-cmd-tail el args)))
           (process-cmd-tail (el cod)
             (if cod
                 (cond ((symbolp (car cod))
                        (let* ((path (js-string-split (symbol-name (car cod)) #\.))
                               (obj (deep-oget-1 el path))
                               (fld (car (last path))))
                          (if (> (length path) 1)
                              (jscl::oset (cadr cod) obj fld)
                              ((jscl::oget el "setAttribute")
                               (symbol-name (car cod))
                               (cadr cod)))
                          (process-cmd-tail el (cddr cod))))
                       ((listp (car cod))
                        (progn
                          ((jscl::oget el "appendChild")
                           (process-cmd (caar cod) (cdar cod)))
                          (process-cmd-tail el (cdr cod))))
                       ((stringp (car cod))
                        (let ((txt ((jscl::oget (jscl::%js-vref "document") "createTextNode") (car cod))))
                          ((jscl::oget el "appendChild") txt)
                          (process-cmd-tail el (cdr cod))))
                       (t (progn
                            ((jscl::oget el "appendChild") (car cod))
                            (process-cmd-tail el (cdr cod)))))
                 el)))
    (process-cmd :svg cod)))

(defun-f system-font ()
  "The system font for dialogs, etc. Defined as a function because functions are automatically updated in browsers."
  "1.2em Gill Sans,Gill Sans MT,Calibri,sans-serif")

(defun-f jslog (&rest args)
  "Log function for js"
  (apply (jscl::oget (jscl::%js-vref "console") "log") args)
  nil)

(defun-f jsrandom ()
  "Return a random number in 0..1 range"
  (funcall (jscl::oget (jscl::%js-vref "Math") "random")))

(defun-f jsfloor (x)
  "Math.floor function"
  ((jscl::oget (jscl::%js-vref "Math") "floor") x))

(defun-f jsceil (x)
  "Math.ceil function"
  ((jscl::oget (jscl::%js-vref "Math") "ceil") x))

(defun-f jstrunc (x)
  "Math.trunc function"
  ((jscl::oget (jscl::%js-vref "Math") "trunc") x))

(defun-f jsln (arg)
  "Math.log function"
  ((jscl::oget (jscl::%js-vref "Math") "log") arg))

(defun-f jssin (arg)
  "Math.sin function"
  ((jscl::oget (jscl::%js-vref "Math") "sin") arg))

(defun-f jscos (arg)
  "Math.cos function"
  ((jscl::oget (jscl::%js-vref "Math") "cos") arg))

(defun-f jstan (arg)
  "Math.tan function"
  ((jscl::oget (jscl::%js-vref "Math") "tan") arg))

(defun-f jsasin (arg)
  "Math.asin function"
  ((jscl::oget (jscl::%js-vref "Math") "asin") arg))

(defun-f jsacos (arg)
  "Math.acos function"
  ((jscl::oget (jscl::%js-vref "Math") "acos") arg))

(defun-f jsatan (arg)
  "Math.atan function"
  ((jscl::oget (jscl::%js-vref "Math") "atan") arg))

(defun-f jsatan2 (y x)
  "Math.atan2 function"
  ((jscl::oget (jscl::%js-vref "Math") "atan2") y x))

(defun-f jsmin (&rest args)
  "Math.min function"
  (apply (jscl::oget (jscl::%js-vref "Math") "min") args))

(defun-f jsmax (&rest args)
  "Math.max function"
  (apply (jscl::oget (jscl::%js-vref "Math") "max") args))

(defun-f js-get-element-by-id (id)
  "Get DOM object by ID (must not be called on host!)"
  ((jscl::oget (jscl::%js-vref "document") "getElementById") id))

(defun-f jssort (arr &optional (fn (lambda (x y) (> x y))))
  "Sort an array"
  ((jscl::oget arr "sort") fn))

(defun-f check-element (id)
  "Check if element with ``id'' exists in DOM"
  (not (jscl::js-null-p (js-get-element-by-id id))))

(defun-f random-id ()
  "Get an unique random string to use as a DOM id"
  (let* ((chrs "ABCDEFGHIJKLMOPQRSTUVWXYZ")
         (id (map 'string
                  (lambda (x)
                    (declare (ignore x))
                    (char chrs (jsfloor (* (jsrandom) (length chrs)))))
                  (make-string 8))))
    (if (check-element id) (random-id) id)))

(defun-f get-element-id (el)
  "Returns an id of the element el"
  (let ((id (jscl::oget el "id")))
    (if (> (length id) 0)
        id
        (let ((nid (random-id)))
          (setf (jscl::oget el "id") nid)
          nid))))

(defun-f js-string-split (path ch)
  "Split a string path by the character ch, much, much fater then JSCL function"
  (let* ((pos (position ch path))
         (p0 (subseq path 0 pos)))
    (cons p0 (if pos (js-string-split (subseq path (+ pos 1)) ch) (list)))))

(defun-f deep-oget-1 (el path)
  "Get a value of el.the.list.of.the.property.path"
  (if (cdr path)
      (deep-oget-1 (jscl::oget el (car path)) (cdr path))
      el))

(defun-f create-element (typ &rest args)
  "Create and return a DOM element type ``typ'' and specified attributes: (create-element \"A\" '|href| \"http://google.com\")"
  (let ((el ((jscl::oget (jscl::%js-vref "document") "createElement") typ)))
    (loop for (k v) on args by (function cddr) do
      (labels ((app-el (nel)
                 (append-element (if (stringp v)
                                     ((jscl::oget (jscl::%js-vref "document") "createTextNode") nel)
                                     nel)
                                 el)))
        (cond ((equal k :append-element)
               (app-el v))
              ((equal k :append-elements)
               (map nil #'app-el v))
              ((equal k :add-style)
               (add-style el v))
              ((equal (symbol-name k) (string-upcase (symbol-name k)))
               ((jscl::oget el "setAttribute") (symbol-name k) v))
              (t (let* ((path (js-string-split (symbol-name k) #\.))
                        (obj (deep-oget-1 el path))
                        (fld (car (last path))))
                   (jscl::oset v obj fld))))))
    el))

(defun-f append-element (el &optional parent)
  "Append the element el as a child to the parent"
  (let ((el1 (if (stringp el)
                 ((jscl::oget (jscl::%js-vref "document") "createTextNode") el)
                 el)))
    (if parent
      ((jscl::oget parent "appendChild") el1)
      ((jscl::oget (jscl::%js-vref "document")
                   "body"
                   "appendChild")
       el1))
    el1))

(defun-f parent-element (el)
  "Get parent of the element el"
  (jscl::oget el "parentNode"))

(defun-f remove-element (el)
  "Remove the element el from it's parent children"
  ((jscl::oget (parent-element el) "removeChild") el))

(defun-f element-width (el)
  "Get element width in pixels"
  (jscl::oget el "clientWidth"))

(defun-f element-height (el)
  "Get element height in pixels"
  (jscl::oget el "clientHeight"))

(defun-f page-width ()
  "Get the browser page width"
  (jsmax (jscl::oget (jscl::%js-vref "document") "body" "scrollWidth")
         (jscl::oget (jscl::%js-vref "document") "documentElement" "scrollWidth")
         (jscl::oget (jscl::%js-vref "document") "body" "offsetWidth")
         (jscl::oget (jscl::%js-vref "document") "documentElement" "offsetWidth")
        (jscl::oget (jscl::%js-vref "document") "documentElement" "clientWidth")))

(defun-f page-height ()
  "Get the browser page height"
  (jsmax (jscl::oget (jscl::%js-vref "document") "body" "scrollHeight")
         (jscl::oget (jscl::%js-vref "document") "documentElement" "scrollHeight")
         (jscl::oget (jscl::%js-vref "document") "body" "offsetHeight")
         (jscl::oget (jscl::%js-vref "document") "documentElement" "offsetHeight")
         (jscl::oget (jscl::%js-vref "document") "documentElement" "clientHeight")))

(defun-f visible-width ()
  "Get the browser page visible width"
  (jscl::oget (jscl::%js-vref "document") "body" "clientWidth"))

(defun-f visible-height ()
  "Get the browser page visible height"
  (jscl::oget (jscl::%js-vref "document") "body" "clientHeight"))

(defun-f visible-left ()
  "Get the browser page left coordinate"
  (jscl::oget (jscl::%js-vref "document") "body" "scrollLeft"))

(defun-f visible-top ()
  "Get the browser page top coordinate"
  (jscl::oget (jscl::%js-vref "document") "body" "scrollTop"))

(defparameter-f *scroll-disabled* nil)

(defun-f disable-scroll ()
  "Disable browser page scroll"
  (if (not *scroll-disabled*)
      (progn
        (setf (jscl::oget (jscl::%js-vref "document") "body" "style" "overflow") "hidden")
        (setf *scroll-disabled* t)
        t)))

(defun-f enable-scroll ()
  "Enable browser page scroll"
  (if *scroll-disabled*
      (progn
        (setf (jscl::oget (jscl::%js-vref "document") "body" "style" "overflow") "")
        (setf *scroll-disabled* nil)
        t)))

(defun-f curtain ()
  "Create a curtain, to shadow elements while modal dialog active"
  (create-element "div" :|style.opacity| "80%"
                        :|style.background| "white"
                        :|style.width| (page-width)
                        :|style.height| (page-height)
                        :|style.position| "absolute"
                        :|style.top| "0"
                        :|style.left| "0"
                        :|class| "blurbg"
                        :|zIndex| 100000))

(defun-f dialog-frame ()
  "Create a modal dialog outer frame"
  (create-element "div" :|style.opacity| "0"
                        :|style.border| "0.1em solid black"
                        :|style.border-radius| "0.5em"
                        :|style.padding| "0"
                        :|style.overflowX| "hidden"
                        :|style.width| "auto"
                        :|style.background| "#fffff0"
                        :|style.display| "inline-block"
                        :|style.font| (system-font)
                        :|style.boxShadow| "0.5em 0.5em 1em gray"
                        :|style.position| "absolute"
                        :|omgwidget| "dialog"
                        :|zIndex| 200000))

(defun-f dialog-header (header)
  "Create a modal dialog header"
  (let* ((hdr (create-element "div" :|style.width| "auto"
                                    :|style.background| "#a0f0a0"
                                    :|style.padding| "0.5em"
                                    :|style.minHeight| "1em"
                                    :|innerHTML| (if header header "")))
         (close-btn (create-element "div" :|style.width| "0.75em"
                                          :|style.height| "0.75em"
                                          :|style.margin| "0"
                                          :|style.borderRadius| "0.2em"
                                          :|style.background| "red"
                                          :|style.float| "right"
                                          :|style.boxShadow| "0 0 0.5em grey"
                                          :|style.cursor| "pointer"
                                          :|title| "close"
                                          :|onclick| #'close-current-dialog)))
    (append-element close-btn hdr)
    hdr))

(defun-f execute-after (tim cb)
  "Schedule an execution of cb after tim seconds"
  (funcall (jscl::%js-vref "setTimeout") cb (* tim 1000.0)))

(defparameter-f *dialog-stack* nil)

(defun-f form-line (key txt &optional params)
  "Create modal dialog table line"
  (let* ((row (create-element "div" :|style.display| "table-row"
                                    :|style.width| "auto"))
         (c1 (create-element "div" :|style.display| "table-cell"
                                   :|style.padding| "0.3em"
                                   :|style.width| "auto"
                                   :|style.text-align| "right"
                                   :|style.fontWeight| "lighter"
                                   :|style.verticalAlign| "bottom"
                                   :|innerHTML| txt))
         (c2 (create-element "div" :|style.display| "table-cell"
                                   :|style.padding| "0.3em"
                                   :|style.width| "50%"
                                   :|style.verticalAlign| "bottom"))
         (typ (getf params :type))
         (def1 (getf params :default))
         (def (if def1 def1 ""))
         (inp (create-element "input" :|type| (if typ typ "text")
                                      :|value| def
                                      :|style.font| "inherit"
                                      :|style.fontStyle| "italic"
                                      :|style.fontWeight| "lighter"
                                      :|style.width| "100%"))
         (last-val def)
         (cb (getf params :filter))
         (current-dialog (car *dialog-stack*))
         (data (cdr (assoc :data current-dialog))))
    (if (not (assoc key data))
        (progn
          (push (cons key def) data)
          (setf (cdr (assoc :data current-dialog))
                data)))
    (labels ((tcb (ev) ;; the callback is called after every field update
               (execute-after 0
                 (lambda ()
                   (let ((val (jscl::oget inp "value")))
                     (if (not (equal last-val val))
                       (let ((new-val (if cb (funcall cb val) val))
                             (current-dialog (car *dialog-stack*))
                             (data (cdr (assoc :data current-dialog))))
                         (setf last-val (if (stringp new-val) new-val val))
                         (if (stringp new-val)
                             (let ((selection-start (jscl::oget inp "selectionStart"))
                                   (selection-end (jscl::oget inp "selectionEnd")))
                               (setf (jscl::oget inp "value") new-val)
                               (if (not (assoc key data))
                                   (push (cons key new-val) data)
                                   (setf (cdr (assoc key data)) new-val))
                               (setf (cdr (assoc :data current-dialog)) data) ;; JSCL bug workaround
                               ((jscl::oget inp "setSelectionRange")
                                selection-start
                                selection-end))))))))
               t))
      (setf (jscl::oget inp "onchange") #'tcb)
      (setf (jscl::oget inp "onkeyup") #'tcb)
      (setf (jscl::oget inp "onkeydown") #'tcb)
      (setf (jscl::oget inp "onpaste") #'tcb)
      (setf (jscl::oget inp "ondrop") #'tcb))
    (append-element inp c2)
    (append-element c1 row)
    (append-element c2 row)
    row))

(defun-f form-button-row (btns)
  "Create a row with buttons for dialog"
  (let* ((row (create-element "div" :|style.display| "table-caption"
                                    :|style.width| "auto"
                                    :|style.captionSide| "bottom"))
         (cl (create-element "div" :|style.display| "flex"
                                   :|style.width| "100%"
                                   :|style.height| "100%"
                                   :|style.text-align| "center"
                                   :|style.justifyContent| "center"
                                   :|style.alignItems| "center"
                                   :|style.paddingBottom| "0.5em")))
    (map nil
      (lambda (b)
        (cond ((listp b)
               (append-element
                 (create-element "button" :|innerHTML| (car b)
                                          :|style.marginBottom| "0.3em"
                                          :|style.marginLeft| "1em"
                                          :|style.marginRight| "1em"
                                          :|onclick| (lambda (ev)
                                                       (funcall (eval (cadr b)))))
                 cl))))
      btns)
    (append-element cl row)
    row))

(defun-f dialog-table (lines)
  "Create modal dialog table"
  (let ((tbl (create-element "div" :|style.display| "table"
                                   :|style.width| "auto"
                                   :|style.padding| "1em")))
    (loop for (k v) on lines by (function cddr) do
      (if (equal k :buttons)
          (append-element (form-button-row v) tbl)
          (if (listp v)
            (cond ((stringp (car v))
                   (append-element (form-line k (car v) (cdr v)) tbl)))
            (append-element (form-line k v) tbl))))
    tbl))

(defparameter *dialog-wait-list* (make-hash-table))

(defun-r dialog-wl-send (id dat)
  (let ((sem (gethash id *dialog-wait-list*)))
    (if sem
        (progn
          (setf (gethash id *dialog-wait-list*)
                (remove-if (lambda (x) (equal (car x) 'dialog-id)) dat))
          (signal-semaphore sem))))
  nil)

(defun-f close-current-dialog (&optional ev no-sem)
  (let ((current-dialog (pop *dialog-stack*)))
    (if (assoc :scroll-disabled current-dialog)
        (enable-scroll))
    (let* ((outer (cdr (assoc :outer current-dialog)))
           (curtain (cdr (assoc :curtain current-dialog)))
           (dat (cdr (assoc :data current-dialog)))
           (id (assoc 'dialog-id dat)))
      (if outer
          (remove-element outer))
      (if curtain
          (remove-element curtain))
      (if (and (cdr id) (not no-sem))
          (dialog-wl-send (cdr id) nil)))))

(defun-f get-dialog-data ()
  (cdr (assoc :data (car *dialog-stack*))))

(defun-f make-dialog (header-text dialog-text &key lines id)
  "Create modal dialog with header, text and lines. "
  (let* ((curtain (curtain))
         (outer (dialog-frame)))
    (setf *dialog-stack*
          (cons (list (cons :scroll-disabled (disable-scroll))
                      (cons :curtain curtain)
                      (cons :outer outer)
                      (cons :data (list (cons 'dialog-id id))))
                *dialog-stack*))
    (let* ((hdr (dialog-header header-text))
           (tbl (dialog-table lines))
           (txt (create-element "div" :|innerHTML| dialog-text
                                      :|style.fontWeight| "lighter"
                                      :|style.marginTop| "1em"
                                      :|style.marginLeft| "1em"
                                      :|style.marginRight| "1em"
                                      :|style.width| "auto")))
      (append-element hdr outer)
      (if dialog-text (append-element txt outer))
      (append-element tbl outer)
      (append-element curtain)
      (append-element outer)

      (setf (jscl::oget outer "style" "left")
            (jscl::concat
              (write-to-string
                (+ (visible-left)
                   (jsfloor (/ (- (visible-width)
                                  (element-width outer))
                               2))))
              "px"))
      (setf (jscl::oget outer "style" "top")
            (jscl::concat
              (write-to-string
                (+ (visible-top)
                   (jsfloor (/ (- (visible-height)
                                  (element-height outer))
                               2))))
             "px"))
      (setf (jscl::oget outer "style" "opacity") "100%")
      (get-element-id curtain))))

(defun-f dialog-ok ()
  (let* ((current-dialog (car *dialog-stack*))
         (dat (cdr (assoc :data current-dialog)))
         (id (assoc 'dialog-id dat)))
    (close-current-dialog nil t)
    (if id
        (dialog-wl-send (cdr id) dat))))

(defmacro modal-dialog (header-text dialog-text &key lines)
  `(if (current-session-id)
       (let* ((id (intern (symbol-name (omg::random-key *dialog-wait-list*)) :omgui))
              (sem (make-semaphore))
              (ht ,header-text)
              (dt ,dialog-text)
              (lins ,lines))
         (setf (gethash id *dialog-wait-list*) sem)
         (if (remote-exec `(make-dialog ,ht ,dt :lines ',lins :id ',id))
             (progn
               (wait-on-semaphore sem)
               (let ((res (gethash id *dialog-wait-list*)))
                 (remhash id *dialog-wait-list*)
                 res))))))

(defun-f load-js-script (src &optional onload)
  (let ((el (create-element "script" :|src| src)))
    (if onload
        (setf (jscl::oget el "onload") onload))
    (append-element el)))

(defun-f make-js-function (name code)
  (setf (jscl::oget (jscl::%js-vref "self") name) code))

(defun-f make-js-object (&rest plist)
  (let ((obj (jscl::new)))
    (loop for (k v) on plist by #'cddr do (setf (jscl::oget obj (symbol-name k)) v))
    obj))

(defun-f allow-page-close ()
  (setf *disable-page-unload* nil)
  nil)

(defparameter-f *disable-back* nil)
(defparameter-f *backcnt* 0)
(defparameter-f *fback* t)
(defparameter-f *onpopstate-installed* nil)

(defun-f disable-back-button (&optional cb)
  (if (not *onpopstate-installed*)
    (progn
      ((jscl::oget
         (jscl::%js-vref "history")
         "pushState") "" "" (jscl::oget (jscl::%js-vref "self") "location" "href"))
      ((jscl::oget (jscl::%js-vref "history") "back"))
      (setf *onpopstate-installed* t)
      (setf (jscl::oget (jscl::%js-vref "self") "onpopstate")
            (lambda (ev)
              (let ((bcnt *backcnt*) ;; JSCL bug workaround
                    (fb *fback*))
                (setf *backcnt* (+ 1 *backcnt*))
                (if *disable-back*
                    ((jscl::oget (jscl::%js-vref "history") "forward")))
                (if (equal bcnt 1)
                    (progn
                      (if (and cb (not fb)) (funcall cb))
                      (setf *fback* nil)
                      (setf *backcnt* 0))))))))
  (setf *disable-back* t)
  nil)

(defun-f enable-back-button ()
  (setf *disable-back* nil)
  ((jscl::oget (jscl::%js-vref "history") "back"))
  nil)

(defparameter-f *beforeunload-installed* nil)
(defparameter-f *disable-page-unload* nil)

(defun-f prevent-page-close ()
  (if (not *beforeunload-installed*)
    (progn
      ((jscl::oget
          (jscl::%js-vref "self")
          "addEventListener") "beforeunload"
                              (lambda (ev)
                                (if *disable-page-unload*
                                  (progn
                                    ((jscl::oget ev "preventDefault"))
                                    (setf (jscl::oget ev "returnValue") "")))))
      (setf *beforeunload-installed* t)))
  (setf *disable-page-unload* t)
  nil)

(defparameter-f *youtube-js-loaded* nil)

(defun-f add-youtube-player (element &key onready onstatechange onqualitychange
                              onratechange onerror onapichange width height video-id)
  (let ((cfg (make-js-object
               :|events| (make-js-object
                           :|onReady| (lambda (ev)
                                        (setf *youtube-js-loaded* t)
                                        (if onready (funcall onready ev)))
                           :|onStateChange| (lambda (ev)
                                               (if onstatechange (funcall onstatechange ev)))
                           :|onPlaybackQualityChange| (lambda (ev)
                                                        (if onqualitychange (funcall onqualitychange ev)))
                           :|onPlaybackRateChange| (lambda (ev)
                                                     (if onratechange (funcall onratechange ev)))
                           :|onError| (lambda (ev)
                                        (if onerror (funcall onerror ev)))
                           :|onApiChange| (lambda (ev)
                                            (if onapichange (funcall onapichange ev)))))))
    (if width (setf (jscl::oget cfg "width") width))
    (if height (setf (jscl::oget cfg "height") height))
    (if video-id (setf (jscl::oget cfg "videoId") video-id))
    (labels ((make-player ()
               (jscl::make-new
                 (jscl::oget (jscl::%js-vref "YT") "Player")
                 element
                 cfg)))
      (if (not *youtube-js-loaded*)
        (progn
          (make-js-function "onYouTubeIframeAPIReady"
            (lambda ()
              (jslog "YouTube API loaded")
              (setf *youtube-js-loaded* t)
              (make-player)))
          (load-js-script "https://www.youtube.com/iframe_api"))
        (make-player))))
  nil)


(defparameter-f *hash-change-cbs* nil)

(defun-f register-hash-cb (hsh cb)
  (labels ((mcb (&optional ev)
             (let* ((hs (jscl::oget (jscl::%js-vref "location") "hash"))
                    (cb (assoc hs *hash-change-cbs* :test #'equal)))
               (if cb (funcall (cdr cb))))))
    (let ((need-mcb (not *hash-change-cbs*)))
      (push (cons hsh cb) *hash-change-cbs*)
      (if need-mcb
          (progn
            (setf (jscl::oget (jscl::%js-vref "self") "onhashchange")
                  #'mcb)
            (mcb)))))
  nil)

(defun-f gensym2 (&rest args)
  (intern (symbol-name (apply #'gensym args))))

;; use:
;; (browser-case
;;   (:safari (jslog "Safari detected!"))
;;   ((:firefox :chrome) (jslog "FF or Chrome!"))
;;   (:opera (jslog "Opera!"))
;;   (:edge (jslog "Edge!")))
;;   (t (jslog "Unknown browser!"))

(defmacro-f browser-case (&rest cod)
  (let ((agent (gensym2))
        (browser (gensym2)))
    `(let* ((,agent (string-downcase (jscl::oget (jscl::%js-vref "navigator") "userAgent")))
            (,browser (cond ((or (search "chrome" ,agent)
                                 (search "chromium" ,agent)
                                 (search "crios" ,agent))
                             :chrome)
                            ((or (search "firefox" ,agent)
                                 (search "fxios" ,agent))
                             :firefox)
                            ((search "safari" ,agent)
                             :safari)
                            ((search "opr" ,agent)
                             :opera)
                            ((search "edg" ,agent)
                             :edge)
                            (t nil))))
       (cond
         ,@(mapcar
             (lambda (c)
               `(,(if (listp (car c))
                      `(or ,@(mapcar
                                (lambda (s)
                                  `(equal ,browser ,s))
                                (car c)))
                      (if (equal t (car c))
                          (car c)
                          `(equal ,browser ,(car c))))
                  ,@(cdr c)))
             cod)))))

(defparameter-f *style-cache* nil)

(defun-f add-style (el css-text)
  (let* ((sid (assoc css-text *style-cache* :test #'equal))
         (chrs "ABCDEFGHIJKLMOPQRSTUVWXYZ")
         (clsid (if sid
                    (cdr sid)
                    (map 'string
                      (lambda (x)
                        (declare (ignore x))
                        (char chrs (jsfloor (* (jsrandom) (length chrs)))))
                      (make-string 8)))))
    (if (not sid)
        (progn
          (append-element (create-element "style" :|innerHTML| (jscl::concat "." clsid css-text))
                          (jscl::oget (jscl::%js-vref "document") "head"))
          (push (cons css-text clsid) *style-cache*)))
    ((jscl::oget el "classList" "add") clsid)))

(defparameter-f *notification-container* nil)

(defun-f element-on-page-p (el)
  (or (equal el (jscl::%js-vref "document"))
      (and (not (jscl::js-null-p el))
           (element-on-page-p (jscl::oget el "parentNode")))))

(defparameter-f *global-observer-handlers* nil)

(defun-f on-element-remove (el cb)
  (if (not *global-observer-handlers*)
      (let ((obs (jscl::make-new (winref "MutationObserver")
                    (lambda (&rest args)
                      (let ((rems nil))
                        (map nil
                          (lambda (eh)
                            (if (not (element-on-page-p (car eh)))
                                (if (not (funcall (cdr eh) (car eh)))
                                    (push eh rems))))
                          *global-observer-handlers*)
                        (setf *global-observer-handlers*
                              (remove-if
                                (lambda (x) (position x rems :test #'equal))
                                *global-observer-handlers*))
                        nil)))))
        ((jscl::oget obs "observe") (jscl::oget (jscl::%js-vref "document") "body") (make-js-object :|childList| t))))
  (push (cons el cb) *global-observer-handlers*))

(defun-f show-notification (header body &key period check)
  (let* ((frame (create-element "div" :|style.border| "1pt solid black"
                                      :|style.position| "relative"
                                      :|style.margin| "0.5em"
                                      :|style.background| "white"
                                      :|style.width| "15em"
                                      :|style.borderRadius| "0.2em"
                                      :|style.minHeight| "5em"
                                      :|style.boxShadow| "0 0 1em grey"
                                      :|style.right| 0
                                      :|omgwidget| "notification"))
         (head-line (create-element "div" :|style.left| 0
                                          :|style.right| 0
                                          :|style.top| 0
                                          :|style.padding| "0.25em"
                                          :|style.position| "relative"
                                          :|style.font| (omgui::system-font)))
         (close-btn (create-element "div" :|style.borderRadius| "0.2em"
                                          :|style.background| "red"
                                          :|style.right| "0.25em"
                                          :|style.top| "0.25em"
                                          :|style.width| "0.5em"
                                          :|style.aspectRatio| 1
                                          :|style.boxShadow| "0 0 0.5em grey"
                                          :|style.cursor| "pointer"
                                          :|style.position| "absolute"
                                          :|title| "close"
                                          :|onclick| (lambda (ev)
                                                       (remove-element frame))))
         (body-cont (create-element "div" :|style.padding| "0.25em"
                                          :|style.font| (omgui::system-font)))
         (ncont (if *notification-container*
                    *notification-container*
                    (create-element "div" :|style.position| "absolute"
                                          :|style.right| 0
                                          :|style.top| 0))))
    (if (not *notification-container*)
        (progn
          (setf *notification-container* ncont)
          (append-element ncont)))
    (append-element header head-line)
    (append-element close-btn head-line)
    (append-element head-line frame)
    (append-element body body-cont)
    (append-element body-cont frame)
    (append-element frame ncont)
    (if period
        (on-element-remove frame
          (lambda (el)
            (if (or (not check) (funcall check))
                (progn
                  (execute-after period
                    (lambda ()
                      (append-element frame ncont)))
                  t)
                nil))))))

(defun-f find-widget (ev &optional type)
  (labels ((get-wg (el)
             (let ((w (jscl::oget el "omgwidget")))
               (if (and w (or (not type) (equal w type)))
                   el
                   (let ((par (jscl::oget el "parentNode")))
                     (if (and par (not (jscl::js-null-p par)))
                         (get-wg par)))))))
    (get-wg (jscl::oget ev "target"))))

(def-local-macro-f ensure-element (e &rest body)
  (let ((fn (gensym))
        (e1 (gensym)))
    `(labels ((,fn ()
                (let ((,e1 ,e))
                  (if (and ,e1 (> (jscl::oget ,e1 "clientWidth") 0))
                      (progn ,@body)
                      (execute-after 0.1 #',fn)))))
       (,fn))))

(defparameter-f *global-event-handlers* nil)

(defun-f add-event-handler (path handler)
  (let ((handlers (assoc path *global-event-handlers*  :test #'equal)))
    (if (not handlers)
        (let* ((path-l (omgui::js-string-split path #\.))
               (obj (omgui::deep-oget-1 (jscl::%js-vref "self") (butlast path-l))))
          (setf (jscl::oget obj (car (last path-l)))
                (lambda (ev)
                  (map nil
                    (lambda (cb) (funcall cb ev))
                    (cdr (assoc path *global-event-handlers* :test #'equal)))))
          (push (cons path (list handler)) *global-event-handlers*))
        (push handler (cdr handlers)))))

(defun-f rm-event-handler (path handler)
  (setf *global-event-handlers*
        (mapcar
          (lambda (hnd)
            (if (equal path (car hnd))
                (cons path
                      (remove-if
                        (lambda (h)
                          (equal h handler))
                        (cdr hnd)))
                hnd))
          *global-event-handlers*)))

(defun-f make-tab-form (tab-list)
  (let* ((tab-div (create-element "div" :|style.display| "flex"
                                        :|style.position| "relative"))
         (content (create-element "div" :|style.padding| "1em"
                                        :|style.borderLeft| "1px solid gray"
                                        :|style.borderRight| "1px solid gray"
                                        :|style.borderBottom| "1px solid gray"))
         (outer (create-element "div"
                  :append-element tab-div
                  :append-element content))
         (tbs nil))
    (map nil
      (lambda (tab num)
        (append-element
          (let ((tb (create-element "div" :|style.borderTop| "1px solid gray"
                                          :|style.borderLeft| "1px solid gray"
                                          :|style.borderRight| "1px solid gray"
                                          :|style.borderBottom| (if (= 0 num) "none" "1px solid gray")
                                          :|style.background| (if (= 0 num) "#ffffff" "#f0f0f0")
                                          :|style.paddingTop| "0.25em"
                                          :|style.paddingBottom| "0.25em"
                                          :|style.paddingLeft| "1em"
                                          :|style.paddingRight| "1em"
                                          :|style.borderRadius| "0.5em 0.5em 0 0"
                                          :|style.display| "inline-block"
                      :append-element (create-element "a" :|href| "#"
                                        :append-element (car tab)
                                        :|onclick| (lambda (ev)
                                                     (map nil
                                                       (lambda (tab)
                                                         (setf (jscl::oget (cdr tab) "style" "display") "none")
                                                         (setf (jscl::oget (cdr tab) "style" "display") "none"))
                                                       tab-list)
                                                     (map nil
                                                       (lambda (th tn)
                                                         (setf (jscl::oget th "style" "background")
                                                               (if (= tn num) "#ffffff" "#f0f0f0"))
                                                         (setf (jscl::oget th "style" "borderBottom")
                                                               (if (= tn num) "none" "1px solid gray")))
                                                       tbs
                                                       (loop for i below (length tbs) collect i))
                                                     (setf (jscl::oget (cdr tab) "style" "display") "block")
                                                     nil)))))
            (setf tbs `(,@tbs ,tb))
            (setf (jscl::oget (cdr tab) "style" "display") (if (= num 0) "block" "none"))
            (append-element (cdr tab) content)
            tb)
          tab-div))
      tab-list
      (loop for i below (length tab-list) collect i))
    (append-element (create-element "div" :|style.display| "inline-block"
                                          :|style.width| "auto"
                                          :|style.borderBottom| "1px solid gray"
                                          :|style.flexGrow| 1)
                    tab-div)
    outer))


(defun-f make-dragabble-list (elements &key (outer-type "div") reorder-cb on-drag insert-el)
  (let* ((trans "all 0.5s linear")
         (insert-gizmo (if insert-el
                           (create-element outer-type :|style.position| "absolute"
                                                      :|style.right| "100%"
                                                      :|style.z-index| 20
                                                      :|style.display| "block"
                                                      :|style.visibility| (if elements "hidden" "visible")
                                                      :|style.transition| "opacity 0.5s linear"
                             :append-element insert-el)))
         (insert-gizmo-width nil)
         (inner (create-element "div" :|style.position| "relative"))
         (moving-el nil)
         (moving-el-height nil)
         (moving-el-num nil)
         (max-top nil)
         (last-shift nil)
         (els-pos nil)
         (els-mid nil)
         (element-stack nil)
         (client-y0 nil)
         (page-y0 nil)
         (cur-top nil)
         (last-min-pos nil))
    (if insert-el
        (progn
          (add-style insert-gizmo ":hover {cursor: pointer;}")
          (setf (jscl::oget insert-gizmo "onmouseover")
                (lambda (ev)
                  (if (and (not moving-el) last-min-pos)
                      (progn
                        (setf (jscl::oget inner"omg-insert-position") last-min-pos)
                        (loop for i from (max 0 last-min-pos) to (min last-min-pos (- (length element-stack) 1)) do
                          (progn
                            (setf (jscl::oget (nth i element-stack) "style" "transition") "all 0.1s linear")
                            (setf (jscl::oget (nth i element-stack) "style" "transform")
                                  (if (< i last-min-pos)
                                      "translateY(-0.5em)"
                                      "translateY(0.5em)"))))))))
          (setf (jscl::oget insert-gizmo "onmouseout")
                (lambda (ev)
                  (if (and (not moving-el) last-min-pos)
                      (progn
                        (setf (jscl::oget inner "omg-insert-position") nil)
                        (map nil
                          (lambda (el)
                            (setf (jscl::oget el "style" "transform") "translateY(0)"))
                          element-stack)))))
          (append-element insert-gizmo inner)
          (setf (jscl::oget inner "omg-insert-position") nil)))
    (labels ((set-els-pos ()
               (setf els-pos (mapcar
                               (lambda (el)
                                 (let ((top (jscl::oget el "offsetTop")))
                                   (cons top (+ top (jscl::oget el "offsetHeight")))))
                               element-stack))
               (setf els-mid (mapcar
                               (lambda (pos)
                                 (* 0.5 (+ (car pos) (cdr pos))))
                               els-pos)))
             (up-handler (ev)
               (if moving-el
                   (progn
                     (setf (jscl::oget moving-el "style" "top") 0)
                     (if insert-el (setf (jscl::oget insert-gizmo "style" "display") "block"))
                     (map nil
                          (lambda (el)
                            (if (not (equal el moving-el))
                                (progn
                                  (setf (jscl::oget el "style" "transition") "none")
                                  (setf (jscl::oget el "style" "transform") "translateY(0)"))
                                (setf (jscl::oget el "style" "transform")
                                      (format nil "translateY(~Apx)" (- cur-top (car (nth last-shift els-pos)))))))
                          element-stack)
                     (let ((el moving-el))
                       (execute-after 0.1
                         (lambda ()
                           (setf (jscl::oget el "style" "scale") "100%")
                           (setf (jscl::oget el "style" "boxShadow") "0 0 0")
                           (setf (jscl::oget el "style" "zIndex") 10)
                           (setf (jscl::oget el "style" "transform") "translateY(0)"))))
                     (if (and (not (equal moving-el-num last-shift))
                              (not (and reorder-cb (not (funcall reorder-cb moving-el-num last-shift inner)))))
                         (progn
                           (remove-element moving-el)
                           (if (> last-shift moving-el-num)
                               (if (not (equal last-shift (- (length element-stack) 1)))
                                   ((jscl::oget inner "insertBefore") moving-el (nth (+ last-shift 1) element-stack))
                                   (append-element moving-el inner))
                               ((jscl::oget inner "insertBefore") moving-el (nth last-shift element-stack)))
                           (if (> last-shift moving-el-num)
                               (setf element-stack `(,@(subseq element-stack 0 moving-el-num)
                                                     ,@(subseq element-stack (+ 1 moving-el-num) (+ last-shift 1))
                                                     ,moving-el
                                                     ,@(subseq element-stack (+ last-shift 1))))
                               (setf element-stack `(,@(subseq element-stack 0 last-shift)
                                                     ,moving-el
                                                     ,@(subseq element-stack last-shift moving-el-num)
                                                     ,@(subseq element-stack (+ 1 moving-el-num)))))
                           (setf (jscl::oget inner "omg-list-elements")
                                 (mapcar (lambda (el) (jscl::oget el "omg-orig-element")) element-stack))))
                     (setf moving-el nil)
                     (setf last-shift nil)
                     (set-els-pos))))
             (move-handler (ev)
               (let* ((x1 (jscl::oget ev "pageX"))
                      (y1 (jscl::oget ev "pageY")))
                 (if moving-el
                     (progn
                       (setf cur-top (max 0 (min max-top (+ (- y1 page-y0) client-y0))))
                       (setf (jscl::oget moving-el "style" "top") (format nil "~Apx" (- cur-top client-y0)))
                       ((jscl::oget (funcall (winref "getSelection")) "empty"))
                       ((jscl::oget ev "stopPropagation"))
                       (let* ((mid (+ cur-top (* 0.5 moving-el-height)))
                              (pos (position-if
                                     (lambda (el)
                                         (and (> mid (car el))
                                              (< mid (cdr el))))
                                     els-pos)))
                         (if (and pos (not (equal pos last-shift)))
                             (let ((ls1 (if last-shift  last-shift pos)))
                               (loop for i from (max 0 (min (- ls1 1) pos)) to (min (- (length element-stack) 1) (max (+ ls1 1) pos)) do
                                 (setf (jscl::oget (nth i element-stack) "style" "transform")
                                       (format nil "translateY(~Apx)"
                                                   (cond ((and (>= i pos) (< i moving-el-num))
                                                          moving-el-height)
                                                         ((and (<= i pos) (> i moving-el-num))
                                                          (- moving-el-height))
                                                         (t 0)))))
                               (setf last-shift pos)
                               (if on-drag (funcall on-drag moving-el pos)))))
                       nil)
                     (if (and insert-el insert-gizmo-width)
                         (let* ((rect ((jscl::oget inner "getBoundingClientRect")))
                                (top (+ (jscl::oget rect "top") (winref "scrollY")))
                                (left (+ (jscl::oget rect "left") (winref "scrollX")))
                                (y1 (- y1 top))
                                (minpos (if els-mid
                                            (if (< y1 (car els-mid))
                                                '(0 . 0)
                                                (if (> y1 (car (last els-mid)))
                                                    `(,(cdar (last els-pos)) . ,(length els-mid))
                                                    (loop for i below (length els-mid)
                                                          when (< y1 (nth i els-mid))
                                                          return `(,(cdr (nth (- i 1) els-pos)) . ,i)))))))
                           (if (and (< x1 (+ left (* 0.5 (jscl::oget rect "width"))))
                                    (> x1 (- left (* 2 insert-gizmo-width))))
                               (if (not (equal (jscl::oget insert-gizmo "style" "display") "block"))
                                   (progn
                                     (setf (jscl::oget insert-gizmo "style" "display") "block")
                                     (setf (jscl::oget insert-gizmo "style" "visibility") "visible")
                                     (setf (jscl::oget insert-gizmo "style" "opacity") 0)
                                     (ensure-element insert-gizmo
                                       (setf (jscl::oget insert-gizmo "style" "opacity") 1)))
                                   (progn
                                     (if (not (equal last-min-pos (cdr minpos)))
                                         (progn
                                           (setf (jscl::oget insert-gizmo "style" "top") (car minpos))
                                           (setf last-min-pos (cdr minpos))))))
                               (if (and els-mid (equal (jscl::oget insert-gizmo "style" "display") "block"))
                                 (progn
                                   (setf (jscl::oget insert-gizmo "style" "opacity") 0)
                                   (execute-after 0.5
                                     (lambda ()
                                       (setf (jscl::oget insert-gizmo "style" "display") "none")))))))))))
             (make-rec (el)
               (let* ((rec (create-element "div" :|style.position| "relative"
                                                 :|style.transition| trans
                                                 :|style.zIndex| 10
                             :append-element el)))
                 (setf (jscl::oget rec "omg-orig-element") el)
                 (setf (jscl::oget rec "onmousedown")
                       (lambda (ev)
                         (if (equal (jscl::oget ev "button") 0)
                             (progn
                               (if insert-el (setf (jscl::oget insert-gizmo "style" "display") "none"))
                               (setf moving-el rec)
                               (setf moving-el-num (position rec element-stack))
                               (setf (jscl::oget rec "style" "scale") "101%")
                               (setf (jscl::oget rec "style" "boxShadow") "0 0 1em 0.2em #909090")
                               (setf (jscl::oget rec "style" "zIndex") 100)
                               (setf client-y0 (- (jscl::oget ((jscl::oget rec "getBoundingClientRect")) "top")
                                                  (jscl::oget ((jscl::oget inner "getBoundingClientRect")) "top")))
                               (setf cur-top client-y0)
                               (setf page-y0 (jscl::oget ev "pageY"))
                               (setf max-top (- (jscl::oget inner "offsetHeight") (jscl::oget rec "offsetHeight")))
                               (set-els-pos)
                               (setf moving-el-height (jscl::oget ((jscl::oget rec "getBoundingClientRect")) "height")) ;;(jscl::oget rec "offsetHeight"))
                               (map nil
                                    (lambda (el) (setf (jscl::oget el "style" "transition") trans))
                                    element-stack)
                               (setf (jscl::oget rec "style" "transition") "all 0.1s linear")))))
                 rec)))
      (setf element-stack
            (mapcar
              (lambda (el)
                 (let ((rec (make-rec el)))
                   (append-element rec inner)
                   rec))
              elements))
      (ensure-element inner
        (set-els-pos))
      (setf (jscl::oget inner "omg-list-elements") elements)
      (setf (jscl::oget inner "omg-insert-fn")
            (lambda (el pos)
              (let ((rec (make-rec el)))
                (setf (jscl::oget rec "style" "transition") "none")
                (setf (jscl::oget rec "style" "visibility") "hidden")
                (setf (jscl::oget rec "style" "opacity") 0)
                (append-element rec inner)
                (ensure-element rec
                  (let ((h (jscl::oget ((jscl::oget rec "getBoundingClientRect")) "height")))
                    (remove-element rec)
                    (loop for i below (length element-stack)
                          for el = (nth i element-stack) do
                      (progn
                        (setf (jscl::oget el "style" "transition") "none")
                        (if (>= i pos)
                            (setf (jscl::oget el "style" "transform") (format nil "translateY(~Apx)" (- h))))))
                    (if (>= pos (length element-stack))
                        (append-element rec inner)
                        ((jscl::oget inner "insertBefore") rec (nth pos element-stack)))
                    (setf element-stack `(,@(subseq element-stack 0 pos)
                                          ,rec
                                          ,@(subseq element-stack pos)))
                    (setf (jscl::oget inner "omg-list-elements")
                          (mapcar (lambda (el) (jscl::oget el "omg-orig-element")) element-stack))
                    (execute-after 0.1
                      (lambda ()
                        (setf (jscl::oget rec "style" "transition") trans)
                        (setf (jscl::oget rec "style" "visibility") "visible")
                        (setf (jscl::oget rec "style" "opacity") 1)
                        (map nil
                          (lambda (el)
                            (setf (jscl::oget el "style" "transition") trans)
                            (setf (jscl::oget el "style" "transform") "translateY(0)"))
                          element-stack)
                        (execute-after 0.5
                          (lambda ()
                            (set-els-pos))))))))))
      (ensure-element insert-gizmo
        (setf insert-gizmo-width (jscl::oget ((jscl::oget insert-gizmo "getBoundingClientRect")) "width")))
      (add-event-handler "document.onmouseup" #'up-handler)
      (add-event-handler "document.onmousemove" #'move-handler)
      (on-element-remove inner
        (lambda (el)
          (rm-event-handler "document.onmouseup" #'up-handler)
          (rm-event-handler "document.onmousemove" #'move-handler)))
      inner)))

(defun-f dragabble-list-elements (inner)
  (jscl::oget inner "omg-list-elements"))

(defun-f dragabble-list-insert-position (inner)
  (if (dragabble-list-elements inner)
      (jscl::oget inner "omg-insert-position")
      0))

(defun-f dragabble-list-insert (inner el &optional pos)
  (funcall (jscl::oget inner "omg-insert-fn") el (if pos pos (dragabble-list-insert-position inner))))


(defvar-f *worker-cache* nil)

(defparameter-f *current-service-worker* nil)

(defun-r get-js-path ()
  (format nil "~A~A" omg::*root-path* omg::*js-path*))

(defun-r get-sw-path ()
  (format nil "~A~A" omg::*root-path* omg::*service-worker-path*))

(defun-r get-ww-path ()
  (format nil "~A~A" omg::*root-path* omg::*web-worker-path*))


(defun-r get-root-path ()
  omg::*root-path*)

(defclass-f webworker ()
  ((worker :accessor worker)
   (ready :initform nil
           :accessor ready)
   (busy :initform nil
         :accessor busy)
   (persistent-cache :initform nil
                     :initarg :persistent-cache
                     :accessor persistent-cache)
   (return-handlers :accessor return-handlers
                    :initform nil)))

(defclass-f classic-worker (webworker)
  ())

(defclass-f service-worker (webworker)
  ())

(defparameter-f *web-workers-pool* nil)

(def-local-macro-f when-worker-ready (ww &rest code)
  (let ((wwait (gensym)))
    `(labels ((,wwait ()
                (if (ready ,ww)
                    (progn ,@code)
                    (execute-after 0.1 #',wwait))))
        (,wwait))))

(def-local-macro-f when-worker-free (ww &rest code)
  (let ((wwait (gensym)))
    `(when-worker-ready ,ww
       (labels ((,wwait ()
                  (if (busy ,ww)
                      (execute-after 0.1 #',wwait)
                      (progn ,@code))))
          (,wwait)))))

(defmethod-f msg ((ww webworker) data &key transfer)
  (when-worker-ready ww
    (if (worker ww)
        (if transfer
            ((jscl::oget (worker ww) "postMessage") (make-js-object :|code| data) transfer)
            ((jscl::oget (worker ww) "postMessage") (make-js-object :|code| data))))))

(defmethod-f initialize-instance :after ((ww webworker) &rest args)
  (labels ((wait-ww (timeout)
             (if (and (slot-boundp ww 'worker) (not (ready ww)))
                 (setf (jscl::oget (worker ww) "onmessage")
                       (lambda (ev)
                         (cond ((equal (jscl::oget ev "data") "BOOT")
                                ((jscl::oget (worker ww) "postMessage")
                                 (make-js-object
                                   :|code| (format nil "~Aself.OMG.Base='~A' ; OMG.session_id='~A' ; self.postMessage('BOOT DONE');"
                                                   (if (persistent-cache ww)
                                                       "self.OMG.PersistentCache=true ; "
                                                       "")
                                                   (jscl::oget (winref "self") "OMG" "Base")
                                                   (jscl::oget (winref "self") "OMG" "session_id"))
                                   :|cache| (jscl::oget (winref "OMG") "FetchCache"))))
                               ((equal (jscl::oget ev "data") "BOOT DONE")
                                (setf (slot-value ww 'ready) t)
                                (setf (jscl::oget (worker ww) "onmessage") (lambda (ev))))
                               (t (error "Invalid init message from worker")))))
                 (execute-after timeout (lambda () (wait-ww (* 2 timeout)))))))
    (wait-ww 0.1)))

(defmethod-f initialize-instance :before ((ww service-worker) &key &allow-other-keys)
  ((jscl::oget
     ((jscl::oget (winref "navigator") "serviceWorker" "register") (get-sw-path) (make-js-object :|scope| (get-root-path)))
     "then") (lambda (reg)
               ((jscl::oget reg "update"))
               (labels ((wait-sw (timeout)
                          (if (jscl::js-null-p (jscl::oget (winref "navigator") "serviceWorker"))
                              (execute-after timeout (lambda () (wait-sw (* timeout 2))))
                              (progn
                                (setf (slot-value ww 'worker) (jscl::oget (winref "navigator") "serviceWorker" "controller"))
                                (setf (slot-value ww 'ready) t)
                                (setf *current-service-worker* ww)))))
                 (setf (jscl::oget (winref "navigator") "serviceWorker" "oncontrollerchange")
                       (lambda (ev)
                         (jslog "Controller changed!")
                         (wait-sw 0.1)))
                 (wait-sw 0.1)))))

(defmethod-f initialize-instance :before ((ww classic-worker) &rest args)
  (push ww *worker-cache*)
  (setf (slot-value ww 'worker)
        (jscl::make-new (jscl::%js-vref "Worker") (jscl::lisp-to-js ((jscl::oget (winref "OMG") "get_ww_js_url"))))))

(defmethod-f kill ((ww webworker))
  (if (worker ww) ((jscl::oget (worker ww) "terminate")))
  (setf *worker-cache* (remove-if (lambda (w) (equal w ww)) *worker-cache*)))

(defmethod-f kill ((ww service-worker) &optional (try-count 20))
  (if *current-service-worker*
      ((jscl::oget ((jscl::oget (jscl::%js-vref "navigator") "serviceWorker" "getRegistration") (get-root-path)) "then")
       (lambda (reg)
         ((jscl::oget reg "unregister"))
         (setf *current-service-worker* nil)))
      (if (> try-count 0)
          (execute-after 0.5
            (lambda ()
              (kill ww (- try-count 1))))
          (jslog "Service Worker not spawned yet, cannot kill")))
  nil)

(def-local-macro-f in-service-worker (&rest code)
  (let ((js (omg::compile-to-js `(progn ,@code) *package* :recursive t))
        (fn (gensym)))
    `(progn
       (if *current-service-worker*
           (msg *current-service-worker* ,js)
           (labels ((,fn ()
                      (if *current-service-worker*
                          (msg *current-service-worker* ,js)
                          (execute-after 0.2 #',fn))))
             (make-instance 'service-worker)
             (,fn)))
       nil)))

(defvar-f *linefeed* (format nil "~%"))

(defparameter-f *main-lambdas* nil)

(defun-f register-main-lambda (l)
  (let ((id (gensym2)))
    (push (cons id l) *main-lambdas*)))

(defun-f compile-js (code)
  (setf (jscl::oget (winref "OMG") "disableLIL") t)
  (prog1
    (jscl::with-compilation-environment (jscl::compile-toplevel code t t))
    (setf (jscl::oget (winref "OMG") "disableLIL") nil)))

(defclass-f dont-transfer () ())

(defun-f store-to-buffer (obj buf &key (start 0) respect-transfer background progress-cb final-cb)
  (let ((last-id 100)
        (obj-hash (make-hash-table))
        (len (jscl::oget buf "byteLength"))
        (lambda-stack nil)
        (t0 (now))
        (dt (if (numberp background)
                background
                0.1)))
    (macrolet ((ps (&rest code)
                 `(push (lambda () ,@code) lambda-stack))
               (psp (&rest code)
                 `(ps (pop lambda-stack) ,@code))
               (check-id (typ &rest cod)
                 `(let* ((old-id (gethash obj obj-hash))
                         (obj-id (if old-id old-id (setf (gethash obj obj-hash) (incf last-id)))))
                    (if (not old-id)
                        (progn ,@cod)
                        (store-id-set ,typ 0 obj-id)))))
      (labels ((store-id (id l &optional (id1 l))
                 (multiple-value-bind (d r) (floor l 4)
                   (if (<= (+ 8 l start) len)
                       (let ((idb (jscl::make-new (winref "Int32Array") buf start 2)))
                         (setf (jscl::oget idb 0) id)
                         (setf (jscl::oget idb 1) id1)))
                   (+ 8 l start (if (> r 0) (- 4 r) 0))))
               (store-id-set (id l &optional (id1 l))
                 (setf start (store-id id l id1)))
               (sing-val (id typ obj)
                 (let ((start1 (store-id id 4)))
                   (if (<= start1 len)
                       (let ((b1 (jscl::make-new (winref typ) buf (+ start 8) 1)))
                         (setf (jscl::oget b1 0) obj)))
                   (setf start start1)))
               (store-val (obj)
                 (typecase obj
                   (integer (sing-val 0 "Int32Array" obj))    ;; 0 - integer
                   (real    (sing-val 1 "Float32Array" obj))  ;; 1 - real
                   (string  (let* ((enc (jscl::make-new (winref "TextEncoder")))
                                   (sb ((jscl::oget enc "encode") obj))
                                   (bl (jscl::oget sb "byteLength"))
                                   (start1 (store-id 2 bl))) ;; 2 - string
                              (if (<= start1 len)
                                  (let ((rb (jscl::make-new (winref "Uint8Array") buf (+ start 8) bl)))
                                    (loop for i below bl do
                                      (setf (jscl::oget rb i) (jscl::oget sb i)))))
                              (setf start start1)))
                   (null    (store-id-set 4 0 0)) ;; 4 - NIL is CONS, 0 for NIL
                   (symbol  (store-id-set 3 0) ;; 3 - symbol
                            (psp (store-val (symbol-name obj)))
                            (psp (store-val (package-name (symbol-package obj)))))
                   (jscl::mop-object
                     (if (and respect-transfer (typep obj (find-class 'dont-transfer)))
                         (store-id-set 4 0 0)
                         (check-id 6 ;; 6 - CLOS object, class and alist of slots follows
                           (let ((slts (remove-if #'null (mapcar
                                                           (lambda (slot)
                                                             (let ((name (getf slot :name)))
                                                               (if (slot-boundp obj name)
                                                                   (cons name (slot-value obj name)))))
                                                           (jscl::class-slots (class-of obj))))))
                             (store-id-set 6 0 obj-id)
                             (psp (store-val slts))
                             (psp (store-val (class-name (class-of obj))))))))
                   (list (check-id 4 ;; 4 -- cons
                            (store-id-set 4 0 obj-id)
                            (psp (store-val (cdr obj)))
                            (psp (store-val (car obj)))))
                   (vector (check-id 5
                             (store-id-set 5 0 obj-id) ;; 5 - array, array dims follows
                             (let ((i 0)
                                   (lar (length obj)))
                               (ps (if (< i lar)
                                       (store-val (aref obj i))
                                       (pop lambda-stack))
                                   (incf i))
                               (psp (store-val (list (length obj)))))))
                   (array  (check-id 5
                             (store-id-set 5 0 obj-id)
                             (let ((i 0)
                                   (lar (apply #'* (array-dimensions obj))))
                               (ps (if (< i lar)
                                       (store-val (aref obj i))
                                       (pop lambda-stack))
                                   (incf i))
                               (psp (store-val (array-dimensions obj))))))
                   (jscl::js-object (store-id-set 4 0 0)) ;; JS objects are not serializable, store NIL instead
                   (function (let ((ml (car (rassoc obj *main-lambdas*))))
                               (if ml
                                   (progn
                                     (store-id-set 7 0 0)  ;; 7 -- main thread lambda, symbol key follow
                                     (psp (store-val ml)))
                                   (store-id-set 4 0 0)))) ;; other lambdas are not serializable, store NIL instead)
                   (t (error (format nil "Don't know, how to serialize ~A" (type-of obj)))))))
        (psp (store-val obj))
        (if background
            (labels ((doit ()
                       (setf t0 (now))
                       (loop while (and lambda-stack (< (- (now) t0) dt)) do
                         (funcall (car lambda-stack)))
                       (if progress-cb (funcall progress-cb start))
                       (if lambda-stack
                           (execute-after 0 #'doit)
                           (if final-cb (funcall final-cb start)))))
              (if progress-cb (funcall progress-cb start))
              (execute-after 0 #'doit))
            (loop while lambda-stack do (funcall (car lambda-stack))))
        start))))

(defun-f load-from-buffer (buf &key (start 0) background progress-cb final-cb)
  (let ((len (jscl::oget buf "byteLength"))
        (obj-hash (make-hash-table))
        (exit-flg nil)
        (lambda-stack (list (if background
                                (lambda (obj)
                                  (if progress-cb (funcall progress-cb start))
                                  (if final-cb (funcall final-cb obj start))
                                  (setf exit-flg t))
                                (lambda (obj)
                                  (return-from load-from-buffer (values obj start))))))
        (dt (if (numberp background)
                background
                0.1)))
    (labels ((ld ()
               (let ((t0 (now)))
                 (loop while (and (<= start (- len 8))
                                  (or (not background)
                                      (< (- (now) t0) dt))) do
                   (let* ((idb (jscl::make-new (winref "Int32Array") buf start 2))
                          (typ (jscl::oget idb 0))
                          (b2 (jscl::oget idb 1)))
                     (labels ((cur-lam (obj)
                                (funcall (car lambda-stack) obj))
                              (eob ()
                                (error "End of the buffer reached!")
                                (return-from load-from-buffer (values nil start)))
                              (sing-val (typ)
                                (if (> (+ start 4) len) (eob))
                                (incf start 4)
                                (cur-lam (jscl::oget (jscl::make-new (winref typ) buf (- start 4) 1) 0))))
                       (macrolet ((ps (&rest code)
                                    `(push (lambda (obj) ,@code) lambda-stack))
                                  (psp (&rest code)
                                    `(ps (pop lambda-stack) ,@code)))
                         (incf start 8)
                         (case typ
                           (0 (sing-val "Int32Array")) ;; integer
                           (1 (sing-val "Float32Array")) ;; real
                           (2 (if (<= (+ start b2) len) ;; string
                                  (multiple-value-bind (x r) (floor b2 4)
                                    (let ((s1 start))
                                      (incf start (+ b2 (if (> r 0) (- 4 r) 0)))
                                      (cur-lam ((jscl::oget (jscl::make-new (winref "TextDecoder")) "decode")
                                                ((jscl::oget (jscl::make-new (winref "Uint8Array") buf s1 b2) "slice"))))))
                                  (eob)))
                           (3 ;; symbol
                              (let (pkg)
                                (psp (cur-lam (intern obj (find-package pkg))))
                                (psp (setf pkg obj))))
                           (4 ;; cons
                              (if (= b2 0)
                                  (cur-lam nil) ;; nil
                                  (multiple-value-bind (obj fnd) (gethash b2 obj-hash)
                                    (if fnd
                                        (cur-lam obj)
                                        (let ((cns (cons nil nil)))
                                          (setf (gethash b2 obj-hash) cns)
                                          (psp (setf (cdr cns) obj) (cur-lam cns))
                                          (psp (setf (car cns) obj)))))))
                           (5 ;; array
                              (multiple-value-bind (obj fnd) (gethash b2 obj-hash)
                                (if fnd
                                    (cur-lam obj)
                                    (let (arr
                                          len
                                          (cnt 0)
                                          (b22 b2))
                                      (ps (if (< cnt len)
                                              (setf (aref arr cnt) obj))
                                          (incf cnt)
                                          (when (>= cnt len)
                                            (pop lambda-stack)
                                            (cur-lam arr)))
                                      (psp (setf arr (make-array obj))
                                           (setf len (apply #'* obj))
                                           (setf (gethash b22 obj-hash) arr))))))
                           (6 ;; MOP object
                              (multiple-value-bind (obj fnd) (gethash b2 obj-hash)
                                (if fnd
                                    (cur-lam obj)
                                    (let (inst
                                          (b22 b2))
                                      (psp (map nil (lambda (slt) (setf (slot-value inst (car slt)) (cdr slt))) obj)
                                           (map nil (lambda (slt)
                                                      (let ((name (jscl::slot-definition-name slt)))
                                                        (when (not (find name obj :key #'car))
                                                          (let ((ini (jscl::slot-definition-initform slt)))
                                                            (when ini
                                                              (setf (slot-value inst name) (eval ini)))))))
                                                    (jscl::class-slots (class-of inst)))
                                           (cur-lam inst))
                                      (psp (setf inst (allocate-instance (find-class obj)))
                                           (setf (gethash b22 obj-hash) inst))))))
                           (7 ;; main thread lambda
                              (psp (cur-lam (jscl::js-to-lisp ;; without js-to-lisp it is not working (WHY?!!!)
                                              (let* ((cur-buf-len (* 1024 1024 128))
                                                     (req-buf (jscl::make-new (winref "SharedArrayBuffer") cur-buf-len)))
                                                (lambda (&rest args)
                                                  (labels ((stor-all (ofs)
                                                             (let ((siz (store-to-buffer (cons obj args) req-buf :start ofs)))
                                                               (if (> siz cur-buf-len)
                                                                   (progn
                                                                     (setf cur-buf-len (* 2 cur-buf-len))
                                                                     (setf req-buf (jscl::make-new (winref "SharedArrayBuffer") cur-buf-len))
                                                                     (stor-all ofs)))))
                                                           (try-fetch (rc)
                                                             (let ((ibuf (jscl::make-new (winref "Int32Array") req-buf 0 2)))
                                                               (setf (jscl::oget ibuf 0) 0)
                                                               (setf (jscl::oget ibuf 1) rc)
                                                               (funcall (winref "postMessage") req-buf)
                                                               ((jscl::oget (jscl::%js-vref "Atomics") "wait") ibuf 0 0)
                                                               (let ((res-len (jscl::oget ibuf 0)))
                                                                 (if (> res-len 0)
                                                                     (if (> res-len (jscl::oget req-buf "byteLength"))
                                                                         (progn
                                                                           (setf cur-buf-len res-len)
                                                                           (setf req-buf (jscl::make-new (winref "SharedArrayBuffer") cur-buf-len))
                                                                           (store-to-buffer sym req-buf :start 8)
                                                                           (try-fetch 3))
                                                                         (return-from try-fetch (load-from-buffer req-buf :start 4))))))))
                                                    (stor-all 8)
                                                    (apply #'values (try-fetch 2))))))))))))))))
             (lp ()
               (when (not exit-flg)
                 (ld)
                 (if progress-cb (funcall progress-cb start))
                 (execute-after 0 #'lp))))
      (if background
          (progn
            (if progress-cb (funcall progress-cb start))
            (lp))
          (progn
            (ld)
            (error "End of the buffer!"))))
    (values nil nil)))

(defun-f get-free-worker ()
  (let* ((w1 (car (remove-if (lambda (w) (busy w)) *web-workers-pool*)))
         (w (if w1 w1 (make-instance 'classic-worker))))
    (if (not w1) (push w *web-workers-pool*))
    w))

(defparameter-f *main-thread-result-cache* nil)

(def-local-macro-f run-in-web-worker (ww1 &rest code)
  (let* ((sym (gensym))
         (req-buf (gensym))
         (req-buf2 (gensym))
         (ibuf (gensym))
         (ev (gensym))
         (res-len (gensym))
         (cur-buf-len (gensym))
         (rvals (gensym))
         (v0 (gensym))
         (ww (gensym))
         (hl (gensym))
         (cache-lst (gensym))
         (cache-all (gensym))
         (cache-h (gensym))
         (tmp-cache-h (gensym))
         (lam (gensym))
         (val (gensym))
         (fnd (gensym))
         (fhandler (gensym))
         (req-code (gensym))
         (alt-buf (gensym))
         (local-symbols (remove-if #'null
                          (mapcar #'jscl::binding-name
                                  (remove-if-not #'null
                                                 (cadr jscl::*environment*)
                                                 :key #'jscl::binding-declarations))
                          :key #'symbol-package))
         (jscode (let ((jscl::*environment* (jscl::copy-lexenv jscl::*environment*)))
                   (setf (cadr jscl::*environment*)
                         (remove-if #'null
                                    (cadr jscl::*environment*)
                                    :key #'jscl::binding-declarations))
                   (omg::compile-to-js
                      `(progn
                         (if (and (jscl::oget (winref "OMG") "PersistentCache")
                                  (not (jscl::oget (winref "OMG") "VarCache")))
                             (setf (jscl::oget (winref "OMG") "VarCache") (make-hash-table)))
                         (let* ((,cache-lst ',(if (and (listp (car code))
                                                       (equal 'cache-vars (caar code)))
                                                  (cdar code)))
                                (,cache-all (position t ,cache-lst))
                                (,cur-buf-len (* 1024 1024 128))
                                (,req-buf (jscl::make-new (winref "SharedArrayBuffer") ,cur-buf-len))
                                (,req-buf2 (jscl::make-new (winref "SharedArrayBuffer") ,cur-buf-len))
                                (,cache-h (if (jscl::oget (winref "OMG") "PersistentCache")
                                              (jscl::oget (winref "OMG") "VarCache")
                                              (make-hash-table)))
                                (,tmp-cache-h (make-hash-table)))
                            (labels ((,fhandler (,sym &optional (,req-code 0) ,alt-buf)
                                       (labels ((try-fetch ()
                                                  (let ((,ibuf (jscl::make-new (winref "Int32Array") (if ,alt-buf ,req-buf2 ,req-buf) 0 2)))
                                                    (store-to-buffer ,sym (if ,alt-buf ,req-buf2 ,req-buf) :start 8)
                                                    (setf (jscl::oget ,ibuf 0) 0)
                                                    (setf (jscl::oget ,ibuf 1) ,req-code)
                                                    (funcall (winref "postMessage") (if ,alt-buf ,req-buf2 ,req-buf))
                                                    ((jscl::oget (jscl::%js-vref "Atomics") "wait") ,ibuf 0 0)
                                                    (let ((,res-len (jscl::oget ,ibuf 0)))
                                                      (if (> ,res-len (jscl::oget (if ,alt-buf ,req-buf2 ,req-buf) "byteLength"))
                                                          (progn
                                                            (setf ,cur-buf-len ,res-len)
                                                            (setf ,req-buf (jscl::make-new (winref "SharedArrayBuffer") ,cur-buf-len))
                                                            (setf ,req-buf2 (jscl::make-new (winref "SharedArrayBuffer") ,cur-buf-len))
                                                            (try-fetch))
                                                          (load-from-buffer (if ,alt-buf ,req-buf2 ,req-buf) :start 4))))))
                                         (multiple-value-bind (,val ,fnd) (gethash ,sym ,tmp-cache-h)
                                           (if ,fnd
                                               ,val
                                               (multiple-value-bind (,val ,fnd) (gethash ,sym ,cache-h)
                                                 (if ,fnd
                                                     ,val
                                                     (let ((,val (try-fetch)))
                                                       (if (and (jscl::oget (winref "OMG") "PersistentCache")
                                                                (not (position ,sym ',local-symbols))
                                                                (not (position ,sym ,cache-lst)))
                                                           (setf (jscl::oget ,sym "value") ,val)
                                                           (if (position ,sym ,cache-lst)
                                                               (setf (gethash ,sym ,tmp-cache-h) ,val)
                                                               (if (or ,cache-all (jscl::oget (winref "OMG") "PersistentCache"))
                                                                   (setf (gethash ,sym ,cache-h) ,val))))
                                                       ,val))))))))
                              (setf (jscl::oget (winref "OMG") "FValueFetchHandler")
                                    (lambda (,sym)
                                      (if (not (or (eq ,sym 'store-to-buffer)
                                                   (eq ,sym 'load-from-buffer)))
                                          (let* ((,val (,fhandler ,sym 4 t)))
                                            (if (stringp ,val)
                                                ,val)))))
                              (setf (jscl::oget (winref "OMG") "symbolValueFetchHandler") #',fhandler))
                            (let ((,rvals (multiple-value-list
                                            (progn
                                              ,@(if (and (listp (car code))
                                                         (equal 'cache-vars (caar code)))
                                                    (cdr code)
                                                    code)))))
                              (labels ((try-store ()
                                         (let ((,res-len (store-to-buffer ,rvals ,req-buf :start 8 :respect-transfer t)))
                                           (if (> ,res-len (jscl::oget ,req-buf "byteLength"))
                                               (progn
                                                 (setf ,cur-buf-len ,res-len)
                                                 (setf ,req-buf (jscl::make-new (winref "SharedArrayBuffer") ,cur-buf-len))
                                                 (try-store))))))
                                (try-store)
                                (let ((,ibuf (jscl::make-new (winref "Int32Array") ,req-buf 0 2)))
                                  (setf (jscl::oget ,ibuf 1) 1)
                                  (funcall (winref "postMessage") ,req-buf))))))
                      *package*))))
    `(let ((,ww (if ,ww1 ,ww1 (get-free-worker))))
       (when-worker-ready ,ww
         (setf (jscl::oget (worker ,ww) "onmessage")
               (lambda (,ev)
                 (let* ((,req-buf (jscl::oget ,ev "data"))
                        (,ibuf (jscl::make-new (winref "Int32Array") ,req-buf 0 2))
                        (,v0 (jscl::oget ,ibuf 1)))
                   (case ,v0
                     (0 (let* ((,sym (load-from-buffer ,req-buf :start 8)) ;; 0 -- symbol requested
                               (,res-len (store-to-buffer
                                           (case ,sym
                                              ,@(mapcar (lambda (sym)
                                                          `(,sym ,sym))
                                                        local-symbols)
                                             (t (symbol-value ,sym)))
                                           ,req-buf
                                           :start 4
                                           :respect-transfer t)))
                          ; (format t "REQ: ~A" ,sym)
                          ((jscl::oget (jscl::%js-vref "Atomics") "store") ,ibuf 0 ,res-len)
                          ((jscl::oget (jscl::%js-vref "Atomics") "notify") ,ibuf 0)))
                     (1 (let ((,rvals (load-from-buffer ,req-buf :start 8)) ;; 0 -- return values arrived
                              (,hl (return-handlers ,ww)))
                          (setf (slot-value ,ww 'return-handlers) nil)
                          (setf (slot-value ,ww 'busy) nil)
                          (setf (jscl::oget (worker ,ww) "onmessage") (lambda (ev)))
                          (map nil (lambda (l) (apply l ,rvals)) ,hl)))
                     (2 (if (not *main-thread-result-cache*) ;; 2 -- execute a lambda
                            (setf *main-thread-result-cache* (make-hash-table)))
                        (let* ((,sym (load-from-buffer ,req-buf :start 8))
                               (,lam (cdr (assoc (car ,sym) *main-lambdas*))))
                          (if ,lam
                              (let ((,val (multiple-value-list (apply ,lam (cdr ,sym)))))
                                (setf (gethash (car ,sym) *main-thread-result-cache*) ,val)
                                ((jscl::oget (jscl::%js-vref "Atomics") "store") ,ibuf 0 (store-to-buffer ,val ,req-buf :start 4  :respect-transfer t))
                                ((jscl::oget (jscl::%js-vref "Atomics") "notify") ,ibuf 0))
                              (progn
                                ((jscl::oget (jscl::%js-vref "Atomics") "store") ,ibuf 0 -1)
                                ((jscl::oget (jscl::%js-vref "Atomics") "notify") ,ibuf 0)
                                (error "Unregistered main thread lambda execution requested!")))))
                     (3 (let ((,sym (load-from-buffer ,req-buf :start 8))) ;; 3 -- send the result again
                          ((jscl::oget (jscl::%js-vref "Atomics") "store") ,ibuf 0 (store-to-buffer (gethash ,sym *main-thread-result-cache*) ,req-buf :start 4  :respect-transfer t))
                          (remhash ,sym *main-thread-result-cache*)
                          ((jscl::oget (jscl::%js-vref "Atomics") "notify") ,ibuf 0)))
                     (4 (let* ((,sym (load-from-buffer ,req-buf :start 8))) ;; 4 -- send a value from FetchCache
                          ((jscl::oget (jscl::%js-vref "Atomics") "store") ,ibuf 0
                             (store-to-buffer
                               (jscl::oget (winref "OMG") "FetchCache"
                                 (format nil "~A::~A"
                                   (package-name (symbol-package ,sym))
                                   (symbol-name ,sym)))
                               ,req-buf :start 4))
                          ((jscl::oget (jscl::%js-vref "Atomics") "notify") ,ibuf 0)))))))
         (setf (slot-value ,ww 'busy) t)
         (msg ,ww ,jscode))
       ,ww)))

(def-local-macro-f bind-exit-values-for (args ww &rest code)
  (let ((ww1 (gensym)))
    `(let ((,ww1 ,ww))
       (setf (slot-value ,ww1 'return-handlers) (cons (lambda (,(car args) &optional ,@(cdr args)) ,@code) (return-handlers ,ww1)))
       ,ww1)))


(defmacro-f set-service-worker-uri-handler (uri-req-event &rest code)
  (let ((ev (gensym2))
        (opts (gensym2))
        (body (gensym2))
        (options (gensym2))
        (args (gensym2))
        (uri (gensym2))
        (old-handler (gensym2))
        (unused-args (list (gensym2) (gensym2) (gensym2))))
    `(in-service-worker
       (let ((,old-handler (jscl::oget (jscl::%js-vref "self") "OMG" "fetchHandler")))
         (setf (jscl::oget (jscl::%js-vref "self") "OMG" "fetchHandler")
               (lambda (,ev)
                 (apply (lambda (,@uri-req-event &optional ,@unused-args)
                          (labels ((respond-with (&optional ,body ,options)
                                     (let ((,opts (jscl::new)))
                                       (loop for (k v) on ,options by #'cddr do (setf (jscl::oget obj (symbol-name k)) v))
                                       ((jscl::oget ,ev "respondWith")
                                        (jscl::make-new (jscl::%js-vref "Response")
                                                        ,body
                                                        ,opts))))
                                   (jslog (&rest ,args)
                                     (apply (jscl::oget (jscl::%js-vref "self") "console" "log") ,args))
                                   (default-action ()
                                     (funcall ,old-handler ,ev))
                                   (uri-path (,uri)
                                     (jscl::oget (jscl::make-new (jscl::oget (jscl::%js-vref "self") "URL")
                                                                 (jscl::lisp-to-js ,uri))
                                                 "pathname")))
                            ,@code))
                        (list (jscl::oget ,ev "request" "url")
                              (jscl::oget ,ev "request")
                              ,ev))))))))

(defvar-f *pwa-mode* nil)

(defun make-pwa (&key (name "Application")
                      (short-name "app")
                      (display "standalone")
                      (theme-color "#000000")
                      (background-color "#ffffff")
                      icon-path)
  (setf *pwa-mode* t)
  (let* ((manifest-path (format nil "~A.json" (omg::random-string 20)))
         (icon-path (if icon-path
                        (if (equal (type-of icon-path) 'pathname)
                            icon-path
                            (parse-namestring icon-path))))
         (icon-ext (if icon-path
                       (pathname-type icon-path)
                       "png"))
         (icon-type (media-types:extension-media-type icon-ext))
         (icon-size (if icon-path
                        (cond ((equal icon-type "image/png")
                               (let ((png (pngload:load-file icon-path :decode nil)))
                                 (format nil "~Ax~A" (pngload:get-metadata png :width)
                                                     (pngload:get-metadata png :height))))
                              ((equal icon-type "image/jpeg")
                               (multiple-value-bind (h w ncomp trans) (cl-jpeg:jpeg-file-dimensions icon-path)
                                 (declare (ignore ncomp))
                                 (declare (ignore trans))
                                 (format nil "~Ax~A" w h)))
                              ((equal icon-type "image/gif")
                               (let ((gif (skippy:load-data-stream icon-path)))
                                 (format nil "~Ax~A" (skippy:width gif) (skippy:height gif))))
                              (t (error (format nil "Unsupported icon format: ~A" icon-type))))
                        "512x512"))
         (icon-url-path (format nil "~A.~A" (omg::random-string 20) icon-ext)))
    (add-serve-path manifest-path
                    `(200 (:content-type "text/javascript; charset=utf-8")
                          (,(format nil "{\"name\":\"~A\",\"short_name\":\"~A\",\"display\":\"~A\",\"start_url\":\"~A\",\"theme_color\":\"~A\",\"background_color\":\"~A\",\"icons\"\: [{\"src\": \"~A~A\",\"sizes\": \"~A\",\"type\": \"~A\"}]}"
                                    name short-name display omg::*root-path* theme-color background-color
                                    omg::*root-path* icon-url-path icon-size icon-type))))
    (add-serve-path icon-url-path
                    `(200 (:content-type ,icon-type)
                          ,(if icon-path
                               icon-path
                               *default-icon*)))
    (add-to-root-html-head
      (format nil "
        <title>~A</title>
        <meta name=\"viewport\" content=\"width=device-width, user-scalable=no\"/>
        <link rel=\"manifest\" href=\"~A~A\" />
      " name omg::*root-path* manifest-path))
    (add-to-root-html-head
      (format nil "<link rel=\"icon\" href=\"~A~A\" type=\"~A\" />"
              omg::*root-path* icon-url-path
              icon-type))
    (pushnew '(make-instance 'service-worker) omg::*pre-boot-functions*)))

(defclass-f idb-connection ()
  ((connection :initarg :connection
               :accessor connection)))

(defclass-f idb-object-store ()
  ((name :initarg :name
         :accessor name
         :initform nil)
   (conn :initarg :connection
         :accessor conn
         :initform nil)
   (key :initarg :key
        :accessor key
        :initform nil)
   (auto-increment :initarg :auto-increment
                   :accessor auto-increment
                   :initform nil)
   (trans :initarg :trans
          :initform nil
          :accessor trans)
   (stor :initarg :trans
         :initform nil
         :accessor stor)))

(defclass-f idb-transaction ()
  ((stores :initarg :stores
           :accessor stores
           :initform nil)
   (mode :initarg :mode
         :accessor mode
         :initform :readonly)
   (durability :initarg :durability
               :accessor durability
               :initform :default)
   (conn :initarg :connection
         :accessor conn
         :initform (error "Please, specify a connection for idb-transaction!"))
   (trans :accessor trans)))

(defmethod-f initialize-instance :after ((tr idb-transaction) &rest args)
  (setf (slot-value tr 'stores)
        (map 'vector #'jscl::lisp-to-js
             (let ((str (stores tr)))
               (if str
                   (if (stringp str)
                       (list str)
                       str)
                   (store-names (conn tr))))))
  (setf (slot-value tr 'trans)
        ((jscl::oget (connection (conn tr)) "transaction")
         (stores tr)
         (case (mode tr)
           (:readonly "readonly")
           (:readwrite "readwrite")
           (:readwriteflush "readwriteflush")
           (t (error (format nil "Invalid mode for idb-transaction: ~A, must be :readonly, :readwrite or :readwriteflush!" (mode tr)))))
         (make-js-object :|durability|
           (case (durability tr)
              (:default "default")
              (:strict "strict")
              (:relaxed "relaxed")
              (t (error (format nil "Invalid durability for idb-transaction: ~A, must be :default, :strict or :relaxed" (durability tr)))))))))

(defmethod-f close ((c idb-connection))
  ((jscl::oget (connection c) "close")))

(defmethod-f store-names ((c idb-connection))
  (jscl::oget (connection c) "objectStoreNames"))

(defmethod-f create ((s idb-object-store))
  ((jscl::oget (connection (conn s)) "createObjectStore")
   (name s)
   (apply #'make-js-object
     `(,@(if (key s) `(:|keyPath| ,(key s)))
       ,@(if (auto-increment s) `(:|autoIncrement| t))))))

(defmethod-f initialize-instance :after ((s idb-object-store) &rest args)
  (if (and (not (conn s)) (not (trans s)))
      (error "Please specify a connection for idb-object-store!")
      (when (trans s)
        (setf (slot-value s 'conn) (conn (trans s)))
        (if (not (name s))
            (if (= 1 (length (stores (trans s))))
                (setf (slot-value s 'name) (jscl::js-to-lisp (aref (stores (trans s)) 0)))
                (error "Please, specify a name for idb-object-store! The transaction belongs to multiple stores!")))
        (setf (slot-value s 'stor) ((jscl::oget (trans (trans s)) "objectStore") (jscl::lisp-to-js (name s))))
        (setf (slot-value s 'key) (jscl::oget (stor s) "keyPath"))))
  (if (not (name s))
      (error "Please, specify a name for idb-object-store!"))
  (loop for name across (store-names (conn s)) when (equal (jscl::js-to-lisp name) (name s)) return t
    finally (setf (slot-value s 'stor) (create s))))

(defmethod-f add ((s idb-object-store) key data &key when-ok when-err put raw)
  (let* ((len (if raw
                  (jscl::oget data "byteLength")
                  (omgui::store-to-buffer data (jscl::make-new (winref "SharedArrayBuffer") 1))))
         (buf (if raw
                  data
                  (jscl::make-new (winref "ArrayBuffer") len))))
    (if (not raw) (omgui::store-to-buffer data buf))
    (let ((req ((jscl::oget (stor s) (if put "put" "add")) buf key)))
      (if when-ok (setf (jscl::oget req "onsuccess") (lambda (ev) (funcall when-ok))))
      (setf (jscl::oget req "onerror")
            (lambda (ev)
               (if when-err
                   (funcall when-err (jscl::oget req "error" "message"))
                   (error (format nil "IDB add error: ~A" (jscl::oget req "error" "message")))))))))

(defmethod-f get ((s idb-object-store) key cb &optional err raw)
  (let ((req ((jscl::oget (stor s) "get") key)))
    (setf (jscl::oget req "onsuccess")
          (lambda (ev)
            (let ((res (jscl::oget req "result")))
              (funcall cb
                       (if raw
                           res
                           (if res (omgui::load-from-buffer res)))))))
    (setf (jscl::oget req "onerror")
          (lambda (ev)
            (if err
                (funcall err (jscl::oget req "error" "message"))
                (error (format nil "idb get error: ~A" (jscl::oget req "error" "message"))))))))

(defmethod-f idb-delete ((s idb-object-store) key &key when-ok when-err)
  (let ((req ((jscl::oget (stor s) "delete") key)))
    (if when-ok (setf (jscl::oget req "onsuccess") (lambda (ev) (funcall when-ok))))
    (setf (jscl::oget req "onerror")
          (lambda (ev)
             (if when-err
                 (funcall when-err (jscl::oget req "error" "message"))
                 (error (format nil "IDB delete error: ~A" (jscl::oget req "error" "message"))))))))


(defmethod-f get-all-keys ((s idb-object-store) cb &optional err)
  (let ((req ((jscl::oget (stor s) "getAllKeys"))))
    (setf (jscl::oget req "onsuccess")
          (lambda (ev)
            (funcall cb (map 'vector #'jscl::js-to-lisp (jscl::oget req "result")))))
    (setf (jscl::oget req "onerror")
          (lambda (ev)
            (if err
                (funcall err (jscl::oget req "error" "message"))
                (error (format nil "get-all-keys error: ~A" (jscl::oget req "error" "message"))))))))

(def-local-macro-f with-indexed-db (conn-name-version &rest code)
  (let* ((conn (car conn-name-version))
         (name (cadr conn-name-version))
         (version (caddr conn-name-version))
         (version (if version version 1))
         (req (gensym))
         (err (gensym))
         (ev (gensym)))
    `(let ((,req ((jscl::oget (winref "indexedDB") "open") ,name ,version))
           (,err nil))
       (setf (jscl::oget ,req "onerror")
             (lambda (,ev)
               (error (format nil "Error when opening database ~A: ~A"
                        ,name
                        (jscl::oget ,req "error" "message")))))
       (setf (jscl::oget ,req "onsuccess")
             (lambda (,ev)
               (if (not ,err)
                   (let ((,conn (make-instance 'idb-connection :connection (jscl::oget ,req "result"))))
                     ,@code
                     (close ,conn)))))
       (setf (jscl::oget ,req "onupgradeneeded")
             (lambda (,ev)
               (setf ,err t)
               (if (= ,version 1)
                   (format t "Database ~A does not exists" ,name)
                   (format t "Database upgrade not yet implemented"))
               ((jscl::oget (jscl::oget ,req "result") "close"))))
       ,req)))

(def-local-macro-f setup-indexed-db (conn-name-version &rest code)
  (let* ((conn (car conn-name-version))
         (name (cadr conn-name-version))
         (version (caddr conn-name-version))
         (version (if version version 1))
         (req (gensym))
         (err (gensym))
         (ev (gensym)))
    `(let ((,req ((jscl::oget (winref "indexedDB") "open") ,name ,version))
           (,err nil))
       (setf (jscl::oget ,req "onerror")
             (lambda (,ev)
               (error (format nil "Error when opening database ~A: ~A"
                        ,name
                        (jscl::oget ,req "error" "message")))))
       (setf (jscl::oget ,req "onsuccess")
             (lambda (,ev)
               (let ((,conn (jscl::oget ,req "result")))
                 ((jscl::oget ,conn "close")))))
       (setf (jscl::oget ,req "onupgradeneeded")
             (lambda (,ev)
               (let ((,conn (make-instance 'idb-connection :connection (jscl::oget ,req "result"))))
                 ,@code))))))

(defun-f indexed-db-add (db store key val &key when-ok when-err raw)
  (with-indexed-db (conn db)
    (let* ((tr (make-instance 'idb-transaction :connection conn :stores store :mode :readwrite))
           (st (make-instance 'idb-object-store :trans tr)))
      (add st key val :when-ok when-ok :when-err when-err :raw raw))))

(defun-f indexed-db-put (db store key val &key when-ok when-err raw)
  (with-indexed-db (conn db)
    (let* ((tr (make-instance 'idb-transaction :connection conn :stores store :mode :readwrite))
           (st (make-instance 'idb-object-store :trans tr)))
      (add st key val :when-ok when-ok :when-err when-err :put t :raw raw))))


(def-local-macro-f indexed-db-get (val-db-store-key-raw &rest code)
  (let* ((conn (gensym))
         (tr (gensym))
         (st (gensym))
         (val (car val-db-store-key-raw))
         (db-store-key (cadr val-db-store-key-raw))
         (db (car db-store-key))
         (store (cadr db-store-key))
         (key (caddr db-store-key))
         (raw (caddr val-db-store-key-raw)))
    `(with-indexed-db (,conn ,db)
       (let* ((,tr (make-instance 'idb-transaction :connection ,conn :stores ,store :mode :readonly))
              (,st (make-instance 'idb-object-store :trans ,tr)))
         (get ,st ,key (lambda (,val) ,@code) nil ,raw)))))

(def-local-macro-f indexed-db-delete (db-store-key &rest code)
  (let* ((conn (gensym))
         (tr (gensym))
         (st (gensym))
         (db (car db-store-key))
         (store (cadr db-store-key))
         (key (caddr db-store-key)))
    `(with-indexed-db (,conn ,db)
       (let* ((,tr (make-instance 'idb-transaction :connection ,conn :stores ,store :mode :readwrite))
              (,st (make-instance 'idb-object-store :trans ,tr)))
         (idb-delete ,st ,key :when-ok (lambda () ,@code))))))

(def-local-macro-f indexed-db-get-all-keys (val-db-store &rest code)
  (let* ((conn (gensym))
         (tr (gensym))
         (st (gensym))
         (val (car val-db-store))
         (db-store (cadr val-db-store))
         (db (car db-store))
         (store (cadr db-store)))
    `(with-indexed-db (,conn ,db)
       (let* ((,tr (make-instance 'idb-transaction :connection ,conn :stores ,store :mode :readonly))
              (,st (make-instance 'idb-object-store :trans ,tr)))
         (get-all-keys ,st (lambda (,val) ,@code))))))

(def-local-macro-f if-idb-key (db-store-key true &optional false)
  (let* ((conn (gensym))
         (tr (gensym))
         (st (gensym))
         (val (gensym))
         (db (car db-store-key))
         (store (cadr db-store-key))
         (key (caddr db-store-key)))
    `(with-indexed-db (,conn ,db)
       (let* ((,tr (make-instance 'idb-transaction :connection ,conn :stores ,store :mode :readonly))
              (,st (make-instance 'idb-object-store :trans ,tr)))
         (get ,st ,key
           (lambda (,val)
             (if ,val ,true ,false)))))))

(def-local-macro-f with-self (var &rest code)
  `(let (,var)
     (setf ,var (progn ,@code))))

(def-local-macro-f oget-bind (vars el keys &rest code)
  (let* ((o (gensym))
         (innr `(let (,@(mapcar
                          (lambda (v k)
                            `(,v (jscl::oget ,(if (listp el) o el) ,k)))
                          vars
                          keys))
                  ,@code)))
    (if (listp el)
        `(let ((,o (jscl::oget ,@el)))
           ,innr)
        innr)))

(defun-f now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun-f add-event-listener (event fn &key passive once capture element)
  (let ((options (make-js-object :|once| once
                                 :|passive| passive
                                 :|capture| capture)))
    (if element
        ((jscl::oget element "addEventListener") event fn options)
        (funcall (winref "addEventListener") event fn options))))

(def-local-macro-f with-promise (p &key then catch)
  (let ((pv (gensym)))
    `(let* ((,pv ,p)
            ,@(if then `((,pv ((jscl::oget ,pv "then") ,then))))
            ,@(if catch `((,pv ((jscl::oget ,pv "catch") ,catch)))))
       ,pv)))

(eval-when (:compile-toplevel)
  (with-open-file (fd (merge-pathnames (make-pathname :name "default_icon.png")
                                       (asdf:system-source-directory :omg))
                      :element-type '(unsigned-byte 8))
    (setf *default-icon* (make-array `(,(file-length fd)) :element-type '(unsigned-byte 8)))
    (read-sequence *default-icon* fd)))
