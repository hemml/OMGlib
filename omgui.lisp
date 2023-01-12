(defpackage :omgui
  (:use cl omg jscl bordeaux-threads omgdaemon)
  (:export add-event-handler
           add-style
           add-youtube-player
           append-element
           async-bind
           allow-page-close
           browser-case
           check-element
           close-current-dialog
           create-element
           dialog-ok
           disable-back-button
           disable-scroll
           dragabble-list
           dragabble-list-elements
           dragabble-list-insert
           dragabble-list-insert-position
           element-width
           element-height
           enable-back-button
           enable-scroll
           ensure-element
           ensure-last-version
           execute-after
           find-widget
           gensym2
           get-dialog-data
           get-element-id
           get-omg-cookie-name
           get-my-version
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
           load-js-script
           local-storage
           make-dialog
           make-dragabble-list
           make-js-function
           make-js-object
           make-svg
           make-tab-form
           modal-dialog
           on-element-remove
           page-width
           page-height
           parent-element
           prevent-page-close
           register-hash-cb
           remove-element
           rm-event-handler
           session-storage
           show-notification
           visible-width
           visible-height
           visible-left
           visible-top
           winref))

(in-package :omgui)

(defun-f winref (name)
  (jscl::oget (jscl::%js-vref "window") name))

(defun-f js-parse-float (s)
  (funcall (winref "parseFloat") (jscl::lisp-to-js s)))

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


(defmacro-f async-bind (vcmd &rest cod)
  (let ((res (gensym)))
    `(funcall (jscl::oget (jscl::%js-vref "jscl") "omgAsyncRPC")
              (write-to-string (list ,(package-name *package*)
                                     ',(caadr vcmd)
                                     (list ,@(cdadr vcmd))
                                     omg::*session-id*))
              (lambda (,(car vcmd))
                ,@cod))))

(defun-f make-svg (&rest cod)
  (labels ((process-cmd (cmd args)
             (let* ((symname (symbol-name cmd))
                    (symdown (string-downcase symname))
                    (el ((jscl::oget (jscl::%js-vref "document") "createElementNS")
                         "http://www.w3.org/2000/svg"
                         (if (equal symname (string-upcase symname))
                             symdown
                             symname))))
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
                            (jslog "Invalid cmd:" (car cod))
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

(defun-f jsln (&rest args)
  "Math.log function"
  (apply (jscl::oget (jscl::%js-vref "Math") "log") args))

(defun-f jssin (&rest args)
  "Math.sin function"
  (apply (jscl::oget (jscl::%js-vref "Math") "sin") args))

(defun-f jscos (&rest args)
  "Math.cos function"
  (apply (jscl::oget (jscl::%js-vref "Math") "cos") args))

(defun-f jstan (&rest args)
  "Math.tan function"
  (apply (jscl::oget (jscl::%js-vref "Math") "tan") args))

(defun-f jsasin (&rest args)
  "Math.asin function"
  (apply (jscl::oget (jscl::%js-vref "Math") "asin") args))

(defun-f jsacos (&rest args)
  "Math.acos function"
  (apply (jscl::oget (jscl::%js-vref "Math") "acos") args))

(defun-f jsatan (&rest args)
  "Math.atan function"
  (apply (jscl::oget (jscl::%js-vref "Math") "atan") args))

(defun-f jsatan2 (&rest args)
  "Math.atan2 function"
  (apply (jscl::oget (jscl::%js-vref "Math") "atan2") args))

(defun-f jsmin (&rest args)
  "Math.min function"
  (apply (jscl::oget (jscl::%js-vref "Math") "min") args))

(defun-f jsmax (&rest args)
  "Math.max function"
  (apply (jscl::oget (jscl::%js-vref "Math") "max") args))

(defun-f js-get-element-by-id (id)
  "Get DOM object by ID (must not be called on host!)"
  ((jscl::oget (jscl::%js-vref "document") "getElementById") id))

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
  (setf (jscl::oget (jscl::%js-vref "window") name) code))

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
         "pushState") "" "" (jscl::oget (jscl::%js-vref "window") "location" "href"))
      ((jscl::oget (jscl::%js-vref "history") "back"))
      (setf *onpopstate-installed* t)
      (setf (jscl::oget (jscl::%js-vref "window") "onpopstate")
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
          (jscl::%js-vref "window")
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
            (setf (jscl::oget (jscl::%js-vref "window") "onhashchange")
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

(defun-r need-reload ()
  (and (not (equal *omg-version* +devel-version+))
       (not (equal *omg-version* *omg-last-version*))))

(defun-r get-omg-cookie-name ()
  +omg-version-cookie+)

(defun-r get-last-version ()
  *omg-last-version*)

(defun-r get-my-version ()
  *omg-version*)

(defun-f ensure-last-version ()
  (if (need-reload)
      (progn
        (setf (jscl::oget (jscl::%js-vref "document") "cookie")
              (format nil "~A=~A" (get-omg-cookie-name) (get-last-version)))
        ((jscl::oget (jscl::%js-vref "location") "reload") t))))

(defmacro-f ensure-element (e &rest body)
  (let ((fn (gensym2)))
    `(labels ((,fn ()
                (if (> (jscl::oget ,e "clientWidth") 0)
                    (progn ,@body)
                    (execute-after 0.1 #',fn))))
       (,fn))))

(defparameter-f *global-event-handlers* nil)

(defun-f add-event-handler (path handler)
  (let ((handlers (assoc path *global-event-handlers*  :test #'equal)))
    (if (not handlers)
        (let* ((path-l (omgui::js-string-split path #\.))
               (obj (omgui::deep-oget-1 (jscl::%js-vref "window") (butlast path-l))))
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
