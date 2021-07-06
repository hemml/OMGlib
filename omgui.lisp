(defpackage :omgui
  (:use cl omg jscl bordeaux-threads)
  (:export add-youtube-player
           append-element
           allow-page-close
           check-element
           close-current-dialog
           create-element
           dialog-ok
           disable-back-button
           disable-scroll
           element-width
           element-height
           enable-back-button
           enable-scroll
           execute-after
           get-dialog-data
           get-element-id
           js-get-element-by-id
           jsfloor
           jslog
           jsmax
           jsmin
           jsrandom
           js-string-split
           load-js-script
           make-dialog
           make-js-function
           make-js-object
           modal-dialog
           page-width
           page-height
           parent-element
           prevent-page-close
           remove-element
           visible-width
           visible-height
           visible-left
           visible-top))

(in-package :omgui)

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
        (let* ((path (js-string-split (symbol-name k) #\.))
               (obj (deep-oget-1 el path))
               (fld (car (last path))))
          (jscl::oset v obj fld)))
    el))

(defun-f append-element (el &optional parent)
  "Append the element el as a child to the parent"
  (if parent
    ((jscl::oget parent "appendChild") el)
    ((jscl::oget (jscl::%js-vref "document")
                 "body"
                 "appendChild")
     el))
  el)

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
                        :|class| "blurbg"))

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
                        :|style.position| "absolute"))

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
(defparameter-f *current-dialog* nil)

(defun-f form-line (key txt &optional (cb (lambda (x) x)))
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
         (inp (create-element "input" :|type| "text"
                                      :|style.font| "inherit"
                                      :|style.fontStyle| "italic"
                                      :|style.fontWeight| "lighter"
                                      :|style.width| "100%"))
         (last-val ""))
    (let ((data (cdr (assoc :data *current-dialog*))))
      (if (not (assoc key data))
          (push (cons key "") (cdr (assoc :data *current-dialog*))))
      (labels ((tcb (ev) ;; the callback is called after every field update
                 (execute-after 0
                   (lambda ()
                     (jslog "MCB" (write-to-string key))
                     (let ((val (jscl::oget inp "value")))
                       (if (not (equal last-val val))
                         (let ((new-val (funcall cb val)))
                           (setf last-val (if (stringp new-val) new-val val))
                           (if (stringp new-val)
                               (let ((selection-start (jscl::oget inp "selectionStart"))
                                     (selection-end (jscl::oget inp "selectionEnd")))
                                 (setf (jscl::oget inp "value") new-val)
                                 (setf (cdr (assoc key (cdr (assoc :data *current-dialog*)))) new-val)
                                 ((jscl::oget inp "setSelectionRange")
                                  selection-start
                                  selection-end))))))))
                 t))
        (setf (jscl::oget inp "onchange") #'tcb)
        (setf (jscl::oget inp "onkeyup") #'tcb)
        (setf (jscl::oget inp "onkeydown") #'tcb)
        (setf (jscl::oget inp "onpaste") #'tcb)
        (setf (jscl::oget inp "ondrop") #'tcb)))
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
                                          :|onclick| (lambda (ev) (funcall (cadr b))))
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
                   (append-element (form-line k (car v) (cadr v)) tbl)))
            (append-element (form-line k v) tbl))))
    tbl))

(defparameter *dialog-wait-list* (make-hash-table))

(defun-r dialog-wl-send (id dat)
  (let ((sem (gethash id *dialog-wait-list*)))
    (if sem
        (progn
          (setf (gethash id *dialog-wait-list*)
                (remove-if (lambda (x) (equal (car x) 'dialog-id)) dat))
          (signal-semaphore sem)))))

(defun-f close-current-dialog (&optional ev no-sem)
  (if (assoc :scroll-disabled *current-dialog*)
      (enable-scroll))
  (let* ((outer (cdr (assoc :outer *current-dialog*)))
         (curtain (cdr (assoc :curtain *current-dialog*)))
         (dat (get-dialog-data))
         (id (assoc 'dialog-id dat)))
    (if outer
        (remove-element outer))
    (if curtain
        (remove-element curtain))
    (if (and (cdr id) (not no-sem))
        (dialog-wl-send (cdr id) nil)))
  (setf *current-dialog* (pop *dialog-stack*)))

(defun-f get-dialog-data ()
  (cdr (assoc :data *current-dialog*)))

(defun-f make-dialog (header-text dialog-text &key lines id)
  "Create modal dialog with header, text and lines. "
  (let* ((curtain (curtain))
         (outer (dialog-frame)))
    (if *current-dialog*
        (push *current-dialog* *dialog-stack*))
    (setf *current-dialog*
          (list (cons :scroll-disabled (disable-scroll))
                (cons :curtain curtain)
                (cons :outer outer)
                (cons :data (list (cons 'dialog-id id)))))
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

(defun-r dialog-ok ()
  (let* ((dat (get-dialog-data))
         (id (assoc 'dialog-id dat)))
    (if id
        (dialog-wl-send (cdr id) dat))
    (close-current-dialog nil t)))

(defmacro modal-dialog (&rest args)
  `(if (current-session-id)
       (let ((id (intern (symbol-name (omg::random-key *dialog-wait-list*)) :omgui))
             (sem (make-semaphore))
             (ar ',args))
         (setf (gethash id *dialog-wait-list*) sem)
         (if (eval `(make-dialog ,@ar :id ',id)) ;; make-dialog is a macro, eval it to pass id
             (progn
               (wait-on-semaphore sem)
               (let ((res (gethash id *dialog-wait-list*)))
                 (remhash id *dialog-wait-list*)
                 res))))))

(defun-f load-js-script (src)
  (append-element (create-element "script" :|src| src)))

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
(defparameter-f *onpopstate-installed* nil)

(defun-f disable-back-button ()
  (if (not *onpopstate-installed*)
    (progn
      ((jscl::oget
         (jscl::%js-vref "history")
         "pushState") "" "" (jscl::oget (jscl::%js-vref "window") "location" "href"))
      ((jscl::oget (jscl::%js-vref "history") "back"))
      (setf *onpopstate-installed* t)
      (setf (jscl::oget (jscl::%js-vref "window") "onpopstate")
            (lambda (ev)
              (if *disable-back* ((jscl::oget (jscl::%js-vref "history") "forward")))))))
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
