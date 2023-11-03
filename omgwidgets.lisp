(defpackage :omgwidgets
  (:use cl omg jscl omgui)
  (:export omg-widget
           editable-field
           root
           render-widget
           redraw
           modal-dialog-window
           progress-bar
           set-progress))

(in-package :omgwidgets)

(defparameter-f *last-edt-fld* nil)

(defclass-f omg-widget ()
  ((root :accessor root)
   (id :initform (omgui::random-id)
       :accessor id)))

(defclass-f editable-field (omg-widget)
  ((root :initform (create-element "span"
                     :|style.textDecorationStyle| "dashed"
                     :|style.textDecorationLine| "underline"
                     :|style.textDecorationThicknes| "1.75pt"
                     :|style.color| "blue"
                     :|title| "change"
                     :add-style ":hover {cursor:pointer;}"))
   (input-size :initarg :input-size
               :initform 20
               :accessor input-size)
   (value :initarg :value
          :initform ""
          :accessor val)
   (ok :initarg :ok
       :initform (lambda (val)
                   (declare (ignore val))
                   (setf (slot-value w 'value) val)
                   val)
       :reader ok-callback)
   (cancel :initarg :cancel
           :initform (lambda (w) (declare (ignore w)))
           :reader cancel-callback)
   (edt-blk :accessor edt-blk
          :initform nil)))

(defclass-f modal-dialog-window (omg-widget)
  ((mdw-curtain :accessor mdw-curtain)))

(defclass-f progress-bar (omg-widget)
  ((width :accessor width
          :initarg :width
          :initform "20em")
   (height :accessor height
          :initarg :height
          :initform "1em")
   (bg-style :accessor bg-style
             :initarg :bg-style
             :initform '(:|style.border| "1px solid black"
                         :|style.background| "white"))
   (fg-style :accessor fg-style
             :initarg :fg-style
             :initform '(:|style.background| "blue"))
   (value :accessor value
          :initarg :value
          :initform 0)
   (bar :accessor bar)))

;;;;;;;;;;;;;;;;; render-widget ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric-f render-widget (w))

(defmethod-f render-widget ((w omg-widget))
  (root w))

(defmethod-f render-widget ((w editable-field))
  (setf (jscl::oget (root w) "onclick")
        (lambda (ev)
          (declare (ignore ev))
          (to-edit-state w)))
  (setf (jscl::oget (root w) "innerHTML") (val w))
  (root w))

(defmethod-f render-widget ((w modal-dialog-window))
  (append-element (setf (slot-value w 'mdw-curtain)
                        (create-element "div" :|style.opacity| "80%"
                                              :|style.background| "white"
                                              :|style.width| (page-width)
                                              :|style.height| (page-height)
                                              :|style.position| "absolute"
                                              :|style.top| "0"
                                              :|style.left| "0"
                                              :|zIndex| 100000)))
  (setf (slot-value w 'root)
        (create-element "div" :|style.visibility| "hidden"
                              :|style.width| "auto"
                              :|style.display| "inline-block"
                              :|style.position| "absolute"
                              :|zIndex| 200000))
  (ensure-element (root w)
    (setf (jscl::oget (root w) "style" "left")
          (format nil "~Apx"
                    (+ (visible-left)
                       (jsfloor (/ (- (visible-width)
                                      (element-width (root w)))
                                   2)))))
    (setf (jscl::oget (root w) "style" "top")
          (format nil "~Apx"
              (+ (visible-top)
                 (jsfloor (/ (- (visible-height)
                                (element-height (root w)))
                             2)))
           "px"))
    (setf (jscl::oget (root w) "style" "visibility") "visible"))
  (root w))

(defmethod-f render-widget ((b progress-bar))
  (setf (slot-value b 'root)
        (apply #'create-element
          `("div" ,@(bg-style b) :|style.width| ,(width b) :|style.height| ,(height b)
                                 :|style.position| "relative"
            :append-element
              ,(make-svg :|viewBox| "0 0 100 100"
                         :|style.width| "100%"
                         :|style.height| "100%"
                         :|style.left| "0"
                         :|style.top| "0"
                         :|style.display| "block"
                         :|style.position| "absolute"
                         :|preserveAspectRatio| "none"
                  '(:|defs| (:|linearGradient| :|id| "boxgrad"
                                               :|x1| "0"
                                               :|x2| "1"
                              (:|stop| :|offset| "0%" :|stop-color| "white")
                              (:|stop| :|offset| "50%" :|stop-color| "#e0e0e0")
                              (:|stop| :|offset| "100%" :|stop-color| "white")
                              (:|animate| :|attributeName| "x1"
                                          :|values| "-1;1"
                                          :|dur| "1s"
                                          :|repeatCount| "indefinite")
                              (:|animate| :|attributeName| "x2"
                                          :|values| "0;2"
                                          :|dur| "1s"
                                          :|repeatCount| "indefinite")))
                  '(:|rect| :|x| 0 :|y| 0 :|width| "100%" :|height| "100%" :|fill| "url('#boxgrad')"))
            :append-element
              ,(setf (slot-value b 'bar)
                     (apply #'create-element
                       `("div" ,@(fg-style b)
                               :|style.height| "100%"
                               :|style.position| "absolute"
                               :|style.left| 0
                               :|style.top| 0
                               :|style.width| ,(format nil "~A%" (jsfloor (* 100 (value b))))))))))
  (root b))
;;;;;;;;;;;;;;;; redraw ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric-f redraw (omg-widget))

(defmethod-f redraw ((w omg-widget))
  (if (slot-boundp w 'root)
    (let* ((old-root (root w))
           (new-root (render-widget w))
           (parent (parent-element old-root)))
      (if (and old-root (jscl::oget old-root "isConnected"))
        (progn
          (setf (jscl::oget old-root "style" "display") "none")
          ((jscl::oget parent "insertBefore") new-root old-root)
          (remove-element old-root))))))

;;;;;;;;;;;;;; editable-field ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod-f to-edit-state ((w editable-field))
  (if *last-edt-fld*
      (to-show-state *last-edt-fld*))
  (let ((ok-button (create-element "button" :|innerHTML| "update"
                                            :|style.marginLeft| "1em"))
        (cancel-button (create-element "button" :|innerHTML| "cancel"
                                                :|style.marginLeft| "1em"))
        (edit-fld (create-element "input" :|type| "text"
                                          :|value| (val w)
                                          :|size| (input-size w))))
    (setf (jscl::oget ok-button "onclick")
          (lambda (ev)
            (let ((res (funcall (ok-callback w) (jscl::oget edit-fld "value"))))
              (if res
                  (progn
                    (setf (slot-value w 'value) res)
                    (to-show-state w)
                    (setf *last-edt-fld* nil))))))
    (setf (jscl::oget cancel-button "onclick")
          (lambda (ev)
            (to-show-state w)
            (if (cancel-callback w) (funcall (cancel-callback w)))))
    (setf (slot-value w 'edt-blk)
          (create-element "span" :|style.display| "inline-block"
            :append-elements `(,edit-fld ,cancel-button ,ok-button)))
    (setf (jscl::oget (root w) "style" "display") "none")
    ((jscl::oget (parent-element (root w)) "insertBefore") (edt-blk w) (root w))
    ((jscl::oget edit-fld "focus"))))

(defmethod-f to-show-state ((w editable-field))
  (setf (jscl::oget (root w) "innerHTML" ) (val w))
  (setf (jscl::oget (root w) "style" "display") "inline-block")
  (if (edt-blk w) (remove-element (edt-blk w))))

;;;;;;;;;;;;;; modal-dialog-window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod-f close ((w modal-dialog-window))
  (remove-element (mdw-curtain w))
  (remove-element (root w)))

;;;;;;;;;;;;;; progress-bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod-f set-progress ((b progress-bar) val)
  (setf (slot-value b 'value) (min 1 (max 0 val)))
  (setf (jscl::oget (bar b) "style" "width") (format nil "~A%" (* 100 (value b)))))0

