(defpackage :omgwidgets
  (:use cl omg jscl omgui)
  (:export omg-widget
           editable-field
           root
           render-widget
           redraw))

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
           :initform (lambda ())
           :reader cancel-callback)
   (edt-blk :accessor edt-blk
          :initform nil)))

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
