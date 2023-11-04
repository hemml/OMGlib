(defpackage :omgwidgets
  (:use cl omg jscl omgui omgutil)
  (:export omg-widget
           editable-field
           root
           render-widget
           redraw
           list-view
           list-view-element
           list-view-element-class
           elements
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

(defclass-m list-view (omg-widget data-sync)
  ((elements :accessor elements
     :initform nil)
   (current-position :accessor current-position
                     :initform 150
                     :mirrored t)
   (transfer-chunk :initform 50
                   :accessor transfer-chunk
                   :browser-side t)
   (mean-height :browser-side t
                :initform nil
                :accessor mean-height)
   (f-elements :accessor f-elements
               :initform nil
               :browser-side t)
   (list-view-element-class :browser-side t
                  :initform 'list-view-element
                  :accessor list-view-element-class)
   (root :initform (create-element "div")
         :accessor root)))

(defclass-f list-view-element (omg-widget)
  ((loaded :initform nil
           :accessor loaded
           :initarg :loaded)
   (pos :initform (error "Position must be defined!")
        :accessor pos
        :initarg :pos)))

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

(defmethod-f render-widget ((e list-view-element))
  (setf (slot-value e 'root)
        (if (loaded e)
            (render-widget (loaded e))
            (create-element "div" :|style.position| "relative"
                                                  :|style.left| 0
                                                  :|style.right| 0
                                                  :|style.height| "1em"
                                                  :|style.marginTop| "0.2em"
                                                  :|style.background| "#f0f0f0"))))

(defmethod-f render-widget ((l list-view))
  (let ((el (create-element "div" :append-elements (mapcar #'render-widget (slot-value l 'f-elements)))))
    (ensure-element el
      (onscroll l (f-elements l) (current-position l))
      (let ((scroll-tim nil))
        (setf (jscl::oget (parent-element el) "onscroll")
              (lambda (ev)
                (if scroll-tim (funcall (jscl::%js-vref "clearTimeout") scroll-tim))
                (setf scroll-tim (execute-after 0.1 (lambda () (onscroll l)))))))
      (let ((cur-el (nth (current-position l) (f-elements l))))
        (if cur-el
            (setf (jscl::oget (parent-element el) "scrollTop")
                  (- (jscl::oget (slot-value cur-el 'root) "offsetTop")
                     (jscl::oget ((jscl::oget (parent-element el) "getBoundingClientRect")) "top"))))))
    (setf (slot-value l 'root) el)))

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

;;;;;;;;;;;;;; list-view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod-f initialize-instance :after ((l list-view) &key &rest args)
  (sync-data l))

(defmethod-r elements-length ((l list-view))
  (length (elements l)))

(defmethod-r get-elements ((l list-view) from to)
  (subseq (elements l) from to))

(defmethod-r get-element ((l list-view) num)
  (nth num (elements l)))

(defmethod-f sync-data :after ((l list-view))
  (setf (slot-value l 'f-elements)
        (let* ((cs (transfer-chunk l))
               (ps (current-position l))
               (start (max 0 (- ps cs)))
               (el (elements-length l))
               (end (min (- el 1) (+ ps cs cs)))
               (iitms (get-elements l start (+ end 1)))
               (ec (list-view-element-class l)))
          (append (loop for i from 0 below start collect
                        (make-instance ec :pos i))
                  (loop for i from start below (+ end 1) collect
                        (make-instance ec :pos i :loaded (nth (- i start) iitms)))
                  (loop for i from (+ end 1) below el collect
                        (make-instance ec :pos i)))))
  (redraw l))

(defmethod-f onscroll ((l list-view) &optional (cdl (f-elements l)) first-passed)
  (if cdl
      (let* ((eh (jscl::oget (root (car cdl)) "clientHeight"))
             (tc (transfer-chunk l))
             (scroll-top (jscl::oget (parent-element (root l)) "scrollTop"))
             (ofs (- scroll-top (* 3 tc eh)))
             (bot (+ ofs (jscl::oget (parent-element (root l)) "clientHeight") (* 3 tc eh)))
             (bnd (jscl::oget ((jscl::oget (parent-element (root l)) "getBoundingClientRect")) "top"))
             (stk nil)
             (rem nil)
             (first-el nil))
        (loop for el on cdl do
          (let* ((e (car el))
                 (r (root e))
                 (rtop (- (jscl::oget r "offsetTop") bnd)))
            (setf rem el)
            (if (and (> rtop (- scroll-top bnd)) (not first-el))
                (setf first-el (pos e)))
            (if (> rtop bot)
                (loop-finish)
                (if (and (>= rtop ofs)
                         (not (loaded e)))
                    (progn
                      (push e stk)
                      (if (>= (length stk) (transfer-chunk l))
                          (loop-finish)))))))
        (if (and (not first-passed) first-el)
            (setf (slot-value l 'current-position) first-el))
        (if stk
            (map nil
                 (lambda (e i)
                   (setf (slot-value e 'loaded) i)
                   (redraw e))
                 (reverse stk)
                 (get-elements l (pos (car (last stk))) (+ 1 (pos (car stk))))))
        (if (and stk rem) (execute-after 0.01 (lambda () (onscroll l rem first-el)))))))

(defmethod-f sync-slot :after ((l list-view) slot)
  (if (equal slot 'elements)
      (redraw l)))
