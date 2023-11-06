(defpackage :omgwidgets
  (:use cl omg jscl omgui omgutil)
  (:export omg-widget root render-widget redraw
           editable-field input-size value
           modal-dialog-window
           progress-bar width height bg-style fg-style set-progress
           list-view elements current-position transfer-chunk list-view-element-class
           graph xmin xmax ymin ymax adjust xticks yticks xdelta ydelta xcaption ycaption show-scales plots need-rescale add-plot
                 remove-plot remove-all-plots preserve-aspect-ratio rescale rescale-auto
           plot color parent
           func-plot func
           tabular-plot table
           matrix-plot nx ny palette matrix norm))

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

(defclass-f graph (omg-widget)
  ((xmin :initarg :xmin
         :initform -1
         :accessor xmin)
   (xmax :initarg :xmax
         :initform 1
         :accessor xmax)
   (ymin :initarg :ymin
         :initform -1
         :accessor ymin)
   (ymax :initarg :ymax
         :initform 1
         :accessor ymax)
   (adjust :initarg :adjust
           :initform t
           :accessor adjust)
   (xticks :initarg :xticks
           :initform 5
           :accessor xticks)
   (yticks :initarg :yticks
           :initform 5
           :accessor yticks)
   (xdelta :initarg :xdelta)
   (ydelta :initarg :ydelta)
   (xcaption :initarg :xcaption
           :initform nil
           :accessor xcaption)
   (ycaption :initarg :ycaption
           :initform nil
           :accessor ycaption)
   (show-scales :initarg :scales
                :initform '(:bottom :left)
                :accessor show-scales)
   (graph :accessor graph)
   (plots :initform (list)
          :accessor plots)
   (preserve-aspect-ratio :initform nil
                    :initarg :preserve-aspect-ratio
                    :accessor preserve-aspect-ratio)
   (need-rescale :initform nil
                 :accessor need-rescale)))

(defclass-f graph-scale (omg-widget)
  ((root :initform nil)
   (parent :initarg :graph
           :initform (error "Parent graph must be supplied!")
           :accessor parent)
   (pos :initarg :position
        :accessor pos
        :initform (error "Scale position must be provided!"))))

(defclass-f plot (omg-widget)
  ((root :initform nil)
   (color :initform "red"
          :initarg :color
          :accessor color)
   (parent :accessor parent)))

(defclass-f func-plot (plot)
  ((func :initarg :func
         :initform (error "Function must be provided for func-plot!")
         :accessor func)
   (last-ymin :initform nil
              :accessor last-ymin)
   (last-ymax :initform nil
              :accessor last-ymax)))

(defclass-f tabular-plot (plot)
  ((table :initarg :table
          :initform (error "Table must be provided for tabular-plot!")
          :accessor table)))

(defclass-f matrix-plot (plot)
  ((nx)
   (ny)
   (palette :initarg :palette
            :initform (lambda (x) (list x x x))
            :accessor palette)
   (matrix :initarg :matrix
           :initform (error "Matrix must be provided for matrix-plot!")
           :accessor matrix)
   (norm :initarg :norm
         :initform nil
         :accessor norm)))

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

(defmethod-f render-widget ((g graph))
  (setf (slot-value g 'graph)
        (apply #'create-element
          `("div" :|style.width| "100%"
                  ,@(if (not (preserve-aspect-ratio g))
                        (list :|style.height| "100%")
                        (list :|style.aspectRatio| (/ (- (xmax g) (xmin g)) (- (ymax g) (ymin g)))))
                  :|style.position| "relative"
                  :|style.left| 0
                  :|style.top| 0
                  :|style.overflow| "hidden"
                  :|style.boxSizing| "border-box")))
  (let* ((scales (show-scales g))
         (top-scale (position :top scales))
         (bottom-scale (position :bottom scales))
         (left-scale (position :left scales))
         (right-scale (position :right scales))
         (xcap (xcaption g))
         (ycap (ycaption g)))
    (setf (slot-value g 'root)
          (apply #'create-element
            `("table" :|style.width| "100%"
                      ,@(if (not (preserve-aspect-ratio g)) (list :|style.height| "100%"))
                      :|style.borderSpacing| 0
                      :append-elements
                        ,(if top-scale
                             (list (create-element "tr"
                                     :append-elements (if ycap (list (create-element "td")))
                                     :append-elements (if left-scale (list (create-element "td")))
                                     :append-element
                                       (create-element "td" :|style.height| "2em"
                                                            :|style.padding| 0
                                                            :|style.position| "relative"
                                          :append-element (render-widget (make-instance 'graph-scale :graph g :position :top)))
                                     :append-elements (if right-scale (list (create-element "td"))))))
                      :append-element
                        ,(create-element "tr"
                           :append-elements
                             (if ycap
                                 (list (create-element "td" :|style.padding| 0
                                                            :|style.width| "1em"
                                         :append-element (create-element "div" :|style.position| "relative"
                                                                               :|style.transform| "translateY(-50%) rotate(-90deg)"
                                                                               :append-element ycap))))
                           :append-elements
                             (if left-scale
                                 (list (create-element "td" :|style.padding| 0
                                                            :|style.width| "3em"
                                                            :|style.position| "relative"
                                         :append-element (render-widget (make-instance 'graph-scale :graph g :position :left)))))
                           :append-element
                             (create-element "td" :|style.padding| 0
                                                  :|style.borderSpacing| 0
                               :append-element
                                 (create-element "div" :|style.width| "100%"
                                                       :|style.height| "100%"
                                                       :|style.position| "relative"
                                                       :|style.boxSizing| "border-box"
                                                       :|style.background| "white"
                                                       :|style.boxShadow| "black 0px 0px 0px 1px"
                                   :append-element (graph g)))
                           :append-elements
                             (if right-scale
                                 (list (create-element "td" :|style.padding| 0
                                                            :|style.width| "3em"
                                                            :|style.position| "relative"
                                         :append-element (render-widget (make-instance 'graph-scale :graph g :position :right))))))
                      :append-elements
                        ,(if bottom-scale
                             (list (create-element "tr"
                                     :append-elements (if ycap (list (create-element "td")))
                                     :append-elements (if left-scale (list (create-element "td")))
                                     :append-element
                                       (create-element "td" :|style.height| "2em"
                                                            :|style.padding| 0
                                                            :|style.position| "relative"
                                          :append-element (render-widget (make-instance 'graph-scale :graph g :position :bottom)))
                                     :append-elements (if right-scale (list (create-element "td"))))))
                      :append-elements
                        ,(if xcap
                             (list (create-element "tr"
                                     :append-elements (if ycap (list (create-element "td")))
                                     :append-elements (if left-scale (list (create-element "td")))
                                     :append-element (create-element "td" :|align| "center"
                                                                          :|style.height| "1em"
                                                                          :|style.padding| 0
                                                       :append-element xcap)
                                     :append-elements (if right-scale (list (create-element "td"))))))))))
  (ensure-element (graph g)
    (loop for p in (plots g) do
      (if (not (and (root p) (jscl::oget (root p) "isConnected")))
          (append-element (render-widget p) (graph g)))))
  (rescale-auto g)
  (root g))


(defmethod-f render-widget ((s graph-scale))
  (setf (slot-value s 'root)
        (labels ((trnk (x delta)
                   (let* ((dg (floor (/ (jsln delta) (jsln 10))))
                          (digits (if (< dg 1)
                                      (+ (if (< dg 0)
                                             (- dg)
                                             dg)
                                         1)
                                      0)))
                     (/ (jstrunc (* (expt 10 digits) x))
                        (expt 10 digits)))))
          (let* ((g (parent s))
                 (adjust (adjust g))
                 (pos (pos s))
                 (horizontal (or (equal :top pos)
                                 (equal :bottom pos)))
                 (min (if horizontal (xmin g) (ymin g)))
                 (max (if horizontal (xmax g) (ymax g)))
                 (delta (if horizontal (xdelta g) (ydelta g)))
                 (ft (if adjust
                         (* delta (jsceil (/ min delta)))
                         min)))
            (create-element "div" :|style.position| "absolute"
                                  :|style.top| 0
                                  :|style.bottom| 0
                                  :|style.left| 0
                                  :|style.right| 0
              :append-elements
                (loop for x100 from (* 100 ft) to (* 100 max) by (* 100 delta)
                      for x = (/ x100 100) append
                  (let* ((x1 (/ (- x min) (- max min))))
                    (if horizontal
                        (list (apply #'create-element
                                `("div" :|style.position| "absolute"
                                        :|style.width| ,(format nil "~A%" (* 100 x1))
                                        :|style.borderRight| "1px solid"
                                        :|style.height| "0.5em"
                                        :|style.left| 0
                                        :|style.boxSizing| "border-box"
                                        ,(if (equal pos :bottom) :|style.top| :|style.bottom|) 0))
                              (apply #'create-element "div"
                                `(:|style.position| "absolute"
                                  :|style.left| ,(format nil "~A%" (* 100 x1))
                                  :|style.transform| "translateX(-50%)"
                                  :|innerHTML| ,(format nil "~A" (trnk x delta))
                                  ,(if (equal pos :top) :|style.bottom| :|style.top|) "0.6em")))
                        (list (apply #'create-element
                                `("div" :|style.position| "absolute"
                                        :|style.height| ,(format nil "~A%" (* 100 x1))
                                        :|style.borderTop| "1px solid"
                                        :|style.width| "0.5em"
                                        :|style.bottom| 0
                                        :|style.boxSizing| "border-box"
                                        ,(if (equal pos :right) :|style.left| :|style.right|) 0))
                              (apply #'create-element
                                `("div" :|style.position| "absolute"
                                        :|style.bottom| ,(format nil "~A%" (* 100 x1))
                                        :|style.transform| "translateY(50%)"
                                        ,(if (equal pos :right) :|style.left| :|style.right|) "0.6em"
                                        :|innerHTML| ,(format nil "~A" (trnk x delta)))))))))))))


(defmethod-f render-widget ((p plot))
  (setf (slot-value p 'root)
        (let* ((g (parent p))
               (xmin (xmin g))
               (xmax (xmax g))
               (ymin (ymin g))
               (ymax (ymax g)))
          (make-svg :|viewBox| (format nil "~A ~A ~A ~A" xmin ymin (- xmax xmin) (- ymax ymin))
                    :|style.width| "100%"
                    :|style.height| "100%"
                    :|style.left| "0"
                    :|style.top| "0"
                    :|style.display| "block"
                    :|style.position| "absolute"
                    :|preserveAspectRatio| "none"
                    (make-curve p)))))


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

;;;;;;;;;;;;;; list-view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric make-curve (p))

(defmethod-f make-curve ((p plot))
  (error "Please subclass plot and define the make-curve method!"))

(defmethod-f make-curve ((p tabular-plot))
  (let* ((g (parent p))
         (ymin (ymin g))
         (ymax (ymax g))
         (tbl (table p)))
    `(path :|stroke| ,(color p)
           :|stroke-width| "1px"
           :|vector-effect| "non-scaling-stroke"
           :|fill| "none"
           :|d| ,(apply #'jscl::concat
                        (cons (format nil "M~A,~A " (caar tbl) (+ ymin (- ymax (cdar tbl))))
                              (loop for i in (cdr tbl) collect
                                (format nil "L~A,~A " (car i) (+ ymin (- ymax (cdr i))))))))))

(defmethod-f make-curve ((p func-plot))
  (let* ((g (parent p))
         (xmin (xmin g))
         (xmax (xmax g))
         (ymin (ymin g))
         (ymax (ymax g))
         (grf (graph g))
         (wi (jscl::oget grf "clientWidth"))
         (he (jscl::oget grf "clientWidth")))
    (setf (slot-value p 'last-ymin)
          (setf (slot-value p 'last-ymax)
                (funcall (func p) xmin)))
    `(path :|stroke| ,(color p)
           :|stroke-width| "1px"
           :|vector-effect| "non-scaling-stroke"
           :|fill| "none"
           :|d| ,(apply #'jscl::concat
                        (cons (format nil "M~A,~A " xmin (+ ymin (- ymax (funcall (func p) xmin))))
                              (loop for i from 1 to wi collect
                                (let* ((x (+ xmin (* (- xmax xmin) (/ i wi))))
                                       (f (funcall (func p) x)))
                                  (setf (slot-value p 'last-ymin) (jsmin f (last-ymin p)))
                                  (setf (slot-value p 'last-ymax) (jsmax f (last-ymax p)))
                                  (format nil "L~A,~A " x (+ ymin (- ymax f))))))))))

(defmethod-f make-curve ((p matrix-plot))
  (let* ((g (parent p))
         (ymin (ymin g))
         (ymax (ymax g))
         (canv (create-element "canvas" :|width| (nx p)
                                        :|height| (ny p)))
         (ctx ((jscl::oget canv "getContext") "2d"))
         (idat ((jscl::oget ctx "getImageData") 0 0 (nx p) (ny p)))
         (dat (jscl::oget idat "data"))
         (mt (matrix p))
         (nx (nx p))
         (ny (ny p))
         (min-cf (if (norm p)
                     (loop for i below (* nx ny) minimize (aref mt i))
                     0.0))
         (max-cf (if (norm p)
                     (loop for i below (* nx ny) maximize (aref mt i))
                     1.0))
         (delta-cf (if (and (norm p) (< min-cf max-cf))
                       (/ 1.0 (- max-cf min-cf))
                       1.0)))
    (loop for i below nx do
      (loop for j below ny do
        (let* ((adr (+ i (* nx j)))
               (colors (funcall (palette p)
                                (* delta-cf (- (aref mt adr) min-cf))))
               (idx (* 4 adr)))
          (setf (aref dat (+ idx 3)) 255)
          (loop for o below 3 do
            (setf (aref dat (+ idx o))
                  (* 255 (- 1.0 (nth o colors))))))))
    ((jscl::oget ctx "putImageData") idat 0 0)
    `(image :|width| ,(- (xmax p) (xmin p))
            :|height| ,(- (ymax p) (ymin p))
            :|preserveAspectRatio| "none"
            :|style.imageRendering| "pixelated"
            :|transform| ,(format nil "translate(~A ~A)" (xmin p) (ymin p))
            :|href| ,((jscl::oget canv "toDataURL") "image/bmp"))))

(defgeneric xmin (p))
(defgeneric xmax (p))
(defgeneric ymin (p))
(defgeneric ymax (p))

(defmethod-f xmin ((p plot))
  (xmin (parent p)))

(defmethod-f xmax ((p plot))
  (xmax (parent p)))

(defmethod-f ymin ((p plot))
  (ymin (parent p)))

(defmethod-f ymax ((p plot))
  (ymax (parent p)))

(defmethod-f xmin ((p tabular-plot))
  (apply #'min (mapcar #'car (table p))))

(defmethod-f xmax ((p tabular-plot))
  (apply #'max (mapcar #'car (table p))))

(defmethod-f ymin ((p tabular-plot))
  (apply #'min (mapcar #'cdr (table p))))

(defmethod-f ymax ((p tabular-plot))
  (apply #'max (mapcar #'cdr (table p))))

(defmethod-f ymin ((p func-plot))
  (if (last-ymin p)
      (last-ymin p)
      (ymin (parent p))))

(defmethod-f ymax ((p func-plot))
  (if (last-ymax p)
      (last-ymax p)
      (ymax (parent p))))

(defmethod-f nx ((p matrix-plot))
  (car (array-dimensions (matrix p))))

(defmethod-f ny ((p matrix-plot))
  (cadr (array-dimensions (matrix p))))

(defmethod-f add-plot ((g graph) (p plot) &key rescale)
  (if (position p (plots g))
      (remove-element (root p)))
  (setf (slot-value p 'parent) g)
  (pushnew p (slot-value g 'plots))
  (if rescale
      (setf (slot-value g 'need-rescale) t))
  (if (slot-boundp g 'graph)
      (ensure-element (graph g)
        (if (not (jscl::oget (root p) "isConnected"))
            (append-element (render-widget p) (graph g))
          (rescale-auto g)))))

(defmethod-f remove-plot ((p plot))
  (setf (slot-value (parent p) 'plots)
        (loop for pl in (plots (parent p)) when (not (eql p pl)) collect pl))
  (if (and (root p) (jscl::oget (root p) "isConnected"))
      (remove-element (root p))))

(defmethod-f remove-all-plots ((g graph))
  (map nil
    (lambda (p)
      (if (and (root p) (jscl::oget (root p) "isConnected"))
          (remove-element (root p))))
    (plots g))
  (setf (slot-value g 'plots) (list))
  (redraw g))

(defmethod-f rescale ((g graph) &key xmin xmax ymin ymax xdelta ydelta)
  (macrolet ((upd (&rest vl)
               `(progn
                  ,@(mapcar
                      (lambda (v)
                        `(if ,v (setf (slot-value g ',v) ,v)))
                      vl))))
    (upd xmin xmax ymin ymax xdelta ydelta)
    (redraw g)))

(defmethod-f rescale-auto ((g graph))
  (if (need-rescale g)
      (let* ((plots (plots g))
             (xmin (apply #'min (mapcar #'xmin plots)))
             (xmax (apply #'max (mapcar #'xmax plots)))
             (ymin (apply #'min (mapcar #'ymin plots)))
             (ymax (apply #'max (mapcar #'ymax plots)))
             (xadd (* 0.01 (- ymax ymin)))
             (yadd (* 0.01 (- ymax ymin))))
        (setf (slot-value g 'need-rescale) nil)
        (rescale g :xmin (- xmin xadd) :xmax (+ xmax xadd) :ymin (- ymin yadd) :ymax (+ ymax yadd)))))

(defun-f guess-delta (dx nt)
  (let* ((delta (/ dx nt))
         (dgl (jsfloor (/ (jsln delta) (jsln 10))))
         (dgu (jsceil (/ (jsln delta) (jsln 10))))
         (dg (if (<= (abs (- delta (expt 10 dgl)))
                     (abs (- delta (expt 10 dgu))))
                 dgl
                 dgu))
         (d (expt 10 dg))
         (d1 (if (<= (abs (- delta d))
                     (abs (- delta (* 2 d))))
                 d
                 (* 2 d)))
         (d2 (if (<= (abs (- delta d1))
                     (abs (- delta (* 1.5 d))))
                 d1
                 (* 1.5 d))))
    d2))

(defmethod-f xdelta ((g graph))
  (if (slot-boundp g 'xdelta)
      (slot-value g 'xdelta)
      (guess-delta (- (xmax g) (xmin g)) (xticks g))))

(defmethod-f ydelta ((g graph))
  (if (slot-boundp g 'ydelta)
      (slot-value g 'ydelta)
      (guess-delta (- (ymax g) (ymin g)) (yticks g))))
