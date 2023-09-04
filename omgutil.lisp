(defpackage omgutil
  (:use omg cl)
  (:export defclass-m             ;; define a both-side class
           sync-slot              ;; slot syncronization method for mirrored instances
           data-sync              ;; a class for objects which needs a data syncronization
           sync-data)              ;; method called if data synchronization needed for data-sync clacc
  (:import-from omg defclass-m
                    mirrored-object
                    sync-all-data
                    omg-id
                    sync-slot-r
                    sync-slot
                    gethash-lock
                    scope
                    *session-list*
                    *current-session*
                    *m-scope-funcs*))

(in-package :omgutil)

(defvar-f *data-sync-objects* nil)

(defclass-f mirrored-object ()
  ((omg-id :initarg :omg-internal-id)))

(defmethod-f initialize-instance :after ((obj mirrored-object) &key &rest args)
  (setf (jscl::oget (jscl::%js-vref "self") "OMG" "objectRegistry" (slot-value obj 'omg-id)) obj)
  (setf (jscl::oget obj "omgObjId") (slot-value obj 'omg-id)))

(defclass-m data-sync ()
  ((data-epoch :mirrored t
               :initform 0
               :accessor data-epoch)))

(defmethod-f sync-data ((obj data-sync))
  (sync-slot obj 'data-epoch))

(defmethod sync-data ((obj data-sync))
  (incf (data-epoch obj))
  (let ((scf (gethash-lock (class-name (class-of obj)) *m-scope-funcs*)))
    (loop for *current-session* being the hash-values of *session-list* do
      (if (equal (slot-value obj 'scope) (funcall scf obj))
          (remote-exec `(sync-data ,obj))))))

(defun-f sync-all-data ()
  (map nil
    (lambda (obj)
      (let ((new-epoch (sync-slot-r obj 'data-epoch)))
        (if (not (= (data-epoch obj) new-epoch))
            (progn
              (sync-data obj)
              (setf (slot-value obj 'data-epoch) new-epoch)))))
    *data-sync-objects*))

(defmethod-f initialize-instance :after ((obj data-sync) &key &rest args)
  (pushnew obj *data-sync-objects*))
