(defpackage :daemonui
  (:use cl omg omgdaemon)
  (:export ensure-last-version
           get-omg-cookie-name
           get-my-version))

(in-package :daemonui)

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
