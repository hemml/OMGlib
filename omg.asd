(defsystem "omg"
  :description "A Common Lisp library to build fully dynamic web interfaces"
  :version "0.2.0"
  :author "Pavel Kaygorodov <hemml@me.com>"
  :licence "GPLv3"
  :homepage "https://github.com/hemml/OMGlib"
  :source-control "https://github.com/hemml/OMGlib.git"
  :depends-on ("clack" "websocket-driver-server" "bordeaux-threads" "trivial-utf-8" "media-types" "hunchentoot"
               "pngload" "skippy" "cl-jpeg" "cl-parallel" "cl-base64")
  :build-operation "static-program-op"
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:file "omg")
               (:file "omgui" :depends-on ("omg"))
               (:file "omgutil" :depends-on ("omg"))
               (:file "omgwidgets" :depends-on ("omg" "omgui"))))

(defparameter *jscl-dir* (merge-pathnames (make-pathname :directory '(:relative "jscl"))
                                          (asdf:system-source-directory :omg)))

(load (merge-pathnames (make-pathname :name "jscl.lisp") *jscl-dir*))

(defparameter jscl::*jscl-js* (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))

;; Adopted from jscl.lisp:
(let ((jscl::*features* (list* :jscl :jscl-xc jscl::*features*))
      (jscl::*package* (find-package "JSCL"))
      (jscl::*default-pathname-defaults* *jscl-dir*))
  (setq jscl::*environment* (jscl::make-lexenv))
  (jscl::with-compilation-environment
    (jscl::with-output-to-string (jscl::out jscl::*jscl-js*)
      (format jscl::out "(function(){~%")
      (format jscl::out "'use strict';~%")
      (jscl::write-string (jscl::read-whole-file (jscl::source-pathname "prelude.js")) jscl::out)
      (jscl::do-source input :target
        (jscl::!compile-file input jscl::out :print nil))
      (jscl::dump-global-environment jscl::out)
      (jscl::!compile-file "src/toplevel.lisp" jscl::out :print nil)
      (format jscl::out "})();~%"))))
