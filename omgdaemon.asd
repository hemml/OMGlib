(defsystem "omgdaemon"
  :description "A deployment system for omg-based apps"
  :version "0.0.1"
  :author "Pavel Kaygorodov <hemml@me.com>"
  :licence "GPLv3"
  :homepage "https://github.com/hemml/OMGlib"
  :source-control "https://github.com/hemml/OMGlib.git"
  :depends-on ("omg" "bordeaux-threads" "trivial-utf-8" "swank" "osicat" "uiop" "find-port" "inferior-shell" "iolib" "log4cl" "trivial-dump-core" "cffi")
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:file "omgdaemon")
               (:file "daemonui" :depends-on ("omgdaemon"))))
