;; Load this file via (load "example.lisp") in the REPL and open http://localhost:7500 in the browser

(require :omg)

(defpackage :my-test
  (:use cl omg omgui jscl))
(in-package :my-test)

;; (setf omg::*ssl-cert* "sslcert.pem")
;; (setf omg::*ssl-key* "sslkey.pem")
;; (setf omg::*port* 7500)

(defun-f play-yt (video-id)
  (add-youtube-player (append-element (create-element "div"))
                      :video-id video-id
                      :width 800
                      :onready (lambda (ev)
                                 ((jscl::oget ev "target" "mute")) ;; respect autoplay restrictions
                                 ((jscl::oget ev "target" "playVideo"))
                                 (jslog "Starting playback"))
                      :onstatechange (lambda (ev)
                                       (if (equal (jscl::oget ev "data")
                                                  (jscl::oget (jscl::%js-vref "YT") "PlayerState" "PLAYING"))
                                           (progn
                                             (jslog "Playback started!")
                                             (execute-after 10.0 ;; Pause video after 10 sec. playback
                                               (lambda ()
                                                 ((jscl::oget ev "target" "pauseVideo")))))))))


(defun-f up1 (s)
  (string-upcase s))

(defun-r up2 (s)
  (string-upcase s))

(defun-r my-boot ()
  (print "BOOT!")
  (set-debug-session (current-session-id)))

(add-to-boot '(my-boot)) ;; call my-boot just after connection

(restart-server) ;; (re)start a server on default port 7500

;; execute this via SLIME after browser connection:

(in-debug-session
  (print
     (modal-dialog "The header"
                   "The modal dialog test"
                   :lines (list :line1 (list "Upcase on backend:" :filter #'up2)
                                :line2 (list "Upcase on frontend:" :filter #'up1)
                                :pass (list "Upcase on frontend:" :type "password")
                                :line3 "Enter something:"
                                :buttons (list (list "OK" #'dialog-ok)
                                               (list "Cancel" #'close-current-dialog))))))

(play-yt "vla6vpa1-Bk")
