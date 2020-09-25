;;; -*- lexical-binding: t; -*-

(defun ymacs-doctor//stop-doctor-timer (&optional keep-timer)
  (require 'notifications)

  (and (not keep-timer) ymacs-doctor--timer (cancel-timer ymacs-doctor--timer))
  (and ymacs-doctor--notification-id
       (notifications-close-notification ymacs-doctor--notification-id))

  (setq ymacs-doctor--working-secs 0)
  (setq ymacs-doctor--rest-secs 0)
  (setq ymacs-doctor--last-time (current-time)))

(defun ymacs-doctor//start-doctor-timer (check-duration working-duration rest-duration rest-threshhold)
  (require 'notifications)

  (ymacs-doctor//stop-doctor-timer)

  (setq ymacs-doctor--timer
        (run-with-timer
         0 check-duration
         (lambda ()
           (let ((duration (time-since ymacs-doctor--last-time))
                 (idle-time (current-idle-time)))
             (when (and idle-time (> (time-to-seconds idle-time) rest-threshhold))
               (cl-incf ymacs-doctor--rest-secs (time-to-seconds idle-time)))

             (setq ymacs-doctor--working-secs (time-to-seconds duration))
             (when (> ymacs-doctor--rest-secs rest-duration)
               (ymacs-doctor//stop-doctor-timer t))

             (when (> ymacs-doctor--working-secs working-duration)
               (setq
                ymacs-doctor--notification-id
                (notifications-notify
                 :title "WARNING"
                 :replaces-id ymacs-doctor--notification-id
                 :body (format "You have been working on emacs for %.2f minutes!!!\n You need to rest at least %.2f minutes."
                               (/ ymacs-doctor--working-secs 60.0)
                               (/ (- rest-duration ymacs-doctor--rest-secs) 60.0))
                 :urgency 'critical))))))))
