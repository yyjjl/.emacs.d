;;; -*- lexical-binding: t; -*-

(defvar core--doctor-last-time nil)
(defvar core--doctor-timer nil)
(defvar core--doctor-notification-id nil)
(defvar core--doctor-working-secs nil)
(defvar core--doctor-rest-secs nil)

(require 'notifications)

(defun core//stop-doctor-timer (&optional keep-timer)
  (and (not keep-timer) core--doctor-timer (cancel-timer core--doctor-timer))
  (and core--doctor-notification-id
       (notifications-close-notification core--doctor-notification-id))

  (setq core--doctor-working-secs 0)
  (setq core--doctor-rest-secs 0)
  (setq core--doctor-last-time (current-time)))

(defun core//start-doctor-timer (check-duration working-duration rest-duration rest-threshhold)
  (core//stop-doctor-timer)

  (setq core--doctor-timer
        (run-with-timer
         0 check-duration
         (lambda ()
           (let ((duration (time-since core--doctor-last-time))
                 (idle-time (current-idle-time)))
             (when (and idle-time (> (time-to-seconds idle-time) rest-threshhold))
               (cl-incf core--doctor-rest-secs (time-to-seconds idle-time)))

             (setq core--doctor-working-secs (time-to-seconds duration))
             (when (> core--doctor-rest-secs rest-duration)
               (core//stop-doctor-timer t))

             (when (> core--doctor-working-secs working-duration)
               (setq
                core--doctor-notification-id
                (notifications-notify
                 :title "WARNING"
                 :replaces-id core--doctor-notification-id
                 :body (format "You have been working on emacs for %.2f minutes!!!\n You need to rest at least %.2f minutes."
                               (/ core--doctor-working-secs 60.0)
                               (/ (- rest-duration core--doctor-rest-secs) 60.0))
                 :urgency 'critical))))))))

(core//start-doctor-timer 60 3600 900 60)

(provide 'init-doctor)
