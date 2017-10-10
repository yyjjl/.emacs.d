(defmacro define-exwm-app! ($app &optional $name)
  (unless $name (setq $name $app))
  `(defun ,(intern (format "exwm/switch-to-%s" $app)) (&optional $force)
     (interactive "P")
     (let ((targets
            (and (not $force)
                 (--filter (string-match-p
                            ,(concat exwm-buffer-name-regexp $name)
                            it)
                           (--map (buffer-name (cdr it))
                                  exwm--id-buffer-alist))))
           target)
       (if targets
           (exwm-workspace-switch-to-buffer
            (if (= 1 (length targets))
                (car targets)
              (ivy-read ,(format "[%s]: " $app) targets)))
         (exwm%run-command ,$app)))))

(define-key! :prefix "C-z"
  ("g" . (define-exwm-app! "google-chrome" "Google-chrome"))
  ("n" . (define-exwm-app! "nautilus" "Nautilus"))
  ("p" . (define-exwm-app! "evince" "Evince"))
  ("c" . (define-exwm-app! "unity-control-center" "Unity-control-center"))
  ("t" . (define-exwm-app! "gnome-terminal" "Gnome-terminal")))

(provide 'exwm-apps)
