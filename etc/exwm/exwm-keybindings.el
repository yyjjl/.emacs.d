;; (setq exwm-input-prefix-keys '(?\M-x ?\M-` ?\M-& ?\M-: ?\C-g ?\C-x ?\C-z))
(setq exwm-input-prefix-keys '(?\M-x ?\M-` ?\M-& ?\M-: ?\C-g ?\C-x ?\C-z))

(defvar exwm--command-history nil)
(defun exwm%run-command (command)
  (interactive (list (read-shell-command "$ " nil)))
  (start-process-shell-command command nil command))

(defun exwm/switch-buffer (&optional $same-app?)
  (interactive "P")
  (let ((ivy-wrap t)
        (apps (--map (buffer-name (cdr it))
                     (--filter
                      (let ((buffer (cdr it)))
                        (and (not (string-match-p "^ " (buffer-name buffer)))
                             (or (not $same-app?)
                                 (equal exwm-class-name
                                        (buffer-local-value 'exwm-class-name buffer)))))
                      exwm--id-buffer-alist))))
    (if (not apps)
        (message "No apps !!")
      (add-to-list 'apps (buffer-name))
      (ivy-read "Switch app: "
                apps
                :preselect (buffer-name)
                :require-match t
                :action (lambda (buffer)
                          (let ((win (get-buffer-window buffer)))
                            (if (window-live-p win)
                                (select-window win)
                              (exwm-workspace-switch-to-buffer buffer))))
                :keymap (let ((map (make-sparse-keymap)))
                          (define-key map (this-command-keys)
                            #'ivy-next-line-and-call)
                          map)))))


(defmacro define-exwm-command! (cmd)
  `(lambda!
     (message "%s" (shell-command-to-string ,cmd))))

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

(defvar exwm-input-prefix-keys-extra nil)
(defun exwm/define-key (key def)
  (when (stringp key)
    (setq key (kbd key)))
  (exwm-input-set-key key def)
  (add-to-list 'exwm-input-prefix-keys-extra (elt key 0)))

(exwm-input-set-key [XF86KbdBrightnessDown]
                    (define-exwm-command! "kbdbacklight.sh down"))
(exwm-input-set-key [XF86KbdBrightnessUp]
                    (define-exwm-command! "kbdbacklight.sh up"))
;; (exwm-input-set-key [XF86AudioMute]
;;                     (define-exwm-command!
;;                       "amixer -D pulse set Master 1+ toggle"))
;; (exwm-input-set-key [XF86AudioLowerVolume]
;;                     (define-exwm-command!
;;                       "amixer -D pulse set Master 5%-"))
;; (exwm-input-set-key [XF86AudioRaiseVolume]
;;                     (define-exwm-command!
;;                       "amixer -D pulse set Master 5%+"))
(exwm-input-set-key [print]
                    (lambda!
                      (start-process-shell-command "gnome-screenshot"
                                                   nil "gnome-screenshot")))

(define-key! :prefix "C-z"
  ("g" . (define-exwm-app! "google-chrome" "Google-chrome"))
  ("n" . (define-exwm-app! "nautilus" "Nautilus"))
  ("p" . (define-exwm-app! "evince" "Evince"))
  ("c" . (define-exwm-app! "unity-control-center" "Unity-control-center"))
  ("t" . (define-exwm-app! "gnome-terminal" "Gnome-terminal")))

(exwm/define-key  "s-r" #'exwm-reset)
(exwm/define-key  "s-w" #'exwm-workspace-switch)
(exwm/define-key  "s-o" #'other-window)
;; (exwm/define-key  "s-0" #'delete-window)
;; (exwm/define-key  "s-1" #'delete-other-windows)
(exwm/define-key  "s--" #'window/split-vertically)
(exwm/define-key  "s-\\" #'window/split-horizontally)
(exwm/define-key  "s-k" #'delete-window)
(exwm/define-key  "s-f" #'delete-other-windows)
(exwm/define-key  "s-m" #'exwm-workspace-move-window)
(exwm/define-key  "s-q" #'exwm-input-send-next-key)
(exwm/define-key  "s-&" #'exwm%run-command)
(exwm/define-key  "s-s" #'ivy-switch-buffer)
(exwm/define-key  "s-u" #'universal-argument)
(exwm/define-key  "M-X" #'counsel-linux-app)
(exwm/define-key  "M-<tab>" #'exwm/switch-buffer)
(exwm/define-key  "M-`" (lambda! (exwm/switch-buffer :same-app?)))
(exwm/define-key [f12] #'hydra-emms/body)
(dotimes (i 10)
  (exwm/define-key (format "s-%d" i)
                   (intern (format "select-window-%d" i))))

(exwm-input-set-simulation-keys
 '(([?\s-x] . ?\C-x)
   ([?\s-g] . ?\C-g)
   ([?\s-z] . ?\C-z)))

(global-set-key (kbd "C-c") nil)

(provide 'exwm-keybindings)
