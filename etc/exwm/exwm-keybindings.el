;; (setq exwm-input-prefix-keys '(?\M-x ?\M-` ?\M-& ?\M-: ?\C-g ?\C-x ?\C-z))
(setq exwm-input-prefix-keys '(?\M-x ?\M-` ?\M-& ?\M-: ?\C-g ?\C-x ?\C-z ?\C-u))

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
(exwm-input-set-key [XF86AudioMute]
                    (define-exwm-command!
                      "amixer -D pulse set Master 1+ toggle"))
(exwm-input-set-key [XF86AudioLowerVolume]
                    (define-exwm-command!
                      "amixer -D pulse set Master 5%-"))
(exwm-input-set-key [XF86AudioRaiseVolume]
                    (define-exwm-command!
                      "amixer -D pulse set Master 5%+"))
(exwm-input-set-key [print]
                    (lambda!
                      (start-process-shell-command "gnome-screenshot"
                                                   nil "gnome-screenshot  --interactive")))

(exwm/define-key  "s-r" #'exwm-reset)
(exwm/define-key  "s-w" #'exwm-workspace-switch)
(exwm/define-key  "s-o" #'other-window)
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
  (exwm/define-key (format "M-%d" i)
                   (intern (format "select-window-%d" i))))
(exwm/define-key "s-1" (lambda! (exwm-workspace-switch 0)))
(exwm/define-key "s-2" (lambda! (exwm-workspace-switch 1)))
(exwm/define-key "s-3" (lambda! (exwm-workspace-switch 2)))
(exwm/define-key "s-4" (lambda! (exwm-workspace-switch 3)))

(exwm-input-set-simulation-keys
 '(([?\s-x] . ?\C-x)
   ([?\s-g] . ?\C-g)
   ([?\s-z] . ?\C-z)))

(provide 'exwm-keybindings)
