(require! 'exwm)
(require 'exwm)
(require 'exwm-config)
(require 'exwm-systemtray)

(add-to-list 'package-selected-packages 'exwm)

(display-time-mode 1)

;; Set workspace number
(setq exwm-workspace-number 2)

;; Set floating window border
(setq exwm-floating-border-width 1)
(setq exwm-floating-border-color "#1b1d1e")

(defun mode-line%exwm-window? (bn)
  (eq major-mode 'exwm-mode))

(defun exwm%create-button (button-line
                                    button-name
                                    button-face
                                    mouse-1-action)
  (propertize button-name
              'face button-face
              'mouse-face 'mode-line-highlight
              'local-map
              (let ((map (make-sparse-keymap)))
                (define-key map (vector button-line 'mouse-1)
                  `(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-1-action)))
                map)))

(defun mode-line%exwm-button ()
  (list " "
        (exwm%create-button
         'mode-line "[X]" 'flycheck-fringe-error
         '(kill-buffer))
        " "
        (exwm%create-button
         'mode-line "[-]" 'flycheck-fringe-info
         '(if (one-window-p)
              (switch-to-buffer (other-buffer))
            (delete-window)))
        " "
        (exwm%create-button
         'mode-line "[T]" 'flycheck-fringe-warning
         '(exwm-floating-toggle-floating))))

(add-to-list 'mode-line-config-alist
             '(mode-line%exwm-window? (mode-line%window-number
                                       mode-line%exwm-button
                                       mode-line%buffer-id)
                                      ()
                                      :no-tail))

(push ?\C-z exwm-input-prefix-keys)
(push ?\C-g exwm-input-prefix-keys)

(defvar exwm--command-history nil)
(defun exwm-run-command (command)
  (interactive (list (read-shell-command "Command: "
                                         (car-safe exwm--command-history)
                                         'exwm--command-history)))
  (start-process-shell-command command nil command))

(defun exwm/switch-buffer (&optional $same-app?)
  (interactive "P")
  (let ((ivy-wrap t)
        (apps (--map (buffer-name (cdr it)) exwm--id-buffer-alist)))
    (if (not apps)
        (message "No apps !!")
      (unless exwm-instance-name
        (switch-to-buffer (car apps)))
      (ivy-read "Switch app: "
                apps
                :initial-input (and $same-app? exwm-instance-name)
                :require-match t
                :action (lambda (x)
                          (delete-other-windows)
                          (switch-to-buffer x))
                :keymap (let ((map (make-sparse-keymap)))
                          (define-key map (this-command-keys) #'ivy-next-line-and-call)
                          map)))))

(defun exwm/use-exwm ()
  (interactive)
  (let ((file (expand-file-name "~/.xsession")))
    (when (file-exists-p file)
      (rename-file file
                   (expand-file-name "~/.xsessionrc")))))

(defun exwm/use-unity ()
  (interactive)
  (let ((file (expand-file-name "~/.xsessionrc")))
    (when (file-exists-p file)
      (rename-file file
                   (expand-file-name "~/.xsession")))))

(define-hook! exwm|floating-setup-hook (exwm-floating-setup-hook)
  (with-current-buffer (window-buffer)
    (force-mode-line-update)
    (when (string-match "unity-control-center.*" exwm-instance-name)
      (exwm-floating--unset-floating exwm--id))))

(define-hook! exwm|auto-rename-buffer (exwm-update-class-hook
                                       exwm-update-title-hook)
  (exwm-workspace-rename-buffer
   (concat "[EXWM: " exwm-instance-name "] " exwm-title)))

(defun exwm*which-key-hack ($fn &rest $args)
  (unless (bound-and-true-p exwm-instance-name)
    (apply $fn $args)))
(advice-add 'which-key--create-buffer-and-show :around #'exwm*which-key-hack)


(defmacro define-exwm-command! (cmd)
  `(lambda!
     (message "%s" (shell-command-to-string ,cmd))))

(defmacro define-exwm-app! ($app &optional $name)
  (unless $name (setq $name $app))
  `(lambda (&optional $force)
     (interactive "P")
     (let ((targets
            (and (not $force)
                 (--filter (string-match ,(format "^\\[EXWM: .*?%s.*?\\]" $name)
                                         it)
                           (mapcar #'buffer-name (buffer-list)))))
           target)
       (if targets
           (switch-to-buffer (if (= 1 (length targets))
                                 (car targets)
                               (ivy-read ,(format "[%s]: " $app) targets)))
         (exwm-run-command ,$app)))))

(with-eval-after-load 'popwin
  (define-key! :map popwin:keymap
    ("g" . (define-exwm-app! "google-chrome"))
    ("n" . (define-exwm-app! "nautilus"))
    ("p" . (define-exwm-app! "evince"))
    ("c" . (define-exwm-app! "unity-control-center"))
    ("t" . (define-exwm-app! "gnome-terminal"))))

(define-key!
  ([XF86KbdBrightnessDown] . (define-exwm-command! "kbdbacklight.sh down"))
  ([XF86KbdBrightnessUp] . (define-exwm-command! "kbdbacklight.sh up"))
  ([XF86AudioMute] . (define-exwm-command!
                       "amixer -D pulse set Master 1+ toggle"))
  ([XF86AudioLowerVolume] . (define-exwm-command!
                              "amixer -D pulse set Master 5%-"))
  ([XF86AudioRaiseVolume] . (define-exwm-command!
                              "amixer -D pulse set Master 5%+"))
  ([print] . (lambda!
               (start-process-shell-command "gnome-screenshot"
                                            nil "gnome-screenshot"))))

(exwm-input-set-key (kbd "s-&") #'exwm-run-command)
(exwm-input-set-key (kbd "M-<tab>") #'exwm/switch-buffer)
(exwm-input-set-key (kbd "M-`") (lambda! (exwm/switch-buffer t)))
(exwm-input-set-key [f8] #'term/pop-shell)
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "M-%d" i))
                      `(lambda ()
                         (interactive)
                         (,(intern (format "select-window-%d" i))))))

;; The following example demonstrates how to use simulation keys to mimic the
;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
;; DEST is what EXWM actually sends to application. Note that SRC must be a key
;; sequence (of type vector or string), while DEST can also be a single key.
(exwm-input-set-simulation-keys
 '(([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\C-y] . ?\C-v)
   ([?\M-w] . ?\C-c)))

;; Don't delete it
(exwm-enable)

(setq exwm-systemtray-height 24)
(exwm-systemtray-enable)

(fset 'save-buffers-kill-terminal 'save-buffers-kill-emacs)

;; Local Variables:
;; no-byte-compile: t
;; End:
