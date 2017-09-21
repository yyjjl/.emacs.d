(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)

(setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "HDMI-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --right-of HDMI-1 --auto")))

(setq fcitx-use-dbus nil)

(add-to-list 'package-selected-packages 'exwm)

;; Set workspace number
(setq exwm-workspace-number 4)

(defvar exwm-buffer-name-format "[EXWM: %s] %s")
(defvar exwm-buffer-name-regexp "^ ?\\[EXWM: ")

;; Set floating window border
(setq exwm-floating-border-width 1)
(setq exwm-floating-border-color "black")

(defun exwm%exwm-window? (bn &optional action)
  (with-current-buffer bn
    (eq major-mode 'exwm-mode)))

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

(defun exwm*mode-line-workspace (fn)
  (when mode-line-active?
    (set-window-parameter (selected-window)
                          'workspace-index
                          exwm-workspace-current-index))
  (let ((index (window-parameter (selected-window) 'workspace-index)))
    (list (funcall fn)
          (propertize (format " <W%s>" (or index "?"))
                      'face font-lock-string-face))))

(advice-add 'mode-line%window-number :around #'exwm*mode-line-workspace)


(add-to-list 'mode-line-config-alist
             '(exwm%exwm-window? (mode-line%window-number
                                  mode-line%exwm-button
                                  mode-line%buffer-id)
                                 ()
                                 :no-tail))

(push ?\C-z exwm-input-prefix-keys)
(push ?\C-g exwm-input-prefix-keys)
(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
(define-key exwm-mode-map (kbd "C-x RET") #'exwm-workspace-move-window)

(defvar exwm--command-history nil)
(defun exwm-run-command (command)
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

(defun exwm/use-exwm ()
  (interactive)
  (let ((file (expand-file-name "~/.xinitrc")))
    (when (file-exists-p file)
      (rename-file file
                   (expand-file-name "~/.xsessionrc")))))

(defun exwm/use-unity ()
  (interactive)
  (let ((file (expand-file-name "~/.xsessionrc")))
    (when (file-exists-p file)
      (rename-file file
                   (expand-file-name "~/.xinitrc")))))

(define-hook! exwm|floating-setup-hook (exwm-floating-setup-hook)
  (when (string-match "unity-control-center.*" exwm-instance-name)
    (exwm-floating--unset-floating exwm--id))
  (force-window-update)
  (exwm-layout--refresh))

(define-hook! exwm|auto-rename-buffer (exwm-update-class-hook
                                       exwm-update-title-hook)
  (exwm-workspace-rename-buffer
   (format exwm-buffer-name-format exwm-class-name exwm-title)))

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
         (exwm-run-command ,$app)))))

(define-key! :prefix "C-z"
  ("g" . (define-exwm-app! "chrome" "Google-chrome"))
  ("n" . (define-exwm-app! "nautilus" "Nautilus"))
  ("p" . (define-exwm-app! "evince" "Evince"))
  ("c" . (define-exwm-app! "unity-control-center" "Unity-control-center"))
  ("t" . (define-exwm-app! "gnome-terminal" "Gnome-terminal")))

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
                                                   nil "gnome-screenshot")))
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-1") (lambda! (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-2") (lambda! (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-3") (lambda! (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-4") (lambda! (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-&") #'exwm-run-command)
(exwm-input-set-key (kbd "M-<tab>") #'exwm/switch-buffer)
(exwm-input-set-key (kbd "M-`") (lambda! (exwm/switch-buffer :same-app?)))
(exwm-input-set-key [f8] #'term/pop-shell)
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "M-%d" i))
                      (symbol-function (intern (format "select-window-%d" 1)))))

;; The following example demonstrates how to use simulation keys to mimic the
;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
;; DEST is what EXWM actually sends to application. Note that SRC must be a key
;; sequence (of type vector or string), while DEST can also be a single key.
(exwm-input-set-simulation-keys
 '(([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-y] . ?\C-v)
   ([?\C-w] . ?\C-x)
   ([?\M-w] . ?\C-c)))

;; Don't delete it
(exwm-enable)
(exwm-randr-enable)

(fset 'save-buffers-kill-terminal 'save-buffers-kill-emacs)

;; Local Variables:
;; no-byte-compile: t
;; End:
