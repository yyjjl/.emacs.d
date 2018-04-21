(defvar exwm-config-directory
  (file-name-directory load-file-name))

(add-to-list 'load-path exwm-config-directory)

(require 'exwm-init)

(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)

(defvar exwm-buffer-name-format "#[%s] %s")
(defvar exwm-buffer-name-regexp "^ ?#\\[")

(setq exwm-systemtray-height 24)
(setq exwm-systemtray-icon-gap 5)

(setq exwm-randr-workspace-output-plist '(0 "eDP-1" 4 "HDMI-1" 6 "HDMI-1"))
;; Change to hydra
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --left-of HDMI-1 --auto")))

;; Set workspace number
(setq exwm-workspace-number 9)


;; Set floating window border
(setq exwm-floating-border-width 2)
(setq exwm-floating-border-color "black")

(define-hook! exwm|floating-setup-hook (exwm-floating-setup-hook)
  (when (string-match "unity-control-center.*" exwm-instance-name)
    (exwm-floating--unset-floating exwm--id))
  (run-with-timer 0.3 nil (lambda ()
                            (force-window-update)
                            (exwm-layout--refresh))))

(define-hook! exwm|auto-rename-buffer (exwm-update-title-hook)
  (exwm-workspace-rename-buffer
   (format exwm-buffer-name-format exwm-class-name exwm-title)))

(setq exwm-manage-configurations
      '(((string-match-p "Minibuffer-i3wm" exwm-instance-name)
         floating t
         floating-mode-line nil
         prefix-keys nil)
        ((string-match-p "emacs" exwm-class-name)
         floating-mode-line nil
         tiling-mode-line nil
         prefix-keys nil)))

(define-hook! exwm|setup-apps (exwm-update-class-hook)
  (let ((case-fold-search t))
    (cond ((string-match-p "emacs" exwm-class-name)
           (setq mode-line-format nil))
          ((string-match-p "minibufer-i3wm" exwm-instance-name)
           (exwm-floating-toggle-floating)))
    (when (string-match-p "emacs" exwm-class-name)
      (setq mode-line-format nil))))

;; Don't delete it
(exwm-enable)
(exwm-randr-enable)
(exwm-systemtray-enable)

(require 'exwm-mode-line)
(require 'exwm-keybindings)
(require 'exwm-apps)

(fset 'save-buffers-kill-terminal 'save-buffers-kill-emacs)
