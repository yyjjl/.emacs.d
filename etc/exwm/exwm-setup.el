(defvar emacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configuration in this directory")
(defvar exwm-config-directory (file-name-directory load-file-name))

;; Add `emacs-config-directory' to `load-path'
(add-to-list 'load-path emacs-config-directory)
(add-to-list 'load-path exwm-config-directory)

(require 'exwm-init)

(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)

(defvar exwm-buffer-name-format "[EXWM: %s] %s")
(defvar exwm-buffer-name-regexp "^ ?\\[EXWM: ")

(require 'exwm-mode-line)
(require 'exwm-keybindings)

(setq exwm-systemtray-height 24)
(setq exwm-systemtray-icon-gap 5)

(setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "HDMI-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --left-of HDMI-1 --auto")))

;; Set workspace number
(setq exwm-workspace-number 4)


;; Set floating window border
(setq exwm-floating-border-width 1)
(setq exwm-floating-border-color "black")

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
  (run-with-timer 0.3 nil (lambda ()
                            (force-window-update)
                            (exwm-layout--refresh))))

(define-hook! exwm|auto-rename-buffer (exwm-update-title-hook)
  (exwm-workspace-rename-buffer
   (format exwm-buffer-name-format exwm-class-name exwm-title)))

(define-hook! exwm|setup-apps (exwm-update-class-hook)
  (let ((case-fold-search t))
    (when (string-match-p "emacs" exwm-class-name)
      (setq mode-line-format nil)
      (setq-local exwm-input-prefix-keys exwm-input-prefix-keys-extra))))

;; Don't delete it
(exwm-enable)
(exwm-randr-enable)
(exwm-systemtray-enable)

(require! 'symon)
(require 'symon)
(setq symon-delay 2)
(setq symon-sparkline-type 'plain)
(add-to-list 'symon-monitors
             'symon-linux-battery-monitor :append)
(add-to-list 'symon-monitors
             'symon-current-time-monitor :append)
(symon-mode 1)
(add-to-list 'package-selected-packages 'symon)

(fset 'save-buffers-kill-terminal 'save-buffers-kill-emacs)
