(require 'color-theme)
(require 'color-theme-molokai)

(defun disable-themes-first ()
  ;; diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(advice-add 'load-theme :before #'disable-themes-first)

(color-theme-molokai)

;; make emacs transparent
;; (add-to-list 'default-frame-alist '(alpha 100 70))

;; This line must be after color-theme-molokai! Don't know why.
(setq color-theme-illegal-faces
      "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock-\\)")

;; If we do not use idle-timer, it will fail to setup on daemon mode
(run-with-idle-timer 1 1
                     (lambda ()
                       (set-fontset-font (frame-parameter nil 'font)
                                         'symbol (font-spec :size 18 :family
                                                            symbol-font-name))))

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)
;; NO tool bar or scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(setq-default mode-line-format nil)

(defun default-console-setup-frame (frame)
  (xterm-mouse-mode 1)
  (if window-system
      (mwheel-install)))

(defun default-setup-frame (frame)
  (with-selected-frame frame
    (unless window-system
      (set-frame-parameter nil 'menu-bar-lines 0))))

(defvar after-make-console-frame-hooks '(default-console-setup-frame)
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '(default-setup-frame)
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hook-with-args (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks) frame))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(provide 'init-color-theme)
