(require 'color-theme)
(require 'color-theme-molokai)

(defun core|disable-themes-first (&rest args)
  ;; Diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(advice-add 'load-theme :before #'core|disable-themes-first)

;; Load molokai theme
(color-theme-molokai)

;; Set font family and font size
(add-to-list 'default-frame-alist `(font . ,emacs|default-font-name))
(when (or window-system (daemonp))
  (run-with-idle-timer 1 nil #'unicode-fonts-setup))

;; This line must be after color-theme-molokai! Don't know why.
(setq color-theme-illegal-faces
      "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock-\\)")

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; NO tool bar or scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
;; Do not show mode-line until setup finished
(setq-default mode-line-format nil)

(provide 'init-color-theme)