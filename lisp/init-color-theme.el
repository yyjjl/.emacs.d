(require 'color-theme)
(require 'color-theme-molokai)

(defun main|disable-themes-first (&rest args)
  ;; Diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(advice-add 'load-theme :before #'main|disable-themes-first)

;; Load molokai theme
(color-theme-molokai)

;; Set font family and font size
(add-to-list 'default-frame-alist `(font . ,emacs|default-font-name))
(when (or window-system (daemonp))
  (run-with-idle-timer 1 nil #'unicode-fonts-setup))

;; This line must be after color-theme-molokai! Don't know why.
(setq color-theme-illegal-faces
      "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock-\\)")

(provide 'init-color-theme)
