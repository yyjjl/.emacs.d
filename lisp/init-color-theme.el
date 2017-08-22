(require 'color-theme)
(require 'color-theme-molokai)

(defun main|disable-themes-first (&rest args)
  ;; Diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(advice-add 'load-theme :before #'main|disable-themes-first)

;; Load molokai theme
(color-theme-molokai)

(when (or window-system (daemonp))
  (run-with-idle-timer 1 nil #'unicode-fonts-setup))

;; This line must be after color-theme-molokai! Don't know why.
(setq color-theme-illegal-faces
      "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock-\\)")

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'right-curly-arrow
    [#b11111111
     #b11111111
     #b00000011
     #b00000011
     #b00000011
     #b00000011
     #b00000011
     #b00000011])
  (define-fringe-bitmap 'left-curly-arrow
    [#b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11000000
     #b11111111
     #b11111111]))

(provide 'init-color-theme)
