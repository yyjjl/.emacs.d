(require 'color-theme)
(require 'color-theme-molokai)

(defadvice load-theme (before disable-themes-first activate)
  ;; diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; make emacs transparent
;; (add-to-list 'default-frame-alist '(alpha 100 70))
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))
(set-frame-font "Ubuntu Mono-12")

(color-theme-molokai)

;; This line must be after color-theme-molokai! Don't know why.
(setq color-theme-illegal-faces
      "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock-\\)")



(provide 'init-color-theme)
