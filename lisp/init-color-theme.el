(require 'color-theme)
(require 'color-theme-molokai)

(defadvice load-theme (before disable-themes-first activate)
  ;; diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; make emacs transparent
;; (add-to-list 'default-frame-alist '(alpha 100 70))

(color-theme-molokai)

;; This line must be after color-theme-molokai! Don't know why.
(setq color-theme-illegal-faces
      "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock-\\)")

;; config cjk fonts
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-13"))

;; (when (display-graphic-p)
;;            (dolist (charset '(kana han cjk-misc bopomofo))
;;              (set-fontset-font
;;               (frame-parameter nil 'font)
;;               charset (font-spec :family "Noto Sans S Chinese" :size 22))))

(provide 'init-color-theme)
