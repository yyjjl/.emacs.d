(require-packages! doom-themes)

(require 'doom-themes)

(defface empty-line-face
  '((t (:foreground "grey50")))
  "empty-line-face"
  :group 'fringe)

(defface winum-inactive-face '()
  "empty-line-face"
  :group 'winum)

(load-theme 'doom-molokai t)
(doom-themes-org-config)

(custom-theme-set-faces
 'user

 '(font-lock-builtin-face ((t (:foreground "#749e20"))))
 '(font-lock-constant-face ((t (:foreground "#AE81FF"))))
 '(font-lock-warning-face
   ((t (:background "black" :weight bold :underline t))))
 '(font-lock-doc-face ((t (:foreground "#777777" :weight bold))))

 '(eldoc-highlight-function-argument
   ((t (:foreground "#86DC2F" :weight bold))))
 '(hl-line ((t (:background "gray17"))))

 '(org-meta-line ((t :inherit font-lock-doc-face)))
 '(org-document-info-keyword ((t :inherit font-lock-builtin-face)))
 '(org-latex-and-related ((t (:foreground "orange red" :weight bold))))

 '(flycheck-error ((t (:underline (:style wave :color "#e74c3c")))))
 '(flycheck-info ((t (:underline (:style wave :color "#b6e63e")))))
 '(flycheck-warning ((t (:underline (:style wave :color "#e2c770")))))

 '(diredp-dir-name ((t (:inherit font-lock-type-face :weight bold))))

 '(haskell-keyword-face ((t (:inherit font-lock-builtin-face))))
 '(haskell-type-face ((t (:inherit font-lock-constant-face))))
 '(show-paren-match ((t (:background "#000000"))))
 '(haskell-constructor-face ((t (:inherit font-lock-type-face))))

 '(js2-external-variable ((t (:inherit font-lock-negation-char-face))))
 '(js2-function-call ((t (:inherit font-lock-regexp-grouping-backslash))))
 '(web-mode-jsx-depth-1-face ((t (:background "#2d2e2e"))))
 '(easy-kill-selection ((t (:inverse-video t :inherit warning))))

 '(ivy-current-match ((t (:background "gray0"))))

 '(semantic-highlight-func-current-tag-face ((t (:background "gray5"))))
 '(shm-current-face ((t (:inherit hl-line))))

 '(winum-face ((t (:inherit org-level-2 :background "black"))))
 '(winum-inactive-face ((t (:background "black")))))

(with-eval-after-load 'org
  (dolist (face org-level-faces)
    (set-face-attribute face nil :height 1.0)))

(provide 'core-theme)
