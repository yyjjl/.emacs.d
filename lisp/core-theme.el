(require-packages! doom-themes)

(require 'doom-themes)

(defface empty-line-face
  '((t (:foreground "grey50")))
  "Window number in mode-line"
  :group 'fringe)

(load-theme 'doom-molokai t)
(doom-themes-org-config)

(custom-theme-set-faces
 'doom-molokai
 '(mode-line
   ((t (:foreground "#F8F8F2" :background "#2D2D2D"
                    :box (:line-width 2 :color "#4D4D4D")))))
 '(mode-line-inactive
   ((t (:foreground "#BCBCBC" :background "#2D2D2D"
                    :box (:line-width 2 :color "#2D2D2D")))))
 '(mode-line-highlight ((t (:background "black"))))
 '(header-line ((t (:foreground "grey90" :background "#2D2D2D"))))

 '(font-lock-builtin-face ((t (:foreground "#749e20"))))
 '(font-lock-constant-face ((t (:foreground "#AE81FF"))))
 '(font-lock-warning-face
   ((t (:background "black" :weight bold :underline t))))
 '(font-lock-doc-face ((t (:foreground "#777777" :weight bold))))

 '(eldoc-highlight-function-argument
   ((t (:foreground "#86DC2F" :weight bold))))
 '(hl-sexp-face ((t (:background "gray7"))))
 '(hl-line ((t (:background "gray17"))))

 '(org-meta-line ((t :inherit font-lock-doc-face)))
 '(org-document-info-keyword ((t :inherit font-lock-builtin-face)))

 '(flycheck-error ((t (:underline (:style wave :color "#e74c3c")))))
 '(flycheck-info ((t (:underline (:style wave :color "#b6e63e")))))
 '(flycheck-warning ((t (:underline (:style wave :color "#e2c770")))))

 '(widget-field ((t (:inherit hl-line :underline t))))

 ;; '(window-numbering-face
 ;;   ((t (:foreground "DeepPink" :weight bold :underline t))))

 '(haskell-keyword-face ((t (:inherit font-lock-builtin-face))))
 '(haskell-type-face ((t (:inherit font-lock-constant-face))))
 '(haskell-constructor-face ((t (:inherit font-lock-type-face))))

 '(js2-external-variable ((t (:inherit font-lock-negation-char-face))))
 '(js2-function-call ((t (:inherit font-lock-regexp-grouping-backslash))))

 '(easy-kill-selection ((t (:inverse-video t :inherit warning))))
 '(nlinum-current-line ((t (:inherit font-lock-builtin-face))))
 '(semantic-highlight-func-current-tag-face ((t (:background "gray5"))))
 '(shm-current-face ((t (:inherit hl-line))))
 '(ivy-highlight-face ((t (:background "#444155"))))
 '(ivy-current-match ((t (:background "black")))))

(with-eval-after-load 'org
  (dolist (face org-level-faces)
    (set-face-attribute face nil :height 1.0)))

(provide 'core-theme)
