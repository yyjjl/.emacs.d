;;; my-theme.el --- A Theme based on the material theme

(deftheme mymaterial
  "A UI Theme for Emacs based on material design colors")
(display-color-cells (selected-frame))
(let* ((class '((class color) (min-colors 89)))
       (256color  (eq (display-color-cells (selected-frame)) 256))
       (truecolor (eq (display-color-cells (selected-frame)) 16777216))

       (background "#1b1d1e") ;; sidebar-container
       (current-line (if (or window-system truecolor)  "#37474f" "#3a3a3a")) ;; tree-row
       (far-background (if (or window-system truecolor)  "#1c1f26" "#121212")) ;; panel-control
       (inactive-gray (if (or window-system truecolor) "#78909c" "#8a8a8a"))
       (header-color (if (or window-system truecolor) "#455A64" "#5f5f5f"))
       (subtle "#a7adba") ;; tree-row-hover-disclosure-button-control
       (selection "#555555") ;; tab-control-dirty-tab-close-button
       (secondary-selection "#bf616a") ;; tab-control-hover-tab-close-button
       (foreground "#f8f8f2")
       (comment "#b0bec5") ;; table-row
       (red "#f36c60") ;; tab-control-hover-tab-close-button
       (orange "#ff9800") ;; darker tab-control-dirty-tab-close-butto
       (yellow "#fff59d") ;; tab-control-dirty-tab-close-button
       (green "#8bc34a") ;; complement tab-control-dirty-tab-close-button
       (aqua "#81d4fa") ;; lighter complement tab-control-dirty-tab-close-button
       (blue "#4dd0e1") ;; complement tab-control-dirty-tab-close-button
       (purple "#b39ddb")) ;; complement tab-control-dirty-tab-close-button

  (custom-theme-set-faces
   'mymaterial
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))
   `(font-lock-builtin-face ((,class (:foreground "#ff8A65"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground "moccasin"))))
   `(font-lock-doc-string-face ((,class (:foreground ,yellow))))
   `(font-lock-function-name-face ((,class (:foreground ,"#84ffff"))))
   `(font-lock-keyword-face ((,class (:foreground ,yellow))))
   `(font-lock-negation-char-face ((,class (:foreground ,blue))))
   `(font-lock-preprocessor-face ((,class (:foreground "gold"))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
   `(font-lock-string-face ((,class (:foreground "#9ccc65"))))
   `(font-lock-type-face ((,class (:foreground "#84ffff"))))
   `(font-lock-variable-name-face ((,class (:foreground ,"#ffcc80"))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red))))
   `(highlight-numbers-number ((,class (:foreground ,"#9ccc65"))))
   `(shadow ((,class (:foreground ,comment))))
   `(success ((,class (:foreground "SeaGreen2"))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,orange))))

   ;; ace-window faces
   `(aw-leading-char-face ((,class (:foreground ,
                                    foreground
                                    :background ,
                                    "#ef6c00"
                                    :height ,
                                    1.7
                                    :weight
                                    bold))))

   ;; ace-jump-faces
   `(ace-jump-face-foreground ((,class (:foreground ,foreground
                                        :background ,"#ef6c00"
                                        :weight bold))))

   `(ace-jump-face-background ((,class (:foreground ,inactive-gray
                                        :weight normal))))

   ;; avy-jump-mode
   `(avy-background-face ((,class (:foreground ,inactive-gray
                                   :weight normal))))
   `(avy-lead-face-0 ((,class (:foreground ,foreground
                               :background ,"#ef6c00"
                                        :weight bold))))
   `(avy-lead-face ((,class (:foreground ,foreground
                             :background ,"#ef6c00"
                             :weight bold))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,orange)))))

   ;; highlight indentation
   `(highlight-indentation-face ((,class (:background, current-line))))
   `(highlight-indentation-current-column-face ((,class (:background, far-background))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,"#e91e63"))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,"#2196F3"))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,"#EF6C00"))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,"#B388FF"))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,"#76ff03"))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,"#26A69A"))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,"#FFCDD2"))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,"#795548"))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,"#DCE775"))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,foreground :background ,"#EF6C00"))))

   ;; MMM-mode
   `(mmm-code-submode-face ((,class (:background ,current-line))))
   `(mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((,class (:background ,current-line))))

   ;; Search
   `(match ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(isearch ((,class (:foreground ,foreground :background ,green))))
   `(isearch-lazy-highlight-face ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(lazy-highlight-face ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

   ;; which-function
   `(which-func ((,class (:foreground ,blue :background nil))))

   ;; Emacs interface
   `(cursor ((,class (:background ,orange))))
   `(fringe ((,class (:background ,background))))
   `(linum ((,class (:background ,background :foreground ,subtle))))
   `(linum-highlight-face ((,class (:background ,current-line :foreground ,foreground))))
   `(border ((,class (:background ,current-line))))
   `(vertical-border ((,class (:background ,selection
                               :foreground, selection))))
   `(border-glyph ((,class (nil))))
   `(highlight ((,class (:inverse-video nil :background ,current-line))))
   `(hl-line ((,class (:inverse-video nil :background ,current-line))))
   `(gui-element ((,class (:background ,current-line :foreground ,foreground))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(region ((,class (:background ,selection))))
   `(secondary-selection ((,class (:background ,secondary-selection))))

   `(header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))

   `(trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-line ((,class (:background nil :foreground ,red))))
   `(whitespace-indentation ((,class (:background nil :foreground ,aqua))))
   `(whitespace-space ((,class (:background nil :foreground ,selection))))
   `(whitespace-newline ((,class (:background nil :foreground ,selection))))
   `(whitespace-tab ((,class (:background nil :foreground ,selection))))
   `(whitespace-hspace ((,class (:background nil :foreground ,selection))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match-face ((,class (:background ,aqua :foreground "black"))))
   `(show-paren-mismatch-face ((,class (:background "red1" :foreground "white"))))

   ;; Smartparens paren matching
   `(sp-show-pair-match-face ((,class (:foreground "black" :background ,aqua :inherit show-paren-match))))
   `(sp-show-pair-mismatch-face ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((,class (:foreground ,comment :background nil))))

   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((,class (:weight bold))))
   `(slime-repl-input-face ((,class (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
   `(slime-repl-result-face ((,class (:foreground ,green))))
   `(slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

   `(csv-separator-face ((,class (:foreground ,orange))))

   `(diff-hl-insert ((,class (:background ,green :foreground ,green))))
   `(diff-hl-change ((,class (:background ,aqua :foreground ,aqua))))
   `(diff-hl-delete ((,class (:background ,orange :foreground ,orange))))

   `(diff-added ((,class (:foreground ,green))))
   `(diff-changed ((,class (:foreground ,aqua))))
   `(diff-removed ((,class (:foreground ,orange))))
   `(diff-header ((,class (:foreground ,aqua :background nil))))
   `(diff-file-header ((,class (:foreground ,blue :background nil))))
   `(diff-hunk-header ((,class (:foreground ,purple))))
   `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
   `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

   `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

   `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

   ;; macrostep
   `(macrostep-expansion-highlight-face ((,class (:inherit highlight :foreground nil))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

   ;; dired+
   `(diredp-compressed-file-suffix ((,class (:foreground ,blue))))
   `(diredp-deletion ((,class (:inherit error :inverse-video t))))
   `(diredp-deletion-file-name ((,class (:inherit error))))
   `(diredp-dir-heading ((,class (:foreground ,green :weight bold))))
   `(diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
   `(diredp-exec-priv ((,class (:foreground ,blue :background nil))))
   `(diredp-executable-tag ((,class (:foreground ,red :background nil))))
   `(diredp-file-name ((,class (:foreground ,yellow))))
   `(diredp-file-suffix ((,class (:foreground ,green))))
   `(diredp-flag-mark ((,class (:foreground ,green :inverse-video t))))
   `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((,class (:foreground ,comment))))
   `(diredp-link-priv ((,class (:background nil :foreground ,purple))))
   `(diredp-mode-line-flagged ((,class (:foreground ,red))))
   `(diredp-mode-line-marked ((,class (:foreground ,green))))
   `(diredp-no-priv ((,class (:background nil))))
   `(diredp-number ((,class (:foreground ,yellow))))
   `(diredp-other-priv ((,class (:background nil :foreground ,purple))))
   `(diredp-rare-priv ((,class (:foreground ,red :background nil))))
   `(diredp-read-priv ((,class (:foreground ,green :background nil))))
   `(diredp-symlink ((,class (:foreground ,purple))))
   `(diredp-write-priv ((,class (:foreground ,yellow :background nil))))

   `(link ((,class (:foreground nil :underline t))))
   `(widget-button ((,class (:underline t :weight bold))))
   `(widget-field ((,class (:background ,current-line))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((,class (:foreground ,yellow))))
   `(compilation-line-number ((,class (:foreground ,yellow))))
   `(compilation-message-face ((,class (:foreground ,blue))))
   `(compilation-mode-line-exit ((,class (:foreground ,green))))
   `(compilation-mode-line-fail ((,class (:foreground ,red))))
   `(compilation-mode-line-run ((,class (:foreground ,blue))))

   ;; Grep
   `(grep-context-face ((,class (:foreground ,comment))))
   `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,blue))))
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; guide-key
   `(guide-key/key-face ((,class (:foreground ,foreground ))))
   `(guide-key/highlight-command-face ((,class (:foreground ,yellow ))))
   `(guide-key/prefix-command-face ((,class (:foreground ,aqua ))))

   `(org-agenda-structure ((,class (:foreground ,aqua :bold t))))
   `(org-agenda-date ((,class (:foreground ,blue :underline nil))))
   `(org-agenda-done ((,class (:foreground ,green))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
   `(org-block ((,class (:foreground ,green :background ,far-background))))
   `(org-block-background ((,t (:background ,far-background))))
   `(org-code ((,class (:foreground ,green :background ,far-background))))
   `(org-column ((,class (:background ,current-line))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,"#80cbc4" :underline t))))
   `(org-document-info ((,class (:foreground ,aqua :height 1.35))))
   `(org-document-info-keyword ((,class (:foreground ,green :height 1.35))))
   `(org-document-title ((,class (:weight bold :foreground ,foreground :height 1.35))))
   `(org-done ((,class (:foreground ,green :bold t :background,"#1b5e20"))))
   `(org-ellipsis ((,class (:foreground ,comment))))
   `(org-footnote ((,class (:foreground ,aqua))))
   `(org-formula ((,class (:foreground ,red))))
   `(org-hide ((,class (:foreground ,background :background ,background))))
   `(org-link ((,class (:foreground ,blue :underline t))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,orange))))
   `(org-scheduled-today ((,class (:foreground ,green))))
   `(org-special-keyword ((,class (:foreground ,comment))))
   `(org-table ((,class (:foreground ,"#e3f2fd" :background ,far-background))))
   `(org-todo ((,class (:foreground ,"#ffab91" :bold t :background ,"#dd2c00"))))
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-warning ((,class (:weight bold :foreground ,red))))
   `(org-block-begin-line ((,class (:foreground ,"#b3e5fc" :background "#1e2930"))))
   `(org-block-end-line ((,class (:foreground ,"#b3e5fc" :background "#1e2930"))))
   `(org-kbd ((,class (:background ,inactive-gray :foreground ,foreground))))

   `(org-level-1 ((,class (:inherit outline-1
                         :background ,header-color
                         :weight bold
                         :height 1.3))))
   `(org-level-2 ((,class (:inherit outline-2
                                  :background ,"#35575b"
                         :height 1.2))))
   `(org-level-3 ((,class (:inherit outline-3 :height 1.1))))
   `(org-level-4 ((,class (:inherit outline-4 :height 1.0))))
   `(org-level-5 ((,class (:inherit outline-5 ))))
   `(org-level-6 ((,class (:inherit outline-6 ))))
   `(org-level-7 ((,class (:inherit outline-7 ))))
   `(org-level-8 ((,class (:inherit outline-8 ))))
   `(org-level-9 ((,class (:inherit outline-9 ))))

   `(markdown-header-face-1 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.3 ))))
   `(markdown-header-face-2 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.2 ))))
   `(markdown-header-face-3 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-4 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-5 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-6 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-7 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-8 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-9 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-delimiter-face ((,class (:inherit font-lock-function-name-face :weight bold
                                              :height 1.2))))
   `(markdown-url-face ((,class (:inherit link))))
   `(markdown-link-face ((,class (:foreground ,blue :underline t))))

   ;`(hl-sexp-face ((,class (:background ,current-line))))
   `(highlight-symbol-face ((,class (:background ,selection))))
   `(highlight-80+ ((,class (:background ,current-line))))

   ;; Python-specific overrides
   `(py-builtins-face ((,class (:foreground ,"#ff7043" :weight normal))))

   ;; js2-mode
   `(js2-warning ((,class (:underline ,orange))))
   `(js2-error ((,class (:foreground nil :underline ,red))))
   `(js2-external-variable ((,class (:foreground ,purple))))
   `(js2-function-param ((,class (:foreground ,blue))))
   `(js2-instance-member ((,class (:foreground ,blue))))
   `(js2-private-function-call ((,class (:foreground ,red))))

   ;; js3-mode
   `(js3-warning-face ((,class (:underline ,orange))))
   `(js3-error-face ((,class (:foreground nil :underline ,red))))
   `(js3-external-variable-face ((,class (:foreground ,purple))))
   `(js3-function-param-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,orange))))
   `(js3-jsdoc-type-face ((,class (:foreground ,aqua))))
   `(js3-jsdoc-value-face ((,class (:foreground ,yellow))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
   `(js3-instance-member-face ((,class (:foreground ,blue))))
   `(js3-private-function-call-face ((,class (:foreground ,red))))

   ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((,class (:underline ,red))))

   ;; Message-mode
   `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
   `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
   `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
   `(message-header-name ((,class (:foreground ,blue :background nil))))
   `(message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))))
   `(message-separator ((,class (:foreground ,purple))))

   ;; cfw emacs calendar
   `(cfw:face-title ((,class (:background ,background :foreground ,"#eceff1" :height 1.3 :weight bold))))
   `(cfw:face-today ((,class (:foreground ,foreground))))
   `(cfw:face-day-title ((,class (:background ,current-line :foreground ,foreground))))
   `(cfw:face-today-title ((,class (:background ,secondary-selection :foreground ,foreground))))
   `(cfw:face-header ((,class (:background ,current-line :foreground ,foreground))))
   `(cfw:face-sunday ((,class (:background ,current-line :foreground ,aqua :weight bold))))
   `(cfw:face-saturday ((,class (:background ,current-line :foreground ,aqua :weight bold))))
   `(cfw:face-select ((,class (:background ,selection :foreground ,foreground))))
   `(cfw:face-toolbar ((,class (:background ,aqua :foreground ,background :weight bold))))
   `(cfw:face-toolbar-button-off ((,class (:background ,aqua :foreground ,background :weight bold))))
   `(cfw:face-toolbar-button-on ((,class (:background ,aqua :foreground ,secondary-selection :weight bold))))
   `(cfw:face-holiday ((,class (:background ,current-line :foreground ,green :weight bold))))

   ;; Company autocomplete
   ;; `(company-echo ((,class ())))
   ;; `(company-echo-common ((,class ())))

   `(company-preview ((,class (:foreground ,comment :background ,inactive-gray))))
   `(company-preview-common ((,class (:foreground ,comment :background ,inactive-gray)))) ; same background as highlight-line
   ;; `(company-preview-search ((,class ())))
   `(company-scrollbar-bg ((,class (:background "#F0F0F0"))))
   `(company-scrollbar-fg ((,class (:background "#C0C0C0"))))
   `(company-template-field ((,class (:background ,inactive-gray))))
   `(company-tooltip ((,class (:weight bold :foreground, far-background :background ,inactive-gray))))
   `(company-tooltip-annotation ((,class (:weight normal :foreground ,comment :background ,inactive-gray))))
   `(company-tooltip-annotation-selection ((,class (:weight normal :inherit company-tooltip-selection))))
   `(company-tooltip-common ((,class (:weight normal :inherit company-tooltip))))
   `(company-tooltip-common-selection ((,class (:weight normal :inherit company-tooltip-selection))))
   ;; `(company-tooltip-mouse ((,class ())))
   ;; `(company-tooltip-search ((,class ())))
   `(company-tooltip-selection ((,class (:weight bold :foreground ,foreground :background ,current-line))))

   `(outline-1 ((,class (:inherit nil :foreground ,"#eceff1"))))
   `(outline-2 ((,class (:inherit nil :foreground ,"#e1f5fe"))))
   `(outline-3 ((,class (:inherit nil :foreground ,"#a5d6a7" ))))
   `(outline-4 ((,class (:inherit nil :foreground ,"#ffcc80" ))))
   `(outline-5 ((,class (:inherit nil :foreground ,"#b3e5fc"))))
   `(outline-6 ((,class (:inherit nil :foreground ,"CadetBlue1"))))
   `(outline-7 ((,class (:inherit nil :foreground ,"aquamarine1"))))
   `(outline-8 ((,class (:inherit nil :foreground ,purple))))
   `(outline-9 ((,class (:inherit nil :foreground ,"LightSteelBlue1"))))



   `(font-latex-bold-face                 ((t (:inherit bold :foreground ,foreground))))
   `(font-latex-doctex-documentation-face ((t (:background unspecified))))
   `(font-latex-doctex-preprocessor-face ((t (:inherit (font-latex-doctex-documentation-face
                                                        font-lock-builtin-face font-lock-preprocessor-face)))))
   `(font-latex-italic-face               ((t (:inherit italic :foreground ,foreground))))
   `(font-latex-math-face                 ((t (:foreground ,blue))))
   `(font-latex-sectioning-0-face         ((t (:inherit outline-1 :height 1.4))))
   `(font-latex-sectioning-1-face         ((t (:inherit outline-2 :height 1.35))))
   `(font-latex-sectioning-2-face         ((t (:inherit outline-3 :height 1.3))))
   `(font-latex-sectioning-3-face         ((t (:inherit outline-4 :height 1.25))))
   `(font-latex-sectioning-4-face         ((t (:inherit outline-5 :height 1.2))))
   `(font-latex-sectioning-5-face         ((t (:inherit outline-6 :height 1.1))))
   `(font-latex-sedate-face               ((t (:foreground ,green))))
   `(font-latex-slide-title-face          ((t (:inherit font-lock-type-face :weight bold :height 1.2))))
   `(font-latex-string-face               ((t (:inherit font-lock-string-face))))
   `(font-latex-subscript-face            ((t (:height 0.8))))
   `(font-latex-superscript-face          ((t (:height 0.8))))
   `(font-latex-warning-face              ((t (:inherit font-lock-warning-face)))))
  )

(provide-theme 'mymaterial)
(provide 'my-theme)
