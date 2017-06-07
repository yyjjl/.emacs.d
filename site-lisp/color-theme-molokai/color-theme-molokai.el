;; Author: Adam Lloyd <adam@alloy-d.net>
;;
;; Package-Requires: ((color-theme "6.6.0"))
;;
;; Note: Based on the molokai theme for vim by Tomas Restrepo, which
;; is in turn based on the monokai theme for textmate by Wimer
;; Hazenberg and a darker variant by Hamish Stuart Macpherson.

(eval-when-compile
  (require 'color-theme)
  (require 'color))

(defun color-theme-molokai ()
  "Color theme based on the Molokai color scheme for vim."
  (interactive)
  (color-theme-install
   '(color-theme-molokai
     ((foreground-color . "#F8F8F2")
      (background-color . "#1B1D1E")
      (cursor-color . "#F8F8F0")
      (background-mode . dark))
     (default ((t (:foreground "#F8F8F2" :background "#1B1D1E"))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:weight bold :slant italic))))
     (custom-face-tag ((t (:foreground "#66D9EF" :weight bold))))
     (custom-state ((t (:foreground "#A6E22E"))))
     (italic ((t (:slant italic))))
     (region ((t (:background "#403D3D"))))
     (underline ((t (:underline t))))
     (css-selector ((t (:foreground "#F92672"))))
     (css-property ((t (:foreground "#66D9EF"))))
     (diff-added ((t (:foreground "#A6E22E" :weight bold))))
     (diff-context ((t (:foreground "#F8F8F2"))))
     (diff-file-header ((t (:foreground "#66D9EF" :background nil))))
     (diff-indicator-added ((t (:foreground "#A6E22E"))))
     (diff-indicator-removed ((t (:foreground "#F92672"))))
     (diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
     (diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
     (diff-removed ((t (:foreground "#F92672" :weight bold))))
     (escape-glyph ((t (:foreground "#E6DB74"))))
     (minibuffer-prompt ((t (:foreground "#66D9EF"))))

     (mode-line-inactive ((t (:foreground "#F8F8F2" :background "#181818"))))
     (mode-line-buffer-id ((t (:foreground nil :background "#000000"
                                           :weight semi-bold))))
     (mode-line ((t (:foreground "#BCBCBC" :background "#101010"))))
     (mode-line-highlight ((t (:background "#020303"))))
     (mode-line-mousable ((t (:foreground "#BCBCBC" :background "#000000"))))
     (mode-line-mousable-minor-mode
      ((t (:foreground "#BCBCBC" :background "#000000"))))
     (header-line ((t (:forground "grey90" :background "#2d2d2d"))))

     (font-lock-builtin-face ((t (:foreground "#A6E22E"))))
     (font-lock-comment-face ((t (:foreground "#465457" :slant italic))))
     (font-lock-comment-delimiter-face ((t (:foreground "#465457"
                                                        :slant italic))))
     (font-lock-constant-face ((t (:foreground "#AE81FF"))))
     (font-lock-doc-face ((t (:foreground "#06DB74" :slant italic))))
     (font-lock-function-name-face ((t (:foreground "#F92672"
                                                    :inherit bold-italic))))
     (font-lock-keyword-face ((t (:foreground "#66D9EF" :inherit bold))))
     (font-lock-negation-char-face ((t (:weight bold))))
     (font-lock-preprocessor-face ((t (:foreground "#A6E22E" :inherit bold))))
     (font-lock-reference-face ((t (:foreground "#AE81FF"))))
     (font-lock-regexp-grouping-backslash ((t (:weight bold))))
     (font-lock-regexp-grouping-construct ((t (:weight bold))))
     (font-lock-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-type-face ((t (:foreground "#66D9EF" :inherit bold))))
     (font-lock-variable-name-face ((t (:foreground "#F92672"))))
     (font-lock-warning-face ((t (:foreground "#FFFFFF"
                                              :background "#333333"))))
     (fringe ((t (:background "#1B1D1E"))))
     (highlight-indentation-face ((t (:background "#111213"))))
     ;; (vertical-border ((t (:foreground "#111213"))))
     (vertical-border ((t (:foreground "#1B1D1E"))))
     (highlight ((t (:foreground "#000000" :background "#C4BE89"))))
     (hl-line ((t (:background "#293739"))))
     (icompletep-choices ((t (:foreground "#F92672"))))
     (icompletep-determined ((t (:foreground "#A6E22E"))))
     (icompletep-keys ((t (:foreground "#F92672"))))
     (icompletep-nb-candidates ((t (:foreground "#AE81FF"))))
     (isearch ((t (:foreground "#C4BE89" :background "#000000"))))
     (isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
     (lazy-highlight ((t (:foreground "#465457" :background "#000000"))))
     (markdown-italic-face ((t (:slant italic))))
     (markdown-bold-face ((t (:weight bold))))
     (markdown-header-face ((t (:weight normal))))
     (markdown-header-face-1 ((t (:foreground "#66D9EF"))))
     (markdown-header-face-2 ((t (:foreground "#F92672"))))
     (markdown-header-face-3 ((t (:foreground "#A6E22E"))))
     (markdown-header-face-4 ((t (:foreground "#AE81FF"))))
     (markdown-header-face-5 ((t (:foreground "#E6DB74"))))
     (markdown-header-face-6 ((t (:foreground "#66D9EF"))))
     (markdown-inline-code-face ((t (:foreground "#66D9EF"))))
     (markdown-list-face ((t (:foreground "#A6E22E"))))
     (markdown-blockquote-face ((t (:slant italic))))
     (markdown-pre-face ((t (:foreground "#AE81FF"))))
     (markdown-link-face ((t (:foreground "#66D9EF"))))
     (markdown-reference-face ((t (:foreground "#66D9EF"))))
     (markdown-url-face ((t (:foreground "#E6DB74"))))
     (markdown-link-title-face ((t (:foreground "#F92672"))))
     (markdown-comment-face ((t (:foreground "#465457"))))
     (markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))
     (mumamo-background-chunk-major ((t (:background "#272822"))))
     (mumamo-background-chunk-submode ((t (:background "#1B1D1E"))))
     (outline-1 ((t (:foreground "#66D9EF"))))
     (outline-2 ((t (:foreground "#F92672"))))
     (outline-3 ((t (:foreground "#A6E22E"))))
     (outline-4 ((t (:foreground "#AE81FF"))))
     (outline-5 ((t (:foreground "#E6DB74"))))
     (outline-6 ((t (:foreground "#66D9EF"))))
     (outline-7 ((t (:foreground "#F92672"))))
     (outline-8 ((t (:foreground "#A6E22E"))))
     (secondary-selection ((t (:background "#272822"))))
     (show-paren-match-face ((t (:foreground "#000000" :background "#FD971F"))))
     (show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010"))))
     (widget-inactive-face ((t (:background "#ff0000"))))
     (woman-addition ((t (:foreground "#AE81FF"))))
     (woman-bold ((t (:foreground "#F92672"))))
     (woman-italic ((t (:foreground "#A6E22E"))))
     (woman-unknown ((t (:foreground "#66D9EF"))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground "#e91e63"))))
     (rainbow-delimiters-depth-2-face ((t (:foreground "#2196F3"))))
     (rainbow-delimiters-depth-3-face ((t (:foreground "#EF6C00"))))
     (rainbow-delimiters-depth-4-face ((t (:foreground "#B388FF"))))
     (ainbow-delimiters-depth-5-face ((t (:foreground "#76ff03"))))
     (rainbow-delimiters-depth-6-face ((t (:foreground "#26A69A"))))
     (rainbow-delimiters-depth-7-face ((t (:foreground "#FFCDD2"))))
     (rainbow-delimiters-depth-8-face ((t (:foreground "#795548"))))
     (rainbow-delimiters-depth-9-face ((t (:foreground "#DCE775"))))
     (rainbow-delimiters-unmatched-face ((t (:foreground "#F8F8F2"
                                                         :background "#EF6C00"))))
     ;; Company autocomplete

     (company-scrollbar-bg ((t (:background "#3f4346"))))
     (company-scrollbar-fg ((t (:background "#333638"))))
     (company-tooltip ((t (:inherit default :weight bold  :background "#1c1f26"))))
     (company-tooltip-annotation ((t (:weight normal :foreground "orange"))))
     (company-tooltip-annotation-selection
      ((t (:weight normal :foreground "red"))))
     (company-tooltip-common ((t (:inherit font-lock-constant-face))))
     (company-tooltip-selection ((t (:inherit font-lock-keyword-face
                                              :weight bold
                                              :background "#4b5053"))))

     (regex-tool-matched-face ((t (:foreground nil :background nil
                                               :inherit match))))
     (eldoc-highlight-function-argument ((t (:foreground "#86dc2f"
                                                         :inherit bold))))
     (hl-sexp-face ((t (:background "gray5"))))
     (org-block ((t (:foreground "#8bc34a" :background "#1c1f26"))))
     (org-block-background ((t (:background "#1c1f26"))))
     (org-code ((t (:foreground "#8bc34a" :background "#1c1f26"))))
     (org-column ((t (:background "#37474f"))))
     (org-column-title ((t (:inherit org-column :weight bold :underline t))))
     (org-date ((t (:foreground "#80cbc4" :underline t))))
     (org-document-info ((t (:foreground "#81d4fa" :height 1.35))))
     (org-document-info-keyword ((t (:foreground "#8bc34a" :height 1.35))))
     (org-document-title ((t (:weight bold :height 1.35 :foreground "purple4"))))
     (org-done ((t (:foreground "#8bc34a" :bold t :background "#1b5e20"))))
     (org-ellipsis ((t (:foreground "#81d4df"))))
     (org-meta-line ((t (:foreground "#9f8766"))))
     (org-footnote ((t (:foreground "#4dd0e1"))))
     (org-formula ((t (:foreground "#f36c60"))))
     (org-link ((t (:foreground "#b39ddb" :underline t))))
     (org-scheduled ((t (:foreground "#8bc34a"))))
     (org-scheduled-previously ((t (:foreground "#ff9800"))))
     (org-scheduled-today ((t (:foreground "#8bc34a"))))
     (org-special-keyword ((t (:foreground "#b0bec5"))))
     (org-table ((t (:foreground "#e3f2fd" :background "#1c1f26"))))
     (org-todo ((t (:foreground "#ffab91" :bold t :background "#dd2c00"))))
     (org-upcoming-deadline ((t (:foreground "#ff9800"))))
     (org-warning ((t (:weight bold :foreground "#f36c60"))))
     (org-block-begin-line ((t (:foreground "#b3e5fc" :background "#1e2930"))))
     (org-block-end-line ((t (:foreground "#b3e5fc" :background "#1e2930"))))

     (org-level-1 ((class (:inherit outline-1
                                    ;; :background "#455A64"
                                    :weight bold
                                    :overline t
                                    :height 1.3))))
     (org-level-2 ((class (:inherit outline-2 ;; :background "#35575b"
                                    :height 1.2))))
     (org-level-3 ((class (:inherit outline-3 :height 1.1))))
     (org-level-4 ((class (:inherit outline-4))))
     (org-level-5 ((class (:inherit outline-5))))
     (org-level-6 ((class (:inherit outline-6))))
     (org-level-7 ((class (:inherit outline-7))))
     (org-level-8 ((class (:inherit outline-8))))
     (org-level-9 ((class (:inherit outline-9))))

     (window-numbering-face ((t (:foreground "DeepPink" :weight bold))))

     (popup-enu-selection-face ((t (:background "#5e5079"
                                                :foreground "#b2b2b2"))))
     (popup-face ((t (:background "#34323e" :foreground "#9a9aba"))))
     (popup-isearch-match ((t (:inherit match))))
     (popup-menu-face ((t (:background "#34323e" :foreground "#9a9aba"))))
     (popup-menu-mouse-face ((t (:inherit highlight))))
     (popup-scroll-bar-background-face ((t (:background "#212026"))))
     (popup-scroll-bar-foreground-face ((t (:background "#5d4d7a"))))
     (popup-tip-face
      ((t (:background "#5e5079" :foreground "#b2b2b2"
                       :bold nil :italic nil :underline nil))))

     (web-mode-builtin-face ((t (:inherit font-lock-builtin-face))))
     (web-mode-comment-face ((t (:inherit font-lock-comment-face))))
     (web-mode-constant-face ((t (:inherit font-lock-constant-face))))
     (web-mode-doctype-face ((t (:inherit font-lock-comment-face))))
     (web-mode-function-name-face ((t (:inherit font-lock-function-name-face))))
     (web-mode-html-attr-name-face ((t (:inherit font-lock-function-name-face
                                                 :bold nil))))
     (web-mode-html-attr-value-face ((t (:inherit font-lock-doc-face))))
     (web-mode-html-tag-face ((t (:inherit font-lock-keyword-face :bold nil))))
     (web-mode-keyword-face ((t (:inherit font-lock-keyword-face ))))
     (web-mode-string-face ((t (:inherit font-lock-string-face))))
     (web-mode-symbol-face ((t (:inherit font-lock-type-face :bold nil))))
     (web-mode-type-face ((t (:inherit font-lock-type-face))))
     (web-mode-warning-face ((t (:inherit font-lock-warning-face))))

     (whitespace-empty ((t (:background nil :foreground "#9f8766"))))
     (whitespace-indentation ((t (:background nil :foreground "#dc752f"))))
     (whitespace-line ((t (:background nil :foreground "#c56ec3"))))
     (whitespace-newline ((t (:background nil :foreground "#c56ec3"))))
     (whitespace-space ((t (:background nil :foreground "#5d4d7a"))))
     (whitespace-space-after-tab ((t (:background nil :foreground "#9f8766"))))
     (whitespace-space-before-tab ((t (:background nil :foreground "#9f8766"))))
     (whitespace-tab ((t (:background nil))))
     (whitespace-trailing ((t (:background "#dc752f"))))
;;;;; flycheck
     (flycheck-error
      ((((supports :underline (:style line)))
        (:underline (:style line :color "#e0211d")))
       (t (:background "#e0211d" :inherit bold :underline t))))
     (flycheck-error-list-checker-name ((t (:foreground "#4f97d7"))))
     (flycheck-fringe-error ((t (:foreground "#e0211d" :inherit bold))))
     (flycheck-fringe-info ((t (:foreground "#4f97d7" :inherit bold))))
     (flycheck-fringe-warning ((t (:foreground "#dc752f" :inherit bold))))
     (flycheck-running ((t (:weight normal :foreground "Green"))))
     (flycheck-info
      ((((supports :underline (:style line)))
        (:underline (:style line :color "#4f97d7")))
       (t (:background "#4f97d7" :inherit bold :underline t))))
     (flycheck-warning
      ((((supports :underline (:style line)))
        (:underline (:style line :color "#dc752f")))
       (t (:background "#dc752f" :inherit bold :underline t)))))))

(provide 'color-theme-molokai)




