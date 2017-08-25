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
     (link ((t (:underline t :weight bold :foreground "#66D9EF"))))
     (link-visited ((t (:underline t :weight bold :foreground "#AE81EF"))))

     (custom-face-tag ((t (:foreground "#66D9EF" :weight bold))))
     (custom-state ((t (:foreground "#A6E22E"))))
     (region ((t (:background "#403D3D"))))
     (css-selector ((t (:foreground "#F92672"))))
     (css-property ((t (:foreground "#66D9EF"))))
     (diff-added ((t (:foreground "#A6E22E" :weight bold))))
     (diff-context ((t (:foreground "#F8F8F2"))))
     (diff-file-header ((t (:foreground "#66D9EF"))))
     (diff-indicator-added ((t (:foreground "#A6E22E"))))
     (diff-indicator-removed ((t (:foreground "#F92672"))))
     (diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
     (diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
     (diff-removed ((t (:foreground "#F92672" :weight bold))))
     (escape-glyph ((t (:foreground "#E6DB74"))))
     (minibuffer-prompt ((t (:foreground "#AE81EF"))))

     (mode-line ((t (:foreground
                     "#F8F8F2" :background "#2D2D2D"
                     :box (:line-width 5 :color "#2D2D2D")))))
     (mode-line-buffer-id ((t (:background "#000000" :weight semi-bold))))
     (mode-line-inactive ((t (:foreground
                              "#BCBCBC" :background "#1B1D1E"
                              :box (:line-width 5 :color "#1B1D1E")))))
     (mode-line-numbering-face ((t (:box (:line-width 5 :color "black")
                                         :background "black"))))
     (mode-line-highlight ((t (:background "black"
                                           :box (:line-width 5 :color "black")))))
     (mode-line-mousable ((t (:foreground "#BCBCBC" :background "#000000"))))
     (mode-line-mousable-minor-mode
      ((t (:foreground "#BCBCBC" :background "#000000"))))
     (header-line ((t (:foreground "grey90" :background "#2D2D2D"))))

     (font-lock-builtin-face ((t (:foreground "#A6E22E"))))
     (font-lock-comment-face ((t (:foreground "grey" :slant italic))))
     (font-lock-comment-delimiter-face ((t (:foreground "#345678" :slant italic))))
     (font-lock-constant-face ((t (:foreground "#AE81FF"))))
     (font-lock-doc-face ((t (:foreground "#06DB74" :slant italic))))
     (font-lock-function-name-face
      ((t (:foreground "#F92672" :slant italic :weight bold))))
     (font-lock-keyword-face ((t (:foreground "#66D9EF" :weight bold))))
     (font-lock-negation-char-face ((t (:foreground "#CD7673" :weight bold))))
     (font-lock-preprocessor-face ((t (:foreground "#A6E22E" :weight bold))))
     (font-lock-reference-face ((t (:foreground "#AE81FF"))))
     (font-lock-regexp-grouping-backslash
      ((t (:foreground "#AE81FF" :weight bold))))
     (font-lock-regexp-grouping-construct ((t ( :weight bold))))
     (font-lock-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-type-face ((t (:foreground "#66D9EF" :weight bold))))
     (font-lock-variable-name-face ((t (:foreground "#F92672"))))
     (font-lock-warning-face ((t (:foreground "#FFFFFF"
                                              :background "#333333"))))
     (fringe ((t (:background "#1B1D1E"))))
     (highlight-indentation-face ((t (:background "#111213"))))
     ;; (vertical-border ((t (:foreground "#111213"))))
     (vertical-border ((t (:foreground "black"))))
     (highlight ((t (:foreground "#000000" :background "#C4BE89"))))
     (icompletep-choices ((t (:foreground "#F92672"))))
     (icompletep-determined ((t (:foreground "#A6E22E"))))
     (icompletep-keys ((t (:foreground "#F92672"))))
     (icompletep-nb-candidates ((t (:foreground "#AE81FF"))))
     (isearch ((t (:foreground "#C4BE89" :background "#000000"))))
     (isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
     (lazy-highlight ((t (:inherit font-lock-comment-face :background "#000000"))))
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
     (markdown-comment-face ((t (:inherit font-lock-comment-face))))
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
     (show-paren-match
      ((t (:foreground "#000000" :background "#FD971F"))))
     (show-paren-mismatch
      ((t (:foreground "#960050" :background "#1E0010"))))
     (widget-inactive-face ((t (:background "#FF0000"))))
     (woman-addition ((t (:foreground "#AE81FF"))))
     (woman-bold ((t (:foreground "#F92672"))))
     (woman-italic ((t (:foreground "#A6E22E"))))
     (woman-unknown ((t (:foreground "#66D9EF"))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground "#E91E63"))))
     (rainbow-delimiters-depth-2-face ((t (:foreground "#2196F3"))))
     (rainbow-delimiters-depth-3-face ((t (:foreground "#EF6C00"))))
     (rainbow-delimiters-depth-4-face ((t (:foreground "#B388FF"))))
     (rainbow-delimiters-depth-5-face ((t (:foreground "#006600"))))
     (rainbow-delimiters-depth-6-face ((t (:foreground "#26A69A"))))
     (rainbow-delimiters-depth-7-face ((t (:foreground "#FFCDD2"))))
     (rainbow-delimiters-depth-8-face ((t (:foreground "#795548"))))
     (rainbow-delimiters-depth-9-face ((t (:foreground "#DCE775"))))
     (rainbow-delimiters-unmatched-face
      ((t (:foreground "#F8F8F2" :background "#EF6C00"))))
     ;; Company autocomplete

     (company-scrollbar-bg ((t (:background "#3F4346"))))
     (company-scrollbar-fg ((t (:background "#333638"))))
     (company-tooltip ((t (:inherit default :weight bold  :background "#1C1F26"))))
     (company-tooltip-annotation ((t (:weight normal :foreground "orange"))))
     (company-tooltip-annotation-selection
      ((t (:weight normal :foreground "red"))))
     (company-tooltip-common
      ((t (:inherit font-lock-constant-face :underline t))))
     (company-tooltip-selection ((t (:inherit font-lock-keyword-face
                                              :weight bold
                                              :background "#4B5053"))))

     (regex-tool-matched-face ((t (:foreground nil :background nil
                                               :inherit match))))
     (eldoc-highlight-function-argument ((t (:foreground "#86DC2F"
                                                         :weight bold))))
     (hl-sexp-face ((t (:background "gray5"))))
     (hl-line ((t (:background "gray5"))))
     (org-block ((t (:foreground "#8BC34A" :background "#1C1F26"))))
     (org-block-background ((t (:background "#1C1F26"))))
     (org-code ((t (:foreground "#8BC34A" :background "#1C1F26"))))
     (org-checkbox ((t (:foreground "#8BC34A"))))
     (org-column ((t (:background "#37474F"))))
     (org-column-title ((t (:inherit org-column :weight bold :underline t))))
     (org-date ((t (:foreground "#80CBC4" :underline t))))
     (org-document-info ((t (:foreground "#81D4FA" :height 1.35))))
     (org-document-info-keyword ((t (:foreground "#8BC34A" :height 1.35))))
     (org-document-title ((t (:weight bold :height 1.35 :foreground "purple4"))))
     (org-done ((t (:foreground "#8BC34A" :bold t :background "#1B5E20"))))
     (org-ellipsis ((t (:foreground "#81D4DF"))))
     (org-meta-line ((t (:foreground "#9F8766"))))
     (org-footnote ((t (:foreground "#4DD0E1"))))
     (org-formula ((t (:foreground "#F36C60"))))
     (org-link ((t (:foreground "#B39DDB" :underline t))))
     (org-scheduled ((t (:foreground "#8BC34A"))))
     (org-scheduled-previously ((t (:foreground "#FF9800"))))
     (org-scheduled-today ((t (:foreground "#8BC34A"))))
     (org-special-keyword ((t (:foreground "#B0BEC5"))))
     (org-table ((t (:foreground "#E3F2FD" :background "#1C1F26"))))
     (org-todo ((t (:foreground "#FFAB91" :bold t :background "#DD2C00"))))
     (org-upcoming-deadline ((t (:foreground "#FF9800"))))
     (org-warning ((t (:weight bold :foreground "#F36C60"))))
     (org-block-begin-line ((t (:foreground "#B3E5FC" :background "#1E2930"))))
     (org-block-end-line ((t (:foreground "#B3E5FC" :background "#1E2930"))))

     (org-level-1 ((class (:inherit outline-1
                                    :background "grey20"
                                    :weight bold
                                    :overline t
                                    ;; :height 1.3
                                    ))))
     (org-level-2 ((class (:inherit outline-2
                                    :background "grey20"
                                    ;; :height 1.2
                                    ))))
     (org-level-3 ((class (:inherit outline-3 :height 1.1))))
     (org-level-4 ((class (:inherit outline-4))))
     (org-level-5 ((class (:inherit outline-5))))
     (org-level-6 ((class (:inherit outline-6))))
     (org-level-7 ((class (:inherit outline-7))))
     (org-level-8 ((class (:inherit outline-8))))
     (org-level-9 ((class (:inherit outline-9))))

     (window-numbering-face ((t (:foreground "green" :weight bold))))

     (popup-enu-selection-face ((t (:background "#5E5079"
                                                :foreground "#B2B2B2"))))
     (popup-face ((t (:background "#34323E" :foreground "#9A9ABA"))))
     (popup-isearch-match ((t (:inherit match))))
     (popup-menu-face ((t (:background "#34323E" :foreground "#9A9ABA"))))
     (popup-menu-mouse-face ((t (:inherit highlight))))
     (popup-scroll-bar-background-face ((t (:background "#212026"))))
     (popup-scroll-bar-foreground-face ((t (:background "#5D4D7A"))))
     (popup-tip-face
      ((t (:background "#5E5079" :foreground "#B2B2B2"
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

     (whitespace-empty ((t (:background nil :foreground "#9F8766"))))
     (whitespace-indentation ((t (:background nil :foreground "#DC752F"))))
     (whitespace-line ((t (:background nil :foreground "#C56EC3"))))
     (whitespace-newline ((t (:background nil :foreground "#C56EC3"))))
     (whitespace-space ((t (:background nil :foreground "#5D4D7A"))))
     (whitespace-space-after-tab ((t (:background nil :foreground "#9F8766"))))
     (whitespace-space-before-tab ((t (:background nil :foreground "#9F8766"))))
     (whitespace-tab ((t (:background nil))))
     (whitespace-trailing ((t (:background "#DC752F"))))

;;;;; flycheck
     (flycheck-error
      ((((supports :underline (:style line)))
        (:underline (:style line :color "#E0211D")))
       (t (:background "#E0211D" :weight bold :underline t))))
     (flycheck-error-list-checker-name ((t (:foreground "#4F97D7"))))
     (flycheck-fringe-error ((t (:foreground "#E0211D" :weight bold))))
     (flycheck-fringe-info ((t (:foreground "#4F97D7" :weight bold))))
     (flycheck-fringe-warning ((t (:foreground "#DC752F" :weight bold))))
     (flycheck-running ((t (:weight normal :foreground "Green"))))
     (flycheck-info
      ((((supports :underline (:style line)))
        (:underline (:style line :color "#4F97D7")))
       (t (:background "#4F97D7" :weight bold :underline t))))
     (flycheck-warning
      ((((supports :underline (:style line)))
        (:underline (:style line :color "#DC752F")))
       (t (:background "#DC752F" :weight bold :underline t))))

     (diredp-dir-heading ((((background dark))
                           (:inherit font-lock-keyword-face))))

     (haskell-keyword-face ((t (:inherit font-lock-builtin-face))))
     (haskell-type-face ((t (:inherit font-lock-constant-face))))
     (haskell-constructor-face  ((t (:inherit font-lock-type-face))))

     (semantic-highlight-func-current-tag-face ((t (:background "gray5"))))
     (linum-highlight-face ((t (:inherit default :foreground "orange red"))))
     (shm-current-face ((t (:underline (:style line :color "blue")))))
     (ivy-highlight-face ((t (:background "#444155")))))))

(provide 'color-theme-molokai)
