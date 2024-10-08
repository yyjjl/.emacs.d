;;; -*- lexical-binding: t; -*-

(ymacs-editor//set-forward-sexp-handler
 :modes (latex-mode tex-mode)
 :forward #'ymacs-latex/forward-sexp)

(after! latex
  (ymacs-latex//setup-latexmk)

  (define-key! :map TeX-mode-map
    ("C-c '"))

  (define-key! :map LaTeX-mode-map
    ([f10] . ymacs-latex/build)
    ([f9] . reftex-parse-all)
    ([f5] . TeX-view)
    ("M-=" . ymacs-latex/count-words)
    ("M-RET" . latex-insert-item)
    ("C-c E" . TeX-error-overview)
    (("C-c f l" "C-c l") . TeX-error-overview)
    ("C-t" . TeX-fold-dwim)
    ("C-c h" . TeX-doc)
    ("C-c C-u" . ymacs-latex/force-update-style)
    ("C-c s" . LaTeX-section)
    ("C-c e" . LaTeX-environment)
    ("C-c x" . TeX-font)
    ("C-c C-x" . TeX-font))

  (setq TeX-outline-extra nil))

(setq TeX-fold-command-prefix (kbd "C-c o"))
(after! tex-fold
  (define-key! :map TeX-fold-keymap
    ("B" . TeX-fold-clearout-buffer)
    ("R" . TeX-fold-clearout-region)
    ("P" . TeX-fold-clearout-paragraph)
    ("I" . TeX-fold-clearout-item)
    ("o" . TeX-fold-dwim) ("C-o")
    ("c" . TeX-fold-comment) ("C-c")
    ("e" . TeX-fold-env) ("C-e")
    ("b" . TeX-fold-buffer) ("C-b")
    ("p" . TeX-fold-paragraph) ("C-p")
    ("r" . TeX-fold-region) ("C-r")))

(after! tex
  (define-key! :map TeX-mode-map
    ([remap next-error] . ymacs-latex/next-error)
    ([remap previous-error] . ymacs-latex/previous-error))

  (add-to-list
   'TeX-command-list
   '("XeLaTeX" "%`xelatex -interaction nonstopmode -shell-escape %(mode)%' %t"
     TeX-run-TeX nil t))

  (setq TeX-debug-bad-boxes nil
        TeX-debug-warnings t
        TeX-auto-save t                 ; parse on save
        TeX-parse-self t                ; parse on load
        TeX-electric-math '("$" . "$")
        TeX-complete-expert-commands t
        LaTeX-electric-left-right-brace t
        TeX-electric-sub-and-superscript nil
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; font-latex-fontify-script 'multi-level
        font-latex-fontify-script nil
        ;; font-latex-script-display '((raise -0.3) . (raise 0.3))
        font-latex-quotes nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil))

(after! preview
  (define-key! :map preview-map
    (("C-r" "C-p" "C-b" "C-d" "C-i" "C-e" "C-f" "C-w" "C-s" "C-c"))
    ("p" . preview-at-point)
    ("r" . preview-region)
    ("b" . preview-buffer)
    ("d" . preview-document)
    ("f" . preview-cache-preamble)
    ("i" . preview-goto-info-page)
    ("e" . preview-environment)
    ("s" . preview-section)
    ("w" . preview-copy-region-as-mml)
    ("F" . preview-cache-preamble-off)
    ("P" . preview-clearout-at-point)
    ("R" . preview-clearout)
    ("S" . preview-clearout-section)
    ("B" . preview-clearout-buffer)
    ("D" . preview-clearout-document))

  (define-key! :map LaTeX-mode-map
    ("C-c p" :map preview-map))

  (setq preview-auto-cache-preamble t
        preview-transparent-color "white"
        preview-transparent-border 0
        preview-scale-function 1.4
        preview-default-document-pt 12)

  (set-face-background 'preview-face (face-attribute 'default :background))
  (set-face-background 'preview-reference-face (face-attribute 'default :background)))

(after! reftex
  (setq reftex-cite-format
        '((?t . "\\textcite[]{%l}")
          (?a . "\\autocite[]{%l}")
          (?c . "\\cite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?b . "\\blockcquote[]{%l}{}"))
        reftex-plug-into-AUCTeX '(t t t t t)
        reftex-use-fonts t))
