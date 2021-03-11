;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (setq lsp-clients-texlab-executable ymacs-texlab-path)

  (after! lsp-diagnostics
    (add-to-list 'lsp-diagnostics-disabled-modes 'latex-mode)
    (add-to-list 'lsp-diagnostics-disabled-modes 'tex-mode))

  (after! lsp-tex
    (lsp-register-custom-settings
     `(("latex.rootDirectory"
        (lambda ()
          (or (when (stringp TeX-master)
                (file-name-directory TeX-master))
              ".")))
       ("latex.lint.onSave" nil t)))

    (let ((client (ht-get lsp-clients 'texlab)))
      (setf (lsp--client-notification-handlers client)
            (ht ("textDocument/publishDiagnostics" #'ignore)))
      (setf (lsp--client-download-server-fn client)
            (ymacs-lsp//make-download-from-github-fn
             "latex-lsp/texlab"
             (lambda (x) (string-match-p (if sys/linuxp "linux" "macos") x))
             :tgz)))))

(after! latex
  (ymacs-editor//add-toggles
   "LaTeX"
   '(derived-mode-p 'latex-mode)
   '("u" ymacs-latex/force-update-style "Update Style" :exit t)
   '("=" ymacs-latex/count-words "Count Words" :exit t)
   '("i" TeX-interactive-mode "Interactive" :toggle t)
   '("p" TeX-PDF-mode "PDF" :toggle t)
   '("f" TeX-fold-mode "Fold" :toggle t)
   '("s" TeX-source-correlate-mode "Source Correlate" :toggle t)
   '("d w" TeX-toggle-debug-warnings "Debug Warning" :toggle TeX-debug-warnings)
   '("d b" TeX-toggle-debug-bad-boxes "Debug Bad Boxes" :toggle TeX-debug-bad-boxes))

  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)

  (define-key! :map TeX-mode-map
    ("C-c '"))

  (define-key! :map LaTeX-mode-map
    ([f10] . ymacs-latex/build)
    ([f5] . TeX-view)
    ("M-a" . ymacs-latex/backward-sexp)
    ("M-e" . ymacs-latex/forward-sexp)
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
        TeX-syntactic-comment t
        TeX-electric-math '("$" . "$")
        TeX-complete-expert-commands t
        LaTeX-electric-left-right-brace t
        TeX-electric-sub-and-superscript nil
        ;; Synctex support
        TeX-source-correlate-start-server nil
        font-latex-fontify-script nil
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
