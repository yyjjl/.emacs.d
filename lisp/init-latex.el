;;; -*- lexical-binding: t; -*-

(require-packages!
 (auctex :compile (latex tex preview reftex))
 company-reftex
 company-auctex)

;; C-c RET => Insert macro

(setcdr (assoc-string "\\.[tT]e[xX]\\'" auto-mode-alist) 'latex-mode)
(add-auto-mode! 'latex-mode "\\.tikz\\'")

(autoload 'LaTeX-math-mode "latex" nil t)

(config! latex
  :bind
  (:map LaTeX-mode-map
   ([f5] . TeX-interactive-mode)
   ("M-a" . latex/backward-sexp)
   ("M-e" . latex/forward-sexp)
   ("M-=" . latex/count-words)
   ("C-c E" . TeX-error-overview)
   ("C-c f l" . TeX-error-overview)
   ("C-t" . TeX-fold-dwim)
   ("}" . latex/skip-close-pair)
   (")" . latex/skip-close-pair)
   ("]" . latex/skip-close-pair)
   ("C-c B" . latex/build)
   ("C-c h" . TeX-doc)
   ("C-c C-u" . latex/force-update-style)
   ("C-c s" . LaTeX-section)
   ("C-c e" . LaTeX-environment)
   ("C-c x" . TeX-font)
   ("C-c C-x" . TeX-font))

  :hook
  (setup
   :define (LaTeX-mode-hook)
   (flycheck-mode -1)

   (company-auctex-init)
   (company//add-backend 'company-reftex-labels :main-backend? nil)
   (company//add-backend 'company-reftex-citations :main-backend? nil)

   (setq next-error-function 'TeX-next-error)

   (setq-local prettify-symbols-alist tex--prettify-symbols-alist)
   (add-function :override
     (local 'prettify-symbols-compose-predicate)
     #'tex--prettify-symbols-compose-p)
   (prettify-symbols-mode 1)

   (unless TeX-master
     (setq TeX-master 'dwim))

   ;; Will conflict with latex-mode
   (electric-pair-local-mode -1)
   (electric-indent-local-mode -1)
   (TeX-fold-mode 1)
   (turn-off-auto-fill)
   (LaTeX-math-mode 1)

   (when (buffer-enable-rich-feature-p)
     (reftex-mode 1)
     (TeX-source-correlate-mode 1)
     (TeX-PDF-mode 1)

     (setq TeX-engine 'xetex)

     (outline-minor-mode 1)))

  :config
  (setq TeX-outline-extra nil))

(config! tex-fold
  :bind
  (:map TeX-fold-keymap
   ("B" . TeX-fold-buffer) ("C-b")
   ("c" . TeX-fold-comment) ("C-c")
   ("e" . TeX-fold-env) ("C-e")
   ("d" . TeX-fold-dwim) ("C-o")
   ("P" . TeX-fold-paragraph) ("C-p")
   ("R" . TeX-fold-region) ("C-r"))

  :config
  (setq TeX-fold-command-prefix (kbd "C-c C-o")))

(config! tex
  :bind
  (:map TeX-mode-map
   ([remap next-error] . latex/next-error)
   ([remap previous-error] . latex/previous-error))

  :config
  (add-to-list
   'TeX-command-list
   '("XeLaTeX" "%`xelatex -interaction nonstopmode -shell-escape %(mode)%' %t"
     TeX-run-TeX nil t))

  (setq TeX-debug-bad-boxes nil
        TeX-debug-warnings t
        TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        TeX-electric-math '("$" . "$")
        LaTeX-electric-left-right-brace t
        TeX-complete-expert-commands t
        TeX-electric-sub-and-superscript nil
        ;; Synctex support
        TeX-source-correlate-start-server nil
        font-latex-fontify-script nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil))

(config! tex-mode
  :config
  (dolist (key '("--" "---"))
    (setq tex--prettify-symbols-alist
          (delq (assoc key tex--prettify-symbols-alist)
                tex--prettify-symbols-alist))))

(config! preview
  :bind
  (:map (make-sparse-keymap)
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
  (:map LaTeX-mode-map ("C-c p" :map preview-map))
  :config
  (setq preview-auto-cache-preamble t
        preview-transparent-color "white"
        preview-transparent-border 0
        preview-scale-function 1.4
        preview-default-document-pt 12)

  (set-face-background 'preview-face
                       (face-attribute 'default :background))
  (set-face-background 'preview-reference-face
                       (face-attribute 'default :background)))

(config! reftex
  :bind (:map reftex-mode-map ("C-c i i" . reftex-goto-label))

  :config
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

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

(provide 'init-latex)
