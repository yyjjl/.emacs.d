(autoload 'LaTeX-math-mode "latex" nil t)
(defun LaTeX-mode-setup ()
  (add-to-list  'company-backends 'company-auctex-labels)
  (add-to-list  'company-backends 'company-auctex-bibs)
  (add-to-list  'company-backends '(company-auctex-macros
                                   company-auctex-symbols
                                   company-auctex-environments))
  (turn-on-reftex)
  (LaTeX-math-mode 1)
  (TeX-source-correlate-mode 1)
  (TeX-PDF-mode 1)
  (TeX-fold-mode 1)

  (setq TeX-engine 'xetex)

  (outline-minor-mode 1))

(add-hook 'LaTeX-mode-hook 'LaTeX-mode-setup)

(with-eval-after-load 'tex
  (defun latex-build ()
    (interactive)
    (progn
      (let ((TeX-save-query nil))
        (TeX-save-document (TeX-master-file)))
      (let ((command (if (save-excursion
                           (goto-char 1)
                           (search-forward-regexp "\\\\usepackage\\s-*{\\s-*minted"
                                                  nil t))
                         "XeLaTeX"
                       TeX-command-default)))
        (TeX-command command  'TeX-master-file -1))))

  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%`xelatex -shell-escape%(mode)%' %t" TeX-run-TeX nil t))

  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil))

(with-eval-after-load 'latex
  (setq TeX-fold-command-prefix (kbd "C-c f"))

  (require 'tex-fold)

  (defun latex-font-bold () (interactive) (TeX-font nil ?\C-b))
  (defun latex-font-medium () (interactive) (TeX-font nil ?\C-m))
  (defun latex-font-code () (interactive) (TeX-font nil ?\C-t))
  (defun latex-font-emphasis () (interactive) (TeX-font nil ?\C-e))
  (defun latex-font-italic () (interactive) (TeX-font nil ?\C-i))
  (defun latex-font-clear () (interactive) (TeX-font nil ?\C-d))
  (defun latex-font-calligraphic () (interactive) (TeX-font nil ?\C-a))
  (defun latex-font-small-caps () (interactive) (TeX-font nil ?\C-c))
  (defun latex-font-sans-serif () (interactive) (TeX-font nil ?\C-f))
  (defun latex-font-normal () (interactive) (TeX-font nil ?\C-n))
  (defun latex-font-serif () (interactive) (TeX-font nil ?\C-r))
  (defun latex-font-oblique () (interactive) (TeX-font nil ?\C-s))
  (defun latex-font-upright () (interactive) (TeX-font nil ?\C-u))

  (bind-keys :map TeX-fold-keymap
             ("B" . TeX-fold-buffer) ("C-b")
             ("c" . TeX-fold-comment) ("C-c")
             ("e" . TeX-fold-env) ("C-e")
             ("d" . TeX-fold-dwim) ("C-o")
             ("P" . TeX-fold-paragraph) ("C-p")
             ("R" . TeX-fold-region) ("C-r"))
  (bind-keys :map LaTeX-mode-map
             ("C-c b" . latex-build)
             ("C-c h" . TeX-doc)
             ("C-c x b" . latex-font-bold)
             ("C-c x m" . latex-font-medium)
             ("C-c x c" . latex-font-code)
             ("C-c x e" . latex-font-emphasis)
             ("C-c x i" . latex-font-italic)
             ("C-c x c" . latex-font-clear)
             ("C-c x C" . latex-font-calligraphic)
             ("C-c x s m" . latex-font-small)
             ("C-c x s a" . latex-font-sans)
             ("C-c x n" . latex-font-normal)
             ("C-c x s e" . latex-font-serif)
             ("C-c x o" . latex-font-oblique)
             ("C-c x u" . latex-font-upright)))

(with-eval-after-load 'preview
  (setq preview-auto-cache-preamble t
        preview-transparent-color "white"
        preview-transparent-border 0
        preview-scale-function 1.2)
  (set-face-background 'preview-face "#1b1d1e")
  (set-face-background 'preview-reference-face "#1b1d1e")
  (setq preview-map
        (let ((map (make-sparse-keymap)))
          (bind-keys :map map
                     ("p" . preview-at-point)
                     ("r" . preview-region)
                     ("b" . preview-buffer)
                     ("d" . preview-document)
                     ("f" . preview-cache-preamble)
                     ("c f" . preview-cache-preamble-off)
                     ("i" . preview-goto-info-page)
                     ;; -q" #'preview-paragraph)
                     ("e" . preview-environment)
                     ("s" . preview-section)
                     ("w" . preview-copy-region-as-mml)
                     ("c p" . preview-clearout-at-point)
                     ("c r" . preview-clearout)
                     ("c s" . preview-clearout-section)
                     ("c b" . preview-clearout-buffer)
                     ("c d" . preview-clearout-document))
          map))
  (define-key LaTeX-mode-map "\C-cp" preview-map))

(with-eval-after-load 'reftex
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

(provide 'init-latex)
