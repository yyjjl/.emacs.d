(autoload 'LaTeX-math-mode "latex" nil t)
(defhook latex|setup (LaTeX-mode-hook)
  (company-auctex-init)
  (turn-off-auto-fill)
  (setq company-backends (delete 'company-dabbrev company-backends))
  (LaTeX-math-mode 1)
  (unless (buffer-temporary-p)
    (turn-on-reftex)
    (TeX-source-correlate-mode 1)
    (TeX-PDF-mode 1)
    (TeX-fold-mode 1)
    ;; Will conflict with latex-mode
    (electric-pair-local-mode -1)

    (setq TeX-engine 'xetex)

    (outline-minor-mode 1)))

(with-eval-after-load 'tex
  (defun latex|build ()
    (interactive)
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (let ((command (if (save-excursion
                         (goto-char 1)
                         (search-forward-regexp
                          "\\\\usepackage\\s-*{\\s-*minted"
                          nil t))
                       "XeLaTeX"
                     TeX-command-default)))
      (TeX-command command  'TeX-master-file -1)))

  (add-to-list
   'TeX-command-list
   '("XeLaTeX" "%`xelatex -interaction nonstopmode -shell-escape%(mode)%' %t"
     TeX-run-TeX nil t))

  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        TeX-electric-math '("$" . "$")
        LaTeX-electric-left-right-brace t
        TeX-complete-expert-commands t
        TeX-electric-sub-and-superscript nil
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil))

(with-eval-after-load 'latex
  (setq TeX-outline-extra
        '(("^\\\\begin{[a-zA-Z]+}" 7)))
  (setq TeX-fold-command-prefix (kbd "C-c C-o"))

  (require 'tex-fold)

  (defun latex|skip-close-pair ()
    (interactive)
    (let ((char (char-after)))
      (if (and (equal char (string-to-char (this-command-keys)))
               (member char '(?\) ?\} ?\])))
          (forward-char)
        (self-insert-command 1))))

  (defun latex|force-update-style ()
    (interactive)
    (TeX-update-style t))

  (defun latex|font-bold () (interactive) (TeX-font nil ?\C-b))
  (defun latex|font-medium () (interactive) (TeX-font nil ?\C-m))
  (defun latex|font-code () (interactive) (TeX-font nil ?\C-t))
  (defun latex|font-emphasis () (interactive) (TeX-font nil ?\C-e))
  (defun latex|font-italic () (interactive) (TeX-font nil ?\C-i))
  (defun latex|font-clear () (interactive) (TeX-font nil ?\C-d))
  (defun latex|font-calligraphic () (interactive) (TeX-font nil ?\C-a))
  (defun latex|font-small-caps () (interactive) (TeX-font nil ?\C-c))
  (defun latex|font-sans-serif () (interactive) (TeX-font nil ?\C-f))
  (defun latex|font-normal () (interactive) (TeX-font nil ?\C-n))
  (defun latex|font-serif () (interactive) (TeX-font nil ?\C-r))
  (defun latex|font-oblique () (interactive) (TeX-font nil ?\C-s))
  (defun latex|font-upright () (interactive) (TeX-font nil ?\C-u))

  (define-keys :map TeX-fold-keymap
    ("B" . TeX-fold-buffer) ("C-b")
    ("c" . TeX-fold-comment) ("C-c")
    ("e" . TeX-fold-env) ("C-e")
    ("d" . TeX-fold-dwim) ("C-o")
    ("P" . TeX-fold-paragraph) ("C-p")
    ("R" . TeX-fold-region) ("C-r"))
  (define-keys :map LaTeX-mode-map
    ("}" . latex|skip-close-pair)
    (")" . latex|skip-close-pair)
    ("]" . latex|skip-close-pair)
    ("C-c b" . latex|build)
    ("C-c h" . TeX-doc)
    ("C-c C-l" . latex|force-update-style)
    ("C-c C-s") ("C-c s" . LaTeX-section)
    ("C-c C-e") ("C-c e" . LaTeX-environment)
    ("C-c x b" . latex|font-bold)
    ("C-c x m" . latex|font-medium)
    ("C-c x c" . latex|font-code)
    ("C-c x e" . latex|font-emphasis)
    ("C-c x i" . latex|font-italic)
    ("C-c x c" . latex|font-clear)
    ("C-c x C" . latex|font-calligraphic)
    ("C-c x s m" . latex|font-small)
    ("C-c x s a" . latex|font-sans)
    ("C-c x n" . latex|font-normal)
    ("C-c x s e" . latex|font-serif)
    ("C-c x o" . latex|font-oblique)
    ("C-c x u" . latex|font-upright)))

(with-eval-after-load 'preview
  (setq preview-auto-cache-preamble t
        preview-transparent-color "white"
        preview-transparent-border 0
        preview-scale-function 1.4
        preview-default-document-pt 12)
  (set-face-background 'preview-face
                       (face-attribute 'default :background))
  (set-face-background 'preview-reference-face
                       (face-attribute 'default :background))
  (setq preview-map
        (let ((map (make-sparse-keymap)))
          (define-keys :map map
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
  (define-key LaTeX-mode-map "\C-cl" preview-map))

(with-eval-after-load 'reftex
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

(provide 'init-latex)
