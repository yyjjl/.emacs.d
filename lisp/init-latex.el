
(require-packages!
 (auctex :compile (latex tex preview reftex))
 company-reftex
 company-auctex)



(defun latex/narrow-to-section (&optional -no-subsections)
  (interactive "P")
  (save-mark-and-excursion
    (LaTeX-mark-section -no-subsections)
    (call-interactively 'narrow-to-region)))

(defun latex/count-words ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'tex-count-words)
    (let* ((options (or (and (eq current-prefix-arg '(16))
                             (read-string "Options: " "-inc -ch -brief"))
                        "-inc -ch -brief"))
           (file (or (cond ((not current-prefix-arg)
                            (expand-file-name (TeX-master-file "tex")))
                           ((equal current-prefix-arg '(4))
                            (buffer-file-name)))
                     (read-file-name "TeX file: "))))
      (let ((default-directory (TeX-master-directory)))
        (with-current-buffer (compilation-start (concat "texcount " options " "
                                                        (shell-quote-argument file)))
          (add-transient-hook! (compilation-finish-functions
                                :local t
                                :name latex|after-count-words
                                :arguments (buffer _))
            (with-current-buffer buffer
              (goto-char (point-min))
              (re-search-forward "^texcount" nil :noerror)
              (forward-line 0))))))))

(defun latex/convert-to-svg ()
  (interactive)
  (let* ((fn (file-name-base (buffer-file-name)))
         (output-file (concat fn ".svg")))
    (message (shell-command-to-string
              (format "pdf2svg %s.pdf %s.svg" fn fn)))
    (if (file-exists-p output-file)
        (pop-to-buffer
         (find-file-noselect output-file))
      (message "Something wrong !!!"))))

(defun latex/build ()
  (interactive)
  (reftex-parse-all)
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

(defun latex/skip-close-pair ()
  (interactive)
  (let ((char (char-after)))
    (if (and (equal char (string-to-char (this-command-keys)))
             (member char '(?\) ?\} ?\])))
        (forward-char)
      (self-insert-command 1))))

(defun latex/force-update-style ()
  (interactive)
  (TeX-update-style t))

(autoload 'LaTeX-math-mode "latex" nil t)
(define-hook! latex|setup (LaTeX-mode-hook)
  (flycheck-mode -1)

  (setq next-error-function 'TeX-next-error)

  (company-auctex-init)
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations)
  (add-to-list 'company-backends 'company-files)
  (turn-off-auto-fill)
  ;; (visual-line-mode 1)
  ;; (setq company-backends (delete 'company-dabbrev company-backends))
  (LaTeX-math-mode 1)
  ;; Fix conflit with `orgtbl-mode'
  (define-key LaTeX-math-mode-map "``" (lambda () (interactive)
                                         (insert-char ?\`)))
  ;; (orgtbl-mode 1)

  (setq-local prettify-symbols-alist tex--prettify-symbols-alist)
  (add-function :override (local 'prettify-symbols-compose-predicate)
                #'tex--prettify-symbols-compose-p)
  (prettify-symbols-mode 1)

  (unless TeX-master
    (setq TeX-master 'dwim))

  (unless (buffer-temporary?)
    (reftex-mode 1)
    (TeX-source-correlate-mode 1)
    (TeX-PDF-mode 1)
    (TeX-fold-mode 1)
    ;; Will conflict with latex-mode
    (electric-pair-local-mode -1)

    (setq TeX-engine 'xetex)

    (outline-minor-mode 1)))

(with-eval-after-load 'tex
  (add-to-list
   'TeX-command-list
   '("XeLaTeX" "%`xelatex -interaction nonstopmode -shell-escape %(mode)%' %t"
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
        font-latex-fontify-script nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil))

(with-eval-after-load 'tex-mode
  (dolist (key '("--" "---"))
    (setq tex--prettify-symbols-alist
          (delq (assoc key tex--prettify-symbols-alist)
                tex--prettify-symbols-alist))))

(with-eval-after-load 'latex
  (require 'tex-fold)

  (setq TeX-debug-bad-boxes nil)
  (setq TeX-debug-warnings t)
  (setq TeX-outline-extra nil)
  (setq TeX-fold-command-prefix (kbd "C-c C-o"))

  (define-key! :map TeX-fold-keymap
    ("B" . TeX-fold-buffer) ("C-b")
    ("c" . TeX-fold-comment) ("C-c")
    ("e" . TeX-fold-env) ("C-e")
    ("d" . TeX-fold-dwim) ("C-o")
    ("P" . TeX-fold-paragraph) ("C-p")
    ("R" . TeX-fold-region) ("C-r"))

  (define-key! :map LaTeX-mode-map
    ("M-=" . latex/count-words)
    ("C-c t" :map org-table-extra-map)
    ("C-t" . TeX-fold-dwim)
    ("}" . latex/skip-close-pair)
    (")" . latex/skip-close-pair)
    ("]" . latex/skip-close-pair)
    ("C-c b" . latex/build)
    ("C-c h" . TeX-doc)
    ("C-c C-u" . latex/force-update-style)
    ("C-c s" . LaTeX-section)
    ("C-c e" . LaTeX-environment)
    ("C-c x" . TeX-font)
    ("C-c C-x" . TeX-font)
    ("C-c f l" . TeX-error-overview)))

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
        (define-key! :map (make-sparse-keymap)
          ("p" . preview-at-point)
          ("M-p" . preview-at-point)
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
          ("D" . preview-clearout-document)))
  (define-key LaTeX-mode-map (kbd "M-p") preview-map))

(with-eval-after-load 'reftex
  (setq reftex-cite-format
        '((?t . "\\textcite[]{%l}")
          (?a . "\\autocite[]{%l}")
          (?c . "\\cite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?b . "\\blockcquote[]{%l}{}")))

  (setq reftex-plug-into-AUCTeX '(t t t t t)
        reftex-use-fonts t))

(provide 'init-latex)
