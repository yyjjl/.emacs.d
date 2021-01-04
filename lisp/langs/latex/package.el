;;; -*- lexical-binding: t; -*-

(require-packages!
 (auctex :compile (latex tex preview reftex)))

(executable! texlab :-exe [(expand-cache! "lsp/texlab") "texlab"])

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client
   'texlab
   :package 'lsp-tex
   :manual `(:title "Texlab"
             :repo "latex-lsp/texlab"
             :exe ,ymacs-texlab-path)))

(add-to-list
 'ymacs-editor-narrow-dwim-alist
 '(latex-mode LaTeX-narrow-to-environment ymacs-latex/narrow-to-section))

(autoload 'LaTeX-math-mode "latex" nil t)

(add-auto-mode! 'latex-mode "\\.tikz\\'")
(setcdr (assoc-string "\\.[tT]e[xX]\\'" auto-mode-alist) 'latex-mode)

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
