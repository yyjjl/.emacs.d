;;; -*- lexical-binding: t; -*-

(executable! latexmk)

(require-packages!
 (auctex :compile (latex tex preview reftex))
 auctex-latexmk
 cdlatex)

(executable! texlab :-exe [(expand-cache! "lsp/texlab") "texlab"])

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'texlab :package 'lsp-tex))

(add-to-list
 'ymacs-editor-narrow-dwim-alist
 '(latex-mode LaTeX-narrow-to-environment ymacs-latex/narrow-to-section))

(autoload 'LaTeX-math-mode "latex" nil t)

(add-to-list 'auto-mode-alist '("\\.tikz\\'" . latex-mode))

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
