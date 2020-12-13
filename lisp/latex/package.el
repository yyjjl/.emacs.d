;;; -*- lexical-binding: t; -*-

(require-packages! lsp-mode)

(executable! texlab :exe [(expand-cache! "lsp/texlab") "texlab"])

(ymacs-lsp//register-client
 'texlab
 :package 'lsp-tex
 :manual `(:title "Texlab"
           :repo "latex-lsp/texlab"
           :exe ,ymacs-texlab-path))


(require-packages!
 (auctex :compile (latex tex preview reftex)))

(autoload 'LaTeX-math-mode "latex" nil t)

(add-auto-mode! 'latex-mode "\\.tikz\\'")
(setcdr (assoc-string "\\.[tT]e[xX]\\'" auto-mode-alist) 'latex-mode)

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
