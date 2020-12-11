;;; -*- lexical-binding: t; -*-

(require-packages!
 (auctex :compile (latex tex preview reftex)))

(autoload 'LaTeX-math-mode "latex" nil t)

(add-auto-mode! 'latex-mode "\\.tikz\\'")
(setcdr (assoc-string "\\.[tT]e[xX]\\'" auto-mode-alist) 'latex-mode)

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
