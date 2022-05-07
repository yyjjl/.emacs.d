;;; -*- lexical-binding: t; -*-

(option! latex-build-on-save nil
  :type 'boolean
  :safe #'booleanp)

(executable! latexmk)

(require-packages! (auctex :compile (latex tex preview reftex)) )

(executable! texlab :exe [(expand-cache! "lsp/texlab") "texlab"])

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'texlab :package 'lsp-tex))

(ymacs-editor//set-narrow-handler
 :mode latex-mode
 :fn1 LaTeX-narrow-to-environment
 :fn2 ymacs-latex/narrow-to-section)

(autoload 'LaTeX-math-mode "latex" nil t)

(add-to-list 'auto-mode-alist '("\\.tikz\\'" . latex-mode))

(put 'TeX-command-extra-options 'safe-local-variable #'stringp)
(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
