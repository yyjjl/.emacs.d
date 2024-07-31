;;; -*- lexical-binding: t; -*-

(require-packages!
 typescript-mode)

(eval-when-has-feature! term
  (let ((repl '(:program "node" :program-args () :cmd-fmt ".load %s\n")))
    (dolist (mode '(typescript-mode typescript-ts-mode js-mode js-ts-mode))
      (add-to-list 'ymacs-term-repl-alist (cons mode repl)))))
