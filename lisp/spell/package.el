;;; -*- lexical-binding: t; -*-

(require-packages! ispell flyspell)

(executable! aspell)
(executable! hunspell)

(autoload 'ymacs-hydra/flyspell/body "spell/commands" nil t)

(define-key!
  ("C-\"" . ymacs-hydra/flyspell/body))
