;;; -*- lexical-binding: t; -*-

(require-packages!
 zone-sl
 zone-nyan
 zone-rainbow)

(autoload 'ymacs-hydra/select-games/body "play/commands" nil t)

(define-key!
  ("C-x , z" . ymacs-hydra/select-games/body))
