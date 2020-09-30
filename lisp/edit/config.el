;;; -*- lexical-binding: t; -*-

(after! iedit
  (setq iedit-auto-narrow t))

(after! picture
  (define-key! :map picture-mode-map
    ("C-c C-a" . artist-mode)))

(after! easy-kill
  (require 'easy-kill-extras)

  (define-key! :map easy-kill-base-map
    ("=" . easy-kill-er-expand)
    ("-" . easy-kill-er-unexpand)
    ("[" . easy-kill-shrink)
    ("]" . easy-kill-expand))

  (setq easy-kill-alist
        '((?w word " ")
          (?s sexp "\n")
          (?l list "\n")
          (?d defun "\n\n")
          (?D defun-name " ")
          (?e line "\n")
          (?b buffer-file-name)

          (?^ backward-line-edge "")
          (?$ forward-line-edge "")
          (?h buffer "")
          (?< buffer-before-point "")
          (?> buffer-after-point "")
          (?f string-to-char-forward "")
          (?F string-up-to-char-forward "")
          (?t string-to-char-backward "")
          (?T string-up-to-char-backward "")))

  (setq easy-kill-try-things '(url email word)
        easy-mark-try-things '(url email sexp)))
