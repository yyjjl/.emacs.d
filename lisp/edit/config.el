;;; -*- lexical-binding: t; -*-

(after! iedit
  (setq iedit-auto-narrow t))

(after! picture
  (define-key! :map picture-mode-map
    ("C-c C-a" . artist-mode)))

(after! multiple-cursors
  (add-to-list 'mc--default-cmds-to-run-for-all 'ymacs/easy-kill-er-expand)
  (add-to-list 'mc--default-cmds-to-run-for-all 'ymacs/easy-kill-er-unexpand)
  (add-to-list 'mc/cursor-specific-vars 'ymacs-easy-kill-er-history))

(after! easy-kill
  (define-key! :map easy-kill-base-map
    ("-" . ymacs/easy-kill-er-expand)
    ("M--" . ymacs/easy-kill-er-expand)
    ("=" . ymacs/easy-kill-er-unexpand)
    ("M-=" . ymacs/easy-kill-er-unexpand)
    ("[" . easy-kill-shrink)
    ("]" . easy-kill-expand))

  (setq easy-kill-try-things '(url email word)
        easy-mark-try-things '(url email word)))
