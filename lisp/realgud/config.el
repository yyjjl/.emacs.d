;;; -*- lexical-binding: t; -*-

(after! realgud:pdb-init
  (setf (gethash "breakpoints" realgud:pdb-command-hash) "break")
  (setf (gethash "display" realgud:pdb-command-hash) "display %s")
  (setf (gethash "display-all" realgud:pdb-command-hash) "display"))

(after! realgud:gdb-init
  (setf (gethash "breakpoints" realgud:gdb-command-hash) "info breakpoints"))

(after! realgud-gdb
  (define-key! :map realgud:gdb-track-mode-map
    ("C-c C-z" . ymacs-realgud/jump-to-srcbuf)
    ("`" :map realgud:shortkey-mode-map)))

(after! realgud-track-mode
  (define-key! :map realgud-track-mode-map
    ("C-c C-z" . ymacs-realgud/jump-to-srcbuf)
    ("M-n" . comint-next-input)
    ("M-p" . comint-previous-input)
    ("M-h" . counsel-shell-history)
    ("`" :map realgud:shortkey-mode-map)))

(after! realgud
  (define-key! :map realgud:shortkey-mode-map
    ("M" . ymacs-realgud/cmd-display)
    ("B" . ymacs-realgud/cmd-breakpoints)
    ("G" . ymacs-realgud/jump-to-cmdbuf)
    ("L" . ymacs-realgud/restore-breakpoints)
    ("C-c C-z" . ymacs-realgud/jump-to-cmdbuf))

  (setq realgud-populate-common-fn-keys-function nil)
  (setq realgud-safe-mode nil))

(after! comint
  (define-key! :map comint-mode-map
    ([F5] . realgud-track-mode)))
