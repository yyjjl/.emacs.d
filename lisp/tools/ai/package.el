;;; -*- lexical-binding: t; -*-

(require-packages!
 agent-shell)

(executable! claude-code :exe "claude")

(define-key!
  ([f7] . agent-shell))

(after! agent-shell
  (advice-add #'agent-shell--setup-modeline :override #'ignore)

  (setq agent-shell-show-session-id nil)
  (setq agent-shell-show-usage-at-turn-end t)
  (setq agent-shell-thought-process-expand-by-default nil)
  (setq agent-shell-tool-use-expand-by-default nil)
  (setq agent-shell-preferred-agent-config 'claude-code)
  (setq agent-shell-context-sources '(region error))
  (define-key! :map agent-shell-mode-map
    ("S-<return>" . shell-maker-submit)
    ("C-<return>" . shell-maker-submit)
    ("RET" . newline)))
