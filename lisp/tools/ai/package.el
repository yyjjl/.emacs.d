;;; -*- lexical-binding: t; -*-

(executable! claude-code :exe "claude")

(require-packages!
 agent-shell)

(when ymacs-claude-code-path
  (when (and (not (package-installed-p 'claude-code-ide))
             (fboundp #'package-vc-install))
    (package-vc-install "https://github.com/manzaltu/claude-code-ide.el")))

(define-key!
  ;; ([f7] . agent-shell)
  ([f7] . claude-code-ide))

(after! shell-maker
  (setq shell-maker-root-path ymacs-cache-direcotry))

(after! agent-shell
  (advice-add #'agent-shell--setup-modeline :override #'ignore)

  (setq agent-shell-show-session-id nil)
  (setq agent-shell-show-welcome-message nil)
  (setq agent-shell-show-usage-at-turn-end t)
  (setq agent-shell-thought-process-expand-by-default nil)
  (setq agent-shell-tool-use-expand-by-default nil)
  (setq agent-shell-preferred-agent-config 'claude-code)
  (setq agent-shell-context-sources '(region error))
  (define-key! :map agent-shell-mode-map
    ("S-<return>" . shell-maker-submit)
    ("C-<return>" . shell-maker-submit)
    ("RET" . newline)))

(after! claude-code-ide
  ;; (claude-code-ide-emacs-tools-setup)
  ;; (setq claude-code-ide-enable-mcp-server nil)
  ;; (setq claude-code-ide-mcp-server-port 8003)

  (defun yamcs-ai//setup-vterm-buffer (&rest _)
    (when (and (eq major-mode  'vterm-mode)
               (string-prefix-p "*AI agent" (buffer-name)))
      (local-set-key (kbd "C-'") #'(lambda () (interactive) (vterm-send-C-g)))
      (local-set-key (kbd "C-c '") #'(lambda () (interactive) (vterm-send-C-g)))))

  (advice-add #'claude-code-ide--configure-vterm-buffer :after #'yamcs-ai//setup-vterm-buffer)

  (setq claude-code-ide-buffer-name-function
        #'(lambda (-directory)
            (format "*AI agent[%s]*"
                    (file-name-nondirectory (directory-file-name -directory)))))
  (setq claude-code-ide-debug nil)
  (setq claude-code-ide-use-side-window nil)
  (setq claude-code-ide-cli-path ymacs-claude-code-path))
