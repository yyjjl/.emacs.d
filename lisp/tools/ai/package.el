;;; -*- lexical-binding: t; -*-

(executable! claude-code :exe "claude")

(when ymacs-claude-code-path
  (when (and (not (package-installed-p 'claude-code-ide))
             (fboundp #'package-vc-install))
    (package-vc-install "https://github.com/manzaltu/claude-code-ide.el")))

(define-key!
  ([f7] . claude-code-ide))

(after! claude-code-ide
  (defun ymacs-ai//setup-agent-buffer (&rest _)
    (when (and (eq major-mode 'ghostel-mode)
               (string-prefix-p "*AI agent" (buffer-name)))
      (local-set-key (kbd "C-'") #'(lambda () (interactive) (ghostel-send-C-g)))
      (local-set-key (kbd "C-c '") #'(lambda () (interactive) (ghostel-send-C-g)))
      (setq-local ymacs-term--is-not-a-shell t)
      (setq-local mode-line-format '(:eval (ymacs-modeline//format--ai-agent)))))

  ;; `claude-code-ide--setup-terminal-keybindings' runs in the freshly created
  ;; agent buffer for every backend, so it is the reliable hook point now that
  ;; ghostel (not vterm) has no dedicated `configure' step.
  (advice-add #'claude-code-ide--setup-terminal-keybindings
              :after #'ymacs-ai//setup-agent-buffer)

  (setq claude-code-ide-terminal-backend 'ghostel)

  (setq claude-code-ide-buffer-name-function #'(lambda (-directory) "*AI agent*"))
  (setq claude-code-ide-debug nil)
  (setq claude-code-ide-use-side-window nil)
  (setq claude-code-ide-cli-path ymacs-claude-code-path)

  (defun ymacs-ai//cli-is-traecli-p ()
    (let ((name (file-name-nondirectory
                 (or claude-code-ide-cli-path ""))))
      (string-match-p "\\(traecli\\|traex\\)" name)))

  (defun ymacs-ai//cli-is-codex-p ()
    (let ((name (file-name-nondirectory
                 (or claude-code-ide-cli-path ""))))
      (string-match-p "\\(codex\\)" name)))

  (when (ymacs-ai//cli-is-traecli-p)
    (require 'claude-code-ide-emacs-tools nil t)
    (when (fboundp 'claude-code-ide-emacs-tools-setup)
      (claude-code-ide-emacs-tools-setup))
    (setq claude-code-ide-enable-mcp-server t))

  (defun ymacs-ai//build-mcp-command (session-id)
    "Build the traecli/codex command line, wiring in Emacs MCP server URL.
Both CLIs take `-c key=value' configuration overrides instead of the
Claude-specific flags that `claude-code-ide--build-claude-command'
would emit (e.g. `--append-system-prompt')."
    (let* ((port (and (fboundp 'claude-code-ide-mcp-server-ensure-server)
                      (claude-code-ide-mcp-server-ensure-server)))
           (cmd (shell-quote-argument claude-code-ide-cli-path))
           (extra (and claude-code-ide-cli-extra-flags
                       (not (string-empty-p claude-code-ide-cli-extra-flags))
                       claude-code-ide-cli-extra-flags)))
      (when port
        (setq cmd
              (concat cmd " -c "
                      (shell-quote-argument
                       (format "mcp_servers.emacs-tools.url=\"http://localhost:%d/mcp/%s\""
                               port session-id)))))
      (when extra
        (setq cmd (concat cmd " " extra)))
      cmd))

  (define-advice claude-code-ide--build-claude-command
      (:around (orig &optional continue resume session-id) ymacs-ai//codex)
    (cond
     ((ymacs-ai//cli-is-codex-p)
      (ymacs-ai//build-mcp-command (or session-id "emacs")))
     ((ymacs-ai//cli-is-traecli-p)
      (ymacs-ai//build-mcp-command (or session-id "emacs")))
     (t
      (funcall orig continue resume session-id)))))
