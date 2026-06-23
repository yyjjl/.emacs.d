;;; -*- lexical-binding: t; -*-

(executable! claude-code :exe "claude")

(when ymacs-claude-code-path
  (when (and (not (package-installed-p 'claude-code-ide))
             (fboundp #'package-vc-install))
    (package-vc-install "https://github.com/manzaltu/claude-code-ide.el")))

(define-key!
  ([f7] . claude-code-ide))

(after! claude-code-ide
  (defun yamcs-ai//setup-vterm-buffer (&rest _)
    (when (and (eq major-mode  'vterm-mode)
               (string-prefix-p "*AI agent" (buffer-name)))
      (local-set-key (kbd "C-'") #'(lambda () (interactive) (vterm-send-C-g)))
      (local-set-key (kbd "C-c '") #'(lambda () (interactive) (vterm-send-C-g)))
      (setq-local ymacs-term--is-not-a-shell t)
      (setq-local mode-line-format '(:eval (ymacs-modeline//format--ai-agent)))))

  (advice-add #'claude-code-ide--configure-vterm-buffer :after #'yamcs-ai//setup-vterm-buffer)

  (setq claude-code-ide-buffer-name-function
        #'(lambda (-directory)
            (format "*AI agent[%s]*"
                    (file-name-nondirectory (directory-file-name -directory)))))
  (setq claude-code-ide-debug nil)
  (setq claude-code-ide-use-side-window nil)
  (setq claude-code-ide-cli-path ymacs-claude-code-path)

  (defun yamcs-ai//cli-is-traecli-p ()
    (let ((name (file-name-nondirectory
                 (or claude-code-ide-cli-path ""))))
      (string-match-p "\\(traecli\\|traex\\)" name)))

  (when (yamcs-ai//cli-is-traecli-p)
    (require 'claude-code-ide-emacs-tools nil t)
    (when (fboundp 'claude-code-ide-emacs-tools-setup)
      (claude-code-ide-emacs-tools-setup))
    (setq claude-code-ide-enable-mcp-server t))

  (defun yamcs-ai//build-traecli-command (session-id)
    "Build the traecli command line, wiring in Emacs MCP server URL.
Bypasses the Claude-specific flag generation entirely."
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
      (:around (orig &optional continue resume session-id) yamcs-ai//traecli)
    (if (yamcs-ai//cli-is-traecli-p)
        (yamcs-ai//build-traecli-command (or session-id "emacs"))
      (funcall orig continue resume session-id))))
