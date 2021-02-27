;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'lsp-mode))

(declare-function lsp-semantic-tokens--disable 'lsp-mode)
(declare-function treemacs-current-visibility 'treemacs)
(declare-function treemacs-select-window 'treemacs)

(defun ymacs-lsp//install-clients--loop (-clients &optional -outputs)
  (if-let (client (car -clients))
      (progn
        (push (format "* Install %s" client) -outputs)

        (let ((install-fn (lsp--client-download-server-fn (ht-get lsp-clients client)))
              (callback-fn
               (lambda (&rest _)
                 (ymacs-lsp//install-clients--callback -clients -outputs))))
          (funcall install-fn client callback-fn callback-fn t)))

    (ymacs-lsp//install-clients--finish -outputs)))

(defun ymacs-lsp//install-clients--callback (-clients -outputs)
  (ymacs-lsp//install-clients--loop
   (cdr -clients)
   (let ((buffer (get-buffer-create ymacs-lsp-process-buffer-name)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (push (buffer-substring (point-min) (point-max)) -outputs)))
     -outputs)))

(defun ymacs-lsp//install-clients--finish (-outputs)
  (with-current-buffer (get-buffer-create ymacs-lsp-process-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (string-join (reverse -outputs) "\n\n") "\n\n")
      (goto-char (point-max))

      (org-mode)
      (pop-to-buffer (current-buffer))
      (set-buffer-modified-p nil))))

;;;###autoload
(defun ymacs-lsp/check-for-updates ()
  (interactive)

  (ymacs-lsp//install-clients--loop
   (cl-loop
    for (client package enable-fn) in ymacs-lsp--enabled-clients
    when (or (null enable-fn)
             (funcall enable-fn))
    collect (progn
              (when package
                (require package nil t))
              client))))

;;;###autoload
(defun ymacs-lsp/toggle-semantic-tokens ()
  (interactive)
  (setq lsp-semantic-tokens-enable (not lsp-semantic-tokens-enable))
  (if lsp-semantic-tokens-enable
      (lsp-semantic-tokens--enable)
    (lsp-semantic-tokens--disable)
    (font-lock-flush))
  (lsp--info "Semantic Tokens %s. "
             (if lsp-semantic-tokens-enable "enabled" "disabled")))

(eval-when! ymacs-lsp-use-modern-ui
  (defun ymacs-lsp/toggle-modern-ui ()
    (interactive)

    (kill-local-variable 'lsp-eldoc-enable-hover)
    (if (bound-and-true-p lsp-ui-mode)
        (lsp-ui-mode -1)
      (setq-local lsp-eldoc-enable-hover nil)
      (lsp--eldoc-message nil)
      (lsp-ui-mode 1))))


(eval-when! ymacs-lsp-use-dap
  (define-minor-mode ymacs-dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    nil nil
    ymacs-dap-running-session-mode-map
    (if ymacs-dap-running-session-mode
        (ymacs-debug//enable)
      (ymacs-debug//disable))

    (force-mode-line-update))

  (defun ymacs-dap/goto-log-buffer ()
    (interactive)
    (let ((session (dap--cur-session-or-die)))
      (when-let* ((proc (dap--debug-session-program-proc session))
                  (buffer (process-buffer proc)))
        (pop-to-buffer buffer))))

  (defun ymacs-dap/goto-repl-buffer ()
    (interactive)
    (dap-hydra/nil)
    (if-let ((buffer (get-buffer dap-ui--repl-buffer))
             (window (get-buffer-window buffer)))
        (if (eq window (selected-window))
            (quit-window)
          (select-window window))
      (dap-ui-repl))))

;;;###autoload
(defun ymacs-lsp/remove-invalid-folders ()
  (interactive)
  (seq-do-interactively!
   #'lsp-workspace-folders-remove
   (lambda (-folder)
     (format "Delete: %s" -folder))
   (cl-remove-if #'file-exists-p (lsp-session-folders (lsp-session)))))


;;;###autoload
(defun ymacs-lsp/select-window-1 ()
  (interactive)
  (let ((path (window-parameter (selected-window) 'ace-window-path)))
    (if (and (equal path "1")
             (eq 'visible (treemacs-current-visibility)))
        (treemacs-select-window)
      (ymacs-ui/aw-select-window))))
