;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'lsp-mode))

(declare-function lsp-semantic-tokens--disable 'lsp-mode)
(declare-function treemacs-current-visibility 'treemacs)
(declare-function treemacs-select-window 'treemacs)

(defun ymacs-lsp//update-or-print-configuration (-section -do-update)
  (dolist (workspace (lsp-workspaces))
    (with-lsp-workspace workspace
      (if -do-update
          (lsp--set-configuration (lsp-configuration-section -section))
        (let ((configuration nil))
          (maphash (-lambda (path (variable boolean?))
                     (when (s-matches? (concat (regexp-quote -section) "\\..*") path)
                       (let* ((symbol-value (-> variable
                                                lsp-resolve-value
                                                lsp-resolve-value))
                              (value (if (and boolean? (not symbol-value))
                                         :json-false
                                       symbol-value)))
                         (when (or boolean? value)
                           (cl-pushnew (format "%s=%s" path value) configuration)))))
                   lsp-client-settings)
          (ymacs-lisp/message (string-join configuration "\n") t))))))


(defun ymacs-python/update-configuration (-do-update)
  (interactive "P")
  (ymacs-lsp//update-or-print-configuration "python" -do-update))

;;;###autoload
(defun ymacs-lsp/open-remote-stderr ()
  (interactive)
  (if-let* ((remote-host (file-remote-p default-directory))
            (workspace (car lsp--buffer-workspaces))
            (name (lsp--client-server-id (lsp--workspace-client workspace)))
            (path (concat remote-host (ymacs-lsp//get-remote-stderr name))))
      (if (file-exists-p path)
          (pop-to-buffer (find-file-noselect path))
        (message "remote-stderr %s not found" path))
    (message "remote-stderr not found")))

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
      (ymacs-editor/aw-select-window))))

;;;###autoload
(defun ymacs-lsp/select-window-2 ()
  (interactive)
  (or (ymacs-editor//aw-switch-window-internal "2")
      (ymacs-editor//aw-switch-window-internal "1")
      (message "No specified window: 1 or 2")))
