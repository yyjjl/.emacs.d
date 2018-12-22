;; -*- lexical-binding:t -*-
(require-packages!
 lsp-mode
 lsp-ui
 dap-mode
 company-lsp)

(defcustom lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)



(require 'lsp)
(require 'lsp-clients)

(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))

(defun lsp/remove-session-folder (-remove-invalid)
  (interactive "P")
  (let* ((session (lsp-session))
         invalid-folders
         valid-folders)
    (cl-loop for folder in (lsp-session-folders session)
             if (file-exists-p folder)
             do (push folder valid-folders)
             else do (push folder invalid-folders))
    (if -remove-invalid
        (if invalid-folders
            (when (yes-or-no-p (format "remove below folders:\n%s\n" (string-join invalid-folders "\n")))
              (setf (lsp-session-folders (lsp-session)) valid-folders))
          (message "Nothing to remove."))
      (let ((folder (completing-read "Folder to remove" valid-folders nil t)))
        (setf (lsp-session-folders (lsp-session)) (delete folder valid-folders))))))

(define-hook! lsp|after-open (lsp-after-open-hook)
  (flycheck-mode 1)

  (lsp-ui-mode 1)
  (lsp-ui-flycheck-enable 1)

  ;; default to sort and filter by server
  (setq-local company-transformers nil)
  (setq-local company-lsp-cache-candidates nil)

  (add-to-list 'company-backends 'company-lsp)
  (add-to-list 'company-backends 'company-files))

(defun lsp/delete-session-cache ()
  (interactive)
  (let* ((session (lsp-session))
         (folders (lsp-session-folders session))
         (folder (completing-read "Delete folder: " folders nil t)))
    (setf (lsp-session-folders session) (delete folder folders))))

(with-eval-after-load 'lsp
  (dap-mode 1)
  (dap-ui-mode 1)

  (setq lsp-session-file (expand-var! "lsp-sessions"))
  (setq lsp-auto-configure nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-signature-enabled t)
  (setq lsp-hover-enabled t)
  (setq lsp-eldoc-hook '(lsp-hover))
  (setq lsp-restart 'auto-restart)

  (define-key!
    ("M-s h h" . lsp-document-highlight)
    ("C-c R" . lsp-rename)
    ("C-c S" . lsp-describe-session)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action)))

(with-eval-after-load 'lsp-ui
  (require 'lsp-ui-flycheck)

  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-max-width 80))

(with-eval-after-load 'company-lsp
  (setq company-lsp-async t))

(provide 'init-lsp-mode)
