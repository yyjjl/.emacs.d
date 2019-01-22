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
(defvar lsp--disable-eldoc-in-minibuffer nil)



(require 'lsp)
(require 'lsp-clients)

(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))

(defun lsp/remove-session-folder (-remove-invalid)
  (interactive "P")
  (require 'dired)
  (let* ((session (lsp-session))
         invalid-folders
         valid-folders)
    (cl-loop for folder in (lsp-session-folders session)
             if (file-exists-p folder)
             do (push folder valid-folders)
             else do (push folder invalid-folders))
    (if -remove-invalid
        (if invalid-folders
            (when (dired-mark-pop-up
                   " *lsp-remove*" 'delete invalid-folders 'yes-or-no-p
                   "Remove these folders ")
              (setf (lsp-session-folders (lsp-session)) valid-folders)
              (lsp--persist-session (lsp-session)))
          (message "Nothing to remove."))
      (let ((folder (completing-read "Folder to remove" valid-folders nil t)))
        (setf (lsp-session-folders (lsp-session)) (delete folder valid-folders))
        (lsp--persist-session (lsp-session))))))

(define-hook! lsp|after-open (lsp-after-open-hook)
  (flycheck-mode 1)

  (lsp-ui-mode 1)
  (lsp-ui-flycheck-enable 1)
  (lsp-ui-sideline-enable 1)

  ;; default to sort and filter by server
  (setq-local company-transformers nil)
  (setq-local company-lsp-cache-candidates nil)

  (add-to-list 'company-backends 'company-lsp)
  (add-to-list 'company-backends 'company-files))

(defun lsp//ui-doc-toggle (-toggle)
  (when (require 'lsp-ui-doc nil t)
    (if (> -toggle 0)
        (progn
          (setq lsp-eldoc-render-all nil)
          (lsp-ui-doc-mode 1))
      (setq lsp-eldoc-render-all t)
      (lsp-ui-doc-mode -1))))

(defun lsp//eldoc-message (&optional msg)
  (unless lsp--disable-eldoc-in-minibuffer
    (run-at-time 0 nil (lambda () (eldoc-message msg)))))

(with-eval-after-load 'lsp
  (dap-mode 1)
  (dap-ui-mode 1)

  (setq lsp-auto-guess-root t)
  (setq lsp-session-file (expand-var! "lsp-sessions"))
  (setq lsp-auto-configure nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-signature-enabled t)
  (setq lsp-hover-enabled t)
  (setq lsp-eldoc-hook '(lsp-hover))
  (setq lsp-restart 'auto-restart)

  (advice-add 'lsp--eldoc-message :override 'lsp//eldoc-message)

  (define-key!
    ("M-s l" . lsp-lens-mode)
    ("M-s h h" . lsp-document-highlight)
    ("C-c R" . lsp-rename)
    ("C-c S" . lsp-describe-session)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action)))

(with-eval-after-load 'lsp-ui
  (require 'lsp-ui-flycheck)
  (require 'lsp-ui-sideline)

  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-doc-max-width 80))

(with-eval-after-load 'company-lsp
  (setq company-lsp-async t))

(provide 'init-lsp-mode)
