;; -*- lexical-binding:t -*-
(require-packages!
 lsp-mode
 lsp-ui
 dap-mode
 company-lsp
 ivy)

(defcustom lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)
(defvar lsp-disable-eldoc-in-minibuffer nil)
(defvar ivy-lsp-symbols-request-id nil)



(ignore-errors
  (require 'lsp)
  (require 'lsp-clients))

(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))


(defun ivy//lsp-cannel-request ()
  (when ivy-lsp-symbols-request-id
    (lsp--cancel-request ivy-lsp-symbols-request-id)
    (setq ivy-lsp-symbols-request-id nil)))

(defun ivy//lsp-transform-candidate (candidate)
  (let* ((container-name (gethash "containerName" candidate))
         (name (gethash "name" candidate))
         (uri (gethash "uri" candidate))
         (type (or (alist-get (gethash "kind" candidate) lsp--symbol-kind)
                   "Unknown"))
         (string (concat (if (s-blank? container-name)
                             name
                           (concat container-name "." name))
                         " "
                         (propertize (concat "(" type ")")
                                     'face 'font-lock-keyword-face)
                         " "
                         (lsp--uri-to-path uri))))
    (put-text-property 0 1 'lsp-symbol candidate string)
    string))

(defun ivy//lsp-callback (candidates)
  (when ivy-lsp-symbols-request-id
    (setq ivy-lsp-symbols-request-id nil)
    (ivy--set-candidates (mapcar #'ivy//lsp-transform-candidate candidates))
    (ivy--insert-minibuffer (ivy--format ivy--all-candidates))))

(defun ivy//lsp-workspace-symbol-candidates (pattern workspaces)
  (with-lsp-workspaces workspaces
    ;; cancel if there is pending request
    (ivy//lsp-cannel-request)

    (-let (((request &as &plist :id request-id)
            (lsp-send-request-async
             (lsp-make-request "workspace/symbol" (list :query pattern))
             #'ivy//lsp-callback
             'detached)))

      (setq ivy-lsp-symbols-request-id request-id)
      (lsp-send-request-async request #'ivy//lsp-callback 'detached)
      nil)))

(defun ivy//lsp-workspace-symbol-action (candidate)
  "Action for ivy workspace symbol. CANDIDATE is the selected item."
  (-let* (((&hash "uri" "range" (&hash "start" (&hash "line" "character")))
           (gethash "location" (get-text-property 0 'lsp-symbol candidate))))
    (find-file (lsp--uri-to-path uri))
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)))

(defun ivy/lsp-workspace-symbol (&optional -global)
  (interactive "P")
  (let ((workspaces (if -global
                        (-uniq (-flatten (ht-values (lsp-session-folder->servers (lsp-session)))))
                      (lsp-workspaces))))
    (unless workspaces
      (user-error "No LSP workspace active"))

    (ivy-read (format "%s workspace: " (if -global "Global" "Current"))
              (lambda (pattern)
                (or (let ((ivy-text pattern))
                      (ivy-more-chars))
                    (ivy//lsp-workspace-symbol-candidates pattern workspaces)))
              :dynamic-collection t
              :unwind (lambda ()
                        (ivy//lsp-cannel-request)
                        (swiper--cleanup))
              :action #'ivy//lsp-workspace-symbol-action
              :caller 'ivy-lsp
              :history 'ivy-lsp-history)))

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
  ;; (lsp-ui-sideline-enable 1)

  ;; default to sort and filter by server
  (setq-local company-transformers nil)

  ;; (company//add-backend 'company-files)
  (company//add-backend 'company-lsp))

(defun lsp/toggle-doc ()
  (interactive)
  (when (require 'lsp-ui-doc nil t)
    (if lsp-ui-doc-mode
        (progn
          (setq lsp-eldoc-render-all t)
          (lsp-ui-doc-mode -1))
      (setq lsp-eldoc-render-all nil)
      (lsp-ui-doc-mode 1))))

(defun lsp*override-eldoc-message (&optional msg)
  (unless lsp-disable-eldoc-in-minibuffer
    (run-at-time 0 nil (lambda () (eldoc-message msg)))))

(defun lsp*around-render-on-hover-content (-fn -contents -render-all)
  (let ((content (funcall -fn -contents -render-all)))
    (unless (equal (buffer-name) "*lsp-help*")
      (let ((content-length (length content))
            (split-pos (string-match (rx line-end) content)))
        (when (or (< split-pos content-length)
                  (>= split-pos (frame-width)))
          (setq content
                (concat (substring content 0 (min split-pos (- (frame-width) 30)))
                        (propertize
                         (if-let (keys (where-is-internal 'lsp-describe-thing-at-point))
                             (format " ... (%s to see more)"
                                     (key-description (car keys)))
                           " ...")
                         'face 'font-lock-comment-face))))))
    content))

(with-eval-after-load 'lsp
  (dap-mode 1)
  (dap-ui-mode 1)

  (setq lsp-auto-guess-root t)
  (setq lsp-session-file (expand-var! "lsp-sessions"))
  (setq lsp-auto-configure nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-restart 'auto-restart)

  (advice-add 'lsp--eldoc-message :override #'lsp*override-eldoc-message)
  (advice-add 'lsp--render-on-hover-content :around #'lsp*around-render-on-hover-content)

  (define-key! :map lsp-mode-map
    ("M-s l" . lsp-lens-mode)
    ("M-s h h" . lsp-document-highlight)
    ("C-c R" . lsp-rename)
    ("C-c I" . ivy/lsp-workspace-symbol)
    ("C-c S" . lsp-describe-session)
    ("C-c B" . lsp-format-buffer)
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
