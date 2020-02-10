;; -*- lexical-binding:t -*-
(require-packages!
 lsp-mode
 lsp-ui
 lsp-ivy
 ivy)

(defcustom lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)



(ignore-errors
  (require 'lsp)
  (require 'lsp-clients))

(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))

(cl-defmacro lsp//try-enable (name &key (enable t) (init nil) (fallback nil))
  `(add-transient-hook! (hack-local-variables-hook :local t :name ,name)
     (if (and ,enable
              lsp-enable-in-project-p
              (ignore-errors (lsp))
              (bound-and-true-p lsp-mode))
         ,init
       ,fallback)))

(defun lsp/signature-activate ()
  (interactive)
  (setq-local lsp--last-signature nil)
  (setq-local lsp--last-signature-index nil)
  (add-hook 'post-command-hook #'lsp-signature-maybe-stop)
  (lsp-signature)
  (lsp-signature-mode t))

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
  (lsp-flycheck-enable 1)
  (lsp-ui-sideline-enable 1)
  ;; default to sort and filter by server
  (setq-local company-transformers nil))

(defun lsp*around-render-on-hover-content (-fn -contents -render-all)
  (let ((content (funcall -fn -contents -render-all)))
    (unless (core-popups//help-buffer-matcher (current-buffer))
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
  (require 'lsp-ui)
  (require 'lsp-ivy)

  (defun lsp--lv-message (message)
    (setq lsp--last-signature-buffer (current-buffer))
    (let ((lv-force-update t))
      (lv-message (replace-regexp-in-string "%" "%%" message))))

  (setq lsp-auto-configure nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-enable-completion-at-point t)
  (setq lsp-diagnostic-package :auto)
  (setq lsp-restart 'auto-restart)
  (setq lsp-session-file (expand-var! "lsp-sessions"))

  (advice-add 'lsp--render-on-hover-content :around #'lsp*around-render-on-hover-content)

  (define-key! :map lsp-mode-map
    ("M-s l" . lsp-lens-mode)
    ("M-s h h" . lsp-document-highlight)
    ("M-s '" . lsp-avy-lens)
    ("C-c R" . lsp-rename)
    ("C-c I" . lsp-ivy-workspace-symbol)
    ("C-c G" . lsp-ivy-global-workspace-symbol)
    ("C-c S" . lsp-describe-session)
    ("C-c B" . lsp-format-buffer)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action)
    ("C-S-SPC" . lsp/signature-activate)))

(with-eval-after-load 'lsp-ui
  (require 'lsp-ui-sideline)

  (add-to-list
   'hydra-local-toggles-heads-list
   '(lsp-mode
     "LSP Doc"
     (("d e" (lsp-ui-doc-enable (not lsp-ui-doc-mode))
       "enable" :toggle lsp-ui-doc-mode)
      ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
       "signature" :toggle lsp-ui-doc-include-signature)
      ("d t" (setq lsp-ui-doc-position 'top)
       "top" :toggle (eq lsp-ui-doc-position 'top))
      ("d b" (setq lsp-ui-doc-position 'bottom)
       "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
      ("d p" (setq lsp-ui-doc-position 'at-point)
       "at point" :toggle (eq lsp-ui-doc-position 'at-point))
      ("d f" (setq lsp-ui-doc-alignment 'frame)
       "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
      ("d w" (setq lsp-ui-doc-alignment 'window)
       "align window" :toggle (eq lsp-ui-doc-alignment 'window)))))

  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-doc-max-width 80)
  (setq lsp-ui-doc-delay 0.5))

(provide 'init-lsp-mode)
