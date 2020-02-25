;; -*- lexical-binding:t -*-

(require-packages!
 dap-mode
 lsp-mode
 lsp-ui
 ivy)

(defcustom lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)

(ignore-errors
  (require 'lsp)
  (require 'lsp-clients))

(cl-defmacro lsp//try-enable (name &key (enable t) (init nil) (fallback nil))
  (declare (indent 1))
  `(add-transient-hook! (hack-local-variables-hook :local t :name ,name)
     (if (and ,enable
              lsp-enable-in-project-p
              (ignore-errors (lsp))
              (bound-and-true-p lsp-mode))
         ,init
       ,fallback)))

(define-hook! lsp|after-open (lsp-after-open-hook)
  (lsp-ui-mode 1)
  (lsp-flycheck-enable)

  ;; default to sort and filter by server
  (setq-local company-transformers nil))

(config! lsp
  :bind
  (:map lsp-mode-map
   ("M-s l" . lsp-lens-mode)
   ("M-s h h" . lsp-document-highlight)
   ("M-s '" . lsp-avy-lens)
   ("C-c R" . lsp-rename)
   ("C-c I" . lsp/ivy-workspace-symbol)
   ("C-c S" . lsp-describe-session)
   ("C-c B" . lsp-format-buffer)
   ("C-c C-d" . lsp-describe-thing-at-point)
   ("C-c C-SPC" . lsp-execute-code-action)
   ("C-S-SPC" . lsp-signature-activate))

  :advice
  (:override lsp--auto-configure :name ignore)

  (:override lsp--lv-message
   :define (message)
   (setq lsp--signature-last-buffer (current-buffer))
   (let ((lv-force-update t))
     (lv-message (replace-regexp-in-string "%" "%%" message))))

  (:around lsp--render-on-hover-content
   :define (-fn -contents -render-all)
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

  :config
  (setq lsp-auto-configure t)
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-diagnostic-package :auto)
  (setq lsp-restart 'auto-restart)
  (setq lsp-session-file (expand-var! "lsp-sessions"))

  (setq lsp-enable-completion-at-point t)
  (setq lsp-symbol-highlighting-skip-current t)

  (add-to-list
   'hydra-local-toggles-heads-list
   '(lsp-mode
     "LSP"
     (("t i" lsp-toggle-trace-io
       "trace io" :toggle lsp-print-io)
      ("t h" lsp-toggle-symbol-highlight
       "highlight" :toggle lsp-enable-symbol-highlighting)
      ("t t" lsp-toggle-on-type-formatting
       "on type formating" :toggle lsp-enable-on-type-formatting)
      ("t s" lsp-toggle-signature-auto-activate
       "signature" :toggle lsp-signature-auto-activate))))) ()

(config! lsp-ui
  :config
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

  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-doc-delay 0.5))

(provide 'init-lsp-mode)
