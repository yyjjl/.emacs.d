;;; -*- lexical-binding: t; -*-

(defun ymacs-ui//pulse (&rest _)
  (xref-pulse-momentarily))


;; Setup `mode-line-format'
(define-hook! ymacs-ui//ui-setup (after-init-hook)
  (ace-window-display-mode 1)
  (ymacs-modeline-set! default main)

  (require 'xref)
  (dolist (cmd '(recenter-top-bottom
                 other-window
                 next-error
                 ace-window
                 pop-to-mark-command
                 pop-global-mark
                 ymacs-editor/goto-last-point))
    (advice-add cmd :after #'ymacs-ui//pulse))

  (which-key-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1))

(define-hook! ymacs-ui//indirect-buffer-setup (clone-indirect-buffer-hook)
  (when (derived-mode-p 'prog-mode 'text-mode)
    (ymacs-ui/view-code-mode 1)))

(add-function :after after-focus-change-function #'ymacs-modeline//focus-change)
(advice-add #'handle-switch-frame :after #'ymacs-modeline//focus-change)

(add-hook 'find-file-hook #'ymacs-modeline//update-buffer-file-name)
(add-hook 'after-save-hook #'ymacs-modeline//update-buffer-file-name)
(add-hook 'clone-indirect-buffer-hook #'ymacs-modeline//update-buffer-file-name)
(advice-add #'not-modified :after #'ymacs-modeline//update-buffer-file-name)
(advice-add #'rename-buffer :after #'ymacs-modeline//update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'ymacs-modeline//update-buffer-file-name)


;; update buffer encoding
(add-variable-watcher
 'buffer-file-coding-system
 (lambda (-sym _value _op -where)
   (when -where
     (with-current-buffer -where
       (ymacs-modeline//update-buffer-encoding (symbol-value -sym))))))

;; update vcs
(add-hook 'find-file-hook #'ymacs-modeline//update-vcs)
(add-hook 'after-save-hook #'ymacs-modeline//update-vcs)
(advice-add #'vc-refresh-state :after #'ymacs-modeline//update-vcs)

;; update checker
(advice-add #'flymake--handle-report :after #'ymacs-modeline//update-checker-state)
(add-hook 'lsp-configure-hook
          (lambda ()
            (run-with-idle-timer 0 nil #'ymacs-modeline//update-lsp-state (current-buffer))))
(add-hook 'lsp-unconfigure-hook #'ymacs-modeline//update-lsp-state)

(ymacs-modeline-set! (comint term vterm compilation) shell)
(ymacs-modeline-set! dired dired)
(ymacs-modeline-set! image media)
(ymacs-modeline-set! (message git-commit) message)
(ymacs-modeline-set! magit vcs)
(ymacs-modeline-set! org-src org-src)
(ymacs-modeline-set! git-timemachine timemachine)

(ymacs-modeline-set! (prog text conf) header :header)
