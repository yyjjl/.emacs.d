;;; -*- lexical-binding: t; -*-

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

(run-after-init! 100
  ;; Start server
  (unless (or noninteractive (daemonp))
    (require 'server)
    (unless (server-running-p)
      (server-start)))

  ;; Collect garbage after focus changes
  (add-function
   :after after-focus-change-function
   (lambda ()
     (unless (frame-focus-state)
       (garbage-collect))))

  ;; Autoloads
  (unless (file-exists-p ymacs-autoloads-file)
    (autoload 'ymacs-default/generate-autoloads (expand! "commands-package") nil t)
    (ymacs-default/generate-autoloads))
  (load ymacs-autoloads-file nil t)

  ;; Restore `file-name-handler-alist' and `gc-cons-threshold'
  (setq file-name-handler-alist
        `((,ymacs-editor-external-file-regexp . ymacs-editor//external-file-handler)
          ,@ymacs-file-name-handler-alist))
  (setq gc-cons-threshold ymacs-gc-cons-threshold)
  (setq gc-cons-percentage 0.3)

  ;; Set default modeline
  (ymacs-modeline-set! default main)

  ;; Setup autoloads and packages
  (setq package-selected-packages ymacs-required-packages)

  (message "Init Time: %.3f (with %d packages activated)"
           (float-time (time-subtract after-init-time before-init-time))
           (length package-activated-list)))
