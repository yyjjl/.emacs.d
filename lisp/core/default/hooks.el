;;; -*- lexical-binding: t; -*-

(run-after-init! 100
  ;; Start server
  (unless (or noninteractive (daemonp))
    (require 'server)
    (unless (server-running-p)
      (server-start)))

  (gcmh-mode 1)

  ;; Autoloads
  (unless (file-exists-p ymacs-autoloads-file)
    (autoload 'ymacs-default/generate-autoloads (expand! "commands-package") nil t)
    (ymacs-default/generate-autoloads))
  (load ymacs-autoloads-file nil t)

  (setq file-name-handler-alist
        `((,ymacs-editor-external-file-regexp . ymacs-editor//external-file-handler)
          ,@file-name-handler-alist))

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

  (ymacs-modeline-set! (comint term vterm)
    :mode-line shell
    :body (tab-line-mode 1))
  (ymacs-modeline-set! dired :mode-line dired)
  (ymacs-modeline-set! image :mode-line media)
  (ymacs-modeline-set! (message git-commit) :mode-line message)
  (ymacs-modeline-set! magit :mode-line vcs)
  (ymacs-modeline-set! org-src :mode-line org-src)
  (ymacs-modeline-set! git-timemachine :mode-line timemachine)
  ;; (ymacs-modeline-set! (prog text conf) :header-line header)
  ;; Set default modeline
  (ymacs-modeline-set! default :mode-line main)

  ;; Setup autoloads and packages
  (setq package-selected-packages ymacs-required-packages)

  (message "Init Time: %.3f (with %d packages activated)"
           (float-time (time-subtract after-init-time before-init-time))
           (length package-activated-list)))
