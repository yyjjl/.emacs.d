;;; -*- lexical-binding: t; -*-

(defun ymacs-debug/quit ()
  (interactive)
  (and (yes-or-no-p "Quit debug session?")
       (gud-call "quit")))

(defun ymacs-term/toggle-window@hack ()
  (interactive)
  (if-let ((buffer (gdb-get-buffer-create 'gdb-inferior-io))
           (window (get-buffer-window buffer)))
      (if (eq window (selected-window))
          (delete-window window)
        (select-window window))
    (display-buffer buffer)))

(defun gud-debug@restore (-fn &rest -args)
  (ymacs-debug//show-help nil)
  (lv-delete-window)
  (setq ymacs-debug--window-configuration (current-window-configuration))

  (delete-other-windows)

  (add-to-list 'display-buffer-alist
               '(ymacs-debug//gud-source-buffer-p ymacs-debug//display-buffer))
  (advice-add #'ymacs-term/toggle-window :override #'ymacs-term/toggle-window@hack)

  (apply -fn -args)

  (when (buffer-live-p gud-comint-buffer)
    (with-current-buffer gud-comint-buffer
      (when (ymacs-debug//gdb-running-p)
        (display-buffer (gdb-get-buffer-create 'gdb-inferior-io))
        (display-buffer (gdb-get-buffer-create 'gdb-locals-buffer)))
      ;; make print command works
      (setq-local comint-prompt-read-only nil)
      (when (not (ymacs-debug//gdb-running-p))
        (local-unset-key [remap comint-delchar-or-maybe-eof]))

      (ymacs-debug-command-buffer-mode 1)

      (when-let (window (get-buffer-window))
        (select-window window 'norecord)))))

(after! gud
  (setq gud-find-expr-function #'ymacs-debug//find-expr)

  (add-hook 'gdb-inferior-io-mode-hook #'ymacs-debug-command-buffer-mode)

  (define-advice gud-display-line (:around (-fn &rest -args) save-position)
    (unless (equal -args ymacs-debug--buffer-position)
      (apply -fn -args)
      ;; make sure the position is visible
      (redisplay t)

      (setq ymacs-debug--buffer-position -args))

    (ymacs-debug//show-help
     (concat ymacs-debug--help-format
             (when (ymacs-debug//gdb-running-p)
               (concat "\n" ymacs-debug--gdb-help-format)))))

  (define-advice gud-sentinel (:after (-proc _) cleanup)
    (advice-remove #'ymacs-term/toggle-window #'ymacs-term/toggle-window@hack)
    (setq display-buffer-alist
          (assq-delete-all 'ymacs-debug//gud-source-buffer-p display-buffer-alist))

    (when (memq (process-status -proc) '(exit signal))
      (dolist (buffer ymacs-debug--buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (ymacs-debug-running-session-mode -1)
            (ymacs-debug-info-buffer-mode -1))))
      (setq ymacs-debug--buffers nil)
      ;; restore windows
      (when (window-configuration-p ymacs-debug--window-configuration)
        (set-window-configuration ymacs-debug--window-configuration)
        (setq ymacs-debug--window-configuration nil)
        (message "Windows restored."))))

  (define-advice gud-find-file (:around (-fn -file) track-files)
    (let ((buffer (funcall -fn -file)))
      (cl-pushnew buffer ymacs-debug--buffers)
      (with-current-buffer buffer
        (unless ymacs-debug-running-session-mode
          (ymacs-debug-running-session-mode 1))
        (unless (or ymacs-debug-info-buffer-mode
                    (not (ymacs-debug//gdb-running-p)))
          (ymacs-debug-info-buffer-mode 1)))
      buffer))

  (advice-add #'pdb :around #'gud-debug@restore)
  (advice-add #'gud-gdb :around #'gud-debug@restore))

(after! gdb-mi
  (advice-add #'gdb :around #'gud-debug@restore))
