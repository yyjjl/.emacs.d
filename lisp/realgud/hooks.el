;;; -*- lexical-binding: t; -*-

(after! realgud
  (advice-add 'realgud-populate-common-fn-keys-standard :override #'ignore)

  (define-advice realgud-term-sentinel (:after (-proc _) restore-window-config)
    (let ((buffer (process-buffer -proc)))
      (when (and (buffer-live-p buffer)
                 (memq (process-status -proc) '(exit signal)))
        ;; Do not kill buffer, save breakpoints
        (when (window-configuration-p ymacs-realgud--window-configuration)
          (set-window-configuration ymacs-realgud--window-configuration)
          (setq ymacs-realgud--window-configuration nil)))))

  (define-advice realgud:run-process (:around (-fn &rest -args) save-window-config)
    (setq ymacs-realgud--window-configuration (current-window-configuration))

    (delete-other-windows)
    (if (>= (frame-width) (or split-width-threshold 120))
        (split-window-horizontally)
      (split-window-vertically))

    (when-let* ((buffer (apply -fn -args))
                (window (get-buffer-window)))
      (set-window-dedicated-p window t)
      buffer))

  (define-advice realgud:terminate (:before (-buffer) save-breakpoints)
    (when (buffer-live-p -buffer)
      (with-current-buffer -buffer
        (setq ymacs-realgud--saved-breakpoints
              (--map
               (cons (realgud-loc-filename it)
                     (realgud-loc-line-number it))
               (realgud-cmdbuf-info-bp-list realgud-cmdbuf-info))))))

  (define-advice realgud:cmd-quit (:before (_arg) display-cmdbuf)
    (when-let (cmdbuf (realgud-get-cmdbuf))
      (pop-to-buffer cmdbuf))))
