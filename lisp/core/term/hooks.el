;;; -*- lexical-binding: t; -*-

(define-advice delete-window (:around (-fn &rest -args) record-term-buffer)
  (let ((term-buffer (current-buffer))
        (term-window? (eq (selected-window) (ymacs-popup//get-term-window))))
    (apply -fn -args)
    (when term-window?
      (setq ymacs-term--last-buffer term-buffer))))


(defun ymacs-term//around-Ctrl-c (-fn &rest -args)
  (when ymacs-term-keep-buffer-alive
    (user-error "can not send Ctrl-c"))
  (apply -fn -args))

(advice-add #'term-interrupt-subjob :around #'ymacs-term//around-Ctrl-c)
(advice-add #'comint-interrupt-subjob :around #'ymacs-term//around-Ctrl-c)

(after! compile
  (define-hook! (ymacs-popup//compilation-finish-hook -buffer _) (compilation-finish-functions)
    (when (buffer-live-p -buffer)
      (ymacs-term//set-quit-keys t)))

  (define-hook! ymacs-term//compilation-setup (compilation-mode-hook)
    (setq mode-line-buffer-identification
          '("%b"
            (:eval
             (propertize
              (if next-error-follow-minor-mode
                  "[follow]"
                (substitute-command-keys "[\\[next-error-follow-minor-mode] to follow]"))
              'face font-lock-comment-face))))))

(after! term
  (define-hook! ymacs-term//term-setup (term-mode-hook)
    (setq term-goto-process-mark nil)

    (setq ymacs-term-extra-name nil)
    (setq mode-line-buffer-identification
          '("%b" (ymacs-term-extra-name (": " ymacs-term-extra-name)))))

  (define-hook! ymacs-term//set-utf8 (term-exec-hook)
    (when-let (proc (get-buffer-process (current-buffer)))
      (set-process-coding-system proc 'utf-8-unix 'utf-8-unix))))


(after! vterm
  (define-hook! ymacs-term//vterm-setup (vterm-mode-hook)
    (setq ymacs-term-extra-name nil)
    (setq mode-line-buffer-identification
          '("%b"
            (vterm-copy-mode "[copy]")
            (ymacs-term-extra-name (": " ymacs-term-extra-name)))))

  (define-advice vterm--set-title (:override (title) set-suffix)
    (setq ymacs-term-extra-name title)))

(after! comint
  (add-hook 'comint-exec-hook #'ymacs-term//sentinel-setup)

  (define-hook! ymacs-term//comint-setup (comint-mode-hook)
    (corfu-mode 1)
    (setq-local corfu-preselect 'prompt)

    ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
    (setq show-trailing-whitespace nil)
    (setq-local corfu-auto nil)
    (setq-local yas-buffer-local-condition nil)

    (when (string-match-p "inferior\\|interactive" (symbol-name major-mode))
      (setq ymacs-term-exit-action 'shell)))

  (define-advice comint-delchar-or-maybe-eof (:override (-arg) maybe-toggle)
    "If point is at the end of the buffer and there is no input, send an EOF.
If not, delete -ARG characters forward."
    (interactive "p")
    (if-let (proc (get-buffer-process (current-buffer)))
        (if (and (eobp)
                 (= (point) (marker-position (process-mark proc))))
            (comint-send-eof)
          (delete-char -arg)))))

(after! shell
  (bash-completion-setup)

  (define-hook! ymacs-term//shell-setup (shell-mode-hook)
    (setq ymacs-term-extra-name nil)
    (setq mode-line-buffer-identification
          '("%b" (ymacs-term-extra-name (": " ymacs-term-extra-name))))))
