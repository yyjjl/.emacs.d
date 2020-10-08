;;; -*- lexical-binding: t; -*-

(after! compile
  (define-advice compilation-start (:around (-fn &rest -args) setup-env)
    (with-temp-env! (ymacs-term//extra-env)
      (apply -fn -args))))

(after! term
  (define-hook! ymacs-term|term-setup (term-mode-hook)
    (setq term-goto-process-mark nil)

    (setq ymacs-term-extra-name nil)
    (setq mode-line-buffer-identification
          '("%b" (ymacs-term-extra-name (": " ymacs-term-extra-name)))))

  (define-hook! ymacs-term|set-utf8 (term-exec-hook)
    (when-let (proc (get-buffer-process (current-buffer)))
      (set-process-coding-system proc 'utf-8-unix 'utf-8-unix))))


(after! vterm
  (define-hook! ymacs-term|vterm-setup (vterm-mode-hook)
    (setq ymacs-term-extra-name nil)
    (setq mode-line-buffer-identification
          '("%b"
            (vterm-copy-mode "[copy]")
            (ymacs-term-extra-name (": " ymacs-term-extra-name)))))

  (define-advice vterm--set-title (:override (title) set-suffix)
    (setq ymacs-term-extra-name title))

  (define-advice vterm--self-insert (:override () fix)
    "Sends invoking key to libvterm. Fix meta key error in terminal"
    (interactive)
    (when vterm--term
      (let* ((modifiers
              (event-modifiers
               (or (and (not (display-graphic-p))
                        (ignore-errors
                          (->> (seq-subseq (nreverse (recent-keys t)) 1)
                               (seq-take-while (lambda (x) (not (consp x))))
                               nreverse
                               key-description
                               kbd
                               listify-key-sequence)))
                   last-input-event)))
             (shift (memq 'shift modifiers))
             (meta (memq 'meta modifiers))
             (ctrl (memq 'control modifiers)))
        (when-let ((key (key-description (vector (event-basic-type last-input-event)))))
          (vterm-send-key key shift meta ctrl))))))

(after! comint
  (add-hook 'comint-exec-hook #'ymacs-term//setup-sentinel))

(after! shell
  (bash-completion-setup)

  (define-hook! ymacs-term|shell-setup (shell-mode-hook)
    (setq ymacs-term-extra-name nil)
    (setq mode-line-buffer-identification
          '("%b" (ymacs-term-extra-name (": " ymacs-term-extra-name))))))
