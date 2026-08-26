;;; -*- lexical-binding: t; -*-

(declare-function term-interrupt-subjob "term")

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
    (setq term-goto-process-mark nil))

  (define-hook! ymacs-term//set-utf8 (term-exec-hook)
    (when-let* ((proc (get-buffer-process (current-buffer))))
      (set-process-coding-system proc 'utf-8-unix 'utf-8-unix))))

(after! comint
  (add-hook 'comint-exec-hook #'ymacs-term//sentinel-setup)

  (define-hook! ymacs-term//comint-setup (comint-mode-hook)
    (corfu-mode 1)
    (setq-local corfu-preselect 'prompt)

    ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
    (setq show-trailing-whitespace nil)
    (setq-local corfu-auto nil)
    (setq-local yas-buffer-local-condition nil)

    ;; (when (string-match-p "inferior\\|interactive" (symbol-name major-mode))
    ;;   (setq ymacs-term-exit-action 'shell))
    ))

(after! shell
  (bash-completion-setup))
