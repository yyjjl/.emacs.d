;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash))

(define-variable!
  :format "term-%s-path"
  zsh
  bash
  (vterm :name term-prefer-vterm (not emacs-lite-setup-p)))

(require-packages!
 (vterm :when term-prefer-vterm))

(defun vterm*self-insert ()
  "Sends invoking key to libvterm. Fix meta key error in terminal"
  (interactive)
  (when vterm--term
    (let* ((modifiers (event-modifiers
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
        (vterm-send-key key shift meta ctrl)))))

(defun term*setup-environment (-fn &rest -args)
  (with-temp-env! (term//extra-env)
    (apply -fn -args)))
(advice-add 'compilation-start :around 'term*setup-environment)

(with-eval-after-load 'term
  (define-key term-raw-map [remap term-send-raw] #'term/conditional-send-raw)
  (define-key term-mode-map (kbd "M-o") #'term/ivy-switch))

(with-eval-after-load 'vterm
  (advice-add 'vterm--self-insert :override #'vterm*self-insert)

  (define-hook! (vterm|set-title title) (vterm-set-title-functions)
    (setq term--extra-name title))

  (define-key! :map vterm-copy-mode-map
    ("C-c C-k" . vterm-copy-mode))
  (define-key! :map vterm-mode-map
    ("M-}" . term/switch-next)
    ("M-{" . term/switch-prev)
    ("M-o" . term/ivy-switch)
    ("M-N" . term/set-extra-name)
    ("M-y" . term/yank-pop)
    ("C-k" . term/kill-line)
    ("C-s" . term/swiper)
    ("C-c C-l" . vterm-copy-mode)
    ("<C-backspace>" . vterm-send-meta-backspace)
    ("C-S-t" . term/pop-shell-current-directory)))

(defun term*eof-hack (_)
  (setq term-directly-kill-buffer t))
(advice-add 'comint-delchar-or-maybe-eof :after #'term*eof-hack)

(define-hook! term|utf8-setup (term-exec-hook)
  (when-let (proc (get-buffer-process (current-buffer)))
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)))

(define-hook! term|autoclose-buffer (comint-exec-hook)
  (when-let (proc (get-buffer-process (current-buffer)))
    (set-process-sentinel proc (term//wrap-sentinel (process-sentinel proc)))))

(define-hook! term|setup-name (term-mode-hook vterm-mode-hook)
  (setq term-goto-process-mark nil)
  (setq term--extra-name nil)
  (setq mode-line-buffer-identification '("%b" (term--extra-name (": " term--extra-name))))

  (when (eq major-mode 'vterm-mode)
    (setq mode-line-show-root-p nil)))

(define-key!
  ([f8] . term/pop-shell)
  ([C-f8] . term/ssh)
  ([C-f9] . term/shell-command-on-server))

(provide 'core-term)
