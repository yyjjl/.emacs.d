;;; -*- lexical-binding: t; -*-

(setvar! term-zsh-path (executable-find "zsh")
         term-bash-path (executable-find "bash")
         term-prefer-vterm t)

(require-packages!
 term
 (vterm :when term-prefer-vterm))

(eval-when-compile
  (require 'dash))



(defun term*setup-environment (-fn &rest -args)
  (with-temp-env! (term//extra-env)
    (apply -fn -args)))
(advice-add 'compilation-start :around 'term*setup-environment)

(with-eval-after-load 'term
  (define-key term-raw-map [remap term-send-raw] #'term/conditional-send-raw)
  (define-key term-mode-map (kbd "M-o") #'term/ivy-switch))

(with-eval-after-load 'vterm-mode
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
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(define-hook! term|autoclose-buffer (comint-exec-hook)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel proc (term//wrap-sentinel (process-sentinel proc))))))

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
