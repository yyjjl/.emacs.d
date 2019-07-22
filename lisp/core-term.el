;;; -*- lexical-binding: t; -*-

(setvar! term-zsh-path (executable-find "zsh")
         term-bash-path (executable-find "bash"))

(require-packages! term)

(eval-when-compile
  (require 'dash))



(defvar term-goto-process-mark nil)

;; Add below code to .zshrc to make term-mode track value changes
;;   if [ -n "$INSIDE_EMACS" ];then
;;       printf '\033AnSiTu %s\n' "$USER"
;;       chpwd() {print -P "\033AnSiTc %d"}
;;   fi

(defun term*setup-environment (-fn &rest -args)
  (with-temp-env! (term//extra-env)
    (apply -fn -args)))
(advice-add 'compilation-start :around 'term*setup-environment)

(with-eval-after-load 'term
  (define-key term-raw-map [remap term-send-raw] #'term/conditional-send-raw)
  (define-key term-mode-map (kbd "M-o") #'term/ivy-switch))

(defun term*eof-hack (_)
  (setq term-directly-kill-buffer t))
(advice-add 'comint-delchar-or-maybe-eof :after #'term*eof-hack)

(define-hook! term|utf8-setup (term-exec-hook)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(define-hook! term|autoclose-buffer (comint-exec-hook)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel proc (term//wrap-sentinel (process-sentinel proc))))))

(define-hook! term|setup-name (term-mode-hook)
  (setq term--extra-name nil)
  (setq mode-line-buffer-identification '("%b" (term--extra-name (": " term--extra-name)))))

(define-key!
  ([f8] . term/pop-shell)
  ([C-f8] . term/ssh)
  ([C-f9] . term/shell-command-on-server))

(provide 'core-term)
