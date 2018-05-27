;;; -*- lexical-binding: t; -*-

(setvar! term-zsh-path (executable-find "zsh")
         term-bash-path (executable-find "bash"))

(eval-when-compile
  (require 'dash))



(defvar term-goto-process-mark nil)
(defvar-local term--ssh-info nil)
(defvar-local term--parent-buffer nil)

(defvar term-or-comint-process-exit-hook nil)

(defsubst term//get-popup-window ()
  (frame-parameter nil 'term-popup-window))
(defsubst term//set-popup-window (popup-window)
  (set-window-dedicated-p popup-window t)
  (set-frame-parameter nil 'term-popup-window popup-window))

(defsubst term//wrap-sentinel (&optional sentinel)
  (lambda ($proc $msg)
    (and sentinel (funcall sentinel $proc $msg))
    (ignore-errors (run-hooks 'term-or-comint-process-exit-hook))
    (when (memq (process-status $proc) '(signal exit))
      (let ((buffer (process-buffer $proc)))
        (kill-buffer buffer)))))

;; Add below code to .zshrc to make term-mode track value changes
;;   if [ -n "$INSIDE_EMACS" ];then
;;       chpwd() {print -P "\033AnSiTc %d"}
;;   fi

(defun term*setup-environment ($fn &rest $args)
  (with-temp-env! (term//extra-env)
    (apply $fn $args)))

(advice-add 'compilation-start :around 'term*setup-environment)

(with-eval-after-load 'term
  (define-key term-raw-map [remap term-send-raw] #'term/conditional-send-raw))

(define-hook! term|utf8-setup (term-exec-hook)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(define-hook! term|autoclose-buffer (comint-exec-hook)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel proc (term//wrap-sentinel (process-sentinel proc))))))

(global-set-key [f8] 'term/pop-shell)

(provide 'core-term)
