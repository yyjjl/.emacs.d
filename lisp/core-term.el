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
    (run-hooks 'term-or-comint-process-exit-hook)
    (when (memq (process-status $proc) '(signal exit))
      (let ((buffer (process-buffer $proc)))
        (kill-buffer buffer)))))

;; Add below code to .zshrc to make term-mode track value changes
;;   if [ -n "$INSIDE_EMACS" ];then
;;       chpwd() {print -P "\033AnSiTc %d"}
;;   fi

(defun term*multi-term-switch-internal-hack (&rest _)
  ;; Reset popup window
  (setq multi-term-buffer-list
        (cl-remove-if-not #'buffer-live-p multi-term-buffer-list)))

(with-eval-after-load 'multi-term
  ;; Fix conflict with `core-popups.el'
  (remove-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook :append)
  (add-hook 'term-mode-hook 'multi-term-keystroke-setup)

  (advice-add 'multi-term-switch-internal :before
              #'term*multi-term-switch-internal-hack)

  (setq multi-term-scroll-to-bottom-on-output t)
  (setq multi-term-program (or term-zsh-path term-bash-path))

  (setq term-bind-key-alist
        '(("C-c C-c" . term-interrupt-subjob)
          ("C-c C-e" . term-send-esc)
          ("C-c C-p" . term/previous-line)
          ;; ("C-n" . next-line)
          ("C-r" . isearch-backward)
          ("C-m" . term-send-return)
          ("C-y" . term-paste)
          ("M-f" . term-send-forward-word)
          ("M-b" . term-send-backward-word)
          ("M-o" . term-send-backspace)
          ("M-p" . term-send-up)
          ("M-n" . term-send-down)
          ("M-M" . term-send-forward-kill-word)
          ("M-N" . term-send-backward-kill-word)
          ("<C-backspace>" . term-send-backward-kill-word)
          ("<M-backspace>" . term-send-backward-kill-word)
          ("C-DEL" . term-send-backward-kill-word)
          ("M-DEL" . term-send-backward-kill-word)
          ("M-r" . term-send-reverse-search-history)
          ("M-d" . term-send-delete-word)
          ("M-," . term-send-raw)
          ("C-s" . swiper/dispatch)
          ("M-]" . multi-term-next)
          ("M-[" . multi-term-prev)
          ("C-g" . keyboard-quit)))

  (setq multi-term-dedicated-close-back-to-open-buffer-p t)

  (define-key term-raw-map [remap term-send-raw] #'term/conditional-send-raw))

(define-hook! term|utf8-setup (term-exec-hook)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(define-hook! term|autoclose-buffer (comint-exec-hook)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel proc (term//wrap-sentinel (process-sentinel proc))))))

(global-set-key [f8] 'term/pop-shell)

(provide 'core-term)
