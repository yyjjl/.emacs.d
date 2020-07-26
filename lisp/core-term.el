;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash))

(define-variable!
  :format "term-%s-path"
  zsh
  bash
  (vterm :name term-prefer-vterm :value (not emacs-lite-setup-p)))

(require-packages! (vterm :when term-prefer-vterm))

(config! compile
  :prefix term
  :advice
  (:around compilation-start
   :use setup-environment
   :define (-fn &rest -args)
   (with-temp-env! (term//extra-env)
     (apply -fn -args))))

(config! term
  :bind
  (:map term-raw-map ([remap term-send-raw] . term/conditional-send-raw))
  (:map term-mode-map ("M-o" . term/ivy-switch))

  :hook
  (set-name
   :define (term-mode-hook)
   (setq term-goto-process-mark nil)
   (setq term-extra-name nil)
   (setq mode-line-buffer-identification
         '("%b" (term-extra-name (": " term-extra-name)))))

  (set-utf8
   :define (term-exec-hook)
   (when-let (proc (get-buffer-process (current-buffer)))
     (set-process-coding-system proc 'utf-8-unix 'utf-8-unix))))

(config! vterm
  :bind
  (:map vterm-copy-mode-map
   ("C-c C-k" . vterm-copy-mode))
  (:map vterm-mode-map
   ("M-}" . term/switch-next)
   ("M-{" . term/switch-prev)
   ("M-o" . term/ivy-switch)
   ("M-N" . term/set-extra-name)
   ("M-y" . term/yank-pop)
   ("C-k" . term/kill-line)
   ("C-s" . term/swiper)
   ("C-c C-j" . vterm-copy-mode)
   ("C-c C-l" . vterm-copy-mode)
   ("<C-backspace>" . vterm-send-meta-backspace)
   ("C-S-t" . term/pop-shell-current-directory))

  :hook
  (set-name
   :define (vterm-mode-hook)
   (setq mode-line-show-root-p nil)
   (setq-local vterm-buffer-name-string (concat (buffer-name) ": %s"))
   (setq term-extra-name nil)
   (setq mode-line-buffer-identification
         '("%b" (vterm-copy-mode (:propertize "[copy]" face font-lock-builtin-face))
           (term-extra-name (": " term-extra-name)))))

  :advice
  (:override vterm--self-insert
   :define ()
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


(config! comint
  :bind
  (:map comint-mode-map ("M-h" . counsel-shell-history))

  :hook
  (autoclose-buffer
   :define (comint-exec-hook)
   (when-let (proc (get-buffer-process (current-buffer)))
     (set-process-sentinel proc (term//wrap-sentinel (process-sentinel proc)))))

  :advice
  (:after comint-delchar-or-maybe-eof
   :define (_) (setq term-directly-kill-buffer t)))

(define-key!
  ([f8] . term/pop-shell)
  ([C-f8] . term/ssh)
  ([C-f9] . term/shell-command-on-server))

(provide 'core-term)
