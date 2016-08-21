;; {{ @see http://emacs-journey.blogspot.com.au/2012/06/improving-ansi-term.html
;; kill the buffer when terminal is exited
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; utf8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)


(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))

(defun term-send-kill-whole-line ()
  "Kill whole line in term mode."
  (interactive)
  (term-send-raw-string "\C-a")
  (term-send-raw-string "\C-k"))

(defun term-send-kill-line ()
  "Kill line in term mode."
  (interactive)
  (term-send-raw-string "\C-k"))

(with-eval-after-load 'multi-term
  (setq multi-term-program "/bin/zsh")
  (setq term-unbind-key-list '("C-x" "<ESC>" "C-y" "C-h" "C-c"))
  (setq term-bind-key-alist
        '(("C-c C-c" . term-interrupt-subjob)
          ("C-c C-n" . multi-term)
          ("M-p" . term-send-up)
          ("M-n" . term-send-down)
          ("C-s" . swiper)
          ("C-r" . term-send-reverse-search-history)
          ("C-m" . term-send-raw)
          ("M-k" . term-send-kill-whole-line)
          ("C-_" . term-send-raw)
          ("M-f" . term-send-forward-word)
          ("M-b" . term-send-backward-word)
          ("C-k" . term-send-kill-line)
          ("C-p" . previous-line)
          ("C-n" . next-line)
          ("M-y" . yank-pop)
          ("M-." . term-send-raw-meta)
          ("M-]" . multi-term-next)
          ("M-[" . multi-term-prev)))

  (setq multi-term-dedicated-close-back-to-open-buffer-p t))



(provide 'init-term-mode)
