(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defadvice gud-display-line (after my-gud-highlight activate)
  "Highlight current line."
  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    (current-buffer)))))

(defun gud-kill-buffer ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook 'gud-kill-buffer)

;; {{ hack buffer
;; move the cursor to the end of last line if it's gud-mode
(defun hack-gud-mode ()
  (when (string= major-mode "gud-mode")
    (goto-char (point-max))))

(defadvice switch-to-buffer
    (after switch-to-buffer-after activate)
  (hack-gud-mode))

(defadvice select-window-by-number
    (after select-window-by-number-after activate)
  (hack-gud-mode))

;; windmove-do-window-select is from windmove.el
(defadvice windmove-do-window-select
    (after windmove-do-window-select-after activate)
  (hack-gud-mode))
;; }}

(defun gud-cls (&optional num)
  "clear gud screen"
  (interactive "p")
  (let ((old-window (selected-window)))
    (save-excursion
      (cond

       ((buffer-live-p (get-buffer "*gud-main*"))
        (select-window (get-buffer-window "*gud-main*"))
        (goto-char (point-max))
        (recenter-top-bottom)
        (if (> num 1) (recenter-top-bottom))
        (select-window old-window))
       (t (error "GUD buffer doesn't exist!"))))))

(with-eval-after-load 'gud
  (add-hook 'gud-mode-hook
            '(lambda ()
               (bind-keys ("C-x C-a C-b") ("C-x a b" . gud-break)
                          ("C-x C-a C-t") ("C-x a t" . gud-tbreak)
                          ("C-x C-a C-d") ("C-x a d" . gud-remove)
                          ("C-x C-a C-s") ("C-x a s" . gud-step)
                          ("C-x C-a C-i") ("C-x a i" . gud-stepi)
                          ("C-x C-a C-n") ("C-x a n" . gud-next)
                          ("C-x C-a C-r") ("C-x a c" . gud-cont)
                          ("C-x C-a C-f") ("C-x a f" . gud-finish)
                          ("C-x C-a C-j") ("C-x a j" . gud-jump)
                          ("C-x C-a <") ("C-x a ," . gud-up)
                          ("C-x C-a >") ("C-x a ." . gud-down)
                          ("C-x C-a p") ("C-x a p" . gud-print)
                          ("C-x C-a C-u") ("C-x a u" . gud-until)
                          ("C-x C-a C-v") ("C-x a v" . gud-pv))
               (company-mode 1)
               (local-set-key (kbd "<tab>") 'indent-for-tab-command)
               (setq comint-prompt-read-only t)
               (setq gdb-many-windows nil)))

  (defun gud-gdb-completions (context command)
    (when (and  (string-match "\\(::\\|->\\|[*.a-zA-Z]\\)$" command)
               (save-excursion (forward-line 0)
                               (looking-at comint-prompt-regexp)))
      (let* ((start (- (point) (field-beginning)))
             (complete-list
              (gud-gdb-run-command-fetch-lines (concat "complete " context command)
                                               (current-buffer)
                                               ;; From string-match above.
                                               (length context))))
        ;; Protect against old versions of GDB.
        (when (or complete-list
                  (string-match "\\*\\*\\*.*?\\*\\*\\*" (car complete-list)))
          (setq complete-list  (cdr complete-list)))
        (and complete-list
            (string-match "^Undefined command: \"complete\"" (car complete-list))
            (error "This version of GDB doesn't support the `complete' command"))
        (gud-gdb-completions-1 complete-list))))

  (defvar gud-pdb-fetch-lines-in-progress nil)
  (defvar gud-pdb-fetch-lines-string nil)
  (defvar gud-pdb-fetched-lines nil)

  (defun gud-pdb-fetch-lines-filter (string)
    (setq string (concat gud-pdb-fetch-lines-string string))
    (while (string-match "\n" string)
      (push (substring string 0 (match-beginning 0))
            gud-pdb-fetched-lines)
      (setq string (substring string (match-end 0))))
    (if (string-match comint-prompt-regexp string)
        (progn
          (setq gud-pdb-fetch-lines-in-progress nil)
          string)
      (progn
        (setq gud-pdb-fetch-lines-string string)
        "")))

  (setq gud-pdb-command-name "pdb-setup")

  (defun gud-pdb-completions (context command)
    (when (and  (string-match "\\([*.a-zA-Z0-9_]\\)$" command)
               (save-excursion (forward-line 0)
                               (looking-at comint-prompt-regexp)))
      (let ((gud-pdb-fetch-lines-in-progress t)
            (gud-pdb-fetch-lines-string nil)
            (gud-pdb-fetched-lines nil)
            (gud-marker-filter #'gud-pdb-fetch-lines-filter)
            (proc (get-buffer-process gud-comint-buffer))
            (inhibit-quit t))
        (with-local-quit
          (comint-send-string proc (concat "complete " context command "\n"))
          (while gud-pdb-fetch-lines-in-progress
            (accept-process-output (get-buffer-process gud-comint-buffer))))
        gud-pdb-fetched-lines)))

  (defun my-pdb-setup (commadn-line)
    (setq gud-gdb-completion-function 'gud-pdb-completions)
    (add-to-list 'completion-at-point-functions
                 'gud-gdb-completion-at-point))
  (advice-add 'pdb :after #'my-pdb-setup))

(with-eval-after-load 'gdb-mi
  (defun gud-gdbmi-completions (context command)
    (when (and  (string-match "\\(::\\|->\\|[*.a-zA-Z0-9_]\\)$" command)
               (save-excursion (forward-line 0)
                               (looking-at gdb-prompt-name)))
      (let ((gud-gdb-fetch-lines-in-progress t)
            (gud-gdb-fetch-lines-string nil)
            (gud-gdb-fetch-lines-break (length context))
            (gud-gdb-fetched-lines nil)
            ;; This filter dumps output lines to `gud-gdb-fetched-lines'.
            (gud-marker-filter #'gud-gdbmi-fetch-lines-filter)
            (inhibit-quit t))
        (with-local-quit
          (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
            (gdb-input (concat "complete " context command)
                       (lambda () (setq gud-gdb-fetch-lines-in-progress nil)))
            (while gud-gdb-fetch-lines-in-progress
              (accept-process-output (get-buffer-process gud-comint-buffer)))))
        (when (or gud-gdb-fetched-lines
                  (string-match "\\*\\*\\*.*?\\*\\*\\*" (car gud-gdb-fetched-lines)))
          (setq gud-gdb-fetched-lines  (cdr gud-gdb-fetched-lines)))
        (gud-gdb-completions-1 gud-gdb-fetched-lines))))
  ;; Force gdb-mi to not dedicate any windows
  (defadvice gdb-display-buffer
      (after undedicate-gdb-display-buffer activate)
    (set-window-dedicated-p ad-return-value nil))
  (defadvice gdb-set-window-buffer
      (after undedicate-gdb-set-window-buffer activate)
    (set-window-dedicated-p window nil)))

(global-set-key [f5] 'gdb)

(provide 'init-gud)