(defvar gud|current-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defun gud|highlight-current-line (true-file line)
  "Highlight current line."
  (let* ((ov gud|current-overlay)
         (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    (current-buffer)))))
(advice-add 'gud-display-line :after #'gud|highlight-current-line)

(defun gud|kill-overlay ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud|current-overlay)))

(add-hook 'gdb-mode-hook
          #'(lambda ()
              (add-hook 'kill-buffer-hook #'gud|kill-overlay)))

;; Move the cursor to the end of last line if it's gud-mode {{
(defun gud|move-to-end (&rest args)
  (when (eq major-mode 'gud-mode)
    (goto-char (point-max))))

(advice-add 'switch-to-buffer :after #'gud|move-to-end)
(advice-add 'select-window-by-number :after #'gud|move-to-end)
(advice-add 'windmove-do-window-select :after #'gud|move-to-end)
;; }}

(with-eval-after-load 'gud
  (defvar gud|last-cmd nil)
  (defun gud|run-cmd (&optional cmd)
    (if cmd
        (progn (call-interactively cmd)
               (setq gud|last-cmd cmd))
      (if gud|last-cmd
          (call-interactively gud|last-cmd)
        (message "No last gud cmd !!!"))))
  (defhydra hydra|gud-cmd (:exit nil :color pink
                                 :body-pre (setq hydra-lv nil)
                                 :post (setq hydra-lv t))
    "GDB"
    ("=" (gud|run-cmd 'gud-break))
    ("-" (gud|run-cmd 'gud-remove))
    ("t" (gud|run-cmd 'gud-tbreak))
    ("s" (gud|run-cmd 'gud-step))
    ("n" (gud|run-cmd 'gud-next))
    ("i" (gud|run-cmd 'gud-stepi))
    ("j" (gud|run-cmd 'gud-jump))
    ("f" (gud|run-cmd 'gud-finish))
    ("<" (gud|run-cmd 'gud-up))
    (">" (gud|run-cmd 'gud-down))
    ("c" (gud|run-cmd 'gud-cont))
    ("u" (gud|run-cmd 'gud-until))
    ("p" (gud|run-cmd 'gud-print))
    ("w" (gud|run-cmd 'gud-watch))
    ("RET" (gud|run-cmd) "last")
    ("q" nil "quit"))
  (add-hook 'gud-mode-hook
            '(lambda ()
               (define-key global-map (kbd  "C-x a") 'hydra|gud-cmd/body)
               (setq comint-prompt-read-only t)
               (setq gdb-many-windows t))))

(provide 'init-gud)
