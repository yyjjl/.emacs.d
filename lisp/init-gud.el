(require-packages! gud gdb-mi)

(defvar gdb-source-window nil)
(defvar gdb--side-window nil)
(defvar gdb-side-window-height 15)
(defvar gud--window-configuration nil)
(defvar-local gud--source-buffer-status nil)
(defvar-local gud--source-buffer-list nil)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'breakpoint [0 255 255 255 255 255 255 0]))

(defun gud*aorund-gdb-put-string (_ -pos &rest __)
  (let ((ovs (--filter (overlay-get it 'put-break)
                       (overlays-in -pos -pos))))
    (when (= 1 (length ovs))
      (save-excursion
        (goto-char -pos)
        (move-overlay (car ovs)
                      (line-beginning-position)
                      (line-beginning-position 2))
        (overlay-put (car ovs)
                     'face '(:box (:line-width 1 :color "red" :style line)))))))

(defun gdb*display-buffer (-buffer)
  (let ((old-window (selected-window)))
    (unless (window-live-p gdb--side-window)
      (when-let ((comint-buffer gud-comint-buffer)
                 (window (get-buffer-window comint-buffer)))
        (with-selected-window window
          (setq gdb--side-window
                (split-window-vertically (- gdb-side-window-height))))))
    (if (window-live-p gdb--side-window)
        (progn
          (set-window-dedicated-p gdb--side-window nil)
          (set-window-buffer gdb--side-window -buffer)
          (set-window-dedicated-p gdb--side-window t)
          ;; (unless (eq (buffer-local-value 'gdb-buffer-type -buffer)
          ;;             'gdb-inferior-io)
          ;;   (select-window gdb--side-window))
          )
      (with-current-buffer -buffer
        (error "Can not display buffer %s" gdb-buffer-type)))))

;; Fix a bug when call commands from source buffer
(defun gud*display-line (-true-file -line)
  (let* ((last-nonmenu-event t)      ; Prevent use of dialog box for questions.
         (buffer (with-current-buffer gud-comint-buffer
                   (gud-find-file -true-file)))
         (window (and buffer
                      (or (get-buffer-window buffer)
                          (when (window-live-p gdb-source-window)
                            (window--display-buffer buffer gdb-source-window 'reuse))
                          (display-buffer buffer 'display-buffer-same-window))))
         (pos))
    (when buffer
      (with-current-buffer buffer
        (unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
          (if (yes-or-no-p
               (format "File %s changed on disk.  Reread from disk? "
                       (buffer-name)))
              (revert-buffer t t)
            (setq gud-keep-buffer t)))
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- -line))
          (setq pos (point))
          (or gud-overlay-arrow-position
              (setq gud-overlay-arrow-position (make-marker)))
          (set-marker gud-overlay-arrow-position (point) (current-buffer))
          ;; ====================
          (when (window-live-p window)
            (with-selected-window window
              (when (save-excursion
                      (goto-char gud-overlay-arrow-position)
                      (swiper--ensure-visible)
                      (forward-line 2)
                      (not (pos-visible-in-window-p)))
                (recenter))))
          ;; ====================
          ;; If they turned on hl-line, move the hl-line highlight to
          ;; the arrow's line.
          (when (featurep 'hl-line)
            (cond
             (global-hl-line-mode
              (global-hl-line-highlight))
             ((and hl-line-mode hl-line-sticky-flag)
              (hl-line-highlight)))))
        (cond ((or (< pos (point-min)) (> pos (point-max)))
               (widen)
               (goto-char pos))))
      (when window
        (set-window-point window gud-overlay-arrow-position)
        ;; Always set gdb-source-window
        (setq gdb-source-window window)))))

(defun gud/pop-to-source-buffer ()
  (interactive)
  (when-let ((file (or (car gud-last-last-frame)
                       gdb-main-file)))
    (pop-to-buffer (gud-find-file file))))

(defun gud/pop-to-comint-buffer ()
  (interactive)
  (pop-to-buffer gud-comint-buffer))

(defun gud/side-window ()
  (interactive)
  (if (window-live-p gdb--side-window)
      (select-window gdb--side-window)
    (gdb-display-io-buffer)))

(defun gud/quit-process ()
  (interactive)
  (when (and (yes-or-no-p "Quit process"))
    (gud-call "quit")))

(defun gud/toggle-breakpoint (-arg)
  (interactive "P")
  (if-let* ((ov (car (--filter (overlay-get it 'put-break)
                               (overlays-in (line-beginning-position)
                                            (line-beginning-position 2)))))
            (string (overlay-get ov 'before-string))
            (bptno (get-text-property 0 'gdb-bptno string)))
      (let* ((enabled (get-text-property 0 'gdb-enabled string))
             (cmd (concat (if enabled "disable" "enable") (format " %s" bptno))))
        (message "Command: %s" cmd)
        (gud-call cmd -arg))
    (message "Can not read info about breakpoint.")))

(defvar gud--source-mode-map
  (define-key! :map (make-sparse-keymap)
    ("o" . gdb-display-io-buffer)
    ("g" . gud/pop-to-comint-buffer)
    ("B" . gdb-display-breakpoints-buffer)
    ("T" . gdb-display-threads-buffer)
    ("R" . gdb-display-registers-buffer)
    ("S" . gdb-display-stack-buffer)
    ("A" . gdb-display-disassembly-buffer)
    ("m" . gdb-display-memory-buffer)
    ("b" . gud-break)
    ("D" . gud-remove)
    ("-" . gud/toggle-breakpoint)
    ("=" . gud/toggle-breakpoint)
    ("t" . gud-tbreak)
    ("s" . gud-step)
    ("n" . gud-next)
    ("i n" . gud-nexti)
    ("i s" . gud-stepi)
    ("j" . gud-jump)
    ("f" . gud-finish)
    ("<" . gud-up)
    (">" . gud-down)
    ("c" . gud-cont)
    ("u" . gud-until)
    ("r" . gud-run)
    ("C" . gud-call)
    ("p" . gud-print)
    ("d" . gud-display)
    ("M" . gud-display-all)
    ("*" . gud-pstar)
    ("w" . gud-watch)
    ("X" . gud/quit-process)
    ("C-c C-z" . gud/side-window)))

(define-minor-mode gud-source-mode "minor mode for source file"
  :init-value nil
  :lighter "GS"
  :keymap gud--source-mode-map
  (if gud-source-mode
      (progn
        (setq gud--source-buffer-status (list buffer-read-only))
        (add-to-list 'mode-line-misc-info
                     '(gud-source-mode " Debugging"))
        (setq buffer-read-only t))
    (when gud--source-buffer-status
      (remove-hook 'mode-line-misc-info
                   '(gud-source-mode " Debugging"))
      (setq buffer-read-only (car gud--source-buffer-status))))
  (force-mode-line-update))

(defun gud*around-find-file (-fn -file)
  (let ((buffer (funcall -fn -file)))
    (when (and buffer gud-comint-buffer)
      (with-current-buffer gud-comint-buffer
        (add-to-list 'gud--source-buffer-list buffer))
      (with-current-buffer buffer
        (when (not gud-source-mode)
          (gud-source-mode 1)))
      buffer)))

(defun gud*before-comint-init (&rest _)
  (setq gud--window-configuration (current-window-configuration))
  (delete-other-windows))

(defun gud*after-sentinel (-proc -msg)
  (let ((buffer (process-buffer -proc)))
    (when (and (buffer-live-p buffer)
               (memq (process-status -proc) '(signal exit)))
      (dolist (src-buf (buffer-local-value 'gud--source-buffer-list buffer))
        (when (buffer-live-p src-buf)
          (with-current-buffer src-buf
            (when gud-source-mode
              (gud-source-mode -1)))))
      (kill-buffer buffer)
      (gud-tooltip-mode -1)
      (when (window-configuration-p gud--window-configuration)
        (set-window-configuration gud--window-configuration)
        (setq gud--window-configuration nil)))))

(with-eval-after-load 'gud
  (define-key gud-mode-map "`" gud--source-mode-map)
  (define-key gud-mode-map (kbd "C-c C-z") #'gud/pop-to-source-buffer)

  (add-hook 'pdb-mode-hook
            (lambda ()
              (gud-def gud-display "display %e" " " "Display expression")
              (gud-def gud-display-all "display" "\C- " "Display all")
              (gud-def gud-python-until "until  %l" "\C-u"
                       "Continue to current line or address.")
              (define-key gud--source-mode-map "u" 'gud-python-until)))

  (define-hook! gud|setup-hook (gud-mode-hook)
    (define-key gud--source-mode-map "u" 'gud-until)

    (gud-tooltip-mode 1)
    (set-window-dedicated-p (get-buffer-window gud-comint-buffer) t)
    ;; `gud-print' need prompt can be modified
    ;; (setq comint-prompt-read-only t)
    (setq-local comint-scroll-to-bottom-on-output t))

  (advice-add 'gud-find-file :around #'gud*around-find-file)
  (advice-add 'gud-sentinel :after #'gud*after-sentinel)
  (advice-add 'gud-common-init :before #'gud*before-comint-init)
  (advice-add 'gud-display-line :override #'gud*display-line))

(with-eval-after-load 'gdb-mi
  (advice-add 'gdb-display-buffer :override 'gdb*display-buffer)
  (advice-add #'gdb-put-string :after #'gud*aorund-gdb-put-string)

  (dolist (map (list gdb-inferior-io-mode-map
                     gdb-breakpoints-mode-map
                     gdb-frames-mode-map
                     gdb-memory-mode-map
                     gdb-registers-mode-map
                     gdb-disassembly-mode-map
                     gdb-locals-mode-map
                     gdb-script-mode-map
                     gdb-threads-mode-map))
    (define-key map "`" gud--source-mode-map)
    (define-key map (kbd "C-c C-z") #'gud/pop-to-source-buffer))
  (setq gdb-show-main t))

(provide 'init-gud)
