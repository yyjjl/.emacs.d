(defvar core--shackle-popup-window-list nil)
(defvar core--shackle-popup-buffer-regexp nil)

(defvar core--shackle-comint-modes
  '(term-mode
    haskell-interactive-mode))

(defvar core--shackle-help-modes
  '(help-mode
    completion-list-mode
    messages-buffer-mode
    profiler-report-mode))

(defvar core--shackle-help-mode-regexp
  (eval-when-compile
    (concat "^"
            (regexp-opt '("*Compile-Log*"
                          "*sdcv*"
                          "*lispy-message*"
                          "*compilation*"))
            "$")))

(defvar-local core--shackle-popup-window nil)
(put 'core--shackle-popup-window 'permanent-local t)

(defun core/last-popup-window ()
  "Display last popup window"
  (interactive)
  (if (buffer-live-p shackle-last-buffer)
      (display-buffer shackle-last-buffer)
    (message "Last buffer killed !!")
    (core/display-buffer)))

(defun core/display-buffer ()
  (interactive)
  (let ((ivy-use-virtual-buffers nil))
    (display-buffer
     (completing-read "Buffer:"
                      #'internal-complete-buffer
                      nil
                      :require-match))))

(defun core/fix-popup-window ()
  "Make a popup window not to close when `C-g' pressed"
  (interactive)
  (let ((window (selected-window)))
    (setq core--shackle-popup-window-list
          (--filter (not (equal (car it) window))
                    core--shackle-popup-window-list))))

(defun core/popup-sdcv ()
  "Display *sdcv* buffer"
  (interactive)
  (let ((word (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (sdcv-current-word))))
    (sdcv-goto-sdcv)
    (setq word (read-string
                (format "Word (default %s): " word)
                nil nil word))
    (sdcv-search-word word)))

(defun core%clean-window-list ()
  ;; Remove inactive window
  (setq core--shackle-popup-window-list
        (loop for (window . buffer) in core--shackle-popup-window-list
              if (and (window-live-p window)
                      (equal (window-buffer window) buffer))
              collect (cons window buffer))))

(defun core*close-popup-window (&rest _)
  "When `C-g' pressed, close latest opened popup window"
  (core%clean-window-list)
  ;; `C-g' can deactivate region
  (when (and (called-interactively-p 'interactive)
             (not (region-active-p)))
    (let (window buffer)
      (if (one-window-p)
          (progn
            (setq window (selected-window))
            (when (equal (buffer-local-value 'core--shackle-popup-window
                                             (window-buffer window))
                         window)))
        (setq window (caar core--shackle-popup-window-list))
        (setq buffer (cdar core--shackle-popup-window-list))
        (when (and (window-live-p window)
                   (equal (window-buffer window) buffer))
          (delete-window window)

          (pop core--shackle-popup-window-list))))))

(defun core*shackle-display-buffer-hack ($fn $buffer $alist $plist)
  (core%clean-window-list)

  ;; Set default size
  (setq shackle-default-alignment
        (if (> (frame-width)
               (or split-width-threshold 120))
            'right
          'below))

  (let* ((autoclose? (plist-get $plist :autoclose))
         (align (plist-get $plist :align))
         (root (car (window-tree)))
         ;; If root window is horizontally splitted
         (num (if (or (atom root) (car root))
                  2
                (length (cdr root))))
         (old-window (get-buffer-window $buffer))
         window)
    ;; When window is an autoclose window and `:align' is not set,
    ;; set it to `shackle-default-alignment'
    (when (and (not align) autoclose?)
      (setq align shackle-default-alignment)
      (setq $plist (append $plist `(:align ,align))))
    (setq shackle-default-size
          (min (if (memq align '(left right)) 0.5 0.4)
               (/ 1.0 num)))

    (setq window (funcall $fn $buffer $alist $plist))
    (unless (eq window old-window)
      (when autoclose?
        ;; Autoclose window should be dedicated
        (set-window-dedicated-p window t)
        ;; Add to autoclose list
        (push (cons window $buffer) core--shackle-popup-window-list))
      (with-current-buffer $buffer
        ;; Record popup window
        (setq core--shackle-popup-window window)))
    window))

(defun core--shackle%comint-mode-matcher ($buffer)
  (let ((case-fold-search t)
        (buffer-name (buffer-name $buffer))
        (mode (buffer-local-value 'major-mode $buffer)))
    (or (derived-mode? 'comint-mode $buffer)
        (memq mode core--shackle-comint-modes)
        (string-match-p "^\\*.*repl.*\\*$" buffer-name))))

(defun core--shackle%help-mode-matcher ($buffer)
  (let ((case-fold-search t)
        (buffer-name (buffer-name $buffer))
        (mode (buffer-local-value 'major-mode $buffer)))
    (or (memq mode core--shackle-help-modes)
        (string-match-p core--shackle-help-mode-regexp buffer-name))))

(with-eval-after-load 'shackle
  (defvar shackle-mode-map (make-sparse-keymap))
  (define-key! :map shackle-mode-map
    ("l" . core/last-popup-window)
    ("d" . core/popup-sdcv)
    ("b" . core/display-buffer)
    ("RET" . core/fix-popup-window))

  (global-set-key (kbd "C-z") shackle-mode-map)
  (global-set-key (kbd "C-x m") #'view-echo-area-messages)

  ;; Can not be advised `:after', keyboard-quit signal `quit'
  (advice-add 'keyboard-quit :before #'core*close-popup-window)
  (advice-add 'other-window :before #'core*close-popup-window)

  (advice-add 'shackle-display-buffer
              :around #'core*shackle-display-buffer-hack)

  (define-hook! core%autoclose-popup-window (kill-buffer-hook)
    "Auto quit popup window after buffer killed"
    (let ((window (get-buffer-window))
          (buffer-name (buffer-name)))
      (when (and window
                 (equal window core--shackle-popup-window)
                 (not (one-window-p))
                 (not (minibuffer-window-active-p window))
                 (or (not (boundp 'lv-wnd))
                     (not (eq (next-window) lv-wnd))))
        (quit-window nil window))))

  (setq core--shackle-popup-buffer-regexp
        (eval-when-compile
          (concat "^\\(?:"
                  (string-join `("\\*Man.*\\*"
                                 "\\*TeX.*\\*"
                                 "\\*Flycheck.*\\*")
                               "\\|")
                  "\\)$")))

  (setq shackle-default-alignment 'below
        shackle-default-size 0.5
        shackle-default-rule nil
        shackle-rules
        `(((:custom core--shackle%comint-mode-matcher)
           :size 0.4 :align below :select t)
          ((:custom core--shackle%help-mode-matcher)
           :align below :select t :autoclose t)
          (,core--shackle-popup-buffer-regexp
           :regexp t :select t :autoclose t)
          ("^ ?\\*.*\\*\\(?:<[0-9]+>\\)?$" :regexp t))))

(provide 'core-popups)
