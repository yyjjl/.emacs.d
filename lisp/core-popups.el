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
  (setq core--shackle-popup-window-list
        (remove (selected-window)
                core--shackle-popup-window-list)))

(defun core/popup-messages ()
  "Display *Messages* buffer"
  (interactive)
  (display-buffer (get-buffer-create "*Messages*")))

(defun core/popup-messages ()
  "Display *Messages* buffer"
  (interactive)
  (display-buffer (get-buffer-create "*Messages*")))

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

(with-eval-after-load 'shackle
  (defvar shackle-mode-map (make-sparse-keymap))
  (define-key! :map shackle-mode-map
    ("l" . core/last-popup-window)
    ("d" . core/popup-sdcv)
    ("b" . core/display-buffer)
    ("RET" . core/fix-popup-window))

  (global-set-key (kbd "C-z") shackle-mode-map)
  (global-set-key (kbd "C-x m") #'view-echo-area-messages)

  (defun core%shackle-align ()
    "Set default align according to frame size and window number."
    (if (> (frame-width) split-width-threshold) 'right 'below))

  (defvar core--shackle-popup-window-list nil)
  (defvar-local core--shackle-popup-window nil)
  (defvar-local core--shackle-autokill? nil)
  (put 'core--shackle-autokill? 'permanent-local t)
  (put 'core--shackle-popup-window 'permanent-local t)
  (define-hook! core%autoclose-popup-window (kill-buffer-hook)
    "Auto close popup window after buffer killed"
    (when core--shackle-popup-window
      (let ((win (get-buffer-window (current-buffer))))
        (when (and (equal win core--shackle-popup-window)
                   (window-live-p win)
                   (not (one-window-p))
                   (not (minibuffer-window-active-p win)))
          (delete-window win)))))

  (defun core%clean-window-list (&optional autokill?)
    ;; Remove inactive window
    (setq core--shackle-popup-window-list
          (loop for (window . buffer) in core--shackle-popup-window-list
                if (and (window-live-p window)
                        (equal (window-buffer window) buffer))
                collect (cons window buffer)
                else if (and autokill? (buffer-live-p buffer))
                do (kill-buffer buffer))))

  (defun core*close-popup-window (&rest _)
    "When `C-g' pressed, close latest opened popup window"
    (core%clean-window-list)
    (let ((quit? (called-interactively-p 'interactive))
          (window (caar core--shackle-popup-window-list))
          (buffer (cdar core--shackle-popup-window-list)))
      ;; `C-g' can deactivate region
      (when (and (not (region-active-p))
                 (window-live-p window)
                 (equal (window-buffer window) buffer)
                 (not (one-window-p)))
        (delete-window window)))
    (core%clean-window-list :autokill))

  ;; Can not be advised `:after', keyboard-quit signal `quit'
  (advice-add 'keyboard-quit :before #'core*close-popup-window)
  (advice-add 'other-window :before #'core*close-popup-window)

  (defun core*shackle-display-buffer-hack ($fn $buffer $alist $plist)
    ;; Set default size
    (let* ((root (car (window-tree)))
           ;; If root window is horizontally splitted
           (num (if (or (atom root) (car root))
                    2
                  (length (cdr root))))
           (align (plist-get $plist :align)))
      (unless align
        (setq align (core%shackle-align)
              $plist (plist-put $plist :align align)))
      (setq shackle-default-size
            (min (if (memq align '(left right)) 0.5 0.4)
                 (/ 1.0 num))))

    (let ((window (funcall $fn $buffer $alist $plist))
          (autoclose? (plist-get $plist :autoclose))
          (autokill? (plist-get $plist :autokill)))
      (when autoclose?
        ;; Add to autoclose list
        (push (cons window $buffer) core--shackle-popup-window-list))
      (with-current-buffer $buffer
        ;; Record popup window
        (when (and autoclose? autokill?)
          (setq core--shackle-autokill? t))
        (setq core--shackle-popup-window window))
      (set-window-dedicated-p window t)
      window))

  (advice-add 'shackle-display-buffer
              :around #'core*shackle-display-buffer-hack)

  (setq shackle-default-alignment 'below
        shackle-default-size 0.5
        shackle-default-rule nil
        shackle-rules
        '(("*sdcv*" :align below :size 15 :select t :autoclose t)
          ((:custom
            (lambda (buffer)
              (or (derived-mode? 'comint-mode buffer)
                  (memq (buffer-local-value 'major-mode buffer)
                        '(term-mode haskell-interactive-mode))
                  (let ((case-fold-search t))
                    (string-match-p "^\\*.*repl.*\\*$"
                                    (buffer-name buffer))))))
           :size 0.4 :align below :select t)
          (help-mode :align below :select t :autoclose t)
          (messages-buffer-mode :select t :align below :autoclose t)
          (ivy-occur-grep-mode :select t)
          (ivy-occur-mode :select t)
          (grep-mode :select t)
          (occur-mode :select t)
          ;; Man-mode don't work !!?
          ("^\\*Man.*\\*$"
           :regexp t :size 0.5 :select t)
          (Info-mode :size 0.5 :select t)
          (calendar-mode :select t :autoclose t :autokill t)
          ("^\\*.*?\\*" :regexp t :noselect t :autoclose t :autokill t)))

  ;; Do not split in popup-buffer
  (defun display-buffer-pop-up-window (buffer alist)
    (let ((frame (or (window--frame-usable-p (selected-frame))
                     (window--frame-usable-p (last-nonminibuffer-frame))))
          window)
      (when (and (or (not (frame-parameter frame 'unsplittable))
                     ;; If the selected frame cannot be split, look at
                     ;; `last-nonminibuffer-frame'.
                     (and (eq frame (selected-frame))
                          (setq frame (last-nonminibuffer-frame))
                          (window--frame-usable-p frame)
                          (not (frame-parameter frame 'unsplittable))))
                 ;; Attempt to split largest or least recently used window.
                 (setq window (or (let ((win (get-largest-window frame t)))
                                    (unless (assoc win core--shackle-popup-window-list)
                                      (window--try-to-split-window win alist)))
                                  (let ((win (get-lru-window frame t)))
                                    (unless (assoc win core--shackle-popup-window-list)
                                      (window--try-to-split-window win alist))))))
        (prog1 (window--display-buffer
                buffer window 'window alist display-buffer-mark-dedicated)
          (unless (cdr (assq 'inhibit-switch-frame alist))
            (window--maybe-raise-frame (window-frame window))))))))

(provide 'core-popups)
