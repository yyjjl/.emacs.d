;; `eshell' setup
(defun term|eshell-autoclose ()
  (let ((window (get-buffer-window)))
    (when (and (window-live-p window)
               (> (length (window-list)) 1))
      (delete-window window))))

(defun term|local-eshell (&optional dir)
  (unless dir
    (setq dir default-directory))
  (let ((buf (or (term|last-buffer (buffer-list) 'eshell-mode dir)
                 (get-buffer-create (term|get-buffer-name "*eshell-%d*")))))
    (with-current-buffer buf
      (cd dir)
      (unless (eq 'eshell-mode major-mode)
        (eshell-mode)
        (when (= 1 (length (window-list)))
          (add-hook 'kill-buffer-hook #'term|eshell-autoclose nil :local))))
    buf))

(defun term|eshell-ctrl-d (&optional args)
  (interactive "p")
  (if (and (eolp)
           (save-excursion
             (goto-char (line-beginning-position))
             (looking-at-p eshell-prompt-regexp))
           (save-excursion
             (goto-char (or eshell-last-output-end (point-max)))
             (re-search-forward "\\s-+" (point-max) t)
             (equal (point) (point-max))))
      (kill-buffer)
    (delete-char args)))

(defun term|eshell-after-process-quit (proc msg)
  (let ((buf (and proc (process-buffer proc))))
    (when (buffer-live-p buf)
      (setq buffer-read-only t)
      (local-set-key (kbd "q") 'kill-this-buffer))))
(advice-add 'eshell-term-sentinel :after #'term|eshell-after-process-quit)

(defun eshell/ccat (filename)
  "Like cat(1) but with syntax highlighting."
  (let* ((recentf-enabled-p nil)
         (existing-buffer (get-file-buffer filename))
         (buffer (find-file-noselect filename)))
    (eshell-print (with-current-buffer buffer
                    (if (fboundp 'font-lock-ensure)
                        (font-lock-ensure)
                      (with-no-warnings
                        (font-lock-fontify-buffer)))
                    (buffer-string)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

(defun eshell/vv (&rest args)
  (let ((args (eshell-flatten-list (eshell-stringify-list args))))
    (condition-case err
        (save-current-buffer
          (switch-to-buffer (term|exec-program (car args) (cdr args)))
          (local-set-key (kbd "C-c C-z") #'term|switch-back))
      (error (progn (message "Error: %s" err) nil)))))

(defun eshell/j (&optional arg)
  (let ((mm (make-hash-table :test #'equal))
        candidates)
    (dolist (dir (ring-elements eshell-last-dir-ring))
      (if (gethash dir mm)
          (puthash dir (1+ (gethash dir mm)) mm)
        (puthash dir 1 mm)))
    (setq candidates (sort (hash-table-keys mm) (lambda (a b)
                                                  (> (gethash a mm)
                                                     (gethash b mm)))))
    (if (and (integerp arg) (> arg 0))
        (eshell-lisp-command
         (mapconcat (lambda (s) (format "%4d %s" (gethash s mm) s))
                    (let ((nth (nthcdr (1- arg) candidates)))
                      (when nth (setcdr nth nil))
                      candidates)
                    "\n"))
      (ivy-read "Directory history: " candidates :initial-input
                (if (stringp arg) arg "")))))

(defhook term|eshell-setup (eshell-mode-hook)
  (setq xterm-color-preserve-properties t)
  ;; `eshell-mode-map' is a local variable
  (define-key eshell-mode-map (kbd "C-d") #'term|eshell-ctrl-d)
  (define-key eshell-mode-map (kbd "<tab>") #'completion-at-point)
  (define-key eshell-mode-map (kbd "C-c i e") #'counsel-esh-history))

(with-no-warnings
  (setq eshell-last-dir-ring-size 512)
  (setq eshell-destroy-buffer-when-process-dies nil)
  (setq eshell-escape-control-x nil))

(with-eval-after-load 'esh-mode
  (add-to-list 'eshell-preoutput-filter-functions
               'xterm-color-filter)
  (setq eshell-output-filter-functions
        (delete 'eshell-handle-ansi-color eshell-output-filter-functions)))

(with-eval-after-load 'em-term
  (defvar term|eshell-extra-visual-commands
    '("ssh" "ipython" "ipython3" "python" "python3" "python2" "root"))
  (dolist (cmd term|eshell-extra-visual-commands)
    (add-to-list 'eshell-visual-commands cmd)))

(with-eval-after-load 'esh-opt
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (when window-system
    (setq eshell-highlight-prompt t
          eshell-prompt-function 'epe-theme-lambda)))



(provide 'init-eshell)
