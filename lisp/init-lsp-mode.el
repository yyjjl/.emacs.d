;; -*- lexical-binding:t -*-
(require-packages!
 lsp-mode
 company-lsp)

(require 'flycheck)



(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))

(defun lsp-flycheck--start (checker callback)
  "Start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  ;; Turn all errors from lsp--diagnostics for the current buffer into
  ;; flycheck-error objects and pass them immediately to the callback
  (let ((errors))
    (dolist (diag (or (gethash buffer-file-name lsp--diagnostics)
                      (gethash (file-truename buffer-file-name) lsp--diagnostics)))
      (push (flycheck-error-new
             :buffer (current-buffer)
             :checker checker
             :filename buffer-file-name
             :line (1+ (lsp-diagnostic-line diag))
             :column (1+ (lsp-diagnostic-column diag))
             :message (lsp-diagnostic-message diag)
             :level (pcase (lsp-diagnostic-severity diag)
                      (1 'error)
                      (2 'warning)
                      (_ 'info))
             :id (lsp-diagnostic-code diag))
            errors))
    (funcall callback 'finished errors)))

(flycheck-define-generic-checker 'lsp
  "A syntax checker using the Language Server Protocol (RLS)
provided by lsp-mode.

See https://github.com/emacs-lsp/lsp-mode."
  :start #'lsp-flycheck--start
  :modes '(python-mode c++-mode c-mode rust-mode) ; Need a default mode
  :predicate (lambda () lsp-mode)
  :error-explainer (lambda (e) (flycheck-error-message e)))

(defvar lsp--code-action-overlay nil)
(defun lsp//move-code-action-overlay (-beg -end &optional -buffer)
  (unless (overlayp lsp--code-action-overlay)
    (setq lsp--code-action-overlay (make-overlay -beg -end -buffer))
    (overlay-put lsp--code-action-overlay
                 'face '(:underline (:style wave :color "blue")))
    (overlay-put lsp--code-action-overlay
                 'modification-hooks
                 '((lambda (ov after-change &rest args)
                     (when after-change
                       (delete-overlay ov))))))
  (move-overlay lsp--code-action-overlay -beg -end -buffer))

(defun lsp*after-set-code-action-params (-buffer -actions -params)
  (when (buffer-live-p -buffer)
    (with-current-buffer -buffer
      (when (equal -params (lsp--text-document-code-action-params))
        (when lsp--code-action-overlay
          (delete-overlay lsp--code-action-overlay))
        (setq lsp-code-actions -actions)
        (setq lsp-code-action-params -params)
        (when -actions
          (when-let (bounds (bounds-of-thing-at-point 'symbol))
            (lsp//move-code-action-overlay (car bounds) (cdr bounds) -buffer)))))))

(defun lsp*xref-make-match-item (-filename -location)
  (let* ((range (gethash "range" -location))
         (pos-start (gethash "start" range))
         (pos-end (gethash "end" range))
         (line (lsp--extract-line-from-buffer pos-start))
         (start (gethash "character" pos-start))
         (end (gethash "character" pos-end))
         (len (length line)))
    (add-face-text-property (max (min start len) 0)
                            (max (min end len) 0)
                            'highlight t line)
    ;; LINE is nil when FILENAME is not being current visited by any buffer.
    (xref-make-match (or line -filename)
                     (xref-make-file-location -filename
                                              (1+ (gethash "line" pos-start))
                                              (gethash "character" pos-start))
                     (let ((length (- end start)))
                       (and (> length 0) (< length len) length)))))

(defun lsp*make-sentinel (-workspace)
  (cl-check-type -workspace lsp--workspace)
  (lambda (process exit-str)
    (let ((status (process-status process)))
      (when (memq status '(exit signal))
        ;; Server has exited.  Uninitialize all buffer-local state for this
        ;; workspace.
        (message "%s: %s has exited (%s)"
                 (lsp--workspace-root -workspace)
                 (process-name (lsp--workspace-proc -workspace))
                 (string-trim-right exit-str))
        (dolist (buf (lsp--workspace-buffers -workspace))
          (with-current-buffer buf
            (lsp--uninitialize-workspace)))
        ;; Kill standard error buffer only if the process exited normally.
        ;; Leave it intact otherwise for debugging purposes.
        (when (and (memq status '(exit signal))
                   (member (process-exit-status process) '(0 9)))
          ;; FIXME: The client structure should store the standard error
          ;; buffer, not its name.
          ;; FIXME: Probably the standard error buffer should be per -workspace,
          ;; not per client.
          (let ((stderr (get-buffer (lsp--client-stderr
                                     (lsp--workspace-client -workspace)))))
            (when (buffer-live-p stderr)
              (kill-buffer stderr))))))))

(define-hook! lsp|after-open (lsp-after-open-hook)
  (lsp-enable-imenu)
  (setq-local flycheck-check-syntax-automatically nil)
  (flycheck-mode 1)
  (add-hook 'lsp-after-diagnostics-hook
            (lambda () (when flycheck-mode (flycheck-buffer)))))

(with-eval-after-load 'lsp-mode
  (require 'lsp-imenu)
  (setq lsp-eldoc-render-all t)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-highlight-symbol-at-point t)
  (setq lsp-hover-text-function 'lsp--text-document-hover-string)

  (add-to-list 'flycheck-checkers 'lsp)

  (define-key!
    ("M-s h h" . lsp-symbol-highlight)
    ("C-c r" . lsp-rename)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-." . lsp-info-under-point)
    ("C-c C-SPC" . lsp-execute-code-action))

  (advice-add 'lsp--set-code-action-params
              :override #'lsp*after-set-code-action-params)
  (advice-add 'lsp--xref-make-item :override #'lsp*xref-make-match-item)
  (advice-add 'lsp--make-sentinel :override #'lsp*make-sentinel))

(with-eval-after-load 'company-lsp
  (setq company-lsp-async t))

(provide 'init-lsp-mode)
