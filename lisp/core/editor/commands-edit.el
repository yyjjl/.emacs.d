;;; -*- lexical-binding: t; -*-

(defvar reb-regexp)
(defvar reb-target-window)
(declare-function reb-update-regexp "reb-builder")

(eval-when-compile
  (require 're-builder))

(defsubst ymacs-editor//skip-out-symbol ()
  (let ((syntax-b (when (> (point) (point-min)) (char-syntax (char-before))))
        (syntax-a (when (< (point) (point-max)) (char-syntax (char-after)))))
    (when (and (or (equal syntax-a ?w) (equal syntax-a ?_))
               (or (equal syntax-b ?w) (equal syntax-b ?_)))
      (skip-syntax-backward "w_" (line-beginning-position)))))

;;;###autoload
(defun ymacs-editor/smart-M-h ()
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively #'vc-region-history))
   ((get-buffer-process (current-buffer))
    (call-interactively #'consult-history))
   (t
    (let ((current-point (point))
          (map (make-sparse-keymap)))
      (if (bound-and-true-p lispy-mode)
          (call-interactively #'lispy-mark-symbol)
        (ymacs-editor//skip-out-symbol)
        (call-interactively #'mark-sexp))

      (define-key map "h" #'er/expand-region)
      (define-key map (kbd "C-g")
        (interactive!
          (deactivate-mark)
          (goto-char current-point)))

      (set-transient-map map)))))

;;;###autoload
(defun ymacs-editor/smart-kill-line (-arg)
  (interactive "p")
  (cond
   ((region-active-p)
    (call-interactively #'kill-region))
   ((equal -arg 0)
    (kill-sexp 1 'interactive))
   ((equal -arg 4)
    (kill-line))
   ((eolp)
    (delete-char 1)
    (delete-horizontal-space))
   (t
    (let (beg end)
      (save-excursion
        ;; Step 1: skip backward to symbol start
        (ymacs-editor//skip-out-symbol)

        (let* ((end-of-line-point (line-end-position))
               (original-point (point))
               (current-point original-point)
               (last-point original-point))
          ;; Step 2: forward sexp repeatedly
          (while (condition-case nil
                     (progn
                       (forward-sexp 1 nil)
                       (setq current-point (point))
                       (and (not (equal last-point current-point))
                            (< current-point end-of-line-point)))
                   (error nil))
            (setq last-point current-point))

          (setq beg original-point)
          (cond
           ((= original-point current-point))
           ((or (<= current-point end-of-line-point)
                (= original-point last-point))
            ;; (a | b c) => (a|)
            ;;
            ;; |(a    =>  | c
            ;;   b) c
            (setq end current-point))
           ((< original-point last-point)
            ;; | a (b  => | (b
            ;;      c)       c)
            (setq end last-point)))

          ;; Step 3: skip punctuations
          (when (and end (< end end-of-line-point))
            (save-excursion
              (goto-char end)
              (cl-incf end (skip-syntax-forward "." end-of-line-point))))))

      (unless (and beg end)
        (user-error "Nothing to kill"))

      (kill-region beg end)))))

;;;###autoload
(defun ymacs-editor/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
`before-save-hook', and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

;;;###autoload
(defun ymacs-editor/occur-dwim ()
  (interactive)
  (let* ((candidate
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (when-let (sym (thing-at-point 'symbol))
              (concat "\\_<" (regexp-quote sym) "\\_>"))))
         (regexp-history
          (if candidate
              (cons candidate regexp-history)
            regexp-history)))
    (call-interactively 'consult-multi-occur)))

;;;###autoload
(defun ymacs-editor/font-faces-at-point ()
  "Get the font face under cursor."
  (interactive)
  (let* ((pos (point))
         (text (buffer-substring pos (1+ pos)))
         (faces (-uniq (-flatten (list (get-char-property pos 'face)
                                       (get-char-property 0 'face text))))))
    (message "%s" faces)))

;;;###autoload
(defun ymacs-editor/find-file-externally (-files)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive (list (if (eq major-mode 'dired-mode)
                         (dired-get-marked-files)
                       (list (buffer-file-name)))))
  (when (or (<= (length -files) 5)
            (y-or-n-p "Open more than 5 files? "))
    (dolist (file -files)
      (ymacs-editor//find-file-extern file))))

;;;###autoload
(defun ymacs-editor/narrow-or-widen-dwim (&optional -arg)
  "If the buffer is narrowed, it widens.
Otherwise,it narrows to region, or Org subtree.
Optional argument -ARG is used to toggle narrow functions."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (let ((cmd-list (cdr (assoc major-mode ymacs-editor-narrow-dwim-alist))))
             (if cmd-list
                 (setq cmd-list (if -arg (cadr cmd-list) (car cmd-list)))
               (setq cmd-list (if -arg #'narrow-to-page #'narrow-to-defun)))
             (when cmd-list
               (message "Use command `%s'" cmd-list)
               (funcall cmd-list))))))

;;;###autoload
(defun ymacs-editor/forward-defun (&optional -n)
  (interactive "^p")
  (forward-thing 'defun -n))

;;;###autoload
(defun ymacs-editor/backward-defun (&optional -n)
  (interactive "^p")
  (forward-thing 'defun (- -n)))

(autoload #'python-nav-forward-statement "python")
(autoload #'c-end-of-statement "cc-mode")

;;;###autoload
(defun ymacs-editor/forward-sexp (&optional -n)
  (interactive "^p")
  (cond
   ((and (= (abs -n) 1)
         (char-before)
         (char-after)
         (or (and (> -n 0) (eq (char-syntax (char-after)) ?\())
             (and (< -n 0)
                  (or (eq (char-syntax (char-before)) ?\))
                      (when (eq (char-syntax (char-after)) ?\))
                        (forward-char 1)
                        t)))))
    (forward-sexp -n))

   ((when-let (handlers (alist-get major-mode ymacs-editor-forward-sexp-handler))
      (let ((forward-fn (car handlers))
            (backward-fn (cdr handlers)))
        (if (>= -n 0)
            (funcall forward-fn -n)
          (if backward-fn
              (funcall backward-fn (- -n))
            (funcall forward-fn -n)))
        t)))

   ((derived-mode-p 'prog-mode 'org-mode)
    (condition-case nil
        (forward-sexp -n)
      (scan-error
       (forward-char -n))))
   (t (forward-sentence -n))))

;;;###autoload
(defun ymacs-editor/backward-sexp (&optional -n)
  (interactive "^p")
  (ymacs-editor/forward-sexp (- -n)))

;;;###autoload
(defun ymacs-editor/smart-move-begining-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))

;;;###autoload
(defun ymacs-editor/comment-dwim ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\s-*$"))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

;;;###autoload
(defun ymacs-editor/iedit-mode (-arg)
  (interactive "P")
  (if (bound-and-true-p iedit-rectangle-mode)
      (iedit-rectangle-mode -1)
    (if (and (equal -arg '(4))
             (bound-and-true-p lsp-mode))
        (lsp-iedit-highlights)
      (call-interactively #'iedit-mode))))

;;;###autoload
(defun ymacs-editor/format-paragraph (&optional -whole-buffer)
  (interactive "P")

  (if (use-region-p)
      (call-interactively #'indent-region)
    (save-mark-and-excursion
      (if -whole-buffer
          (indent-region (point-min) (point-max))
        (mark-paragraph)
        (call-interactively #'indent-region)))))

;;;###autoload
(defun ymacs-editor/query-replace-regexp ()
  (interactive)
  (let ((saved-point (point))
        (saved-window-start (window-start))
        (saved-marker (save-mark-and-excursion--save))
        (saved-restriction (when (buffer-narrowed-p) (cons (point-min) (point-max))))

        (delimited (and current-prefix-arg (not (eq current-prefix-arg '-))))
        (start (if (use-region-p) (region-beginning)))
        (end (if (use-region-p) (region-end)))
        (backward (and current-prefix-arg (eq current-prefix-arg '-)))
        (region-noncontiguous-p (if (use-region-p) (region-noncontiguous-p))))

    (when (use-region-p)
      (deactivate-mark)
      (narrow-to-region start end))

    (re-builder)

    (advice-add
     'reb-quit :around
     (lambda (-fn)
       (advice-remove 'reb-quit 'query-replace-after-quiting-re-builder)

       (reb-update-regexp)
       (let ((regexp (reb-target-binding reb-regexp)))
         (funcall -fn)
         (with-selected-window reb-target-window
           (if (null saved-restriction)
               (widen)
             (apply #'narrow-to-region saved-restriction))

           ;; restore
           (goto-char saved-point)
           (save-mark-and-excursion--restore saved-marker)
           (set-window-start (selected-window) saved-window-start)

           (query-replace-regexp
            regexp
            (query-replace-read-to
             regexp
             (format "Query replace%s regexp%s"
                     (if backward " backward" "")
                     (if (and start end) " in region" ""))
             t)
            delimited start end backward region-noncontiguous-p))))
     '((name . query-replace-after-quiting-re-builder)))))
