;;; -*- lexical-binding: t; -*-

(defvar reb-regexp)
(defvar reb-target-window)

(eval-when-compile
  (require 're-builder))


(defvar ymacs-editor-sexp-suffix-chars ";,. ")

(defsubst ymacs-editor//skip-out-symbol ()
  (let ((syntax-b (when (> (point) (point-min)) (char-syntax (char-before))))
        (syntax-a (when (< (point) (point-max)) (char-syntax (char-after)))))
    (when (and (or (equal syntax-a ?w) (equal syntax-a ?_))
               (or (equal syntax-b ?w) (equal syntax-b ?_)))
      (skip-syntax-backward "w_" (line-beginning-position)))))

(defsubst ymacs-editor//try-narrow-to-comment-or-string (&optional -syntax -bound)
  (when-let ((syntax (or -syntax (syntax-ppss)))
             (string-comment-start (nth 8 syntax)))
    (save-excursion
      (goto-char string-comment-start)
      (narrow-to-region
       (point)
       (progn
         (if (nth 3 syntax)             ; in string
             (condition-case nil
                 (forward-sexp)
               (scan-error
                (goto-char (or -bound (point-max)))))
           (forward-comment 1))
         (point))))))

(defun ymacs-editor//forward-sexp-repeatly (end-of-line)
  (let* ((original-point (point))
         (current-point (point))
         (last-point (point))
         (index 0))
    (while (condition-case nil
               (progn ;; 首先看到的是 symbol, 先用 forwad symbol
                 (if (and (= index 0)
                          (save-excursion
                            (skip-syntax-forward " " end-of-line)
                            (when-let ((char (char-after))
                                       (syn (char-syntax char)))
                              (or (= syn ?w)
                                  (= syn ?_)))))
                     (forward-symbol 1)
                   ;; 之后都使用 forward sexp
                   (forward-sexp 1 nil))
                 (setq current-point (point))
                 (and (not (equal last-point current-point))
                      (< current-point end-of-line)))
             (error nil))
      (cl-incf index)
      (setq last-point current-point))

    (list original-point last-point current-point)))

;;;###autoload
(defun ymacs-editor/smart-kill-line (-arg)
  (interactive "p")
  (cond
   ((region-active-p)
    (call-interactively #'kill-region))
   ((or (eolp) (equal -arg 4))
    (kill-line))
   (t
    (save-restriction
      (save-excursion
        (let* ((end-of-line (line-end-position))
               (sp (syntax-ppss))
               beg
               end)
          (when (or (nth 3 sp) (nth 4 sp))
            (ymacs-editor//try-narrow-to-comment-or-string sp end-of-line))
          ;; Step 1: skip backward to symbol start
          (unless (nth 3 sp)            ; not inside string
            (ymacs-editor//skip-out-symbol))

          ;; Step 2: forward sexp repeatedly
          (-let (((original-point last-point current-point)
                  (ymacs-editor//forward-sexp-repeatly end-of-line)))
            (setq beg original-point)
            (cond
             ((= original-point current-point))
             ((<= current-point end-of-line)
              ;; (a | b c) => (a|)
              (setq end current-point))
             ((= original-point last-point)
              ;; 先跳过空白字符
              (goto-char original-point)
              (skip-syntax-forward " " end-of-line)
              (when-let ((char (char-after))
                         (syntax (char-syntax char)))
                (if (= syntax ?\()
                    ;; |(a    =>  | c
                    ;;   b) c
                    (setq end current-point)
                  ;; kill line
                  (setq end end-of-line))))
             ((< original-point last-point)
              ;; | a (b  => | (b
              ;;      c)       c)
              (setq end last-point)))

            ;; Step 3: skip punctuations and whitespace
            (when (and end (< end end-of-line))
              (goto-char end)
              (cl-incf end (skip-chars-forward ymacs-editor-sexp-suffix-chars end-of-line))))

          (unless (and beg end)
            (user-error "Nothing to kill"))

          (kill-region beg end)))))))

;;;###autoload
(defun ymacs-editor/smart-M-h ()
  (interactive)
  (let ((current-point (point))
        (map (make-sparse-keymap))
        (use-transient-map))
    (cond
     ((eq last-command #'ymacs-editor/smart-M-h)
      (deactivate-mark)
      (setq use-transient-map t)

      (call-interactively #'mark-defun)
      (define-key map "h" #'er/expand-region))
     ((region-active-p)
      (call-interactively #'vc-region-history))
     ((get-buffer-process (current-buffer))
      (call-interactively #'consult-history))
     (t
      (setq use-transient-map t)

      (ymacs-editor//skip-out-symbol)
      (cond
       ((bound-and-true-p lispy-mode)
        (call-interactively #'lispy-mark-symbol))
       ((eq major-mode 'python-mode)
        (require 'python-el-expansions)
        (call-interactively #'er/mark-python-statement))
       ((memq major-mode '(c-mode c++-mode java-mode))
        (require 'cc-mode-expansions)
        (call-interactively #'er/c-mark-statement))
       (t
        (call-interactively #'mark-sexp)))

      (define-key map "h" #'ymacs-editor/smart-M-h)))

    (when use-transient-map
      (define-key map (kbd "C-g")
        (interactive!
          (deactivate-mark)
          (goto-char current-point)))
      (set-transient-map map))))

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
