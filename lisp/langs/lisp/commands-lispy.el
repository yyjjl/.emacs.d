;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'expand-region))

(defvar ymacs-lisp--in-special-mode nil)

(defvar ymacs-lisp-message-limit 4000
  "String length limit for `ymacs-lisp/message' to pop up a window.
For smaller strings `message' is used.")

(defvar ymacs-lisp--eval-other--window nil
  "Target window for `ymacs-lisp--eval-other-window'.")

(defvar ymacs-lisp--eval-other--buffer nil
  "Target buffer for `ymacs-lisp--eval-other-window'.")

(defvar ymacs-lisp--eval-other--cfg nil
  "Last window configuration for `ymacs-lisp--eval-other-window'.")

(defsubst ymacs-lisp//eval--last-live-p ()
  "Return t if the last eval window is still live with same buffer."
  (and (window-live-p ymacs-lisp--eval-other--window)
       (equal (window-buffer ymacs-lisp--eval-other--window)
              ymacs-lisp--eval-other--buffer)
       (equal (cl-mapcan #'window-list (frame-list))
              ymacs-lisp--eval-other--cfg)))

(defun ymacs-lisp//prin1-to-string (-sexp)
  (cond
   ((and (listp -sexp)
         (ignore-errors
           (or (> (length -sexp) 10)
               (> (length (prin1-to-string (car -sexp))) 40))))
    (concat "(" (mapconcat #'prin1-to-string -sexp "\n") ")"))
   ((hash-table-p -sexp)
    (concat "{"
            (mapconcat
             #'prin1-to-string
             (let (res)
               (maphash (lambda (k v) (push (list k v) res)) -sexp)
               (nreverse res))
             "\n")
            "}"))
   (t
    (prin1-to-string -sexp))))

(defun ymacs-lisp/message (-str &optional -popup)
  "Display STR in the echo area.
If STR is too large, pop it to a buffer instead."
  (if (or
       -popup
       (> (length -str) ymacs-lisp-message-limit)
       (> (cl-count ?\n -str)
          (or
           10
           (* (window-height (frame-root-window)) max-mini-window-height))))
      (with-current-buffer (pop-to-buffer "*ymacs-lisp-message*")
        (special-mode)
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))
          (insert -str)
          (ignore-errors (pp-buffer))
          (goto-char (point-min))
          (while (re-search-forward "\\\\n" nil t)
            (replace-match "\n" nil t))
          (goto-char (point-min)))
        -str)
    (condition-case nil
        (message -str)
      (error (message (replace-regexp-in-string "%" "%%" -str))))))

(defsubst ymacs-lisp//mark (beg end)
  "Mark (BEG . END)"
  (setq deactivate-mark nil)
  (set-mark beg)
  (goto-char end))

(defsubst ymacs-lisp//in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
      beg)))

(defsubst ymacs-lisp//in-string-p ()
  "Test if point is inside a string. Return start of string it is."
  (let ((syn (syntax-ppss)))
    (or (and (nth 3 syn)
             (nth 8 syn))
        (and (eq (char-after) ?\")
             (not (eq ?\\ (char-before)))
             (point)))))

(defsubst ymacs-lisp//in-comment-p ()
  "Test if point is inside a comment."
  (save-excursion
    (unless (eolp)
      (forward-char 1))
    (nth 4 (syntax-ppss))))

(defsubst ymacs-lisp//left-p ()
  (member (char-after) '(?\( ?\{ ?\[)))

(defsubst ymacs-lisp//right-p ()
  (member (char-before) '(?\) ?\} ?\])))

(defsubst ymacs-lisp//different ()
  "Switch to the different side of current sexp."
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((ymacs-lisp//right-p)
         (backward-list))
        ((ymacs-lisp//left-p)
         (forward-list))
        (t
         (user-error "Unexpected"))))

(defsubst ymacs-lisp//format-sexp ()
  (when-let (bound (bounds-of-thing-at-point 'sexp))
    (indent-region (car bound) (cdr bound))
    (delete-trailing-whitespace (car bound) (cdr bound))))

(defmacro ymacs-lisp//run-body-or-self-insert (&rest -body)
  `(if (and
        (not (ymacs-lisp//in-string-or-comment-p))
        (or (ymacs-lisp//left-p)
            (ymacs-lisp//right-p)
            (region-active-p)))
       (let ((current-point (point)))
         (condition-case err
             (progn
               (cl-assert (not ymacs-lisp--in-special-mode) "already in special mode")
               (let ((ymacs-lisp--in-special-mode t))
                 ,@-body))
           (error
            (message "ERROR: %s" err)
            (goto-char current-point))))
     (call-interactively #'self-insert-command)))

(defun ymacs-lisp/delete (-arg)
  (interactive "p")
  (let ((current-point (point)))
    (cond
     ((region-active-p)
      (delete-region (region-beginning) (region-end)))
     ((ymacs-lisp//in-string-or-comment-p)
      (if (save-excursion
            (forward-char 1)
            (not (ymacs-lisp//in-string-or-comment-p)))
          ;; "abcd|"  => |"abcd"
          (progn
            (forward-char 1)
            (backward-sexp 1))
        ;; "ab|cd"  => |"ab|d"
        (delete-char -arg)))
     ((save-excursion
        (forward-char 1)
        (ymacs-lisp//in-string-or-comment-p))
      ;; |"abcd" => |<empty>
      (when-let (bound (or (bounds-of-thing-at-point 'string)
                           (bounds-of-thing-at-point 'comment)))
        (delete-region (car bound) (cdr bound))))
     ((ymacs-lisp//left-p)
      ;; |(abcd efg) => |<empty>
      (forward-sexp 1)
      (delete-region current-point (point)))
     ((save-excursion
        (forward-char 1)
        (ymacs-lisp//right-p))
      ;; (abcd efg|) => |(abcd efg)
      (forward-char 1)
      (ymacs-lisp//different))
     ((ymacs-lisp//right-p)
      ;; |(abcd efg) => |<empty>
      (delete-char 1)
      (just-one-space))
     (t
      (delete-char -arg)))))

(defun ymacs-lisp/delete-backward (-arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (cond
   ((region-active-p)
    (delete-region (region-beginning) (region-end)))
   ((ymacs-lisp//in-string-or-comment-p)
    (if (save-excursion
          (backward-char 1)
          (not (ymacs-lisp//in-string-or-comment-p)))
        ;; "|abcd"  => "abcd"|
        (progn
          (backward-char 1)
          (forward-sexp 1))
      ;; "abc|d"  => "ab|d"
      (backward-delete-char-untabify -arg)))
   ((save-excursion
      (backward-char 1)
      (ymacs-lisp//in-string-p))
    ;; "abcd"|  => <empty>|
    (backward-char 1)
    (when-let (bound (bounds-of-thing-at-point 'string))
      (delete-region (car bound) (cdr bound))))
   ((ymacs-lisp//right-p)
    ;; (abcd efg)| => <empty>|
    (let ((current-point (point)))
      (backward-sexp 1)
      (delete-region (point) current-point)))
   ((save-excursion
      (backward-char 1)
      (ymacs-lisp//left-p))
    ;; (|abcd efg) => <empty>|
    (backward-char 1)
    (let ((current-point (point)))
      (forward-sexp 1)
      (delete-region (point) current-point)))
   ;; abcd      | => abcd |
   ((looking-back "[^ ]  +" (line-beginning-position))
    (delete-region (+ (match-beginning 0) 2) (point)))
   ((looking-back "  +" (line-beginning-position))
    (delete-indentation))
   (t
    (backward-delete-char-untabify -arg))))

(defun ymacs-lisp/different ()
  "Switch to the different side of current sexp."
  (interactive)
  (ymacs-lisp//run-body-or-self-insert (ymacs-lisp//different)))

(defun ymacs-lisp/mark-sexp ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (cond
    ((ymacs-lisp//left-p)
     (mark-sexp))
    ((ymacs-lisp//right-p)
     (ymacs-lisp//different)
     (mark-sexp)
     (exchange-point-and-mark)))))

;;;###autoload
(defun ymacs-lisp/mark-symbol ()
  (interactive)
  (require 'expand-region)
  (cond
   ((ymacs-lisp//in-string-or-comment-p)
    (call-interactively #'er/expand-region))
   (t (or (er/mark-symbol) (mark-sexp)))))

(defun ymacs-lisp/copy-sexp ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (when-let (bound (bounds-of-thing-at-point 'sexp))
     (kill-new (buffer-substring (car bound) (cdr bound)))
     (message "Copied %s" bound))))

(defun ymacs-lisp/clone-sexp (-arg)
  (interactive "p")
  (ymacs-lisp//run-body-or-self-insert
   (when-let (str (thing-at-point 'sexp))
     (cl-labels
         ((doit ()
            (let (deactivate-mark)
              (save-excursion
                (newline)
                (insert str)
                (ignore-errors
                  (progn
                    (up-list 1)
                    (ymacs-lisp//format-sexp)))))))
       (dotimes (_ -arg)
         (if (or (and (region-active-p) (= (point) (region-end)))
                 (ymacs-lisp//right-p))
             (doit)
           (ymacs-lisp//different)
           (doit)
           (ymacs-lisp//different)))))))

(defun ymacs-lisp/format-sexp ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert (ymacs-lisp//format-sexp)))

(defun ymacs-lisp/comment-sexp ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (when-let (bound (bounds-of-thing-at-point 'sexp))
     (comment-region (car bound) (cdr bound)))))

(defun ymacs-lisp/eval (-display-style)
  (interactive '(message))
  (ymacs-lisp//run-body-or-self-insert
   (when-let (str (thing-at-point 'sexp))
     (let ((result (ymacs-lisp//prin1-to-string (eval (read str) t))))
       (if (eq -display-style 'message)
           (ymacs-lisp/message result)
         (when (or (ymacs-lisp//left-p)
                   (and (region-active-p)
                        (= (point) (region-beginning))))
           (ymacs-lisp//different))
         (newline)
         (insert result))))))

(defun ymacs-lisp/eval-and-insert ()
  (interactive)
  (ymacs-lisp/eval 'insert))

(defun ymacs-lisp/eval-other-window (-arg)
  (interactive "P")
  (require 'ace-window)
  (ymacs-lisp//run-body-or-self-insert
     (let* ((expr (thing-at-point 'sexp))
            (aw-dispatch-always nil)
            (target-window
             (cond ((and (null -arg) (ymacs-lisp//eval--last-live-p))
                    ymacs-lisp--eval-other--window)
                   ((setq ymacs-lisp--eval-other--window (aw-select " Ace - Eval in Window"))
                    (setq ymacs-lisp--eval-other--buffer (window-buffer ymacs-lisp--eval-other--window))
                    (setq ymacs-lisp--eval-other--cfg (cl-mapcan #'window-list (frame-list)))
                    ymacs-lisp--eval-other--window)
                   (t (setq ymacs-lisp--eval-other--buffer nil)
                      (setq ymacs-lisp--eval-other--cfg nil)
                      (selected-window))))
            (result (save-window-excursion
                      (with-selected-window target-window
                      (ymacs-lisp//prin1-to-string (eval (read expr) t))))))
       (ymacs-lisp/message result))))

(defun ymacs-lisp/down ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (let ((is-left (ymacs-lisp//left-p)))
     (when is-left
       (ymacs-lisp//different))
     (forward-list 1)
     (if is-left
         (ymacs-lisp//different)
       (unless (ymacs-lisp//right-p)
         (user-error "no sexp found"))))))

(defun ymacs-lisp/up ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (let ((is-right (ymacs-lisp//right-p)))
     (progn
       (when is-right
         (ymacs-lisp//different))
       (backward-list 1)
       (if is-right
           (ymacs-lisp//different)
         (unless (ymacs-lisp//left-p)
           (user-error "no sexp found")))))))

(defun ymacs-lisp/right (-arg)
  "Call `up-list' with ARG unless in string or comment.
Self-insert otherwise."
  (interactive "p")
  (if (or (ymacs-lisp//in-string-or-comment-p)
          (looking-back "?\\\\" (line-beginning-position)))
      (self-insert-command -arg)
    (condition-case nil
        (up-list -arg)
      (scan-error
       (when (ymacs-lisp//left-p)
         (ymacs-lisp//different))))))

(defsubst ymacs-lisp//bound-of-sexp1 ()
  (let (region-side sexp1-beg sexp1-end)
    (if (region-active-p)
        (if (= (region-beginning) (point))
            (progn
              (setq region-side 'left)
              (setq sexp1-beg (point))
              (setq sexp1-end (region-end)))
          (setq region-side 'right)
          (setq sexp1-beg (region-beginning))
          (setq sexp1-end (point)))
      (save-excursion
        (when (ymacs-lisp//left-p)
          (ymacs-lisp//different))
        (setq sexp1-end (point))
        (backward-sexp 1)
        (setq sexp1-beg (point))))
    (list region-side sexp1-beg sexp1-end (buffer-substring sexp1-beg sexp1-end))))

(defsubst ymacs-lisp//bound-of-sexp2 (-start-pos -backward-p)
  (save-excursion
    (goto-char -start-pos)
    (let (sexp2-beg sexp2-end)
      (if -backward-p
          (progn
            (backward-sexp 1)
            (setq sexp2-beg (point))
            (forward-sexp 1)
            (setq sexp2-end (point)))
        (forward-sexp 1)
        (setq sexp2-end (point))
        (backward-sexp 1)
        (setq sexp2-beg (point)))
      (list sexp2-beg sexp2-end (buffer-substring sexp2-beg sexp2-end)))))

(defsubst ymacs-lisp//move-internal (-region-side -original-point -sexp1-beg -sexp1-end)
  (let ((final-point (+ (point) (- -original-point -sexp1-beg))))
    (cond
     ((eq -region-side 'left)
      (ymacs-lisp//mark (+ (point) (- -sexp1-end -sexp1-beg)) final-point))
     ((eq -region-side 'right)
      (ymacs-lisp//mark (point) final-point))
     (t (goto-char final-point)))))

(defun ymacs-lisp/move-down ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (-let* ((current-point (point))
           ((region-side sexp1-beg sexp1-end sexp1) (ymacs-lisp//bound-of-sexp1))
           ((sexp2-beg sexp2-end sexp2) (ymacs-lisp//bound-of-sexp2 sexp1-end nil)))

     (goto-char sexp2-beg)
     (save-excursion
       (delete-region sexp2-beg sexp2-end)
       (insert sexp1)
       (goto-char sexp1-beg)
       (delete-region sexp1-beg sexp1-end)
       (insert sexp2))

     (ymacs-lisp//move-internal region-side current-point sexp1-beg sexp1-end))))

(defun ymacs-lisp/move-up ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (-let* ((current-point (point))
           ((region-side sexp1-beg sexp1-end sexp1) (ymacs-lisp//bound-of-sexp1))
           ((sexp2-beg sexp2-end sexp2) (ymacs-lisp//bound-of-sexp2 sexp1-beg t)))
     (save-excursion
       (goto-char sexp1-beg)
       (delete-region sexp1-beg sexp1-end)
       (insert sexp2)
       (goto-char sexp2-beg)
       (delete-region sexp2-beg sexp2-end)
       (insert sexp1))

     (goto-char sexp2-beg)
     (ymacs-lisp//move-internal region-side current-point sexp1-beg sexp1-end))))

(defun ymacs-lisp/raise ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (when-let ((bound (bounds-of-thing-at-point 'sexp))
              (str (buffer-substring (car bound) (cdr bound))))
     (let ((region-side (when (region-active-p)
                          (if (equal (point) (region-beginning))
                              'left
                            'right)))
           (left-p (ymacs-lisp//left-p)))
       (deactivate-mark)
       (up-list 1)
       (cl-assert (ymacs-lisp//right-p))
       (backward-list)
       (let ((current-point (point)))
         (forward-sexp 1)
         (delete-region current-point (point))
         (save-excursion (insert str))

         (let ((other-point (+ (point) (- (cdr bound) (car bound)))))
           (cond
            ((eq region-side 'left)
             (ymacs-lisp//mark other-point (point)))
            ((eq region-side 'right)
             (ymacs-lisp//mark (point) other-point))
            ((not left-p) (ymacs-lisp//different)))))))))

(defun ymacs-lisp/raise-some ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (cl-assert (not (region-active-p)) "region should not be activated")
   (when (ymacs-lisp//right-p)
     (ymacs-lisp//different))
   (let* ((beg (point))
          (end (progn (while (condition-case nil
                                 (prog1 t (forward-sexp 1))
                               (scan-error nil)))
                      (point)))
          (str (buffer-substring beg end)))
     (up-list 1)
     (ymacs-lisp//different)
     (if (ymacs-lisp//left-p)
         (let ((current-point (point)))
           (forward-sexp 1)
           (delete-region current-point (point))
           (save-excursion (insert str))
           (ymacs-lisp//format-sexp))
       (message "Nothing to do")))))

(defun ymacs-lisp/greater ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (cond
    ((region-active-p) (forward-sexp 1))
    ((ymacs-lisp//right-p)
     (let* ((sexp-beg (point))
            (sexp-end (save-excursion (forward-sexp 1) (point)))
            (sexp (buffer-substring sexp-beg sexp-end)))
       (cl-assert (/= sexp-beg sexp-end))
       (delete-region sexp-beg sexp-end)
       (save-excursion
         (backward-char 1)
         (insert sexp))
       (ymacs-lisp//format-sexp)))
    ((ymacs-lisp//left-p)
     (let (sexp sexp-beg sexp-end)
       (save-excursion
         (forward-char 1)
         (setq sexp-beg (point))
         (setq sexp-end (save-excursion (forward-sexp 1) (point)))
         (cl-assert (/= sexp-beg sexp-end))
         (setq sexp (buffer-substring sexp-beg sexp-end))
         (delete-region sexp-beg sexp-end))
       (save-excursion
         (forward-sexp 1)
         (backward-sexp 1)
         (insert sexp " ")
         (ymacs-lisp//format-sexp)))))))

(defun ymacs-lisp/less ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (cond
    ((region-active-p) (backward-sexp 1))
    ((ymacs-lisp//left-p)
     (let* ((sexp-beg (save-excursion (backward-sexp) (point)))
            (sexp-end (save-excursion
                        (ymacs-lisp//different)
                        (backward-sexp 1)
                        (point)))
            (sexp (buffer-substring sexp-beg sexp-end)))
       (delete-region sexp-beg sexp-end)
       (save-excursion
         (forward-char 1)
         (insert sexp))
       (ymacs-lisp//format-sexp)))
    ((ymacs-lisp//right-p)
     (let (sexp sexp-beg sexp-end)
       (save-excursion
         (backward-char 1)
         (setq sexp-end (point))
         (setq sexp-beg (save-excursion (backward-sexp 1) (point)))
         (cl-assert (/= sexp-beg sexp-end))
         (setq sexp (buffer-substring sexp-beg sexp-end))
         (delete-region sexp-beg sexp-end))
       (save-excursion
         (insert " " sexp)
         (ymacs-lisp//format-sexp)))))))

(defun ymacs-lisp/join ()
  (interactive)
  (ymacs-lisp//run-body-or-self-insert
   (cl-assert (not (region-active-p)) "region should not be activated")
   (let ((original-point (point)))
     (if (ymacs-lisp//left-p)
         (let (first-point)
           (save-excursion
             (backward-sexp 1)
             (setq first-point (point))
             (cl-assert (equal (char-after) (char-after original-point)))
             (forward-sexp 1)
             (cl-assert (ymacs-lisp//right-p) "Nothing to do")
             ;; delete )
             (backward-delete-char 1))
           ;; delete (
           (delete-char 1)
           (save-excursion
             (goto-char first-point)
             (ymacs-lisp//format-sexp)))
       (let (last-point)
         (save-excursion
           (forward-sexp 1)
           (setq last-point (point))
           (cl-assert (equal (char-before) (char-before original-point)))
           (cl-assert (ymacs-lisp//right-p) "Nothing to do")
           (ymacs-lisp//different)
           ;; delete (
           (delete-char 1))
         ;; delete )
         (backward-delete-char 1)
         (save-excursion
           (goto-char last-point)
           (ymacs-lisp//format-sexp)))))))

(defun ymacs-lisp/split ()
  (interactive)
  (cond
   ((ymacs-lisp//in-comment-p)
    (comment-indent-new-line))
   ((ymacs-lisp//in-string-p)
    (insert "\"\n\"")
    (ymacs-lisp//format-sexp))
   (t
    (let ((str (save-excursion
                 (up-list 1)
                 (cl-case (char-before)
                   (?\] "]\n[")
                   (?\) ")\n(")
                   (?\} "}\n{")))))
      (insert str)
      (ymacs-lisp//format-sexp)))))

(defun ymacs-lisp/beginning-of-defun ()
  "Forward to `beginning-of-defun' with ARG"
  (interactive)
  (let ((stored-point (get 'ymacs-lisp/beginning-of-defun 'stored-point)))
    (if (and stored-point
             (numberp stored-point)
             (and
              (> stored-point (point))
              (<= stored-point (save-excursion (forward-list) (point)))))
        (progn
          (put 'ymacs-lisp/beginning-of-defun 'stored-point nil)
          (goto-char stored-point))
      (cl-assert (not (looking-at "^(")) "already at beginning-of-defun")
      (put 'ymacs-lisp/beginning-of-defun 'stored-point (point))
      (beginning-of-defun))))

(defun ymacs-lisp/space ()
  (interactive)
  (if (save-excursion
        (backward-char 1)
        (ymacs-lisp//left-p))
      (save-excursion (insert " "))
    (call-interactively #'self-insert-command)))

(defun ymacs-lisp/avy-symbol ()
  "Jump to a symbol within the current sexp"
  (interactive)

  (ymacs-lisp//run-body-or-self-insert
   (when-let (bound (bounds-of-thing-at-point 'sexp))
     (avy-with ymacs-lisp/avy-symbol
       (avy-jump
        "\\_<\\(\\sw\\|\\s_\\)"
        :beg (car bound)
        :end (cdr bound))))))

(defun ymacs-lisp/avy-brace ()
  "Jump to a symbol within the current sexp"
  (interactive)

  (ymacs-lisp//run-body-or-self-insert
   (when-let (bound (bounds-of-thing-at-point 'sexp))
     (avy-with ymacs-lisp/avy-symbol
       (avy-jump
        "[\\[{(]"
        :beg (car bound)
        :end (cdr bound))))))

(defvar ymacs-lisp-minor-mode-map
  (define-key! :map (make-sparse-keymap)
    ("SPC" . ymacs-lisp/space)
    ("q" . ymacs-lisp/avy-brace)
    ("a" . ymacs-lisp/avy-symbol)
    ("A" . ymacs-lisp/beginning-of-defun)
    ("M-m" . ymacs-lisp/mark-symbol)
    ("M-j" . ymacs-lisp/split)
    ("C-d" . ymacs-lisp/delete)
    ("DEL" . ymacs-lisp/delete-backward)
    (">" . ymacs-lisp/greater)
    ("<" . ymacs-lisp/less)
    ("+" . ymacs-lisp/join)
    ("r" . ymacs-lisp/raise)
    ("R" . ymacs-lisp/raise-some)
    ("e" . ymacs-lisp/eval)
    ("p" . ymacs-lisp/eval-other-window)
    ("E" . ymacs-lisp/eval-and-insert)
    (")" . ymacs-lisp/right)
    ("s" . ymacs-lisp/move-down)
    ("w" . ymacs-lisp/move-up)
    ("d" . ymacs-lisp/different)
    ("c" . ymacs-lisp/clone-sexp)
    ("m" . ymacs-lisp/mark-sexp)
    ("n" . ymacs-lisp/copy-sexp)
    ("i" . ymacs-lisp/format-sexp)
    ("j" . ymacs-lisp/down)
    ("k" . ymacs-lisp/up)
    (";" . ymacs-lisp/comment-sexp)))

;;;###autoload
(define-minor-mode ymacs-lisp-minor-mode
  "A mode for edit lisp sexp fork from https://github.com/abo-abo/lispy"
  :init-value nil
  :lighter nil
  :keymap ymacs-lisp-minor-mode-map)
