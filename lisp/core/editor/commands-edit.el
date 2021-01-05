;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/narrow-or-widen-dwim (&optional -backward-p)
  "If the buffer is narrowed, it widens.
Otherwise,it narrows to region, or Org subtree.
Optional argument ARG is used to toggle narrow functions."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (let ((cmd-list (cdr (assoc major-mode ymacs-editor-narrow-dwim-alist))))
             (if cmd-list
                 (setq cmd-list (if -backward-p (cadr cmd-list) (car cmd-list)))
               (setq cmd-list (if -backward-p #'narrow-to-page #'narrow-to-defun)))
             (when cmd-list
               (message "Use command `%s'" cmd-list)
               (funcall cmd-list))))))

;;;###autoload
(defun ymacs-editor/grab-regexp (-regexp)
  "Grab strings matching REGEXP to list."
  (let ((case-fold-search nil)
        (s (buffer-string))
        (pos 0)
        item
        items)
    (while (setq pos (string-match -regexp s pos))
      (setq item (match-string-no-properties 0 s))
      (setq pos (+ pos (length item)))
      (push item items))
    items))

;;;###autoload
(defun ymacs-editor/kill-regexp (-regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string and insert them into `kill-ring'"
  (interactive
   (let ((regexp (read-regexp (format "grep regex (default: %s): "
                                      (car regexp-history))
                              (car regexp-history))))
     (list regexp)))
  (let ((items (ymacs-editor/grab-regexp -regexp)))
    (kill-new (string-join items "\n"))
    (message "matched %d strings => kill-ring" (length items))
    items))

(defconst ymacs-editor-ascii-before-chinese
  (rx (group-n 1 (any (?A . ?Z) (?a . ?z) (?0 . ?9) "-!@#$%^&+|:;?><.,/\\" ")]}"))
      (group-n 2 (category chinese-two-byte))))
(defconst ymacs-editor-non-space-after-punc
  (rx (group-n 1 (in ","))
      (group-n 2 (not blank))))
(defconst ymacs-editor-ascii-after-chinese
  (rx (group-n 1 (category chinese-two-byte))
      (group-n 2 (in (?A . ?Z) (?a . ?z) (?0 . ?9) "-!@#$%^&+|></\\" "([{"))))

;;;###autoload
(defun ymacs-editor/insert-space-around-chinese (&optional -start -end)
  (interactive (cond (current-prefix-arg
                      (list (point-min) (point-max)))
                     ((region-active-p)
                      (list (region-beginning) (region-end)))
                     (t
                      (save-mark-and-excursion
                        (mark-paragraph)
                        (list (region-beginning) (region-end))))))
  (save-excursion
    (goto-char -start)
    (while (re-search-forward ymacs-editor-ascii-before-chinese -end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char -start)
    (while (re-search-forward ymacs-editor-ascii-after-chinese -end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char -start)
    (while (re-search-forward ymacs-editor-non-space-after-punc -end t)
      (replace-match "\\1 \\2" nil nil))))

;;;###autoload
(defun ymacs-editor/goto-next-char-or-minibuffer (-backward-p)
  (interactive "P")
  (let ((window (active-minibuffer-window)))
    (if (and window (not (eq window (selected-window))))
        (select-window window)
      (if -backward-p
          (search-backward (char-to-string (read-char "backward to char:"))
                           (line-beginning-position))
        (search-forward (char-to-string (read-char "forward to char:"))
                        (line-end-position))))))

;;;###autoload
(defun ymacs-editor/forward-defun (&optional -n)
  (interactive "^p")
  (forward-thing 'defun -n))

;;;###autoload
(defun ymacs-editor/backward-defun (&optional -n)
  (interactive "^p")
  (forward-thing 'defun (- -n)))

(autoload #'python-nav-forward-statement "python" nil t)

;;;###autoload
(defun ymacs-editor/forward-sentence-or-sexp (&optional -n)
  (interactive "^p")
  (cond ((derived-mode-p 'c-mode 'c++-mode 'java-mode)
         (if (and (not current-prefix-arg)
                  (or (= (char-after) ?\{)
                      (and (< -n 0)
                           (or (= (char-before) ?\})
                               (when (= (char-after) ?\})
                                 (forward-char 1)
                                 t)))))
             (forward-sexp -n)
           (c-end-of-statement -n nil t)))
        ((derived-mode-p 'python-mode)
         (python-nav-forward-statement -n))
        ((derived-mode-p 'prog-mode 'latex-mode 'org-mode)
         (condition-case nil
             (forward-sexp -n)
           (scan-error
            (forward-char -n))))
        (t (forward-sentence -n))))

;;;###autoload
(defun ymacs-editor/backward-sentence-or-sexp (&optional -n)
  (interactive "^p")
  (ymacs-editor/forward-sentence-or-sexp (- -n)))

;;;###autoload
(defun ymacs-editor/forward-defun-or-paragraph (&optional -n)
  (interactive "^p")
  (if (or (derived-mode-p 'prog-mode))
      (ymacs-editor/forward-defun -n)
    (forward-paragraph -n)))

;;;###autoload
(defun ymacs-editor/backward-defun-or-paragraph (&optional -n)
  (interactive "^p")
  (ymacs-editor/forward-defun-or-paragraph (- -n)))

;;;###autoload
(defun ymacs-editor/avy-goto-word-0-in-line-backward (-backward-p)
  (interactive "P")
  (avy-goto-word-0 -backward-p (point-at-bol) (point)))

;;;###autoload
(defun ymacs-editor/avy-goto-word-0-in-line-forward (-backward-p)
  (interactive "P")
  (avy-goto-word-0 -backward-p (1+ (point)) (point-at-eol)))

;;;###autoload
(defun ymacs-editor/avy-goto-symbol-1-in-defun (-char &optional -backward-p)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (let (beg end)
    (save-excursion
      (beginning-of-defun-comments)
      (setq beg (point))
      (end-of-defun)
      (setq end (point)))
    (avy-with avy-goto-symbol-1
      (avy-goto-word-1 -char -backward-p beg end t))))

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

(defvar ymacs-editor--suround-origin-pos nil)

(defun ymacs-editor//surround-get-pair (-char)
  "Get pair from -CHAR"
  (let ((pair (cl-loop for (keys . pair) in ymacs-editor-surround-pair-alist
                       when (member -char (string-to-list keys))
                       return pair)))
    (cond ((functionp pair) (funcall pair -char)) ; function
          (pair pair)                             ; normal
          ((eq -char 13) nil)
          (t (cons (char-to-string -char)
                   (char-to-string -char))))))

(defun ymacs-editor//surround-get-bounds (-left -right)
  "Get bounds of pair. If -LEFT and -RIGHT is a open/close delimeter.
Use `scan-lists', otherwise use simple algorithm."
  (if (and (string-match-p "^\\s($" -left)
           (string-match-p "^\\s)$" -right))
      (ignore-errors
        (cons
         (scan-lists (point) -1 1)
         (scan-lists (point) 1 1)))
    (cons (save-excursion
            (while (and (search-backward -left)
                        (eq ?\\ (char-before (point)))))
            (point))
          (save-excursion
            (while (and (search-forward -right)
                        (eq ?\\ (char-before (- (point) (length -right))))))
            (point)))))

(defun ymacs-editor//surround-mark ()
  (setq ymacs-editor--suround-origin-pos nil)
  (let* ((char1 (read-char))
         (from-pair (and (not (region-active-p))
                         (ymacs-editor//surround-get-pair char1))))
    (when-let ((pos (and (not (region-active-p))
                         (ymacs-editor//surround-get-bounds (car from-pair) (cdr from-pair)))))
      (setq ymacs-editor--suround-origin-pos (point))
      (set-mark (car pos))
      (goto-char (cdr pos))
      (activate-mark))
    (list (region-beginning)
          (region-end)
          from-pair
          (ymacs-editor//surround-get-pair (read-char)))))

;;;###autoload
(defun ymacs-editor/change-surround (-beg -end -from-pair -to-pair)
  (interactive (ymacs-editor//surround-mark))
  (unless (equal -from-pair -to-pair)
    (if (equal -beg -end)
        (message "Empty region")
      (let ((from-left (car -from-pair))
            (from-right (cdr -from-pair))
            (to-left (car -to-pair))
            (to-right (cdr -to-pair)))
        (save-excursion
          (goto-char -end)
          (let ((rl (length from-right)))
            (when (equal (buffer-substring-no-properties
                          (max (point-min) (- (point) rl)) (point))
                         from-right)
              (delete-char (- rl))))
          ;; Insert right
          (when to-right (insert to-right))
          (goto-char -beg)
          (let ((ll (length from-left)))
            (when (equal (buffer-substring-no-properties
                          (point) (min (point-max) (+ (point) ll)))
                         from-left)
              (when ymacs-editor--suround-origin-pos
                (cl-decf ymacs-editor--suround-origin-pos ll))
              (delete-char ll)))
          ;; Insert left
          (when to-left
            (when ymacs-editor--suround-origin-pos
              (cl-incf ymacs-editor--suround-origin-pos (length to-left)))
            (insert to-left))))
      ;; Restore original point
      (when ymacs-editor--suround-origin-pos
        (goto-char ymacs-editor--suround-origin-pos)))))

;;;###autoload
(defun ymacs-editor/iedit-mode (-arg)
  (interactive "P")
  (if (bound-and-true-p iedit-rectangle-mode)
      (iedit-rectangle-mode -1)
    (if (and (equal -arg '(4))
             (bound-and-true-p lsp-mode))
        (lsp-iedit-highlights)
      (call-interactively #'iedit-mode))))
