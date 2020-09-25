;;; -*- lexical-binding: t; -*-

(defhydra ymacs-hydra/mc (:color blue :hint nil)
  ""
  ("." mc/mark-next-like-this "next" :exit nil)
  ("=" mc/mark-next-like-this "next" :exit nil)
  ("," mc/mark-previous-like-this "prev" :exit nil)
  ("-" mc/mark-previous-like-this "prev" :exit nil)
  ("<" mc/skip-to-previous-like-this "skip prev" :exit nil)
  (">" mc/skip-to-next-like-this "skip next" :exit nil)
  ("RET" nil)
  ("q" nil))

;;;###autoload
(defun ymacs-edit/narrow-or-widen-dwim (&optional -backward-p)
  "If the buffer is narrowed, it widens.
Otherwise,it narrows to region, or Org subtree.
Optional argument ARG is used to toggle narrow functions."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (let ((cmd-list (cdr (assoc major-mode ymacs-edit-narrow-dwim-alist))))
             (if cmd-list
                 (setq cmd-list (if -backward-p (cadr cmd-list) (car cmd-list)))
               (setq cmd-list (if -backward-p #'narrow-to-page #'narrow-to-defun)))
             (when cmd-list
               (message "Use command `%s'" cmd-list)
               (funcall cmd-list))))))

;;;###autoload
(defun ymacs-edit/grab-regexp (-regexp)
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
(defun ymacs-edit/kill-regexp (-regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string and insert them into `kill-ring'"
  (interactive
   (let ((regexp (read-regexp (format "grep regex (default: %s): "
                                      (car regexp-history))
                              (car regexp-history))))
     (list regexp)))
  (let ((items (ymacs-edit/grab-regexp -regexp)))
    (kill-new (string-join items "\n"))
    (message "matched %d strings => kill-ring" (length items))
    items))

(defconst ymacs-edit-ascii-before-chinese
  (rx (group-n 1 (any (?A . ?Z) (?a . ?z) (?0 . ?9) "-!@#$%^&+|:;?><.,/\\" ")]}"))
      (group-n 2 (category chinese-two-byte))))
(defconst ymacs-edit-non-space-after-punc
  (rx (group-n 1 (in ","))
      (group-n 2 (not blank))))
(defconst ymacs-edit-ascii-after-chinese
  (rx (group-n 1 (category chinese-two-byte))
      (group-n 2 (in (?A . ?Z) (?a . ?z) (?0 . ?9) "-!@#$%^&+|></\\" "([{"))))

;;;###autoload
(defun ymacs-edit/insert-space-around-chinese (&optional -start -end)
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
    (while (re-search-forward ymacs-edit-ascii-before-chinese -end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char -start)
    (while (re-search-forward ymacs-edit-ascii-after-chinese -end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char -start)
    (while (re-search-forward ymacs-edit-non-space-after-punc -end t)
      (replace-match "\\1 \\2" nil nil))))

;;;###autoload
(defun ymacs-edit/goto-next-char-or-minibuffer (-backward-p)
  (interactive "P")
  (let ((window (winum-get-window-by-number 0)))
    (if (and window (not (eq window (selected-window))))
        (select-window window)
      (let ((char (read-char))
            (func (if -backward-p 'search-backward 'search-forward)))
        (funcall func (char-to-string char) nil)))))

;;;###autoload
(defun ymacs-edit/forward-defun (&optional -n)
  (interactive "^p")
  (forward-thing 'defun -n))

;;;###autoload
(defun ymacs-edit/backward-defun (&optional -n)
  (interactive "^p")
  (forward-thing 'defun (- -n)))

;;;###autoload
(defun ymacs-edit/forward-sentence-or-sexp (&optional -n)
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
(defun ymacs-edit/backward-sentence-or-sexp (&optional -n)
  (interactive "^p")
  (ymacs-edit/forward-sentence-or-sexp (- -n)))

;;;###autoload
(defun ymacs-edit/forward-defun-or-paragraph (&optional -n)
  (interactive "^p")
  (if (or (derived-mode-p 'prog-mode))
      (ymacs-edit/forward-defun -n)
    (forward-paragraph -n)))

;;;###autoload
(defun ymacs-edit/backward-defun-or-paragraph (&optional -n)
  (interactive "^p")
  (ymacs-edit/forward-defun-or-paragraph (- -n)))

;;;###autoload
(defun ymacs-edit/avy-goto-word-0-in-line-backward (-backward-p)
  (interactive "P")
  (avy-goto-word-0 -backward-p (point-at-bol) (point)))

;;;###autoload
(defun ymacs-edit/avy-goto-word-0-in-line-forward (-backward-p)
  (interactive "P")
  (avy-goto-word-0 -backward-p (1+ (point)) (point-at-eol)))

;;;###autoload
(defun ymacs-edit/avy-goto-symbol-1-in-defun (-char &optional -backward-p)
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
(defun ymacs-edit/smart-move-begining-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))

(defvar ymacs-edit--suround-origin-pos nil)

(defun ymacs-edit//surround-get-pair (-char)
  "Get pair from -CHAR"
  (let ((pair (cl-loop for (keys . pair) in ymacs-edit-surround-pair-alist
                       when (member -char (string-to-list keys))
                       return pair)))
    (cond ((functionp pair) (funcall pair -char)) ; function
          (pair pair)                             ; normal
          ((eq -char 13) nil)
          (t (cons (char-to-string -char)
                   (char-to-string -char))))))

(defun ymacs-edit//surround-get-bounds (-left -right)
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

(defun ymacs-edit//surround-mark ()
  (setq ymacs-edit--suround-origin-pos nil)
  (let* ((char1 (read-char))
         (from-pair (and (not (region-active-p))
                         (ymacs-edit//surround-get-pair char1))))
    (when-let ((pos (and (not (region-active-p))
                         (ymacs-edit//surround-get-bounds (car from-pair)
                                                    (cdr from-pair)))))
      (setq ymacs-edit--suround-origin-pos (point))
      (set-mark (car pos))
      (goto-char (cdr pos))
      (activate-mark))
    (list (region-beginning)
          (region-end)
          from-pair
          (ymacs-edit//surround-get-pair (read-char)))))

;;;###autoload
(defun ymacs-edit/change-surround (-beg -end -from-pair -to-pair)
  (interactive (ymacs-edit//surround-mark))
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
              (when ymacs-edit--suround-origin-pos
                (cl-decf ymacs-edit--suround-origin-pos ll))
              (delete-char ll)))
          ;; Insert left
          (when to-left
            (when ymacs-edit--suround-origin-pos
              (cl-incf ymacs-edit--suround-origin-pos (length to-left)))
            (insert to-left))))
      ;; Restore original point
      (when ymacs-edit--suround-origin-pos
        (goto-char ymacs-edit--suround-origin-pos)))))
