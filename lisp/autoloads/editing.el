;;; -*- lexical-binding: t; -*-

;;;###autoload
(defvar core-narrow-dwim-alist
  '((org-mode org-narrow-to-subtree org-narrow-to-element)
    (latex-mode LaTeX-narrow-to-environment latex/narrow-to-section)))
;;;###autoload
(defun core/narrow-or-widen-dwim (&optional -arg)
  "If the buffer is narrowed, it widens.
Otherwise,it narrows to region, or Org subtree.
Optional argument ARG is used to toggle narrow functions."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (let ((cmd-list (cdr (assoc major-mode core-narrow-dwim-alist))))
             (if cmd-list
                 (setq cmd-list (if -arg (cadr cmd-list) (car cmd-list)))
               (setq cmd-list (if -arg #'narrow-to-page #'narrow-to-defun)))
             (when cmd-list
               (message "Use command `%s'" cmd-list)
               (funcall cmd-list))))))

;;;###autoload
(defun core/grab-regexp (-regexp)
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
(defun core/kill-regexp (-regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string and insert them into `kill-ring'"
  (interactive
   (let ((regexp (read-regexp (format "grep regex (default: %s): "
                                      (car regexp-history))
                              (car regexp-history))))
     (list regexp)))
  (let ((items (core/grab-regexp -regexp)))
    (kill-new (string-join items "\n"))
    (message "matched %d strings => kill-ring" (length items))
    items))

(defconst extra-ascii-before-chinese
  (rx (group-n 1 (in "a-zA-Z0-9!@#$%^&\\-+|)\\]}\\:;?><.,/"))
      (group-n 2 (category chinese-two-byte))))
(defconst extra-non-space-after-punc
  (rx (group-n 1 (in ","))
      (group-n 2 (not blank))))
(defconst extra-ascii-after-chinese
  (rx (group-n 1 (category chinese-two-byte))
      (group-n 2 (in "a-zA-Z0-9@#$%^&\\-+|(\\[{\\></"))))

;;;###autoload
(defun extra/insert-space-around-chinese (&optional -start -end)
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
    (while (re-search-forward extra-ascii-before-chinese -end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char -start)
    (while (re-search-forward extra-ascii-after-chinese -end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char -start)
    (while (re-search-forward extra-non-space-after-punc -end t)
      (replace-match "\\1 \\2" nil nil))))

;;;###autoload
(defun goto-next-char-or-select-minibuffer-window (-arg)
  (interactive "P")
  (let ((window (aref (car (gethash (selected-frame)
                                    window-numbering-table))
                      0)))
    (if (and window (not (eq window (selected-window))))
        (select-window window)
      (let ((char (read-char))
            (func (if -arg 'search-backward 'search-forward)))
        (funcall func (char-to-string char) nil)))))

;;;###autoload
(defun forward-defun (&optional -n)
  (interactive "p")
  (forward-thing 'defun -n))

;;;###autoload
(defun backward-defun (&optional -n)
  (interactive "p")
  (forward-thing 'defun (- -n)))

;;;###autoload
(defun forward-sentence-or-sexp (&optional -n)
  (interactive "p")
  (if (or (derived-mode-p 'prog-mode 'latex-mode 'org-mode))
      (condition-case nil
          (forward-sexp -n)
        (scan-error
         (forward-char -n)))
    (forward-sentence -n)))

;;;###autoload
(defun backward-sentence-or-sexp (&optional -n)
  (interactive "p")
  (forward-sentence-or-sexp (- -n)))

;;;###autoload
(defun forward-defun-or-paragraph (&optional -n)
  (interactive "p")
  (if (or (derived-mode-p 'prog-mode))
      (forward-defun -n)
    (forward-paragraph -n)))

;;;###autoload
(defun backward-defun-or-paragraph (&optional -n)
  (interactive "p")
  (forward-defun-or-paragraph (- -n)))

;;;###autoload
(defun avy-goto-word-0-in-line-backward (-arg)
  (interactive "P")
  (avy-goto-word-0 -arg (point-at-bol) (point)))

;;;###autoload
(defun avy-goto-word-0-in-line-forward (-arg)
  (interactive "P")
  (avy-goto-word-0 -arg (1+ (point)) (point-at-eol)))

;;;###autoload
(defun avy-goto-symbol-1-in-defun (-char &optional -arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (let (beg end)
    (save-excursion
      (beginning-of-defun-comments)
      (setq beg (point))
      (end-of-defun)
      (setq end (point)))
    (avy-with avy-goto-symbol-1
      (avy-goto-word-1 -char -arg beg end t))))
