(defvar core|narrow-dwim-alist
  '((org-mode org-narrow-to-subtree org-narrow-to-element)
    (latex-mode LaTeX-narrow-to-environment TeX-narrow-to-group)))
(defun core|narrow-or-widen-dwim (&optional arg)
  "If the buffer is narrowed, it widens.
Otherwise,it narrows to region, or Org subtree.
Optional argument ARG is used to toggle narrow functions."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (let ((cmd (cdr (assoc major-mode core|narrow-dwim-alist))))
             (if cmd
                 (setq cmd (if arg (cadr cmd) (car cmd)))
               (setq cmd (if arg #'narrow-to-page #'narrow-to-defun)))
             (when cmd
               (message "Use command `%s'" cmd)
               (funcall cmd))))))

(defun comment-current-line (&optional arg)
  "Comment current line."
  (save-excursion
    (beginning-of-line)
    (set-mark-command nil)
    (end-of-line)
    (if arg
        (call-interactively #'uncomment-region)
      (call-interactively #'comment-region))))

(defun comment-region-or-line ()
  "If `mark-active' is non-nil, call `comment-region'. Otherwise
call `comment-current-line'."
  (interactive "*")
  (if mark-active
      (call-interactively 'comment-region)
    (comment-current-line)))

(defun uncomment-region-or-line ()
    "If `mark-active' is non-nil, call
`uncomment-region'. Otherwise call `comment-current-line' with argument t."
  (interactive "*")
  (if mark-active
      (call-interactively 'uncomment-region)
    (comment-current-line t)))

(defun core|grab-regexp (regexp)
  "Grab strings matching REGEXP to list."
  (let ((s (buffer-string))
        (pos 0)
        item
        items)
    (while (setq pos (string-match regexp s pos))
      (setq item (match-string-no-properties 0 s))
      (setq pos (+ pos (length item)))
      (add-to-list 'items item))
    items))

(autoload 'string-join "subr-x")
(defun core|kill-regexp (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string and insert them into `kill-ring'"
  (interactive
   (let ((regexp (read-regexp "grep regex: ")))
     (list regexp)))
  (let ((items (core|grab-regexp regexp)))
    (kill-new (string-join items "\n"))
    (message "matched strings => kill-ring")
    items))

(define-keys
  ("C-c c" . comment-region-or-line)
  ("C-c u" . uncomment-region-or-line)
  ("C-x n n" . core|narrow-or-widen-dwim)
  ("C-x K" . core|kill-regexp)

  ("M-z" . zzz-to-char)
  ("M-Z" . zzz-up-to-char)
  ("C-=" . mc/mark-next-like-this)
  ("C--" . mc/mark-previous-like-this))

(define-keys :prefix "C-c v"
  ("q" . vr/query-replace)
  ("r" . vr/replace)
  ("m" . vr/mc-mark))

(define-keys :prefix "C-c m"
  ("P" . mc/mark-pop)
  ("m" . mc/mark-all-like-this-dwim)
  ("l" . mc/edit-lines)
  ("e" . mc/edit-ends-of-lines)
  ("a" . mc/edit-beginnings-of-lines)
  ("n" . mc/skip-to-next-like-this)
  ("p" . mc/skip-to-previous-like-this)
  ("r" . mc/mark-all-in-region-regexp)
  ("i" . mc/insert-numbers)
  ("L" . mc/insert-letters)
  ("s" . mc/sort-regions)
  ("R" . mc/reverse-regions))

(with-eval-after-load 'picture
  (defhydra hydra|picture-move ()
    "move"
    ("n" picture-move-down "down")
    ("p" picture-move-up "up")
    ("f" picture-motion "forward")
    ("b" picture-motion-reverse "backward")
    ("C-SPC" set-mark-command "mark")
    ("RET" nil nil))
  (define-keys :map picture-mode-map
    ("C-d" . picture-delete-char)
    ("C-c C-f") ("C-c C-b")
    ("C-c a" . artist-mode)
    ("C-f" . hydra|picture-move/picture-motion)
    ("C-b" . hydra|picture-move/picture-motion-reverse)
    ("C-n" . hydra|picture-move/picture-move-down)
    ("C-p" . hydra|picture-move/picture-move-up)))

(with-eval-after-load 'artist
  (define-keys :map artist-mode-map
    ("p" . artist-previous-line)
    ("n" . artist-next-line)
    ("b" . artist-backward-char)
    ("f" . artist-forward-char)

    ("C-c a e" . artist-select-erase-char)
    ("C-c a f" . artist-select-fill-char)
    ("C-c a l" . artist-select-line-char)
    ("C-c a o" . artist-select-operation)
    ("C-c a r" . artist-toggle-rubber-banding)
    ("C-c a t" . artist-toggle-trim-line-endings)
    ("C-c a s" . artist-toggle-borderless-shapes)
    ("C-c s l" . artist-select-op-line)
    ("C-c s L" . artist-select-op-straight-line)
    ("C-c s r" . artist-select-op-rectangle)
    ("C-c s R" . artist-select-op-square)
    ("C-c s s" . artist-select-op-square)
    ("C-c s p" . artist-select-op-poly-line)
    ("C-c s P" . artist-select-op-straight-poly-line)
    ("C-c s e" . artist-select-op-ellipse)
    ("C-c s c" . artist-select-op-circle)
    ("C-c s t" . artist-select-op-text-see-thru)
    ("C-c s T" . artist-select-op-text-overwrite)
    ("C-c s S" . artist-select-op-spray-can)
    ("C-c s z" . artist-select-op-spray-set-size)
    ("C-c s d" . artist-select-op-erase-char)
    ("C-c s E" . artist-select-op-erase-rectangle)
    ("C-c s v" . artist-select-op-vaporize-line)
    ("C-c s V" . artist-select-op-vaporize-lines)
    ("C-c s k" . artist-select-op-cut-rectangle)
    ("C-c s w" . artist-select-op-copy-rectangle)
    ("C-c s y" . artist-select-op-paste)
    ("C-c s f" . artist-select-op-flood-fill)))

(provide 'init-editing)
