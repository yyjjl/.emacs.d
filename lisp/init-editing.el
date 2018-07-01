(require-packages!
 ;; Mark tools
 multiple-cursors
 ;; `wgrep' allows you to edit a grep buffer and apply those changes
 ;; to the file buffer.
 wgrep
 picture
 ;; provide tree style search jump
 avy)



(defvar core-narrow-dwim-alist
  '((org-mode org-narrow-to-subtree org-narrow-to-element)
    (latex-mode LaTeX-narrow-to-environment latex/narrow-to-section)))
(defun core/narrow-or-widen-dwim (&optional $arg)
  "If the buffer is narrowed, it widens.
Otherwise,it narrows to region, or Org subtree.
Optional argument ARG is used to toggle narrow functions."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (let ((cmd-list (cdr (assoc major-mode core-narrow-dwim-alist))))
             (if cmd-list
                 (setq cmd-list (if $arg (cadr cmd-list) (car cmd-list)))
               (setq cmd-list (if $arg #'narrow-to-page #'narrow-to-defun)))
             (when cmd-list
               (message "Use command `%s'" cmd-list)
               (funcall cmd-list))))))

(defun core/grab-regexp ($regexp)
  "Grab strings matching REGEXP to list."
  (let ((case-fold-search nil)
        (s (buffer-string))
        (pos 0)
        item
        items)
    (while (setq pos (string-match $regexp s pos))
      (setq item (match-string-no-properties 0 s))
      (setq pos (+ pos (length item)))
      (add-to-list 'items item))
    items))

(defun core/kill-regexp ($regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string and insert them into `kill-ring'"
  (interactive
   (let ((regexp (read-regexp (format "grep regex (default: %s): "
                                      (car regexp-history))
                              (car regexp-history))))
     (list regexp)))
  (let ((items (core/grab-regexp $regexp)))
    (kill-new (string-join items "\n"))
    (message "matched %d strings => kill-ring" (length items))
    items))

(defconst extra-ascii-before-chinese
  (rx (group-n 1 (in "a-zA-Z0-9!@#$%^&\\-+|)\\]}\\:;?><.,/"))
      (group-n 2 (category chinese-two-byte))))
(defconst extra-non-space-after-punc
  (rx (group-n 1 (in ",?"))
      (group-n 2 (not blank))))
(defconst extra-ascii-after-chinese
  (rx (group-n 1 (category chinese-two-byte))
      (group-n 2 (in "a-zA-Z0-9@#$%^&\\-+|(\\[{\\></"))))

(defun extra/insert-space-around-chinese (&optional $start $end)
  (interactive (cond (current-prefix-arg
                      (list (point-min) (point-max)))
                     ((region-active-p)
                      (list (region-beginning) (region-end)))
                     (t
                      (save-mark-and-excursion
                        (mark-paragraph)
                        (list (region-beginning) (region-end))))))
  (save-excursion
    (goto-char $start)
    (while (re-search-forward extra-ascii-before-chinese $end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char $start)
    (while (re-search-forward extra-ascii-after-chinese $end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char $start)
    (while (re-search-forward extra-non-space-after-punc $end t)
      (replace-match "\\1 \\2" nil nil))))

(defun goto-next-char ($arg)
  (interactive "P")
  (let ((char (read-char))
        (func (if $arg 'search-backward 'search-forward)))
    (funcall func (char-to-string char) nil)))

(defun forward-defun (&optional $n)
  (interactive "p")
  (forward-thing 'defun $n))

(defun backward-defun (&optional $n)
  (interactive "p")
  (forward-thing 'defun (- $n)))

(defun forward-sentence-or-sexp (&optional $n)
  (interactive "p")
  (if (or (derived-mode-p 'prog-mode 'latex-mode 'org-mode))
      (condition-case err
          (forward-sexp $n)
        (scan-error
         (forward-char $n)))
    (forward-sentence $n)))

(defun backward-sentence-or-sexp (&optional $n)
  (interactive "p")
  (forward-sentence-or-sexp (- $n)))

(defun forward-defun-or-paragraph (&optional $n)
  (interactive "p")
  (if (or (derived-mode-p 'prog-mode))
      (forward-defun $n)
    (forward-paragraph $n)))

(defun backward-defun-or-paragraph (&optional $n)
  (interactive "p")
  (forward-defun-or-paragraph (- $n)))

(if (display-graphic-p)
    (define-key!
      ("M-[" . forward-defun-or-paragraph)
      ("M-]" . backward-defun-or-paragraph))
  (define-key!
    ("M-{" . forward-defun-or-paragraph)
    ("M-]" . backward-defun-or-paragraph)))

(define-key!
  ("C-x , SPC" . extra/insert-space-around-chinese)
  ("M-Q" . extra/insert-space-around-chinese)
  ("M-;" . evilnc-comment-or-uncomment-lines)
  ("C-x n n" . core/narrow-or-widen-dwim)
  ("C-x K" . core/kill-regexp)
  ("M-e" . forward-sentence-or-sexp)
  ("M-a" . backward-sentence-or-sexp)
  ("C-M-b" . backward-sentence)
  ("C-M-f" . forward-sentence)

  ("C-=" . hydra-mc/mc/mark-next-like-this)
  ("C--" . hydra-mc/mc/mark-previous-like-this))

(define-key! :prefix "C-c m"
  ("P" . mc/mark-pop)
  ("m" . mc/mark-all-like-this-dwim)
  ("l" . mc/edit-lines)
  ("t" . mc/mark-sgml-tag-pair)
  ("e" . mc/edit-ends-of-lines)
  ("a" . mc/edit-beginnings-of-lines)
  ("i" . mc/insert-numbers)
  ("L" . mc/insert-letters)
  ("s" . mc/sort-regions)
  ("v" . mc/vertical-align)
  ("r" . mc/reverse-regions)
  ("=" . hydra-mc/mc/mark-next-like-this)
  ("-" . hydra-mc/mc/mark-previous-like-this)
  ([C-S-mouse-1] . mc/add-cursor-on-click))

(defhydra hydra-mc (:color blue :hint nil)
  "
_=_ next    _-_ previous    ___ skip-previous  _+_ skip-next _q_ quit
"
  ("=" mc/mark-next-like-this :exit nil)
  ("-" mc/mark-previous-like-this :exit nil)
  ("_" mc/skip-to-previous-like-this :exit nil)
  ("+" mc/skip-to-next-like-this :exit nil)
  ("RET" nil)
  ("q" nil))

(with-eval-after-load 'multiple-cursors-core
  (require 'mc-hide-unmatched-lines-mode))

(with-eval-after-load 'picture
  (define-key! :map picture-mode-map
    ("C-c C-a" . artist-mode)))

;; `avy' jump commands
(define-key!
  ("M-g 1" . avy-goto-char)
  ("M-g ." . avy-goto-char-in-line)
  ("M-g 2" . avy-goto-char-2)
  ("M-g l" . avy-goto-line)
  ("M-g s" . avy-goto-symbol-1)
  ("M-g w" . avy-goto-word-1)
  ("M-g y" . avy-copy-line)
  ("M-g `" . goto-next-char))
(avy-setup-default)

(provide 'init-editing)
