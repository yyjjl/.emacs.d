;;; -*- lexical-binding: t; -*-

(require-packages!
 expand-region
 goto-chg
 ;; Mark tools
 multiple-cursors
 ;; `wgrep' allows you to edit a grep buffer and apply those changes
 ;; to the file buffer.
 wgrep
 ;; provide tree style search jump
 avy)

(autoload 'ymacs-hydra/mc/mc/mark-next-like-this "edit/commands" nil t)
(autoload 'ymacs-hydra/mc/mc/mark-previous-like-this "edit/commands" nil t)

(defvar ymacs-edit-narrow-dwim-alist
  '((org-mode org-narrow-to-subtree org-narrow-to-element)
    (latex-mode LaTeX-narrow-to-environment ymacs-latex/narrow-to-section)))

(defvar ymacs-edit-surround-pair-alist
  '(("()" . ("(" . ")"))
    ("{}" . ("{" . "}"))
    ("[]" . ("[" . "]"))
    ("`" . ("`" . "'"))
    ("<>,." . (lambda (_)
                (let ((tag (read-string "Tag: ")))
                  (cons (concat "<" tag ">")
                        (concat "</" tag ">")))))
    ("\\" . (lambda (_)
              (let ((pair (ymacs-edit//surround-get-pair (read-char))))
                (when pair
                  (cons (concat "\\" (car pair))
                        (concat "\\" (cdr pair)))))))
    ("b" . (lambda (_)
             (let ((env (read-string "environment: ")))
               (when env
                 (cons (concat "\\begin{" env "}")
                       (concat "\\end{" env "}"))))))))

(define-key!
  ("C-x C-_" . goto-last-change)
  ("C-x C-/" . goto-last-change)

  ("M--" . er/expand-region)

  ("C-x , SPC" . ymacs-edit/insert-space-around-chinese)
  ("M-Q" . ymacs-edit/insert-space-around-chinese)
  ("M-;" . ymacs-edit/comment-dwim)
  ("M-}" . ymacs-edit/forward-defun-or-paragraph)
  ("M-{" . ymacs-edit/backward-defun-or-paragraph)
  ("C-x n n" . ymacs-edit/narrow-or-widen-dwim)
  ("C-x K" . ymacs-edit/kill-regexp)
  ("M-e" . ymacs-edit/forward-sentence-or-sexp)
  ("M-a" . ymacs-edit/backward-sentence-or-sexp)
  ("C-M-b" . backward-sentence)
  ("C-M-f" . forward-sentence)

  ([remap move-beginning-of-line] . ymacs-edit/smart-move-begining-of-line)

  ("C-=" . ymacs-hydra/mc/mc/mark-next-like-this)
  ("C--" . ymacs-hydra/mc/mc/mark-previous-like-this)
  ("C->" . ymacs-hydra/mc/mc/mark-next-like-this)
  ("C-<" . ymacs-hydra/mc/mc/mark-previous-like-this))

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
  ("." . ymacs-hydra/mc/mc/mark-next-like-this)
  ("," . ymacs-hydra/mc/mc/mark-previous-like-this)
  ([C-S-mouse-1] . mc/add-cursor-on-click))

;; `avy' jump commands
(define-key!
  ("M-7" . avy-goto-word-1-above)
  ("M-8" . avy-goto-word-1-below)
  ("M-9")
  ("M-9 9" . avy-goto-char-in-line)
  ("M-9 M-9" . avy-goto-char-in-line)
  ("M-9 d" . ymacs-edit/avy-goto-symbol-1-in-defun)
  ("M-9 1" . avy-goto-char)
  ("M-9 2" . avy-goto-char-2)
  ("M-9 l" . avy-goto-line)
  ("M-9 s" . avy-goto-symbol-1)
  ("M-9 w" . avy-goto-subword-1)
  ("M-9 y" . avy-copy-line)
  ("M-'" . ymacs-edit/change-surround))
