;;; -*- lexical-binding: t; -*-

(require-packages!
 ;; Mark tools
 multiple-cursors
 ;; `wgrep' allows you to edit a grep buffer and apply those changes
 ;; to the file buffer.
 wgrep
 ;; provide tree style search jump
 avy)

(avy-setup-default)

(autoload 'hydra-mc/mc/mark-next-like-this "autoloads/editing" nil t)
(autoload 'hydra-mc/mc/mark-previous-like-this "autoloads/editing" nil t)

(config! multiple-cursors
  :advice (:before multiple-cursors-mode
           :define (&rest _)
           (when (bound-and-true-p iedit-mode) (iedit-mode -1))))


(config! iedit
  :advice (:before iedit-mode
           :define (&rest _)
           (when (bound-and-true-p multiple-cursors-mode)
             (multiple-cursors-mode -1)))
  :config
  (setq iedit-auto-narrow t))

(config! picture
  :bind (:map picture-mode-map ("C-c C-a" . artist-mode)))

(define-key!
  ("C-x , SPC" . extra/insert-space-around-chinese)
  ("M-Q" . extra/insert-space-around-chinese)
  ("M-;" . evilnc-comment-or-uncomment-lines)
  ("M-}" . forward-defun-or-paragraph)
  ("M-{" . backward-defun-or-paragraph)
  ("C-x n n" . core/narrow-or-widen-dwim)
  ("C-x K" . core/kill-regexp)
  ("M-e" . forward-sentence-or-sexp)
  ("M-a" . backward-sentence-or-sexp)
  ("C-M-b" . backward-sentence)
  ("C-M-f" . forward-sentence)

  ([remap move-beginning-of-line] . core/smart-move-begining-of-line)
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

;; `avy' jump commands
(define-key!
  ("M-7" . avy-goto-word-1-above)
  ("M-8" . avy-goto-word-1-below)
  ("M-9" . avy-goto-char-in-line)
  ("M-g d" . avy-goto-symbol-1-in-defun)
  ("M-g 1" . avy-goto-char)
  ("M-g 2" . avy-goto-char-2)
  ("M-g l" . avy-goto-line)
  ("M-g s" . avy-goto-symbol-1)
  ("M-g w" . avy-goto-subword-1)
  ("M-g y" . avy-copy-line)
  ("M-'" . core/change-surround))

(provide 'init-editing)
