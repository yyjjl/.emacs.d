;;; -*- lexical-binding: t; -*-

(require-packages!
 ;; Mark tools
 multiple-cursors
 ;; `wgrep' allows you to edit a grep buffer and apply those changes
 ;; to the file buffer.
 wgrep
 picture
 ;; provide tree style search jump
 avy)



(with-eval-after-load 'multiple-cursors
  (advice-add 'multiple-cursors-mode
              :before
              (lambda (&rest _) (when (bound-and-true-p iedit-mode)
                                  (iedit-mode -1)))))

(with-eval-after-load 'iedit
  (setq iedit-auto-narrow t)
  (advice-add 'iedit-mode
              :before
              (lambda (&rest _) (when (bound-and-true-p multiple-cursors-mode)
                                  (multiple-cursors-mode -1)))))

(define-hook! core|init-editing-keys (after-init-idle-hook)
  (when (display-graphic-p)
    (define-key!
      ("M-]" . forward-defun-or-paragraph)
      ("M-[" . backward-defun-or-paragraph))))

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
  ("M-0" . goto-next-char-or-select-minibuffer-window)
  ("M-'" . core/change-surround))

(avy-setup-default)

(provide 'init-editing)
