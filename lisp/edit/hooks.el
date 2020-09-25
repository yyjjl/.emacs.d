;;; -*- lexical-binding: t; -*-

(after! multiple-cursors
  (define-advice multiple-cursors-mode (:before (&rest _) disable-iedit)
    (when (bound-and-true-p iedit-mode)
      (iedit-mode -1))))

(after! iedit
  (define-advice iedit-mode (:before (&rest _) disable-mc)
    (when (bound-and-true-p multiple-cursors-mode)
      (multiple-cursors-mode -1))))
