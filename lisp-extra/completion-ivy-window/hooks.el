;;; -*- lexical-binding: t; -*-

(after! ivy
  (define-advice swiper--isearch-init (:after () fix-height)
    (setq ivy-fixed-height-minibuffer nil))

  (define-advice ivy-shrink-after-dispatching (:around (-fn) fix-height)
    (unless (eq ivy-read-action-function #'ivy-hydra-read-action)
      (funcall -fn)))

  (define-advice ivy--minibuffer-setup (:after () hack)
    (setq ivy-fixed-height-minibuffer nil)
    (setq ymacs-ivy--candidate-window-height
          (ivy--height (ivy-state-caller ivy-last)))))
