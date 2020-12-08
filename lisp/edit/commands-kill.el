;;; -*- lexical-binding: t; -*-

(require 'easy-kill)
(require 'expand-region)

(defvar ymacs-easy-kill-er-history nil)

;;;###autoload
(defun ymacs/easy-kill-er-expand (arg)
  "Expand current candidate using the algorithms used by `expand-region'.

This applies the `er/expand-region' effect to the current
candidate ARG times."
  (interactive "p")
  (pcase (easy-kill-get bounds)
    (`(nil . nil))
    (`(,beg . ,end)
     (apply
      'easy-kill-adjust-candidate (easy-kill-get thing)
      (save-mark-and-excursion
        (or (memq last-command '(ymacs/easy-kill-er-expand ymacs/easy-kill-er-unexpand))
            (setq ymacs-easy-kill-er-history nil))
        (if (< 0 arg)
            (let ((er/history))
              (push-mark beg t t)
              (goto-char end)
              (cl-loop repeat arg
                       until (let ((prev (list (point) (mark))))
                               (or (eq 'early-exit (er--expand-region-1))
                                   (ignore (push prev ymacs-easy-kill-er-history)))))
              (list (point) (mark)))
          (cl-loop repeat (1- (if (zerop arg) (length ymacs-easy-kill-er-history) arg))
                   do (pop ymacs-easy-kill-er-history))
          (pop ymacs-easy-kill-er-history)))))))

;;;###autoload
(defun ymacs/easy-kill-er-unexpand (arg)
  "Undo `ymacs/easy-kill-er-expand'.

This applies the `er/contract-region' effect to the
current candidate ARG times."
  (interactive "p")
  (ymacs/easy-kill-er-expand (- arg)))
