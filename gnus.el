
(setq gnus-select-method '(nnnil))
(setq gnus-startup-file (expand-var! "gnus/news"))

; NO 'passive
(setq gnus-use-cache t)
(setq gnus-read-active-file 'some)
(setq gnus-activate-level 4)
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number))

(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first message.
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)
(setq gnus-always-read-dribble-file t)

(defvar gnus--unread-news-count 0)

(defun gnus//read-count (group)
  "Return read count for gnus GROUP."
  (let* ((range (gnus-range-normalize (gnus-info-read group)))
         (count (car (last range))))
    (or (cdr-safe count) count)))

(define-hook! gnus|count-unread-news (gnus-started-hook
                                      gnus-after-getting-new-news-hook)
  (let ((totoal-unread-count 0))
    (dolist (group gnus-newsrc-alist)
      (let* ((name (gnus-info-group group))
             (unread-count (gnus-group-unread name)))
        (when (numberp unread-count)
          (incf totoal-unread-count unread-count))))
    (setq gnus--unread-news-count totoal-unread-count)
    (force-mode-line-update)))

(defun gnus/summary-exit ()
  (interactive)
  (if-let* ((buffer (get-buffer gnus-article-buffer))
            (window (get-buffer-window buffer)))
      (delete-window window)
    (gnus-summary-exit)))

(defun gnus/summary-next-page ()
  (interactive)
  (gnus-summary-next-page nil t nil))

(with-eval-after-load 'gnus-art
  (define-key! :map gnus-article-mode-map
    ("RET" . scroll-up-line)
    ("y" . scroll-down-line)))

(with-eval-after-load 'gnus-sum
  (define-key! :map gnus-summary-mode-map
    ("SPC" . gnus/summary-next-page)
    ("y" . gnus-summary-scroll-down)
    ("q" . gnus/summary-exit)
    ("n" . gnus-summary-next-article)
    ("p" . gnus-summary-prev-article)
    ("N" . gnus-summary-next-unread-article)
    ("P" . gnus-summary-prev-unread-article)))

(require 'gnus-demon)

(gnus-demon-add-handler 'gnus-demon-scan-news 60 30)

;; (add-to-list 'mode-line-misc-info
;;              '(:eval
;;                (when (> gnus--unread-news-count 0)
;;                  (format " (%d)" gnus--unread-news-count))))

