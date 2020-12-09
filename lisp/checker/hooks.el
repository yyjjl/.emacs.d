;; -*- lexical-binding: t; -*-

(after! flycheck
  (define-advice flycheck-error-level-interesting-p (:override (err) smart)
    (when (flycheck-error-p err)
      (if-let ((min-level flycheck-navigation-minimum-level)
               (min-severity (flycheck-error-level-severity min-level)))
          (or (<= min-severity
                  (flycheck-error-level-severity (flycheck-error-level err)))
              ;; all errors have a severity smaller than min-severity
              (not (seq-some
                    (lambda (e)
                      (>= (flycheck-error-level-severity (flycheck-error-level e))
                          min-severity))
                    flycheck-current-errors)))
        t)))

  (define-advice flycheck-add-overlay (:around (-fn &rest -args) set-priority)
    (let ((ov (apply -fn -args)))
      (when-let ((err (overlay-get ov 'flycheck-error))
                 (priority (- (flycheck-error-level-severity
                               (flycheck-error-level err))
                              10)))
        (overlay-put ov 'priority priority))
      ov)))

