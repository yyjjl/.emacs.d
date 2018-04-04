;; (require-packages! ibuffer-vc)



(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1048576) (format "%5.1fM" (/ (buffer-size) 1048576.0)))
     ((> (buffer-size) 1024) (format "%5.1fK" (/ (buffer-size) 1024.0)))
     (t (format "%6d" (buffer-size)))))
  ;; ibuffer-vc slow down emacs
  ;; +Explicitly require ibuffer-vc to get its column+
  ;; +definitions,which can't be autoloaded+
  ;; (require 'ibuffer-vc)

  (setq ibuffer-expert t
        ibuffer-use-other-window t
        ibuffer-show-empty-filter-groups nil
        ibuffer-movement-cycle nil
        ibuffer-display-summary nil)

  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (or (mode . dired-mode)
                        (mode . sr-mode)))
           ("erc" (mode . erc-mode))
           ("planner" (or (name . "^\\*Calendar\\*$")
                          (name . "^diary$")
                          (mode . muse-mode)
                          (mode . org-mode)
                          (mode . org-agenda-mode)))
           ("text" (predicate . (derived-mode? 'text-mode)))
           ("emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Help\\*$")
                        (name . "^\\*info\\*$")
                        (name . "^\\*Backtrace\\*$")
                        (name . "^\\*Completions\\*$")
                        (name . "^\\*Compile-Log\\*$")
                        (name . "^\\*Man .*\\*$")))
           ("emacs-var" (filename . ".emacs.d/var"))
           ("emacs-config" (filename . ".emacs.d"))
           ("code" (or (mode . c++-mode)
                       (mode . c-mode)
                       (mode . java-mode)
                       (predicate . (derived-mode? 'prog-mode))))
           ("process" (or (predicate . (get-buffer-process (current-buffer)))
                          (mode . eshell-mode)))
           ("gnus" (or (mode . message-mode)
                       (mode . bbdb-mode)
                       (mode . mail-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\.bbdb$")
                       (name . "^\\.newsrc-dribble"))))))
  (define-hook! ibuffer|setup (ibuffer-mode-hook)
    ;; (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))
    (ibuffer-switch-to-saved-filter-groups "default")))


;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only
              " " (name 18 18 :left :elide)
              " " (size-h 9 -1 :right)
              " " (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " " (name 32 -1) " " filename)))

;; (setq ibuffer-formats
;;       '((mark modified read-only vc-status-mini " "
;;               (name 18 18 :left :elide) " "
;;               (size-h 9 -1 :right) " "
;;               (mode 16 16 :left :elide) " "
;;               (vc-status 16 16 :left) " "
;;               filename-and-process)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)


(provide 'init-ibuffer)
