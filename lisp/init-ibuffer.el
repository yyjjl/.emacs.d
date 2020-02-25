;;; -*- lexical-binding: t; -*-

(config! ibuffer
  :hook
  (setup
   :define (ibuffer-mode-hook)
    ;; (ibuffer-vc-set-filter-groups-by-vc-root)
    ;; (ibuffer-auto-mode 1)
   (unless (eq ibuffer-sorting-mode 'filename/process)
     (ibuffer-do-sort-by-filename/process)))

  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1048576) (format "%5.1fM" (/ (buffer-size) 1048576.0)))
     ((> (buffer-size) 1024) (format "%5.1fK" (/ (buffer-size) 1024.0)))
     (t (format "%6d" (buffer-size)))))

  (setq ibuffer-expert t
        ibuffer-use-other-window t
        ibuffer-show-empty-filter-groups nil
        ibuffer-movement-cycle nil
        ibuffer-display-summary nil)

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only
                " " (name 18 18 :left :elide)
                " " (size-h 9 -1 :right)
                " " (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 32 -1) " " filename)))

  (setq ibuffer-filter-group-name-face 'font-lock-doc-face))


(provide 'init-ibuffer)
