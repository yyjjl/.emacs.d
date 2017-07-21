;; prolog system
(setq prolog-system 'swi)
;; csv
(setq csv-separators '("," ";" "|" " "))

(defalias 'perl-mode 'cperl-mode)

(with-eval-after-load 'grep
  (dolist (v '("auto" "target" "node_modules"
               "bower_components" ".sass_cache" ".cache"
               ".git" ".cvs" ".svn" ".hg" "elpa"))
    (add-to-list 'grep-find-ignored-directories v)))

;; Zeal at point
(global-set-key (kbd "C-h z") 'zeal-at-point)
(global-set-key (kbd "C-h Z") 'zeal-at-point-search)
(with-eval-after-load 'zeal-at-point
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"
        (cdr (assoc 'python-mode zeal-at-point-mode-alist)) "python"))

;; Star dictionary lookup
(autoload 'sdcv-search "sdcv" nil t)
(global-set-key (kbd "C-c D") 'sdcv-search)

;; `restclient-mode'
(with-eval-after-load 'restclient
  (define-keys :map restclient-mode-map
    ("M-p" . restclient-jump-prev)
    ("M-n" . restclient-jump-next)))
(defhook extra|restclient-setup (restclient-mode-hook)
  (add-to-list 'company-backends 'company-restclient))

(defconst extra|ascii-before-chinese
  (rx (group-n 1 (in "a-zA-Z0-9!@#$%^&\\-+|)\\]}\\:;?><.,"))
      (group-n 2 (category chinese-two-byte))))
(defconst extra|ascii-after-chinese
  (rx (group-n 1 (category chinese-two-byte))
      (group-n 2 (in "a-zA-Z0-9@#$%^&\\-+|(\\[{\\></"))))

(defun extra|insert-space-around-chinese (&optional start end)
  (interactive)
  (if (region-active-p)
      (setq start (region-beginning)
            end (region-end))
    (setq start (point-min)
          end (point-max)))
  (save-excursion
    (goto-char start)
    (while (re-search-forward extra|ascii-before-chinese end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char start)
    (while (re-search-forward extra|ascii-after-chinese end t)
      (replace-match "\\1 \\2" nil nil))))

(global-set-key (kbd "M-Q") 'extra|insert-space-around-chinese)

;; `whitespace-space' setup
(with-eval-after-load 'whitespace
  (setq whitespace-style '(face spaces tabs newline
                                space-mark tab-mark newline-mark))
  (setcdr (assoc 'newline-mark whitespace-display-mappings)
          '(10 [182 10]))
  (setcdr (assoc 'tab-mark whitespace-display-mappings)
          '(9 [8594] [92 9])))

;; `calc' setup
(with-eval-after-load 'calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

;; `doxygen' setup
(autoload 'doxygen-insert-function-comment "doxygen" "insert comment for the function at point" t)
(autoload 'doxygen-insert-file-comment "doxygen" "insert comment for file" t)
(autoload 'doxygen-insert-member-group-region "doxygen" "insert comment for member group" t)
(autoload 'doxygen-insert-compound-comment "doxygen" "insert comment for compound" t)

(define-keys
  ;; buffer-mode
  ("C-c w i" . buf-move-up)
  ("C-c w k" . buf-move-down)
  ("C-c w j" . buf-move-left)
  ("C-c w l" . buf-move-right)
  ;; `ace-window' and `avy'
  ("C-x o" . ace-window)
  ("C-:" . avy-goto-char)
  ("C-\"" . avy-goto-char-2)
  ("M-g l" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g y" . avy-copy-line))

(provide 'init-extra)
