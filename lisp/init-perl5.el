(require-packages! company-plsense)

(defalias 'perl-mode 'cperl-mode)



;; perltidy
;; Devel::REPL

(defvar perl-perltidy-path "perltidy")
(defvar perl-perltidy-options nil)
(defvar perl-shell-path (expand-etc! "perli"))


(defun perl%format-region (beg end)
  (unless (char-equal ?\n (char-before end))
    (setq end (min (save-excursion ;; must including terminating newline
                     (goto-char end)
                     (1+ (line-end-position)))
                   (point-max))))
  (let ((old-point (point)))
    (apply #'call-process-region
           beg end
           perl-perltidy-path t '(t nil)
           "--quiet"
           "--standard-error-output"
           perl-perltidy-options)
    (goto-char old-point)))

(defun perl/perltidy-format (&optional $arg)
  "Format Perl code with perltidy.
If region is active, operate on it, else operate on line."
  (interactive "p")
  (if (use-region-p)
      (perl%format-region (region-beginning) (region-end))
    (cond
     ((eq $arg 1)
      (perl%format-region (save-excursion (beginning-of-defun)
                                          (point))
                          (save-excursion (end-of-defun)
                                          (point))))
     ((eq $arg 4)
      (perl%format-region (point-min) (point-max))))))


(setq
 ;; highlight all scalar variables not just the instantiation
 cperl-highlight-variables-indiscriminately t
 ;; 4 spaces is the standard indentation
 cperl-indent-level 4
 ;; indent the closing paren back four spaces
 cperl-close-paren-offset -4
 ;; if a statement continues indent it to four spaces
 cperl-continued-statement-offset 4
 ;; parentheses are indented with the block and not with scope
 cperl-indent-parens-as-block t)

(defun run-perl (&optional $kill)
  (interactive "P")
  (let* ((buffer-name "*perl-repl*")
         (buffer (get-buffer buffer-name)))
    (unless (and buffer
                 (not $kill)
                 (process-live-p (get-buffer-process buffer))
                 (eq (buffer-local-value 'major-mode buffer)
                     'term-mode))
      (when buffer (kill-buffer buffer))
      (setq buffer (term/exec-program perl-shell-path nil buffer-name))
      (with-current-buffer buffer
        (local-set-key (kbd "C-c C-z") 'term/switch-back-no-quit)))
    (pop-to-buffer buffer)))

(define-hook! perl|mode-setup (cperl-mode-hook)
  (add-to-list 'company-backends '(company-plsense
                                   :with company-dabbrev-code))
  (setq-local completion-at-point-functions nil))

(with-eval-after-load 'cperl-mode
  (font-lock-add-keywords 'cperl-mode
                          '(("\\_<say\\_>" . cperl-nonoverridable-face)))
  ;; Use less horrible colors for cperl arrays and hashes
  (set-face-attribute 'cperl-array-face nil
                      :foreground  "#DD7D0A"
                      :background  'unspecified
                      :weight 'unspecified)
  (set-face-attribute 'cperl-hash-face nil
                      :foreground "OrangeRed3"
                      :background 'unspecified
                      :weight 'bold)

  ;; Use less horrible colors for cperl arrays and hashes
  (define-key! :map cperl-mode-map
    ("<tab>" . indent-for-tab-command)
    ("C-c b" . perl/perltidy-format)
    ("C-c C-z" . run-perl)
    ([f5] . perldb)))

(provide 'init-perl5)
