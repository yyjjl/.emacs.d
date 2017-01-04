;; {{ shell and conf
(add-to-list 'auto-mode-alist '("\\.[^b][^a][a-zA-Z]*rc$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.aspell\\.en\\.pws\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.meta\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.?muttrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ctags\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mailcap\\'" . conf-mode))
;; }}

;; {{ gnuplot-mode
(add-to-list 'auto-mode-alist '("\\.gpl\\'" . gnuplot-mode))
;; }}

;; {{ groovy-mode
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
;; }}

(add-auto-mode 'tcl-mode "Portfile\\'")

;; {{ crontab
;; in shell "EDITOR='emacs -nw' crontab -e" to edit cron job
(add-to-list 'auto-mode-alist '("crontab.*\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))
;; }}

;; cmake
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))


(define-key global-map (kbd "RET") 'newline-and-indent)

;; M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "<backtab>") 'company-complete)

;; {{ global-keys
;; Use regex to search by default
;;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key [f6] 'toggle-company-ispell)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
;; }}

(setq-default tab-width 4)

(setq history-delete-duplicates t)

(fset 'yes-or-no-p 'y-or-n-p)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;; from RobinH, Time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; prolog system
(setq prolog-system 'swi)

;; regex-tool setup
(setq regex-tool-backend 'perl)

(setq-default buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              save-interprogram-paste-before-kill t
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              line-spacing 0.2
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              ;; void problems with crontabs, etc.
              ;; require-final-newline t ; bad idea, could accidentally edit others' code
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)

;; {{ find-file-in-project (ffip)
(setq ffip-match-path-instead-of-filename t)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only
        t
        point-entered
        minibuffer-avoid-prompt
        face
        minibuffer-prompt))
;;Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; default prog-mode setup
(defun generic-prog-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; don't spell check double words
    (setq flyspell-check-doublon nil)
	;; enable for all programming modes
	(subword-mode 1)
    ;; auto insert closing pair
	(electric-pair-mode 1)
	;; eldoc, show API doc in minibuffer echo area
	(eldoc-mode 1)
	;; show trailing spaces in a programming mod
	(setq show-trailing-whitespace t)))

(add-hook 'prog-mode-hook 'generic-prog-mode-hook-setup)

;; {{ display long lines in truncated style (end line with $)
(defun truncate-lines-setup ()
  (toggle-truncate-lines 1))
(add-hook 'grep-mode-hook 'truncate-lines-setup)
;; (add-hook 'org-mode-hook 'truncate-lines-setup)

(setq system-time-locale "C")

(setq imenu-max-item-length 256)

(provide 'init-defaults)