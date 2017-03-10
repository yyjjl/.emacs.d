(defun c-wx-lineup-topmost-intro-cont (langelem)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
        'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist (cons key  val)))



(with-eval-after-load 'cc-mode
  (add-to-list 'c++-font-lock-extra-types "auto"))

(defun my-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (turn-on-auto-fill)
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)
  (ggtags-mode 1)
  (ggtags-auto-update-mode 1)
  ;; indent
  (fix-c-indent-offset-according-to-syntax-context 'innamespace [0])
  (fix-c-indent-offset-according-to-syntax-context 'substatement-open 0)
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)
  (fix-c-indent-offset-according-to-syntax-context 'case-label 4)
  (local-set-key (kbd "M-n") 'forward-sexp)
  (local-set-key (kbd "M-p") 'backward-sexp))

(defun my-c-mode-setup ()
  "C/C++ only setup"
  ;; (message "my-c-mode-setup called (buffer-file-name)=%s" (buffer-file-name))
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (local-set-key (kbd "C-c b") 'clang-format-buffer)
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-v") 'semantic-decoration-include-visit)
  (local-set-key (kbd "M-RET") 'srefactor-refactor-at-point)
  (local-set-key [f9] 'cppcm-reload-all)
  (local-set-key [f10] 'cppcm-compile)

  (try-turn-on-semantic-mode)
  (setq cc-search-directories '("."
                                "/usr/include"
                                "/usr/local/include/*"
                                "../*/include"))

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-irony-c-headers)

  (when buffer-file-name
    (irony-eldoc)
    (irony-mode 1)))
;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (is-buffer-file-temp)
              (my-common-cc-mode-setup)
              (if (derived-mode-p 'c++-mode)
                  (setq-local flycheck-clang-language-standard "c++14")
                (setq flycheck-clang-language-standard nil))
              (unless (or (derived-mode-p 'java-mode)
                          (derived-mode-p 'groovy-mode))
                (my-c-mode-setup)))))

(add-hook 'flycheck-mode-hook 'flycheck-irony-setup)

(with-eval-after-load 'irony
  (setq irony-additional-clang-options '("-std=c++14"))
  (setq irony--server-executable (expand-file-name
                                  "~/.emacs.d/bin/irony-server"))
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(with-eval-after-load 'disaster
  (setq disaster-cxxflags "--std=c++14"))

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(provide 'init-cc-mode)
