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

(with-eval-after-load 'rtags
  (setq rtags-completions-enabled nil
        rtags-autostart-diagnostics t))

(with-eval-after-load 'cc-mode
  (rtags-enable-standard-keybindings)
  (let ((m (assoc 114 (assoc 3 c-mode-base-map))))
    (ignore-errors
      (mapcar (lambda (x)
                (let ((k (car x)))
                  (if (and (>= k 65) (<= k 90))
                      (setf (car x) (+ k 32))
                    (if (and (>= k 97) (<= k 122))
                        (setf (car x) (- k 32))))))
              (cddr m)))))

(defun common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (turn-on-auto-fill)
  (setq c-basic-offset 4)
  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)
  (c-toggle-auto-newline -1)
  ;; indent
  (fix-c-indent-offset-according-to-syntax-context 'innamespace [0])
  (fix-c-indent-offset-according-to-syntax-context 'substatement-open 0)
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)
  (fix-c-indent-offset-according-to-syntax-context 'case-label 4))

(defun c-mode-setup ()
  "C/C++ only setup"
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (local-set-key (kbd "C-c b") 'clang-format-buffer)
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-v") 'semantic-decoration-include-visit)
  (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "M-,") 'rtags-location-stack-back)
  (local-set-key (kbd "M-n") 'rtags-next-match)
  (local-set-key (kbd "M-p") 'rtags-previous-match)
  (local-set-key [f9] 'cmake-ide-run-cmake)
  (local-set-key [f10] 'cmake-ide-compile)

  (try-turn-on-semantic-mode)

  (unless (cmake-ide--locate-cmakelists)
    (ggtags-mode 1)
    (ggtags-auto-update-mode 1))

  (setq cc-search-directories '("."
                                "/usr/include"
                                "/usr/local/include/*"
                                "../*/include")
        highlight-indentation-offset 4)
  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior '(alignleft))

  (add-to-list 'company-backends '(company-irony :with company-files))
  (add-to-list 'company-backends 'company-irony-c-headers)
  (when buffer-file-name
    (irony-eldoc)
    (irony-mode 1)))
;; donot use c-mode-common-hook or cc-mode-hook
;; because many major-modes use this hook
(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (is-buffer-file-temp)
              (common-cc-mode-setup)
              (if (derived-mode-p 'c++-mode)
                  (setq-local flycheck-clang-language-standard "c++14")
                (setq flycheck-clang-language-standard nil))
              (unless (or (derived-mode-p 'java-mode)
                          (derived-mode-p 'groovy-mode))
                (c-mode-setup)))))


(with-eval-after-load 'irony
  (setq irony-additional-clang-options '("-std=c++14"
                                         "-Wall"))

  (setq irony--server-executable (expand-file-name
                                  "~/.emacs.d/irony/bin/irony-server"))

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook '(lambda ()
                              (cmake-font-lock-activate)
                              (add-to-list 'company-backends 'company-cmake)))

(cmake-ide-setup)
(provide 'init-cc-mode)
