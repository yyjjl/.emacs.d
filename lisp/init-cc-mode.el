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
  (defun rtags-eldoc-mine ()
    (when (and (not (nth 4 (syntax-ppss)))
              (let ((text (thing-at-point 'symbol)))
                (when (and text (sequencep text))
                  (set-text-properties 0 (length text) nil text))
                text))
      (let* ((info (rtags-symbol-info-internal))
             (sym-name (cdr (assoc 'symbolName info)))
             (type (cdr (assoc 'type info))))
        (and sym-name type
            (format "%s ⇒ %s"
                    (propertize sym-name 'face
                                'eldoc-highlight-function-argument)
                    (propertize (substring type 0 (string-match "=>" type))
                                'face
                                'font-lock-keyword-face))))))
  (setq rtags-completions-enabled nil
        rtags-autostart-diagnostics nil))

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

  (try-turn-on-semantic-mode)

  (setq cc-search-directories '("."
                                "/usr/include"
                                "/usr/local/include/*"
                                "../*/include")
        highlight-indentation-offset 4)
  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior '(alignleft))

  (add-to-list 'company-backends '(company-irony :with company-files))
  (add-to-list 'company-backends 'company-irony-c-headers)

  (irony-mode 1)

  (if (cmake-ide--locate-cmakelists)
      (progn
        (local-set-key [f10] 'cmake-ide-compile)
        (setq-local eldoc-documentation-function 'rtags-eldoc-mine)
        (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
        (local-set-key (kbd "M-,") 'rtags-location-stack-back)
        (local-set-key (kbd "M-n") 'rtags-next-match)
        (local-set-key (kbd "M-p") 'rtags-previous-match)
        (local-set-key (kbd "C-c .") 'rtags-symbol-type))
    (local-set-key [f10] 'compile)
    (ggtags-mode 1)
    (setq-local compile-command
                '(ignore-errors
                   (let ((filename (buffer-file-name)))
                     (concat "g++ "
                             (string-join irony--compile-options " ") " "
                             (file-name-nondirectory filename) " -o "
                             (file-name-base filename)))))
    (irony-eldoc)))
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
  (setq irony-additional-clang-options '("-Wall" "-std=c++14"))

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook
          '(lambda ()
             (cmake-font-lock-activate)))

(cmake-ide-setup)

(with-eval-after-load 'cmake-ide
  (defun do-unless-buffer-is-temp (fn &rest args)
    "Do function unless buffer is temporary"
    (unless (is-buffer-file-temp)
      (apply fn args)))
  (advice-add 'cmake-ide--mode-hook :around #'do-unless-buffer-is-temp)
  (advice-add 'cmake-ide--before-save :around #'do-unless-buffer-is-temp))

(provide 'init-cc-mode)
