(defvar! cpp-rtags-path (expand-var! "rtags/bin/")
  "Rtags directory")
(defvar! cpp-irony-path (expand-var! "irony/bin")
  "irony directory")
(defvar! cpp-has-rtags-p
  (file-exists-p (expand-file-name "rdm" cpp-rtags-path))
  "Rtags support, indexing c++ projects")
(defvar! cpp-has-irony-p
  (file-exists-p (expand-file-name "irony-server" cpp-irony-path))
  "Irony support, context sensitive c++ completions")

;; C/C++ I need stable version
(require! 'cmake-ide "melpa-stable")
(require! 'irony)
(require! 'irony-eldoc)
(require! 'company-irony)
(require! 'company-irony-c-headers)
(require! 'flycheck-irony)
(require! 'clang-format)
(require! 'rtags)
(require! 'ivy-rtags)
(require! 'cmake-mode)
(require! 'cmake-font-lock)
(require! 'modern-cpp-font-lock)



(defun cpp/fix-cc-indent-offset ($key $val)
  (let ((pair (assoc $key c-offsets-alist)))
    (if pair
        (setcdr pair $val)
      (push (cons $key $val) c-offsets-alist))))

;; Makefile support for rtags
(defvar-local cpp-cmake-ide-enabled nil)

(defsubst cpp/locate-makefile (&optional $makefile-name)
  (locate-dominating-file default-directory (or $makefile-name "Makefile")))

(defun cpp/rtags-indexing-with-makefile (&optional $name)
  (interactive)
  (if (rtags-is-indexed)
      (message "Project indexed already !!")
    (let ((root (cpp/locate-makefile $name)))
      (if root
          (let ((default-directory root) ret)
            (equal (shell-command (format "make -nk|%s -c -"
                                          (expand-file-name "rc" rtags-path)))
                   0))
        (message "Makefile not found !!!")
        nil))))

(defun cpp/remove-project-rtags-cache (&optional $force)
  (interactive "P")
  (let ((root (ignore-errors (projectile-project-root))))
    (when (and root (or $force (y-or-n-p (format "Delete %s"
                                                 (abbreviate-file-name root)))))
      (let ((default-directory root))
        (shell-command (format "%s -W %s"
                               (expand-file-name "rc" rtags-path)
                               default-directory))
        (cpp/c++-normal-setup)))))

(defun cpp/common-cc-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)"
  (turn-on-auto-fill)
  (setq c-basic-offset 4)
  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)
  ;; (c-toggle-auto-newline 1)
  ;; indent
  (highlight-indentation-set-offset 4)
  (cpp/fix-cc-indent-offset 'innamespace [0])
  (cpp/fix-cc-indent-offset 'substatement-open 0)
  (cpp/fix-cc-indent-offset 'func-decl-cont 0)
  (cpp/fix-cc-indent-offset 'case-label 4))

(defun cpp/rtags-setup ()
  (setq-local eldoc-documentation-function 'cpp/rtags-eldoc)
  (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "M-,") 'rtags-location-stack-back)
  (local-set-key (kbd "M-n") 'rtags-next-match)
  (local-set-key (kbd "M-p") 'rtags-previous-match)
  (local-set-key (kbd "C-c .") 'rtags-symbol-type)
  (local-set-key (kbd "C-c C-k") 'cpp/remove-project-rtags-cache)
  (when cpp-has-irony-p (irony-eldoc -1))
  (when tags-has-ggtags-p (ggtags-mode -1)))

(defun cpp/try-use-rtags ()
  (interactive)
  (if cpp-has-rtags-p
      (if cpp-cmake-ide-enabled
          (progn (cmake-ide-run-cmake)
                 (cpp/rtags-setup))
        (when (cpp/rtags-indexing-with-makefile)
          (message "Rtags is ready !!")
          (cpp/rtags-setup)))
    (message "Rtags not found !!!")))

(defun cpp/c++-normal-setup ()
  (setq-local compile-command
              '(ignore-errors
                 (let ((root (cpp/locate-makefile)))
                   (if root (concat "make -C " root)
                     (let ((filename (buffer-file-name)))
                       (concat "g++ "
                               (when (bound-and-true-p irony-mode)
                                 (string-join
                                  (append (irony--lang-compile-option)
                                          irony-additional-clang-options
                                          irony--compile-options)
                                  " "))
                               " "
                               (file-name-nondirectory filename) " -o "
                               (file-name-base filename)))))))
  (when cpp-has-irony-p (irony-eldoc)))

(defun cpp/compile ()
  (interactive)
  (call-interactively
   (if cpp-cmake-ide-enabled
       'cmake-ide-compile
     'compile)))

(setq hide-ifdef-mode-prefix-key (kbd "C-c h"))
(defun cpp/c++-setup ()
  "C/C++ only setup"
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c b") 'clang-format-buffer)
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-v") 'semantic-decoration-include-visit)
  (local-set-key (kbd "C-c C-c") 'cmake-ide-run-cmake)
  (local-set-key [f9] 'cpp/try-use-rtags)
  (local-set-key [f10] 'cpp/compile)
  (local-set-key [f5] 'gdb)

  (hide-ifdef-mode)

  (modern-c++-font-lock-mode 1)

  (when tags-has-ggtags-p
    (ggtags-mode 1)
    (setq completion-at-point-functions '(t)))

  ;; Make a #define be left-aligned
  (setq c-electric-pound-behavior '(alignleft))
  (if (derived-mode-p 'c++-mode)
      (setq-local flycheck-clang-language-standard "c++14")
    (setq flycheck-clang-language-standard nil))

  (unless (or (file-remote-p default-directory)
              (bound-and-true-p cpp/setup-literally)
              (> (buffer-size) core-large-buffer-size))
    (when cpp-has-irony-p
      (add-to-list 'company-backends #'company-irony)
      (add-to-list 'company-backends #'company-irony-c-headers)
      (add-to-list 'company-backends #'company-files)
      (irony-mode 1))
    ;; Make sure rdm is running
    (when cpp-has-rtags-p
      (rtags-start-process-unless-running))

    (if (cmake-ide--locate-cmakelists)
        (progn
          (setq-local cpp-cmake-ide-enabled t)
          (cpp/rtags-setup))
      (cpp/c++-normal-setup))))

;; Do not use `c-mode-hook' and `c++-mode-hook', there is a bug
(defvar-local cpp/initialized-p nil)
(define-hook! cpp/common-setup (c-mode-common-hook)
  (unless cpp/initialized-p
    (setq cpp/initialized-p t)
    (if (>= (string-to-number c-version) 5.33)
        (run-hooks 'prog-mode-hook))

    (cpp/common-cc-setup)
    (unless (buffer-temporary?)
      (unless (or (derived-mode-p 'java-mode)
                  (derived-mode-p 'groovy-mode))
        (cpp/c++-setup)))))

(with-eval-after-load 'irony
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
  (setq irony-additional-clang-options
        '("-Wall"
          "-I/usr/lib/gcc/x86_64-linux-gnu/5/include/"
          "-std=c++14"))

  (defun cpp/irony-setup (irony-mode-hook)
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(with-eval-after-load 'cmake-ide
  (setq cmake-ide-rdm-buffer-name "*rdm*")
  ;; Use `rtags' function instead
  (fset 'cmake-ide-maybe-start-rdm
        'rtags-start-process-unless-running)
  (setq cmake-ide-build-pool-use-persistent-naming t))

(with-eval-after-load 'rtags
  (defun cpp/rtags-eldoc ()
    (when (and (not (nth 4 (syntax-ppss)))
               (let ((text (thing-at-point 'symbol)))
                 (when (and text (sequencep text))
                   (set-text-properties 0 (length text) nil text))
                 text))
      (let* ((info (rtags-symbol-info-internal))
             (sym-name (cdr (assoc 'symbolName info)))
             (type (cdr (assoc 'type info))))
        (and sym-name type
             (format "%s => %s"
                     (propertize sym-name 'face
                                 'eldoc-highlight-function-argument)
                     (propertize (substring type 0 (string-match "=>" type))
                                 'face
                                 'font-lock-keyword-face))))))
  (setq rtags-display-result-backend 'ivy)
  (setq rtags-completions-enabled nil
        rtags-autostart-diagnostics nil))

(put 'cmake-ide-build-dir 'safe-local-variable #'stringp)
(with-eval-after-load 'cc-mode
  (require 'cmake-ide)

  (rtags-enable-standard-keybindings)
  ;; C-c(3) r(114) make all Upper case to lower-case
  (let ((m (assoc 114 (assoc 3 c-mode-base-map))))
    (ignore-errors
      (mapcar (lambda (x)
                (let ((k (car x)))
                  (if (and (>= k 65) (<= k 90))
                      (setf (car x) (+ k 32))
                    (if (and (>= k 97) (<= k 122))
                        (setf (car x) (- k 32))))))
              (cddr m)))))

(provide 'init-cpp)