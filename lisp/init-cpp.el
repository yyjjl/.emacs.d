(setvar!
 cpp-rtags-path (expand-var! "rtags/bin/")
 cpp-irony-path (expand-var! "irony/bin")
 cpp-has-rtags-p (file-exists-p
                  (expand-file-name "rdm" cpp-rtags-path))
 cpp-has-irony-p (file-exists-p
                  (expand-file-name "irony-server" cpp-irony-path)))

;; C/C++ I need stable version
(require-packages!
 (cmake-ide :archive "melpa-stable")
 (ggtags :when tags-has-gtags-p)
 irony
 irony-eldoc
 company-irony
 company-irony-c-headers
 flycheck-irony
 clang-format
 rtags
 ivy-rtags
 cmake-mode
 cmake-font-lock)



(defun cpp%fix-irony-eldoc ()
  (add-hook 'iedit-mode-end-hook
            (lambda ()
              (while cpp--irony-eldoc-old-overlays
                (delete-overlay (pop cpp--irony-eldoc-old-overlays))))
            nil :local))

(defun cpp%fix-cc-indent-offset ($key $val)
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
    (when (and root (or $force
                        (y-or-n-p (format "Delete %s"
                                          (abbreviate-file-name root)))))
      (let ((default-directory root))
        (shell-command (format "%s -W %s"
                               (expand-file-name "rc" rtags-path)
                               default-directory))
        (cpp/c++-simple-setup)))))

(defun cpp/common-cc-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)"
  (turn-on-auto-fill)
  (setq c-basic-offset 4)
  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)
  ;; (c-toggle-auto-newline 1)
  ;; indent
  (highlight-indentation-set-offset 4)
  (cpp%fix-cc-indent-offset 'innamespace [0])
  (cpp%fix-cc-indent-offset 'substatement-open 0)
  (cpp%fix-cc-indent-offset 'func-decl-cont 0)
  (cpp%fix-cc-indent-offset 'case-label 4))

(defun cpp/rtags-setup ()
  (setq-local eldoc-documentation-function 'cpp/rtags-eldoc)
  (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "M-,") 'rtags-location-stack-back)
  (local-set-key (kbd "M-n") 'rtags-next-match)
  (local-set-key (kbd "M-p") 'rtags-previous-match)
  (local-set-key (kbd "C-c .") 'rtags-symbol-type)
  (local-set-key (kbd "C-c C-k") 'cpp/remove-project-rtags-cache)
  (when cpp-has-irony-p (irony-eldoc -1))
  (when tags-has-gtags-p (ggtags-mode -1)))

(defun cpp/try-misc-setup ()
  (interactive)
  (if cpp-has-rtags-p
      (if cpp-cmake-ide-enabled
          (progn
            (cmake-ide-run-cmake)
            (cpp/rtags-setup))
        (when (cpp/rtags-indexing-with-makefile)
          (message "Rtags is ready !!")
          (cpp/rtags-setup)))
    (message "Rtags not found !!!"))
  (when cpp-has-irony-p
    (unless (featurep 'company-irony-c-headers)
      (require 'company-irony-c-headers))
    (setq-local cc-search-directories
                (company-irony-c-headers--resolved-search-paths t))
    (message "Setting CC search directories done !!")))

(defun cpp/c++-simple-setup ()
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

(defun cpp/load-file-in-root ()
  (interactive)
  (cond
   ((file-remote-p default-directory)
    (message "Not support in remove sever !"))
   ((not (executable-find "root"))
    (message "Executable `root' not found !"))
   ((not (buffer-file-name))
    (message "Buffer has no file !"))
   (t
    (let* ((file (buffer-file-name))
           (buffer-name (format "*root:%s*" (buffer-name)))
           (buffer (get-buffer-create buffer-name))
           (proc (get-buffer-process buffer)))
      (if (and proc
               (process-live-p proc)
               (eq (buffer-local-value 'major-mode buffer)
                   'term-mode))
          (progn
            (with-current-buffer buffer
              (term-send-raw-string (format ".X %s\n" file))))
        (kill-buffer buffer)
        (setq buffer
              (term/exec-program "root"
                                 (list "-l" (or file ""))
                                 buffer-name)))
      (pop-to-buffer buffer)))))

(setq hide-ifdef-mode-prefix-key (kbd "C-c h"))
(defun cpp/c++-setup ()
  "C/C++ only setup"
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c b") 'clang-format-buffer)
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-v") 'semantic-decoration-include-visit)
  (local-set-key (kbd "C-c C-c") 'cmake-ide-run-cmake)
  (local-set-key (kbd "C-c C-l") 'cpp/load-file-in-root)
  (local-set-key [f9] 'cpp/try-misc-setup)
  (local-set-key [f10] 'cpp/compile)
  (local-set-key [f5] 'gdb)

  (hide-ifdef-mode 1)

  (modern-c++-font-lock-mode 1)

  (when tags-has-gtags-p
    (ggtags-mode 1)
    (setq completion-at-point-functions '(t)))

  ;; Make a #define be left-aligned
  (setq c-electric-pound-behavior '(alignleft))
  (if (derived-mode-p 'c++-mode)
      (setq-local flycheck-clang-language-standard "c++14")
    (setq flycheck-clang-language-standard nil))

  (unless (or (file-remote-p default-directory)
              (bound-and-true-p cpp-setup-literally)
              (> (buffer-size) core-large-buffer-size))
    (when cpp-has-irony-p
      (add-to-list 'company-backends #'company-irony)
      (add-to-list 'company-backends #'company-irony-c-headers)
      (add-to-list 'company-backends #'company-files)

      (irony-mode 1)
      (cpp%fix-irony-eldoc))
    ;; Make sure rdm is running
    (when cpp-has-rtags-p
      (rtags-start-process-unless-running))

    (if (cmake-ide--locate-cmakelists)
        (progn
          (setq-local cpp-cmake-ide-enabled t)
          (cpp/rtags-setup))
      (setq cmake-ide-build-dir nil)
      (cpp/c++-simple-setup))))

;; Do not use `c-mode-hook' and `c++-mode-hook', there is a bug
(defvar-local cpp--initialized-p nil)
(define-hook! cpp|common-setup (c-mode-common-hook)
  (unless cpp--initialized-p
    (setq cpp--initialized-p t)
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
        '("-Wall" "-std=c++14"))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(with-eval-after-load 'irony-eldoc
  (defvar cpp--irony-eldoc-old-overlays nil)
  (let ((hook (lambda (ov &rest _)
                (if iedit-mode
                    ;; delay deletion of an overlay
                    (add-to-list 'cpp--irony-eldoc-old-overlays ov)
                  (delete-overlay ov)))))
    (put 'irony-eldoc 'modification-hooks (list hook))
    (put 'irony-eldoc 'insert-in-front-hooks (list hook))
    (put 'irony-eldoc 'insert-behind-hooks (list hook))))

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
  ;; Smart tab
  (advice-add 'c-indent-line-or-region :around #'core%indent-for-tab)

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
