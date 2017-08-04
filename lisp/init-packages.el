(setq package-enable-at-startup nil)
(require 'package)
(require 'subr-x)
(eval-when-compile
  (require 'cl))

;; add site-package's path to `load-path'
(if (fboundp 'normal-top-level-add-to-load-path)
    (dolist (dir (directory-files emacs|site-packages-directory))
      (unless (string-match "^\\." dir)
        (push (expand-file-name dir emacs|site-packages-directory) load-path))))

;; (setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;                          ("melpa-stable" . "http://stable.melpa.org/packages/")
;;                          ;; uncomment below line if you need use GNU ELPA
;;                          ("gnu" . "http://elpa.gnu.org/packages/")))
;; Use mirror in China
;; The index of archive represent its priority
(setq package-archives
      '(("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("gnu"   . "https://elpa.emacs-china.org/gnu/")))

(defvar package|use-priority-p nil
  "Non-nil means to use priority defined in variable `package|priority-alist'.
Archive with high priority will be used when install a package.")

(defvar package|priority-alist nil "Package archive priority.")
(defvar package|required-packages (make-hash-table) "All packages required.")

(defun package|autoclose-autoloads (name pkg-dir)
  "Auto close *-autoloads.el after a package installed."
  (let ((buf (find-file-existing
              (expand-file-name (concat (if (symbolp name)
                                            (symbol-name name)
                                          name)
                                        "-autoloads.el")
                                pkg-dir))))
    (when buf (kill-buffer buf))))

(defvar package|content-freshed-p nil)
(defun package|require (pkg-name &optional archive location)
  (cond ((eq location 'built-in) t)
        (location (add-to-list 'load-path location))
        (t
         (puthash pkg-name archive package|required-packages)
         (unless (package-installed-p pkg-name)
           (unless package|content-freshed-p
             (package-refresh-contents)
             (setq package|content-freshed-p t))
           (message "Installing package '%s' ..." pkg-name)
           (package-install pkg-name)
           (message "Package'%s' installed " pkg-name)))))

(defsubst package|archive-priority (archive)
  (length (member (package-desc-archive archive) package|priority-alist)))

(defun package|get-archive (name archives)
  (unless (null archives)
    (if (string= name (package-desc-archive (car archives)))
        (car archives)
      (package|get-archive name (cdr archives)))))

(defun package|set-archive (pkg &optional archive-name)
  " Set right archive content for PKG. "
  (let ((pkg-archives (cdr pkg)) archive)
    (when archive-name
      (setq archive (package|get-archive archive-name pkg-archives)))
    (when (and (not archive) package|use-priority-p)
      (setq archive (car (sort pkg-archives
                               (lambda (pkg1 pkg2)
                                 (> (package|archive-priority pkg1)
                                    (package|archive-priority pkg2)))))))
    (when archive
      (setf (cdr pkg) (list archive)))))

(defun package|after-read-contents ()
  (dolist (pkg package-archive-contents)
    (package|set-archive pkg (gethash (car pkg) package|required-packages))))

;; Setup to select right archive
(when package|use-priority-p
  (setq package|priority-alist (mapcar #'car package-archives)))
(advice-add 'package-generate-autoloads
            :after #'package|autoclose-autoloads)
(advice-add 'package-read-all-archive-contents
            :after #'package|after-read-contents)

(package-initialize)

(when (>= emacs-major-version 25)
  ;; Do not save to init.el
  (fset 'package--save-selected-packages
        (lambda (value)
          (when value
            (setq package-selected-packages value))))
  (setq package-selected-packages (hash-table-keys package|required-packages)))

;; ----------------------------------------
;; Core packages
;; ----------------------------------------
(package|require 'yasnippet)
(package|require 'auto-yasnippet)
;; Code completion framework
(package|require 'company)
(package|require 'company-statistics)
(package|require 'flycheck " melpa-stable ")
;; Save session to disk
(package|require 'session)
;; Improve `term-mode'
(package|require 'multi-term)
(package|require 'hydra)
(package|require 'ivy)
(package|require 'swiper)
(package|require 'counsel)
(package|require 'projectile)
(package|require 'counsel-projectile)
;; `counsel-M-x' need smex to get history
(package|require 'smex)
;; Delete packages safely
;; In Emacs 25 has builtin support
;; (package|require 'package-safe-delete)
;; Show key bindings when pressing
(package|require 'which-key)
(package|require 'color-theme)
;; Numbering windows
(package|require 'window-numbering)
;; Manage popup windows
(package|require 'popwin)
;; Input method
(package|require 'fcitx)
;; Highlight braces with their depth
(package|require 'rainbow-delimiters)
;; Highlight indentation
(package|require 'highlight-indentation)
;; ^L beautifier
(package|require 'page-break-lines)
;; Show information in header-line for `semantic-mode'
;; It's too slow, when file is large
;; (package|require 'stickyfunc-enhance)
(package|require 'xterm-color)
(package|require 'unicode-fonts)
(package|require 'beginend)

(defhook package|after-init (after-init-hook)
  (setq package-selected-packages
        (hash-table-keys package|required-packages)))

(provide 'init-packages)
