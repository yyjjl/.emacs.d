(require 'package)

(when (>= emacs-major-version 25)
  ;; Do not save to init.el
  (fset 'package--save-selected-packages
        (lambda ($value)
          (when $value (setq package-selected-packages $value)))))

(defvar package-use-priority? nil
  "Non-nil means to use priority defined in variable `package|priority-alist'.
Archive with high priority will be used when install a package.")

(defvar package--priority-alist nil
  "Package archive priority.")

(defvar package--required-packages (make-hash-table)
  "All packages required.")

;; The index of archive represents its priority
(setq package-archives
      '(("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu"   . "https://elpa.emacs-china.org/gnu/")))
;; (setq package-archives
;;       '(("melpa-stable" . "https://stable.melpa.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("org" . "https://orgmode.org/elpa/")
;;         ("gnu"   . "https://elpa.gnu.org/packages/")))

;; Setup to select right archive
(setq package--priority-alist (mapcar #'car package-archives))

(defun package*autoclose-autoloads ($name $pkg-dir)
  "Auto close *-autoloads.el after a package installed."
  (let ((buf (find-file-existing
               (expand-file-name (concat (if (symbolp $name)
                                             (symbol-name $name)
                                           $name)
                                         "-autoloads.el")
                                 $pkg-dir))))
    (when buf (kill-buffer buf))))
(advice-add 'package-generate-autoloads :after #'package*autoclose-autoloads)

(defsubst package%archive-priority ($archive)
  (length (member (package-desc-archive $archive) package--priority-alist)))

(defun package%get-archive ($name $archives)
  (let (archive tmp)
    (while (and $archives (not archive))
      (setq tmp (pop $archives))
      (when (string= $name (package-desc-archive tmp))
        (setq archive tmp)))
    archive))

(defun package%set-archive ($pkg &optional $archive-name)
  "Set right archive content for PKG. "
  (let ((pkg-archives (cdr $pkg)) archive)
    (when $archive-name
      (setq archive (package%get-archive $archive-name pkg-archives)))
    (when (and (not archive) package-use-priority?)
      (setq archive (car (sort pkg-archives
                                (lambda ($pkg1 $pkg2)
                                  (> (package%archive-priority $pkg1)
                                     (package%archive-priority $pkg2)))))))
    (when archive
      (setf (cdr $pkg) (list archive)))))

(defun package*after-read-contents ()
  (dolist (pkg package-archive-contents)
    (package%set-archive pkg (gethash (car pkg)
                                       package--required-packages))))
(advice-add 'package-read-all-archive-contents
            :after #'package*after-read-contents)

(defvar package--content-freshed? nil)
(defun require! ($pkg-name &optional $archive $location)
  (cond ((eq $location 'built-in) t)
        ($location (add-to-list 'load-path $location))
        (t
         (puthash $pkg-name $archive package--required-packages)
         (unless (package-installed-p $pkg-name)
           (unless package--content-freshed?
             (package-refresh-contents)
             (setq package--content-freshed? t))
           (message "Installing package `%s' ..." $pkg-name)
           (let ((inhibit-message t))
             (package-install $pkg-name))))))

(defmacro require-packages! (&rest pkg-list)
  (declare (indent nil))
  (let (forms)
    (dolist (pkg pkg-list)
      (push (if (atom pkg)
                `(require! ',pkg)
              (let ((when-form (plist-get (cdr pkg) :when))
                    (form `(require! ',(car pkg)
                                     ,(plist-get (cdr pkg) :archive)
                                     ,(plist-get (cdr pkg) :location))))
                (if when-form
                    `(if ,when-form ,form)
                  form)))
            forms))
    `(progn
       ,@(nreverse forms))))

(defun core/compile-config (&optional $no-message?)
  (interactive "P")
  (message "Compile configuration files ...")
  (dolist (file (append
                 (directory-files emacs-config-directory :full "\\.el$")
                 (directory-files-recursively emacs-private-directory
                                              "\\.el$")
                 (list user-init-file
                       custom-file)))
    (when file
      (condition-case err
          (let ((inhibit-message $no-message?))
            (byte-compile-file file))
        (error (message "Error: %s" err)
               (backtrace)))))
  (message "Compile finished"))

(package-initialize)

;; ----------------------------------------
;; Core packages
;; ----------------------------------------
(require-packages!
 yasnippet
 yasnippet-snippets
 (flycheck :archive "melpa-stable")
 ;; Code completion framework
 company
 company-statistics
 ;; Save session to disk
 session
 ;; Improve `term-mode'
 multi-term
 hydra
 ivy
 counsel
 ivy-hydra
 swiper
 projectile
 counsel-projectile
 ;; `counsel-M-x' need smex to get history
 smex
 ;; Show key bindings when pressing
 which-key
 (fcitx :when emacs-use-fcitx-p)
 ;; Numbering windows
 window-numbering
 ;; Highlight braces with their depth
 rainbow-delimiters
 ;; Highlight indentation
 highlight-indentation
 ;; ^L beautifier
 page-break-lines
 evil-nerd-commenter
 shackle
 expand-region)

(require 'core-ivy)
(require 'core-company)
(require 'core-popups)
(require 'core-term)
(require 'core-semantic)
(require 'core-hideshow)
(require 'core-misc)
(require 'core-hydra)

(define-hook! package|after-init-hook (after-init-hook)
  (setq package-selected-packages
        (hash-table-keys package--required-packages))
  (add-to-list 'recentf-exclude (file-truename package-user-dir)))

(define-hook! core|enable-modes-hook (after-init-hook)
  (shackle-mode 1)
  (recentf-mode 1)
  (session-initialize)
  (winner-mode 1)
  (ivy-mode 1)
  (counsel-mode 1)
  (projectile-mode 1)
  (counsel-projectile-mode 1)
  (yas-global-mode 1)
  (global-company-mode 1)

  ;; global-modes
  (global-subword-mode 1)
  (global-page-break-lines-mode 1)

  (column-number-mode 1)
  (show-paren-mode 1)
  ;; Auto insert closing pair
  (electric-pair-mode 1)
  (electric-layout-mode 1)
  (electric-indent-mode 1)
  ;;`eldoc', show API doc in minibuffer echo area enabled by default
  ;; (global-eldoc-mode 1)

  (run-with-idle-timer
   1 nil
   (lambda ()
     (when (and (display-graphic-p) emacs-use-fcitx-p)
       (fcitx-aggressive-setup))))

  (which-key-mode 1)
  (core/enable-semantic))

(provide 'core-packages)
