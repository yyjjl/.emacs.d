(require 'package)

;;---------------------------------------
;; Patch up annoying package.el quirks
;;---------------------------------------
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat
                                 ;; name is string when emacs <= 24.3.1,
                                 (if (symbolp name) (symbol-name name) name)
                                 "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))

;;---------------------------------------
;; On-demand installation of packages
;;---------------------------------------
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun package-set-right-archive (pkg)
  "get right archive content by priority"
  (let ((pkg-info (cdr pkg))
        cur-archive
        (cur-priority -1))
    (dolist (info pkg-info)
      (let ((pri (assoc-string (package-desc-archive info)
                               package-archive-priority)))
        (when (> (if pri (cdr pri) 0) cur-priority)
          (setq cur-archive info)
          (setq cur-priority (if pri (cdr pri) 0)))))
    (setf (cdr pkg) (list cur-archive))))

(defadvice package-read-all-archive-contents
    (after manager-packages-with-priority activate)
  (when package-use-priority
    (dolist (pkg package-archive-contents)
      (package-set-right-archive pkg))))

;;---------------------------------------
;; Standard package repositories
;;---------------------------------------

;; We include the org repository for completeness, but don't use it.
;; Lock org-mode temporarily:
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;; uncomment below line if you need use GNU ELPA
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defvar package-archive-priority
  '(("melpa" . 1)
    ("melpa-stable" . 2)
    ("gnu" . 0))
  "package archive priority")

;;---------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;---------------------------------------

(package-initialize)

(let (freshed-p)
  (dolist (pkg required-packages)
    (when (consp pkg)
      (setq pkg (car pkg)))
    (unless (package-installed-p pkg)
      (unless freshed-p
        (setq freshed-p t)
        (package-refresh-contents))
      (package-install pkg))))

(provide 'init-elpa)
