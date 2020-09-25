;; -*- lexical-binding: t; -*-

(defvar ymacs-autoloads-file (expand-var! "autoloads.el")
  "Autoloads file")

(defvar ymacs-package--content-freshed-p nil)

(defvar ymacs-package--use-priority-p nil
  "Non-nil means to use priority defined in variable `ymacs-package--priority-alist'.
Archive with high priority will be used when install a package.")

(defvar ymacs-package--priority-alist nil "Package archive priority.")

(defvar ymacs-package--required-packages (make-hash-table)
  "All packages required.")
