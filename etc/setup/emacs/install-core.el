(defvar core-apt-packages
  '("silversearcher-ag"))

(defun apt/package-installed-p (pkg)
  (string-prefix-p "ii"
                   (shell-command-to-string
                    (format "dpkg-query -l %s |grep ^ii" pkg))))

(dolist (pkg core-apt-packages)
  (if (apt/package-installed-p pkg)
      (message "Package %s is already installed" pkg)
    (run-command "sudo" "apt" "install" pkg)))
