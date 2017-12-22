(defvar core-apt-packages
  '("silversearcher-ag"))

(apply #'run-command "sudo" "apt" "install" core-apt-packages)
