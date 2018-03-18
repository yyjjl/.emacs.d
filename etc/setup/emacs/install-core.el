(defvar core-apt-packages
  '("sdcv"
    "xsel"
    "aspell"
    "git"))

(mapc #'apt/install core-apt-packages)
