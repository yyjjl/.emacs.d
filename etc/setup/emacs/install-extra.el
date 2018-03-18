(defvar extra-apt-packages
  '("fcitx"
    "mplayer"))

(mapc #'apt/install extra-apt-packages)
