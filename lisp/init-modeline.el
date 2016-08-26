(require 'spaceline)

(defface  window-numbering-face
  '((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))
  "Window number face"
  :group 'mode-line)

(defface flycheck-mode-line-off-face
  '((t :weight normal :foreground "tan"))
  "Face for the modeline in buffers with only Flycheck off"
  :group 'flycheck-faces)

(defface flycheck-mode-line-run-face
  '((t :weight normal :foreground "DeepSkyBlue"))
  "Face for the modeline in buffers with Flycheck running."
  :group 'flycheck-faces)

(defface flycheck-mode-line-error-face
  '((t :weight normal :foreground "DarkRed"))
  "Face for the modeline in buffers with only Flycheck off"
  :group 'flycheck-faces)

(defface flycheck-mode-line-ok-face
  '((t  :weight normal :foreground "DarkOliveGreen"))
  "Face for the modeline in buffers with only Flycheck off"
  :group 'flycheck-faces)

(defface flycheck-mode-line-failed-face
  '((t :weight normal :foreground "OrangeRed"))
  "Face for the modeline in buffers with only Flycheck off"
  :group 'flycheck-faces)

(defface flycheck-mode-line-result-face
  '((t :weight normal :foreground "Orange"))
  "Face for the modeline in buffers with only Flycheck off"
  :group 'flycheck-faces)

(spaceline-define-segment buffer-mod-or-tmp
  "Buffer is modified"
  (if (is-buffer-file-temp)
      "tmp"
    (when (buffer-modified-p)
      (propertize "mod"
                  'face nil
                  'help-echo "Buffer has been modified"))))

(spaceline-define-segment buffer-ro
  "Buffer is read-only"
  (when buffer-read-only
    (propertize "ro"
                'face nil
                'help-echo "Buffer is read-only")))

(spaceline-define-segment major-mode
  "Buffer major mode"
  (propertize "%m" 'face font-lock-builtin-face))

(spaceline-define-segment process
  "The process associated with buffer"
  (when (spaceline--mode-line-nonempty mode-line-process)
    (s-trim (powerline-raw mode-line-process))))

(spaceline-define-segment buffer-encoding
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

(spaceline-define-segment buffer-id
  "Buffer id include remote host"
  (let* ((host (and  default-directory
                    (file-remote-p default-directory 'host)))
         (num (window-numbering-get-number-string))
         (real-id (if host
                      (concat "%b" "@" host)
                    "%b")))
    (if (> (recursion-depth) 0)
        (concat num " %[" real-id "%]")
      (concat num " " real-id))))

(spaceline-define-segment flycheck-info
  "flycheck infomation"
  (if (and (boundp 'flycheck-mode)
           (symbol-value 'flycheck-mode))
      (let ((str (s-trim (flycheck-mode-line-status-text)))
            (msg "")
            (face nil))
        (cond ((string= str "")
               (setq msg "ok")
               (setq face 'flycheck-mode-line-ok-face))
              ((string= str "*")
               (setq msg "run")
               (setq face 'flycheck-mode-line-run-face))
              ((string= str "-")
               (setq msg "no")
               (setq face 'flycheck-mode-line-off-face))
              ((string= str "!")
               (setq msg "fail")
               (setq face 'flycheck-mode-line-failed-face))
              ((string= str "?")
               (setq msg "err")
               (setq face 'flycheck-mode-line-error-face))
              (t (setq face 'flycheck-mode-line-result-face)
                 (setq msg (s-replace ":" "" str))))
        (propertize msg 'face face))))

(spaceline-define-segment workgroup
  "workgroups2 extensions"
  (when (and (boundp 'workgroups-mode)
             (symbol-value 'workgroups-mode))
    (let ((name (condition-case nil
                    (wg-workgroup-name (wg-current-workgroup))
                  (error ""))))
      (if (string= name "")
          (setq name "none"))
      (concat "<"
              (propertize name
                          'face 'font-lock-function-name-face
                          'help-echo (format "Current workgroup is %s" name))
              ">"))))

(spaceline-define-segment version-control
  "vc info"
  (powerline-vc 'font-lock-constant-face 'r))

(defun modeline-version-ctrl ()
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if (and window-system (not powerline-gui-use-vcs-glyph))
	(format-mode-line '(vc-mode vc-mode))
      (format " %s%s"
	      (char-to-string #xe0a0)
	      (format-mode-line '(vc-mode vc-mode))))))

(spaceline-install '((buffer-id :face highlight-face)
                     (major-mode process)
                     ((buffer-mod-or-tmp
                       buffer-ro
                       buffer-encoding) :separator "|"))
                   '(version-control
                     (workgroup flycheck-info)
                     "%I(%l-%c)%p"))


(window-numbering-mode 1)
(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'init-modeline)

;;; init-modeline ends here

