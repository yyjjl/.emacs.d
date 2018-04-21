(eval-when-compile
  (require 'dash))

(defmacro define-exwm-command-group! ($name $key $title &rest $bindings)
  (when (symbolp $name)
    (setq $name (symbol-name $name)))
  (let ((function-name (intern (format "exwm/command-group-%s" $name)))
        (map-name (intern (format "exwm/command-group-%s-map" $name))))
    `(progn
       (defvar ,map-name (make-sparse-keymap))
       (define-key! :map ,map-name
         ([t] . (lambda!))
         ("C-g" . exit-minibuffer)
         ("SPC" . exit-minibuffer)
         ,@(--map
            (let ((body (cdr it)))
              (if (eq :command (car-safe body))
                  (cons (car it)
                        `(lambda!
                           (start-process-shell-command ,$name nil ,(cadr body))
                           (exit-minibuffer)))
                it))
            $bindings))
       (defun ,function-name ()
         (interactive)
         (read-from-minibuffer ,$title nil ,map-name))
       (exwm-input-set-key ,(if (stringp $key) (kbd $key) $key) #',function-name))))

(define-exwm-command-group! display "s-x"
  "External Screen (l)left/(r)right of Main Screen, (s)ame, (a)uto "
  ("l" :command "xrandr --auto --output eDP-1 --right-of HDMI-1")
  ("r" :command "xrandr --auto --output eDP-1 --left-of HDMI-1")
  ("a" :command "xrandr --auto && xrandr")
  ("s" :command "xrandr --auto --output eDP-1 --output HDMI-1 --same-as eDP-1"))

(define-exwm-command-group! system [pause]
  "(e) logout, (h) hibernate, (r) reboot, (S) shutdown "
  ("e" . (lambda! (unwind-protect (save-buffers-kill-emacs) (exit-minibuffer))))
  ("r" :command "systemctl reboot")
  ("S" :command "systemctl poweroff -i"))

(define-exwm-command-group! apps "s-o"
  "(g)Chrome, (n)Nautilus, (p)Evince, (o)Emacs, (h)Zeal, (s)Settings "
  ("g" :command "google-chrome")
  ("n" :command "nautilus")
  ("p" :command "evince")
  ("s" :command "gnome-control-center"))

;; Autostart
(exwm%run-command "nm-applet")
(exwm%run-command "gnome-keyring-daemon")
(exwm%run-command "gnome-keyring-daemon -s -c secrets")
(exwm%run-command "emacs --daemon=exwm-bg -q")
(exwm%run-command "emacsclient -s exwm-bg -e \"(load \\\"~/.emacs.d/init-i3wm.el\\\")\"")

(provide 'exwm-apps)
