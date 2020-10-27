;;; os/exwm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun exwm/get-vol ()
  (setq VOL (shell-command-to-string "amixer get Master | grep '%' | head -n 1 | cut -d '[' -f 2 | cut -d '%' -f 1"))
  )


;;;###autoload
(defun exwm/get-brightness ()
  (setq BRI (shell-command-to-string "xbacklight | cut -c 1-2"))
  )


;;;###autoload
(defun exwm/volume-increase ()
  (interactive)
  (eshell-command "amixer -D pulse set Master 5%+ unmute > /dev/null")
  (exwm/get-vol)
  (eshell-command "dunstify -r 423423 Volume: $VOL")
  )


;;;###autoload
(defun exwm/volume-decrease ()
  (interactive)
  (eshell-command "amixer -D pulse set Master 5%- unmute > /dev/null")
  (exwm/get-vol)
  (eshell-command "dunstify -r 423423 Volume: $VOL > /dev/null")
  )


;;;###autoload
(defun exwm/mute ()
  (interactive)
  (eshell-command "amixer -D pulse set Master Playback Switch toggle")
  (exwm/get-vol)
  ;; FIXME
  (if (string= VOL "0")
      (eshell-command "notify-send Mute")
    (eshell-command "notify-send Unmute")
    )
  )


;;;###autoload
(defun exwm/brightness-up ()
  (interactive)
  (call-process-shell-command "xbacklight -inc 5")
  (exwm/get-brightness)
  (eshell-command "dunstify -r 123123 Brightness: $BRI > /dev/null")
  )


;;;###autoload
(defun exwm/brightness-down ()
  (interactive)
  (call-process-shell-command "xbacklight -dec 5")
  (exwm/get-brightness)
  (eshell-command "dunstify -r 123123 Brightness: $BRI > /dev/null")
  )

