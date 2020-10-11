;;; +exwm.el -*- lexical-binding: t; -*-

;; TODO
;; [ ] Use elvish in vterm
;; [ ] Improve buffer and workspace switching
;; [ ] Figure out the autoload situation
;; [ ] Add volume and brightness Meters in either modeline, or preferably exwm dashboard

(use-package exwm
      :config

      (require 'exwm-config)
      (exwm-config-example)
      (require 'exwm-randr)

      (setq exwm-randr-workspace-monitor-plist '(0 "DP-4"))
      (add-hook 'exwm-randr-screen-change-hook
                (lambda()
                  (start-process-shell-command
                   "xrandr" nil "xrandr --output DP-4 --mode 1920x1080 --pos 0x0 --rotate normal")))
      (exwm-randr-enable)

      (require 'exwm-systemtray)
      (exwm-systemtray-enable)

      (defun volume-increase ()
        (interactive)
        (call-process-shell-command "amixer -D pulse set Master 5%+ unmute&" nil 0)
        )
      (defun volume-decrease ()
        (interactive)
        (call-process-shell-command "amixer -D pulse set Master 5%- unmute&" nil 0)
        )
      (defun mute()
        (interactive)
        (call-process-shell-command "amixer -D pulse set Master Playback Switch toggle&" nil 0)
        )
      (defun brightness-up ()
        (interactive)
        (call-process-shell-command "xbacklight -inc 5 && xbacklight > /home/prashant/.config/brightness")
        )
      (defun brightness-down ()
        (interactive)
        (call-process-shell-command "xbacklight -dec 5 && xbacklight > /home/prashant/.config/brightness")
        )
      ;;(map! :niv "<XF86AudioRaiseVolume>" #'volume-increase
      ;;      :niv "<XF86AudioLowerVolume>" #'volume-decrease
      ;;      :niv "<XF86AudioMute>" #'mute
      ;;      :niv "<XF86MonBrightnessUp>" #'brightness-up
      ;;      :niv "<XF86MonBrightnessDown>" #'brightness-down
      ;;      )
      (setq exwm-input-global-keys
            `(
              ;; 's-x': Open app launcher
              ([?\s-x] . counsel-linux-app)
              ;; Brightness and Volume Controls
              ([XF86AudioRaiseVolume] . volume-increase)
              ([XF86AudioLowerVolume] . volume-decrease)
              ([XF86AudioMute] . mute)
              ([XF86MonBrightnessUp] . brightness-up)
              ([XF86MonBrightnessDown] . brightness-down)
              ;; Window Focus
              ([s-right] . windmove-right)
              ([s-left] . windmove-left)
              ([s-up] . windmove-up)
              ([s-down] . windmove-down)
              ;; TODO Add window split shortcuts, and possibly improve it
              ;; 's-r': Reset (to line-mode).
              ([?\s-r] . exwm-reset)
              ;; 's-w': Switch workspace.
              ([?\s-w] . exwm-workspace-switch)
              ;; 's-&': Launch application.
              ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
              ;; 's-N': Switch to certain workspace.
              ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                        (number-sequence 0 9))
              )
            )
   )
