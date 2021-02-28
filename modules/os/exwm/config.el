;;; os/exwm/config.el -*- lexical-binding: t; -*-

;; DEPENDENCIES
;; 1. xbacklight
;; 2. pulse audio
;; 3. alsamixer
;; 4. networkmanager (nm-applet)


;; TODO
;; [ ] FIXME eshell-command output crap
;; [ ] Add keybindings under the s- leader?
;; [ ] Start Workspaces from 1 ffs
;; [ ] Add COPY PASTE commands ASAP
;; [ ] Improve workspace switching (Model Golden Ratio Partitioning?)
;; [ ] Figure out the autoload situation
;; [ ] Add volume and brightness Meters in either modeline, or preferably exwm dashboard
;; [ ] Fix opening links
;; [ ] Remap Move buffer to workspace C-c RET to s-shift-X
;; [X] Fix Dunst notification spam
;; [ ] Add support for polybar under a flag

;;(eshell-command "setq stringvar $XDG_CURRENT_DESKTOP")
;;(if (string= stringvar "EXWM")

(when (get-buffer "*window-manager*")
  (kill-buffer "*window-manager*"))
(when (get-buffer "*window-manager-error*")
  (kill-buffer "*window-manager-error*"))
(when (executable-find "wmctrl")
  (shell-command "wmctrl -m ; echo $?" "*window-manager*" "*window-manager-error*"))


  (when (and (get-buffer "*window-manager-error*")
             (eq window-system 'x))
(use-package exwm
      :config

      (require 'exwm-config)
      (exwm-config-example)

      (defun exwm/polybar-exwm-workspace ()
        (pcase exwm-workspace-current-index
          (0 "")
          (1 "I")
          (2 "II")
          (3 "III")
          (4 "IV")))

      ;;; Display Configurations
      (require 'exwm-randr)
      (setq exwm-randr-workspace-monitor-plist '(0 "DP-4"))
      (add-hook 'exwm-randr-screen-change-hook
                (lambda()
                  (start-process-shell-command
                   "xrandr" nil "xrandr --output DP-4 --mode 1920x1080 --pos 0x0 --rotate normal")))
      (exwm-randr-enable)

      ;;startup process
      (start-process-shell-command "feh" nil "feh --bg-scale ~/Pictures/scenery.png")
      (start-process-shell-command "picom" nil "picom")
      (start-process-shell-command "gnome-authentication" nil "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1")

      ;; Fix for ivy posframe bing covered by X windows
      (after! ivy-posframe
      (setq ivy-posframe-parameters '((parent-frame nil)))
      )

      ;; Hide modeline on X windows
      (add-hook 'exwm-manage-finish-hook #'hide-mode-line-mode)


      ;;; Systemtray
      (require 'exwm-systemtray)
      (exwm-systemtray-enable)
      (server-start)
      (call-process-shell-command "nm-applet" nil 0)
      ;; (call-process-shell-command "killall -q polybar; polybar example &" nil "polybar panel")
      (call-process-shell-command "mpd > /dev/null" nil 0)
      ;; FIXME Multiple Instances
      ;; (call-process-shell-command "clipit" nil 0)

      ;; Function Definitions (Moved to autoload.el)

      ;; Window split on new buffer

      ;;; Keybindings
      (setq exwm-input-global-keys
            `(
              ;; Open app launcher
              ([?\s-x] . counsel-linux-app)
              ;; Buffer list
              ([?\s-b] . ivy-switch-buffer)
              ;; App shortcuts (ADD YOUR OWN HERE)
              ([s-f2] . (lambda ()
                          (interactive)
                          (eshell-command "start-process-shell-command vivaldi nil vivaldi-stable")))
              ([s-f4] . (lambda ()
                          (interactive)
                          (eshell-command "start-process-shell-command discord nil discord")))
              ([s-return] . (lambda ()
                              (interactive)
                              (eshell-command "urxvt > /dev/null")))
              ;; Take screenshots (saved in pwd)
              ([print] . (lambda ()
                           (interactive)
                           (eshell-command "scrot -s")))
              ;; Lock
              ([?\s-l] . (lambda ()
                         (interactive)
                         (start-process-shell-command "lock" nil "i3lock -i ~/Downloads/neon.png")))
              ;; Brightness and Volume Controls
              ([XF86AudioRaiseVolume] . exwm/volume-increase)
              ([XF86AudioLowerVolume] . exwm/volume-decrease)
              ([XF86AudioMute] . exwm/mute)
              ([XF86MonBrightnessUp] . exwm/brightness-up)
              ([XF86MonBrightnessDown] . exwm/brightness-down)
              ;; Window Focus
              ([s-right] . windmove-right)
              ([s-left] . windmove-left)
              ([s-up] . windmove-up)
              ([s-down] . windmove-down)
              ([C-s-left] . windmove-swap-states-left)
              ([C-s-right] . windmove-swap-states-right)
              ([C-s-up] . windmove-swap-states-up)
              ([C-s-down] . windmove-swap-states-down)
              ([?\s-f] . exwm-layout-toggle-fullscreen)
              ([?\s-q] . kill-this-buffer)
   ;;           ([s-space] . exwm-floating-toggle-floating)

              ;; TODO Add window split shortcuts, and possibly improve it
              ;; 's-r': Reset (to line-mode).
              ([?\s-r] . exwm-reset)
              ;; 's-c': Switch to char-mode
              ([?\s-c] . exwm-input-release-keyboard)
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
      (map! exwm-mode-map
            "C-q" 'exwm-input-send-next-key)

      )
)
;;  nil
;;  )

