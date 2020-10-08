;;; app/torrent/config.el -*- lexical-binding: t; -*-

(use-package! transmission
  ;;:init
  ;;(make-process
  ;; :name "Transmission Daemon"
  ;; :buffer nil
  ;; :command '("transmission-daemon")
  ;; )
  ;;(setq evil-snipe-disabled-modes transmission-mode)
  :config
  (setq transmission-refresh-modes '(transmission-mode
                                     transmission-files-mode
                                     transmission-info-mode
                                     transmission-peers-mode))
  ;; (map!
  ;;       :map transmission-mode-map
  ;;       "s" #'transmission-stats)
  )
