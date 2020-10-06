;;; app/torrent/config.el -*- lexical-binding: t; -*-

(use-package! transmission
   ;; :init
   :config
   (setq transmission-refresh-modes '(transmission-mode
                                      transmission-files-mode
                                      transmission-info-mode
                                      transmission-peers-mode))
   )
