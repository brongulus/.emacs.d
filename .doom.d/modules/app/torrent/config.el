;;; app/torrent/config.el -*- lexical-binding: t; -*-

;; TODO
;; [ ] Add progress bar
;;
(use-package! transmission
  :defer t
  :init
  (after! evil-snipe
    (add-to-list 'evil-snipe-disabled-modes 'transmission-mode)
    )
  :config
  (setq transmission-refresh-modes '(transmission-mode
                                     transmission-files-mode
                                     transmission-info-mode
                                     transmission-peers-mode))
  (map! :map transmission-mode-map
        :n "s" #'transmission-stats)
  )
