;;; app/music/config.el -*- lexical-binding: t; -*-

;; https://pspiagicw.github.io/posts/the-weirdest-mode-in-emacs-mpc-mode/
(use-package! mpc
  :config
  (map! :map mpc-mode-map
        :ni "p" #'mpc-toggle-play
        :ni "q" #'mpc-quit
        :ni "+" #'mpc-volume-mouse-set ;; increases
        ;; ("-" . ...)                 ;; how to decrease
        :ni ">" #'mpc-next
        :ni "<" #'mpc-prev
        :ni "f" #'mpc-ffwd             ;; toggles
        :ni "b" #'mpc-rewind           ;; toggles
        :ni "u" #'mpc-update)
  )
