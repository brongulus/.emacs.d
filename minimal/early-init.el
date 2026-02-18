;;; early-init.el -*- lexical-binding: t; -*-
(defvar my/saved-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      file-name-handler-alist nil
      vc-handled-backends '(Git))
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (run-at-time
               2 nil
               (lambda nil            
                 (setq gc-cons-threshold (* 32 1024 1024)
                       gc-cons-percentage 0.1
                       file-name-handler-alist my/saved-file-name-handler-alist))))
          105)
(setq inhibit-redisplay t)
(setq inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq inhibit-redisplay nil inhibit-message nil)
            (redisplay))
          105)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(setq initial-buffer-choice nil
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      package-enable-at-startup nil
      ns-pop-up-frames nil
      inhibit-compacting-font-caches t
      frame-inhibit-implied-resize t
      redisplay-skip-fontification-on-input t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq default-frame-alist '((internal-border-width . 12) (undecorated-round . t)
                            (menu-bar-lines . 0) (tool-bar-lines . 0) (left-fringe . 8)
                            (right-fringe . 8) (vertical-scroll-bars . nil)
                            (horizontal-scroll-bar . nil)))
(fset 'display-startup-echo-area-message 'ignore)
(push '(fullscreen . maximized) initial-frame-alist)
