;;; early-init.el -*- lexical-binding: t; -*-
(defvar my/saved-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      garbage-collection-messages nil)

(add-hook 'after-init-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold (* 60 1024 1024)
                    gc-cons-percentage 0.1
                    file-name-handler-alist my/saved-file-name-handler-alist)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold (* 60 1024 1024))))

(setq mode-line-format nil
      default-frame-alist
			'((menu-bar-lines . 0)
        (alpha 95 95)
			  (tool-bar-lines . 0)
			  (vertical-scroll-bars)
			  (left-fringe . 0)
			  (right-fringe . 0)
			  (internal-border-width . 8)
        (font . "Victor Mono-13:weight=semi-bold")))

;; Set theme and font beforehand to prevent flickering
(load-theme 'dracula :no-confirm)
(set-face-attribute 
 'variable-pitch nil :family "Noto Sans" :weight 'regular :height 130)

(fset 'display-startup-echo-area-message 'ignore)

(setq load-prefer-newer t
      package-enable-at-startup nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil
      create-lockfiles nil
      inhibit-startup-screen t
      frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here
