;;; early-init.el -*- lexical-binding: t; -*-
(defvar my/saved-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      garbage-collection-messages nil)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold (* 60 1024 1024)
                    gc-cons-percentage 0.1
                    file-name-handler-alist my/saved-file-name-handler-alist)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold (* 60 1024 1024))))

(setq-default mode-line-format nil
              frame-inhibit-implied-resize t
              default-frame-alist
			        '((menu-bar-lines . 0)
                ;; (alpha 95 95)
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
(set-face-attribute
 'fixed-pitch-serif nil :family "Victor Mono" :inherit 'default)

(fset 'display-startup-echo-area-message 'ignore)

(setq load-prefer-newer t
      frame-resize-pixelwise t
      package-enable-at-startup nil
      package--init-file-ensured t ;; doom
      auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil
      create-lockfiles nil
      inhibit-startup-screen t
      bidi-inhibit-bpa t)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(provide 'early-init)
;;; early-init.el ends here
