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

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (garbage-collect)
              (setq gc-cons-threshold (* 60 1024 1024))))

(setq-default mode-line-format nil
              frame-inhibit-implied-resize t
              default-frame-alist
              '((menu-bar-lines . 0)
                (alpha 95 95)
                (tool-bar-lines . 0)
                (vertical-scroll-bars)
                (left-fringe . 0)
                (right-fringe . 0)
                (internal-border-width . 8)
                (font . "Victor Mono-13.5:weight=semi-bold")))

;; Set theme and font beforehand to prevent flickering
(defun get-preferred-theme () ;; ymarco
  (let ((hour (string-to-number
               (substring (current-time-string) 11 13))))
    (if (<= 7 hour 18)
        'modus-operandi
      'dracula)))
(setq current-theme (get-preferred-theme))
(load-theme current-theme :no-confirm)
(run-with-timer (* 60 60) (* 60 60)
                (defun update-theme ()
                  (let ((preferred (get-preferred-theme)))
                    (unless (eq preferred current-theme)
                      (disable-theme current-theme)
                      (load-theme preferred :no-confirm)))))

(set-face-attribute 
 'variable-pitch nil :family "Noto Sans" :weight 'regular :height 135)
(set-face-attribute
 'fixed-pitch-serif nil :family "Victor Mono" :inherit 'default)

(fset 'display-startup-echo-area-message 'ignore)

(setq load-prefer-newer t
      frame-resize-pixelwise t
      package-enable-at-startup nil
      package--init-file-ensured t ;; doom
      auto-save-default nil
      redisplay-skip-fontification-on-input t
      server-client-instructions nil
      auto-save-list-file-prefix nil
      vc-follow-symlinks t
      vc-handled-backends '(Git)
      make-backup-files nil
      create-lockfiles nil
      inhibit-startup-screen t
      initial-buffer-choice 'remember-notes
      bidi-inhibit-bpa t)

(setq-default bidi-display-reordering 'left-to-right
              line-spacing 3
              bidi-paragraph-direction 'left-to-right)

(provide 'early-init)
;;; early-init.el ends here
