;;; early-init.el -*- lexical-binding: t; -*-
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar my/saved-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      load-prefer-newer noninteractive
      garbage-collection-messages nil)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold (* 100 1024 1024)
                    gc-cons-percentage 0.1
                    file-name-handler-alist my/saved-file-name-handler-alist)))

;; src: skangas
(when (>= emacs-major-version 27)
  (defun gc-on-last-frame-out-of-focus ()
    "GC if all frames are inactive."
    (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
        (garbage-collect)))
  (add-function :after after-focus-change-function
                #'gc-on-last-frame-out-of-focus))

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (garbage-collect)
              (setq gc-cons-threshold (* 60 1024 1024))))

;; Android
(when (string-equal system-type "android")
  (let ((termuxpath "/data/data/com.termux/files/usr/"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath "bin"))
    (setenv "LD_LIBRARY_PATH" (concat (getenv "LD_LIBRARY_PATH") ":" termuxpath "lib"))
    (push (concat termuxpath "bin") exec-path))
  (setq overriding-text-conversion-style nil)
  (set-frame-font "monospace 16" nil t))

(setq-default default-frame-alist
              '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars)
                (left-fringe . 15)
                (right-fringe . 15)
                (internal-border-width . 00)
                (fullscreen . fullboth)
                (font . "VictorMono Nerd Font Mono-16"))
              cursor-in-non-selected-windows nil
              bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right)

(fset 'display-startup-echo-area-message 'ignore)

(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil right-curly-arrow))

(when (file-exists-p (locate-user-emacs-file "package-quickstart.el"))
  (defvar package-quickstart)
  (setq package-quickstart t))

(setq package-enable-at-startup nil
      redisplay-skip-fontification-on-input t
      window-combination-resize t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))

(provide 'early-init)
;;; early-init.el ends here
