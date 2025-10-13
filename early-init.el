;;; early-init.el -*- lexical-binding: t; -*-
(defvar my/saved-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      load-prefer-newer noninteractive
      garbage-collection-messages nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (run-at-time
               2 nil
               (lambda nil            
                 (setq gc-cons-threshold (* 32 1024 1024)
                       gc-cons-percentage 0.1
                       file-name-handler-alist my/saved-file-name-handler-alist)
                 (garbage-collect))))
          105)

;; src: skangas
;; (when (>= emacs-major-version 27)
;;   (defun gc-on-last-frame-out-of-focus ()
;;     "GC if all frames are inactive."
;;     (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
;;         (garbage-collect)))
;;   (add-function :after after-focus-change-function
;;                 #'gc-on-last-frame-out-of-focus))

(setq-default default-frame-alist
              '((alpha . 100)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil)
                (fullscreen . maximized)
                (left-fringe . 8) (right-fringe . 8) (internal-border-width . 10)
                (bottom-divider-width . 0) (right-divider-width . 0)
                (undecorated-round . t))
              cursor-in-non-selected-windows nil
              bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Android
(defconst is-android (eq system-type 'android))
(defconst is-mac (eq system-type 'darwin))

(when is-android
  (let ((termuxpath "/data/data/com.termux/files/usr/"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath "bin")))
  ;; (push (concat termuxpath "bin") exec-path))
  (set-face-attribute 'default nil :height 160)
  (unless (file-directory-p "~/fonts")
    (copy-directory "~/.emacs.d/fonts/" "~/fonts")))

;; doom
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'after-init-hook
          (lambda nil
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redraw-frame))
          :depth -105)
;;
(fset 'display-startup-echo-area-message 'ignore)

(setcdr (assq 'continuation fringe-indicator-alist)
        '(nil nil))

(when t
  (defvar package-quickstart)
  (setq package-quickstart t))

(setq package-enable-at-startup nil
      inhibit-startup-screen t
      redisplay-skip-fontification-on-input t
      window-combination-resize t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(advice-add 'display-startup-screen :override #'ignore)

(when (string> emacs-version "31")
  (setq load-path-filter-function #'load-path-filter-cache-directory-files))

(when is-mac
  (setq ns-use-proxy-icon nil
        frame-title-format nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (let ((home (getenv "HOME")))
    (setenv "PATH" (concat (getenv "PATH")
                           ":" home "/.nix-profile/bin:/usr/bin"))
    (setq exec-path (append `(,(concat home "/.nix-profile/bin")
                              "/nix/var/nix/profiles/default/bin")
                            exec-path))))

(if is-mac
    (setq mac-option-modifier 'meta)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))

;; native-comp
(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-jit-compilation t
          native-comp-enable-subr-trampolines t
          native-comp-async-report-warnings-errors nil
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(provide 'early-init)
;;; early-init.el ends here
