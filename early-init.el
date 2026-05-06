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
                 (setq gc-cons-threshold (* 64 1024 1024) gc-cons-percentage 0.1
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
      after-init-hook nil
      ns-pop-up-frames nil
      inhibit-compacting-font-caches t
      frame-inhibit-implied-resize t
      redisplay-skip-fontification-on-input t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq default-frame-alist '((undecorated-round . t) (internal-border-width . 6)
                            (menu-bar-lines . 0) (tool-bar-lines . 0) (left-fringe . 8)
                            (right-fringe . 8) (vertical-scroll-bars . nil)
                            (horizontal-scroll-bar . nil)))
(fset 'display-startup-echo-area-message 'ignore)
(push '(fullscreen . maximized) initial-frame-alist)
(if (and (featurep 'native-compile) (fboundp 'native-comp-available-p) (native-comp-available-p))
    (setq native-comp-jit-compilation nil
          native-comp-jit-compilation-deny-list
          '("/emacs-lisp/cl-loaddefs\\.el" "org-loaddefs\\.el")
          native-comp-async-report-warnings-errors 'silent
          package-native-compile t)
    (add-hook 'window-setup-hook
              (lambda () (setq native-comp-jit-compilation t))
              110)
  (setq features (delq 'native-compile features)))
(when (eq system-type 'android)
  ;; Install termux first, "pkg update && pkg upgrade"
  ;; Install git, fish: "pkg install git fish"
  ;; Setup git global user and email
  ;; ssh-keygen -t ed25519 -C "email"
  ;; cat /data/data/com.termux/files/home/.ssh/id_ed25519.pub to GH
  ;; Install emacs, clone .emacs.d repo
  ;; git remote set-url --push origin git@github.com:brongulus/.emacs.d.git
  ;; For doc-view: "pkg install ghostscript mupdf-tools"
  (let ((termuxpath "/data/data/com.termux/files/usr/"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath "bin"))
    (push (concat termuxpath "bin") exec-path))
  (unless (file-directory-p "~/fonts")
    (copy-directory "~/.emacs.d/fonts/" "~/fonts")))
(when (eq system-type 'darwin)
  (setq process-connection-type nil)
  (let ((home (getenv "HOME")))
    (setenv "PATH" (concat (getenv "PATH")
                           ":" home "/.nix-profile/bin:/usr/bin"
                           ":/opt/homebrew/bin"
                           ":/usr/local/bin"))
    (setq exec-path (append `(,(concat home "/.nix-profile/bin")
                              "/opt/homebrew/bin"
                              "/usr/local/bin"
                              "/nix/var/nix/profiles/default/bin")
                            exec-path))))
