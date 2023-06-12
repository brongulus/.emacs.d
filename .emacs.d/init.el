;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Prashant Tak
;;
;; Author: Prashant Tak <prashantrameshtak@gmail.com>
;; Maintainer: Prashant Tak <prashantrameshtak@gmail.com>
;; Created: June 11, 2023
;; Modified: June 11, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/brongulus/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;; GCCEMACS STUFF
(setq comp-deferred-compilation t
      comp-async-report-warnings-errors nil
      gc-cons-threshold (* 16 1024 1024))

;;; TEMP FIX before doom profiles are functioning properly
(setq ignored-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode)))

;;; Package Management
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(el-get-bundle compat
  (require 'compat))

(el-get-bundle orderless)
  
(el-get-bundle vertico
  (require 'vertico)
  (vertico-mode)
  (savehist-mode)
  (setq vertico-scroll-margin 0
	vertico-resize nil
	vertico-cycle t))

(el-get-bundle corfu
  (require 'corfu)
  (global-corfu-mode)
  (setq corfu-cycle t
	corfu-auto t
	corfu-separator ?_
	corfu-quit-no-match 'separator
	corfu-preview-current nil
	tab-always-indent 'complete)
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (setq-local corfu-auto nil)
	      (corfu-mode))))

(el-get 'sync)
	
;;; BETTER DEFAULTS
(setq-default
  viper-mode t
  viper-expert-level 1
  viper-inhibit-startup-message t
  viper-ESC-moves-cursor-back nil
  viper-electric-mode t
  viper-ex-style-motion nil
  completion-styles '(substring orderless basic)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))
  completion-ignore-case t
  read-buffer-completion-ignore-case t
  read-file-name-completion-ignore-case t
  warning-minimum-level :error
  cursor-in-non-selected-windows nil
  cursor-type 'bar
  mouse-yank-at-point t
  recenter-positions '(5 top bottom)
  help-window-select t
  large-file-warning-threshold nil
  shell-file-name "/bin/bash"
  auth-sources '("~/.authinfo")
  show-paren-delay 0
  inhibit-startup-screen t
  initial-buffer-choice 'remember-notes
  remember-notes-buffer-name "*scratch*")

(global-set-key [f2] 'save-buffer)
(global-set-key [f10] 'kill-buffer)
(global-set-key (kbd "C-<return>") 'bookmark-jump)

(add-to-list 'load-path "~/doom-configs/.emacs.d")

(require 'ef-themes)
(load-theme 'ef-summer :no-confirm)
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

(require 'viper)
(define-key viper-vi-global-user-map "-" 'dired-jump)
(define-key viper-insert-global-user-map "\C-v" 'scroll-up)
(viper-record-kbd-macro "gg" 'vi-state
			[1 G]
			t)
(viper-record-kbd-macro "gc" 'vi-state
			[(meta x) c o m m e n t - l i n e return]
			t)
(viper-record-kbd-macro "jk" 'insert-state
			[escape]
			t)
(eval-after-load 'viper
  '(progn
     (setq viper-vi-state-id (concat (propertize "[V]" 'face 'ansi-color-blue) " "))
     (setq viper-emacs-state-id (concat (propertize "[E]" 'face 'ansi-color-red) " "))
     (setq viper-insert-state-id (concat (propertize "[I]" 'face 'ansi-color-yellow) " "))
     (setq viper-replace-state-id (concat (propertize "[R]" 'face 'ansi-color-green) " "))
     ;; The property `risky-local-variable' is a security measure
     ;; for mode line variables that have properties
     (put 'viper-mode-string 'risky-local-variable t)))
(define-key dired-mode-map "v" 'dired-x-find-file)
(define-key dired-mode-map "V" 'dired-view-file)
(define-key dired-mode-map "j" 'dired-next-line)
(define-key dired-mode-map "J" 'dired-goto-file)
(define-key dired-mode-map "k" 'dired-previous-line)
(define-key dired-mode-map "K" 'dired-do-kill-lines)
;; (load "~/doom-configs/.emacs.d/+viper")

(set-face-attribute 'default nil :family "Victor Mono" :height 160)

(save-place-mode 1)
(show-paren-mode)
(global-hl-line-mode)
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'text-mode-hook 'linum-mode)

(provide 'init)
;;; init.el ends here
