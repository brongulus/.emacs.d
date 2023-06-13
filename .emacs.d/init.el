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
;;  Ref: https://zenn.dev/takeokunn/articles/56010618502ccc
;;  TODO: Add fix modeline, file and buffer shortcuts, magit. Outline, TAB folding
;;  TODO: eglot.
;;  TODO: Optimize el-get for startup.
;;  Description
;;
;;; Code:
(require 'profiler)
(profiler-start 'cpu)

;;; Gccemacs stuff / Startup hacks
(setq comp-deferred-compilation t
      comp-async-report-warnings-errors nil)

(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar my/delayed-priority-high-configurations '())
(defvar my/delayed-priority-high-configuration-timer nil)

(defvar my/delayed-priority-low-configurations '())
(defvar my/delayed-priority-low-configuration-timer nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq my/delayed-priority-high-configuration-timer
                  (run-with-timer
                   0.1 0.01
                   (lambda ()
                     (if my/delayed-priority-high-configurations
                         (let ((inhibit-message t))
                           (eval (pop my/delayed-priority-high-configurations)))
                       (progn
                         (cancel-timer my/delayed-priority-high-configuration-timer))))))
            (setq my/delayed-priority-low-configuration-timer
                  (run-with-timer
                   0.3 0.01
                   (lambda ()
                     (if my/delayed-priority-low-configurations
                         (let ((inhibit-message t))
                           (eval (pop my/delayed-priority-low-configurations)))
                       (progn
                         (cancel-timer my/delayed-priority-low-configuration-timer))))))))

(defmacro with-delayed-execution-priority-high (&rest body)
  (declare (indent 0))
  `(setq my/delayed-priority-high-configurations
         (append my/delayed-priority-high-configurations ',body)))

(defmacro with-delayed-execution (&rest body)
  (declare (indent 0))
  `(setq my/delayed-priority-low-configurations
         (append my/delayed-priority-low-configurations ',body)))

;; --------------------------------------------------------------------------------

;;; TEMP FIX before doom profiles are functioning properly
(setq ignored-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode)))

;;; Better Defaults
(setq-default
  bookmark-default-file "~/doom-configs/.emacs.d/bookmarks"
  tab-width 2
  display-line-numbers 'relative
  warning-minimum-level :error
	vc-follow-symlinks t
  cursor-in-non-selected-windows nil
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

;;; Package Management

;;;; Viper
(setq-default
  viper-mode t
  viper-expert-level 5
	viper-want-ctl-h-help t
	viper-want-emacs-keys-in-insert t
  viper-inhibit-startup-message t
  viper-vi-style-in-minibuffer nil
  viper-ESC-moves-cursor-back nil
  viper-electric-mode t
  viper-ex-style-motion nil)
(require 'viper)

(with-eval-after-load 'viper
	(define-key viper-vi-global-user-map ";" 'viper-ex)
	(define-key viper-vi-global-user-map "-" 'dired-jump)
	(define-key viper-vi-global-user-map (kbd "M-<down>") 'scroll-other-window)
	(define-key viper-vi-global-user-map (kbd "M-<up>") 'scroll-other-window-down)
	(define-key viper-insert-global-user-map "\C-v" 'viper-Put-back)
	(define-key viper-insert-global-user-map "\C-y" 'viper-Put-back)

	(viper-record-kbd-macro "gg" 'vi-state [1 G] t)
	(viper-record-kbd-macro "gc" 'vi-state [(meta x) c o m m e n t - l i n e return] t)
	(viper-record-kbd-macro "jk" 'insert-state [escape] t)
	(viper-record-kbd-macro ",w" 'vi-state [(ctrl x) o] t)
	(viper-record-kbd-macro ",v" 'vi-state [(ctrl x) 3] t)
	(viper-record-kbd-macro ",s" 'vi-state [(ctrl x) 2] t)
  (viper-record-kbd-macro ",d" 'vi-state [(ctrl x) 0] t))
;; (viper-record-kbd-macro ",t" 'vi-state [(ctrl x) b] t) ;; FIXME:
;; (viper-record-kbd-macro ",/" 'vi-state [(ctrl x) f] t) ;; FIXME:
(eval-after-load 'viper
  '(progn
     (setq viper-vi-state-id
					 (concat (propertize "●" 'face '(:foreground "#ccdfff")) " "))
     (setq viper-emacs-state-id
					 (concat (propertize "●" 'face '(:foreground "#b9f2c6")) " "))
     (setq viper-insert-state-id
					 (concat (propertize "●" 'face '(:foreground "#fff576")) " "))
     (setq viper-replace-state-id
					 (concat (propertize "●" 'face 'ansi-color-green) " "))
     (put 'viper-mode-string 'risky-local-variable t)))

(with-eval-after-load 'doc-view
				(define-key doc-view-mode-map "j" 'doc-view-next-line-or-next-page)
				(define-key doc-view-mode-map "k" 'doc-view-previous-line-or-previous-page))

;;;; Visual
(add-to-list 'load-path "~/doom-configs/.emacs.d")
(require 'ef-themes)
(setq ef-themes-mixed-fonts t
			ef-themes-variable-pitch-ui t)
(load-theme 'ef-summer :no-confirm)
(set-face-attribute 'default nil :family "Victor Mono" :height 140)

;;;; el-get (Packages)
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; (el-get 'sync)

;; Undo tree
(el-get-bundle undo-fu)
(el-get-bundle undo-fu-session)
(with-delayed-execution-priority-high
  (setq undo-limit 67108864
				undo-strong-limit 100663296
				undo-outer-limit 1006632960)
	(define-key viper-vi-global-user-map "u" #'undo-fu-only-undo)
	(define-key viper-vi-global-user-map "\C-r" #'undo-fu-only-redo)
	(undo-fu-session-global-mode))

;; Consult/Orderless
(el-get-bundle compat) ;; for minad's pkgs
(el-get-bundle orderless)
(with-delayed-execution-priority-high
  (setq completion-styles '(substring orderless basic)
				completion-category-defaults nil
				completion-category-overrides '((file (styles partial-completion)))
				completion-ignore-case t
				read-buffer-completion-ignore-case t
				read-file-name-completion-ignore-case t
				completion-in-region-function
				(lambda (&rest args)
					(apply (if vertico-mode
										 #'consult-completion-in-region
									 #'completion--in-region)
								 args))))
  
(el-get-bundle consult)
(with-delayed-execution-priority-high
	(setq register-preview-function #'consult-register-format)
	(recentf-mode)
  (global-set-key (kbd "C-x f") 'consult-recent-file)
  (global-set-key (kbd "C-<return>") 'consult-bookmark)
  (global-set-key (kbd "C-x b") 'consult-buffer))

;; Helpful
;; Its requires aren't being pulled by el-get automatically
(el-get-bundle dash)
(el-get-bundle f)
(el-get-bundle s)
(el-get-bundle elisp-refs)

(el-get-bundle helpful)
(with-eval-after-load 'helpful
	(add-hook 'helpful-mode-hook
						#'(lambda () (setq-local display-line-numbers nil))))
(with-delayed-execution
	(define-key viper-vi-global-user-map "K" #'helpful-at-point)
	(global-set-key (kbd "C-h f") #'helpful-callable)
	(global-set-key (kbd "C-h v") #'helpful-variable)
	(global-set-key (kbd "C-h k") #'helpful-key)
	(global-set-key (kbd "C-h '") #'describe-face)
	(global-set-key (kbd "C-h x") #'helpful-command))

;; Vertico
(el-get-bundle vertico)
(with-delayed-execution-priority-high
  (vertico-mode)
  (savehist-mode)
  (keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
  (keymap-set vertico-map "TAB" #'vertico-insert)
  ;; RET -> insert and select (FIXME: Selects even if no exact match)
  (defun my/vertico-gg (&optional arg)
    (interactive)
    (vertico-insert)
    (vertico-exit))
  (keymap-set vertico-map "RET" #'my/vertico-gg)
  (keymap-set vertico-map "ESC" #'abort-minibuffers)
  (setq vertico-scroll-margin 0
				vertico-resize nil
				vertico-cycle t))

;; Cape and Corfu
(el-get-bundle cape)
(with-delayed-execution-priority-high
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(el-get-bundle corfu)
(with-delayed-execution-priority-high
  (global-corfu-mode)
  (setq corfu-cycle t
				corfu-auto t
				corfu-auto-prefix 2
				corfu-auto-delay 0
				corfu-separator ?_
				corfu-quit-no-match t
				corfu-preview-current nil
				corfu-preselect-first nil
				tab-always-indent 'complete)
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (setq-local corfu-auto nil)
	      (corfu-mode))))

;; Nov
(el-get-bundle esxml)
(el-get-bundle nov)
(with-delayed-execution
	(add-to-list 'load-path (locate-user-emacs-file "el-get/nov"))
	(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
(with-eval-after-load 'nov
	(add-hook 'nov-mode-hook
						#'(lambda () (setq-local display-line-numbers nil)))
	(define-key nov-mode-map "j" 'next-line)
	(define-key nov-mode-map "k" 'previous-line))

;; eww
(with-eval-after-load 'eww
	(add-hook 'eww-after-render-hook
						#'(lambda () (setq-local display-line-numbers nil)))
	(define-key eww-mode-map "j" 'next-line)
	(define-key eww-mode-map "k" 'previous-line)
	(define-key eww-mode-map "h" 'eww-back-url)
	(define-key eww-mode-map "l" 'eww-forward-url))

;; magit
(el-get-bundle magit/transient)
(el-get-bundle magit/ghub)
(el-get-bundle magit/magit-popup)
(el-get-bundle magit/with-editor)
(el-get-bundle magit/magit)
(el-get-bundle magit/forge)

(with-delayed-execution-priority-high
	(add-to-list 'load-path (locate-user-emacs-file "el-get/transient/lisp"))
  (add-to-list 'load-path (locate-user-emacs-file "el-get/ghub/lisp"))
  (add-to-list 'load-path (locate-user-emacs-file "el-get/magit-pop"))
  (add-to-list 'load-path (locate-user-emacs-file "el-get/with-editor/lisp"))
  (add-to-list 'load-path (locate-user-emacs-file "el-get/magit/lisp"))
	(require 'git-commit)
	(require 'magit)
	(global-git-commit-mode)
	(add-to-list 'load-path (locate-user-emacs-file "el-get/forge/lisp"))
	(setq forge-owned-accounts '(("brongulus")))
  (add-hook 'magit-mode-hook #'(lambda () (require 'forge))))

(el-get 'sync)

;;; Keymaps
(global-set-key [f2] 'save-buffer)
(global-set-key [f10] 'kill-current-buffer)
(global-set-key (kbd "C-x x") 'org-capture)

(with-delayed-execution
	(add-hook 'dired-mode-hook #'(lambda () (setq-local display-line-numbers nil)))
	(define-key dired-mode-map "v" 'dired-x-find-file)
	(define-key dired-mode-map "V" 'dired-view-file)
	(define-key dired-mode-map "j" 'dired-next-line)
	(define-key dired-mode-map "J" 'dired-goto-file)
	(define-key dired-mode-map "k" 'dired-previous-line)
	(define-key dired-mode-map "K" 'dired-do-kill-lines)
	(define-key dired-mode-map "-" 'dired-up-directory)
	(define-key dired-mode-map "~"
		#'(lambda () (interactive) (dired "/home/prashant/")))
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))
(with-eval-after-load 'magit
	(add-hook 'magit-mode-hook
						#'(lambda () (setq-local display-line-numbers nil)))
	(define-key magit-mode-map "j" 'next-line)
	(define-key magit-mode-map "k" 'previous-line))

;;; Random
(with-delayed-execution
				(save-place-mode 1)
				(show-paren-mode)
				(global-hl-line-mode)
				(fset 'yes-or-no-p 'y-or-n-p))

;;; Org-Capture
(with-eval-after-load 'org-capture
	(setq +org-capture-readings-file "~/Dropbox/org/links.org"
      +org-capture-log-file "~/Dropbox/org/log.org"
      +org-capture-todo-file "~/Dropbox/org/inbox.org"
			org-capture-templates
				'(("t" "Personal todo" entry
					 (file+headline +org-capture-todo-file "todo")
					 "* TODO %?\n%i\n%a%f" :prepend t)
					("n" "Personal notes" entry
					 (file+headline +org-capture-notes-file "Notes")
					 "* %u %?\n%i\n%a" :prepend t)
					("r" "Readings" entry
					 (file+headline +org-capture-readings-file "Readings")
					 "* " :prepend t)
					("l" "Personal Log" item
					 (file +org-capture-log-file)
					 "+ %T %?" :prepend t)
					("j" "Journal" entry
					 (file+olp+datetree +org-capture-journal-file)
					 "* %U %?\n** What happened \n** What is going through your mind? \n** What emotions are you feeling? \n** What thought pattern do you recognize? \n** How can you think about the situation differently? " :prepend t))))

;; Startup hacks
(setq file-name-handler-alist my-saved-file-name-handler-alist)

(profiler-report)
(profiler-stop)

(provide 'init)
;;; init.el ends here
