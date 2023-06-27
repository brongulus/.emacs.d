;;; init.el --- Description -*- lexical-binding: t; -*-
;; Author: Prashant Tak <prashantrameshtak@gmail.com>
;; Created: June 11, 2023
;; Modified: June 11, 2023
;; Version: 0.0.1
;; This file is not part of GNU Emacs.
;;; Commentary:
;;  Ref 1: https://zenn.dev/takeokunn/articles/56010618502ccc
;;  Ref 2: https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4
;;  Ref 3: https://robbmann.io/emacsd/
;;  TODO: file and buffer shortcuts, fix eldoc in terminal, bottom popup in gui,
;;        terminal vertico arrow keys
;;; Code:

;;; Startup hacks 
(setq comp-deferred-compilation t
      comp-async-report-warnings-errors nil)

;; ;; (el-get-bundle benchmark-init)
;; (load "/home/prashant/.emacs.d/el-get/benchmark-init/benchmark-init.el"
      ;; 'no-error nil 'no-suffix)
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(defvar my/delayed-priority-high-confs '())
(defvar my/delayed-priority-high-conf-timer nil)

(defvar my/delayed-priority-low-confs '())
(defvar my/delayed-priority-low-conf-timer nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq my/delayed-priority-high-conf-timer
                  (run-with-timer
                   0.1 0.01
                   (lambda ()
                     (if my/delayed-priority-high-confs
                         (let ((inhibit-message t))
                           (eval (pop my/delayed-priority-high-confs)))
                       (progn
                         (cancel-timer my/delayed-priority-high-conf-timer))))))
            (setq my/delayed-priority-low-conf-timer
                  (run-with-timer
                   0.3 0.01
                   (lambda ()
                     (if my/delayed-priority-low-confs
                         (let ((inhibit-message t))
                           (eval (pop my/delayed-priority-low-confs)))
                       (progn
                         (cancel-timer my/delayed-priority-low-conf-timer))))))))

(defmacro with-delayed-execution-priority-high (&rest body)
  (declare (indent 0))
  `(setq my/delayed-priority-high-confs
         (append my/delayed-priority-high-confs ',body)))

(defmacro with-delayed-execution (&rest body)
  (declare (indent 0))
  `(setq my/delayed-priority-low-confs
         (append my/delayed-priority-low-confs ',body)))

;; -----------------------------------------------------------------------------

;;; Better Defaults
(setq-default
  user-full-name "Prashant Tak"
  user-mail-address "prashantrameshtak@gmail.com"
  smtpmail-smtp-server "smtp.gmail.com"
  smtpmail-smtp-service 587
  warning-minimum-level :error
  tab-width 2
	indent-tabs-mode nil
	fill-column 80
  delete-selection-mode t
  mouse-yank-at-point t
  custom-file (concat user-emacs-directory "custom.el")
	scroll-margin 0
	scroll-conservatively 100000
	scroll-preserve-screen-position 1
  recenter-positions '(5 top bottom)
  vc-follow-symlinks t)

(with-delayed-execution-priority-high
	(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))

(with-delayed-execution
  (setq bookmark-default-file "~/.emacs.d/bookmarks"
	cursor-in-non-selected-windows nil
	help-window-select t
	large-file-warning-threshold nil
	shell-file-name "/bin/bash"
	auth-sources '("~/.authinfo")
	show-paren-delay 0
	initial-buffer-choice 'remember-notes
	remember-notes-buffer-name "*scratch*"))

;;;;; Visual
(with-delayed-execution-priority-high
  (set-cursor-color "white")
	(setq dracula-enlarge-headings nil
        dracula-height-title-1 1.0
        dracula-height-title-2 1.0
        drcaula-height-doc-title 1.0))

;; Modeline (Ref: https://github.com/motform/emacs.d/blob/master/init.el)
(with-delayed-execution-priority-high
	(setq-default flymake-mode-line-counter-format
                '("" flymake-mode-line-error-counter
                  flymake-mode-line-warning-counter
                  flymake-mode-line-note-counter "")
                flymake-mode-line-format
								'(" " flymake-mode-line-exception flymake-mode-line-counters)
								global-mode-string nil) ;; avoid duping of vi-indicator
  (setq-default mode-line-end-spaces '("" mode-line-misc-info " %l %p ")))
  (defun my/ml-padding ()
    (let ((r-length (length (format-mode-line mode-line-end-spaces))))
        (propertize " "
          'display `(space :align-to (- right ,r-length)))))
	(setq-default mode-line-format
								'(" %e " viper-mode-string " "
                  (:eval (if (buffer-modified-p)
                             (propertize " %b "
                                         'face '((:inverse-video t))
                                         'help-echo (buffer-file-name))
                           (propertize " %b " 'help-echo (buffer-file-name))))
                  (:eval (when (bound-and-true-p flymake-mode)
													 flymake-mode-line-format))
                  (:eval (my/ml-padding))
                  mode-line-end-spaces))

;;; Package Management
;;;; Viper
(setq-default
	viper-mode t
	viper-expert-level 5
	viper-want-ctl-h-help t
	viper-want-emacs-keys-in-insert t
	viper-ex-style-editing nil
	viper-ex-style-motion nil
	viper-case-fold-search t
	viper-inhibit-startup-message t
	viper-vi-style-in-minibuffer nil
	viper-ESC-moves-cursor-back nil
	viper-shift-width 2
	viper-fast-keyseq-timeout 300
	viper-electric-mode t
	viper-ex-style-motion nil)

(require 'viper)

(with-eval-after-load 'viper
	(push 'org-capture-mode viper-insert-state-mode-list) ;; FIXME:
	;; #times next command is run
	(define-key viper-vi-global-user-map "\M-u" 'universal-argument)
	;; macros
	;; (define-key viper-vi-global-user-map "q" 'kmacro-start-macro-or-insert-counter)
	;; (define-key viper-vi-global-user-map "Q" 'kmacro-end-or-call-macro)
	;; (define-key viper-vi-global-user-map "@" 'consult-kmacro)
	;; WIP: visual mode
	(define-key viper-vi-global-user-map "v" 'set-mark-command)
	(define-key viper-vi-global-user-map "\C-v" 'rectangle-mark-mode)
	(define-key viper-vi-global-user-map "q" 'keyboard-quit)
	(define-key viper-vi-local-user-map "Y" 'copy-region-as-kill)
	(define-key viper-vi-local-user-map "D" 'clipboard-kill-region) ;; DEL works
	(define-key viper-vi-local-user-map "C" 'comment-or-uncomment-region)
	;; ------------
	(define-key viper-vi-global-user-map "(" 'backward-list)
	(define-key viper-vi-global-user-map ")" 'forward-list)
	(define-key viper-vi-global-user-map ";" 'viper-ex)
	(define-key viper-vi-global-user-map "-" 'dired-jump)
	(define-key viper-vi-global-user-map (kbd "M-<down>") 'scroll-other-window)
	(define-key viper-vi-global-user-map (kbd "M-<up>") 'scroll-other-window-down)
	(define-key viper-vi-global-user-map (kbd "C-t") 'tab-new)
	(define-key viper-vi-global-user-map (kbd "C-w") 'tab-close)
	(define-key viper-insert-global-user-map "\C-v" 'viper-Put-back)
	(define-key viper-insert-global-user-map "\C-y" 'viper-Put-back)
	;; Look into incorporating these via viper-vi-basic-map
	(viper-record-kbd-macro "gf" 'vi-state [(meta x) f f a p return] t)
	(viper-record-kbd-macro "gd" 'vi-state [(meta .)] t)
	(viper-record-kbd-macro "gt" 'vi-state [(ctrl x) t o] t)
	(viper-record-kbd-macro "gT" 'vi-state [(ctrl x) t O] t)
	(viper-record-kbd-macro "gg" 'vi-state [1 G] t)
	(viper-record-kbd-macro "gcc" 'vi-state [(ctrl x) (ctrl \;)] t)
	(viper-record-kbd-macro "jk" 'insert-state [escape] t))

(eval-after-load 'viper
	'(progn
		 (setq viper-vi-state-id
			 (concat (propertize "⬤" 'face '(:foreground "#ccdfff")) " "))
		 (setq viper-emacs-state-id
			 (concat (propertize "⬤" 'face '(:foreground "#b9f2c6")) " "))
		 (setq viper-insert-state-id
			 (concat (propertize "⬤" 'face '(:foreground "#fff576")) " "))
		 (setq viper-replace-state-id
			 (concat (propertize "⬤" 'face 'ansi-color-green) " "))
			 (put 'viper-mode-string 'risky-local-variable t)))

;;;; el-get (Packages)
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
 (with-current-buffer
		 (url-retrieve-synchronously
			"https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
	 (goto-char (point-max))
 (eval-print-last-sexp)))
(with-eval-after-load 'el-get-git
	(setq el-get-git-shallow-clone t))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-is-lazy t)

;;;;; Vertico/Marginalia
(el-get-bundle compat) ;; for minad's pkgs
(el-get-bundle vertico)
(vertico-mode)
(savehist-mode)
(with-delayed-execution-priority-high
  (keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
  (keymap-set vertico-map "TAB" #'vertico-insert)
  (keymap-set vertico-map "ESC" #'abort-minibuffers)
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous)
  (keymap-set vertico-map "<up>" #'vertico-previous)
  ;; RET -> insert and select (FIXME: Selects even if no exact match)
  (defun my/vertico-gg (&optional arg)
    (interactive)
    (vertico-insert)
    (vertico-exit))
  (keymap-set vertico-map "RET" #'my/vertico-gg)
  (setq vertico-scroll-margin 0
	vertico-resize nil
	vertico-cycle t))

(el-get-bundle marginalia)
(with-delayed-execution-priority-high
	(marginalia-mode))

;;;;; Consult/Orderless
(el-get-bundle orderless)
(with-delayed-execution-priority-high
	(setq completion-category-defaults nil
	completion-styles '(substring orderless basic)
	completion-category-overrides '((file (styles basic partial-completion)))
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
	(when (executable-find "rg")
		(setq grep-program "rg"))
	(when (executable-find "fd")
		(setq find-program "fd"))
  (setq register-preview-function #'consult-register-format)
	(setq recentf-max-menu-items 10000
				recentf-max-saved-items 10000
				recentf-save-file  "~/.emacs.d/.recentf"
				recentf-exclude '(".recentf" "\\.gpg\\"))
  (recentf-mode)
  (define-key viper-vi-global-user-map (kbd "SPC") 'consult-buffer)
  (define-key viper-vi-global-user-map (kbd "?") 'consult-recent-file)
  (global-set-key (kbd "C-x f") 'consult-recent-file)
  (global-set-key (kbd "C-<return>") 'consult-bookmark)
  (global-set-key (kbd "C-x b") 'consult-buffer)
	(global-set-key [remap list-buffers] 'consult-buffer)
	(global-set-key [remap isearch-forward] 'consult-line))

;;;;; Helpful
(el-get-bundle darkroom
  :url "https://raw.githubusercontent.com/joaotavora/darkroom/master/darkroom.el")
;; Its requires aren't being pulled by el-get automatically
(el-get-bundle f)
(el-get-bundle s)
(el-get-bundle elisp-refs)

(el-get-bundle helpful)
(with-delayed-execution
	(defun my/doc-at-point (&rest args)
			(interactive)
			(if (derived-mode-p 'emacs-lisp-mode)
					(helpful-at-point)
				(eldoc-box-help-at-point)))
	(define-key viper-vi-local-user-map "K" #'my/doc-at-point)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h '") #'describe-face)
  (global-set-key (kbd "C-h x") #'helpful-command))

;;;;; Cape and Corfu
(el-get-bundle corfu)
(with-delayed-execution-priority-high
  (global-corfu-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (keymap-set corfu-map "ESC" #'abort-minibuffers)
  (setq corfu-cycle t
	      corfu-auto t
	      corfu-auto-prefix 2
	      corfu-auto-delay 0
	      corfu-separator ?_
	      corfu-quit-no-match t
	      corfu-preview-current nil
	      corfu-popupinfo-delay '(0.2 . 0.1)
	      corfu-preselect-first nil
	      tab-always-indent 'complete)
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (setq-local corfu-auto nil)
	      (corfu-mode))))

(el-get-bundle cape)
(with-delayed-execution-priority-high
	(defun my/add-capfs ()
		(push 'cape-file completion-at-point-functions)
		(push 'cape-dabbrev completion-at-point-functions)
		(push 'cape-keyword completion-at-point-functions))
	(add-hook 'prog-mode-hook #'my/add-capfs)
	(add-hook 'text-mode-hook #'my/add-capfs))

;;;;; Undo tree/ which-key
(el-get-bundle undo-fu)
(el-get-bundle undo-fu-session)
(with-delayed-execution-priority-high
  (setq undo-limit 67108864
	      undo-strong-limit 100663296
	      undo-outer-limit 1006632960)
  (define-key viper-vi-global-user-map "u" #'undo-fu-only-undo)
  (define-key viper-vi-global-user-map "\C-r" #'undo-fu-only-redo)
  (undo-fu-session-global-mode))

(el-get-bundle which-key)
(with-delayed-execution
	(setq which-key-idle-delay 0.5)
	(which-key-mode))

;;;;; git/tempel
(el-get-bundle git-gutter)
(with-delayed-execution
	(add-hook 'text-mode-hook #'git-gutter-mode)
	(add-hook 'prog-mode-hook #'git-gutter-mode)
  (with-eval-after-load 'git-gutter
    (set-face-foreground 'git-gutter:modified "yellow"))
	(setq git-gutter:update-interval 0.02
				fringes-outside-margins t
				git-gutter:added-sign "│"
				git-gutter:modified-sign "│"
				git-gutter:deleted-sign "│"))

(el-get-bundle magit/transient)
(el-get-bundle magit/ghub)
(el-get-bundle magit/magit-popup)
(el-get-bundle magit/with-editor)
(el-get-bundle magit/magit)
(el-get-bundle magit/forge)

(with-delayed-execution-priority-high
  (push (locate-user-emacs-file "el-get/transient/lisp") load-path)
  (push (locate-user-emacs-file "el-get/ghub/lisp") load-path)
  (push (locate-user-emacs-file "el-get/magit-pop") load-path)
  (push (locate-user-emacs-file "el-get/with-editor/lisp") load-path)
  (push (locate-user-emacs-file "el-get/magit/lisp") load-path)
  (push (locate-user-emacs-file "el-get/forge/lisp") load-path)
  (autoload 'magit "magit" nil t)
	(global-set-key (kbd "C-c g") 'magit)
  (setq forge-owned-accounts '(("brongulus")))
  (add-hook 'magit-mode-hook #'(lambda () (require 'forge))))

;; terminal stuff, taken from doom (cursor, cliboard)
(el-get-bundle xclip)
(el-get-bundle evil-terminal-cursor-changer)

;; tempel
(el-get-bundle tempel)
(with-delayed-execution
	(setq fill-indent-according-to-mode t)
	(defun tempel-setup-capf ()
		(setq-local completion-at-point-functions
								(cons #'tempel-complete
											completion-at-point-functions)))
	(add-hook 'conf-mode-hook 'tempel-setup-capf)
	(add-hook 'prog-mode-hook 'tempel-setup-capf)
	(add-hook 'text-mode-hook 'tempel-setup-capf)
	(with-eval-after-load 'tempel
		;; FIXME: terminal tab
		(define-key tempel-map (kbd "<tab>") 'tempel-next)
		(define-key tempel-map (kbd "TAB") 'tempel-next)
		(define-key tempel-map (kbd "<backtab>") 'tempel-previous))
	(setq tempel-path "~/.emacs.d/templates.el"))

;;;;; eglot
(el-get-bundle! project
	:url "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/progmodes/project.el")
(el-get-bundle rustic)
(el-get-bundle reformatter)
(el-get-bundle zig-mode)
(el-get-bundle external-completion)
(el-get-bundle eldoc-box)
(with-delayed-execution
	(with-eval-after-load 'eldoc-box
		(setq eldoc-box-max-pixel-width 600
					eldoc-box-max-pixel-height 700
					eldoc-box-only-multi-line t)))
(el-get-bundle! eglot)
(with-delayed-execution
	(with-eval-after-load 'eglot
		(if (display-graphic-p)
				(global-eldoc-mode -1))
		(add-hook 'eglot-managed-mode-hook 'tempel-setup-capf)
		(define-key eglot-mode-map "K" 'eldoc-box-help-at-point)
    (define-key viper-vi-local-user-map "X" 'flymake-show-buffer-diagnostics)
		;; pacman -S clang pyright rust-analyzer
		;; yay -S jdtls jdk-openjdk jre-openjdk
		;; rustup component add rust-analyzer
		(push '((c++-mode c-mode) "clangd") eglot-server-programs)
		(push '(java-mode "jdtls") eglot-server-programs)
		(push '(rust-mode "rust-analyzer") eglot-server-programs))
	(setq eglot-autoshutdown t
				rustic-lsp-client 'eglot)
	(add-hook 'c++-mode-hook 'eglot-ensure)
	(add-hook 'java-mode-hook 'eglot-ensure)
	(add-hook 'python-mode-hook 'eglot-ensure)
	(add-hook 'c-mode-hook 'eglot-ensure))

(el-get 'sync)

;;; Compilation
(with-delayed-execution
	(setq compilation-scroll-output 'first-error
				compilation-always-kill t)
	(add-hook 'racket-mode-hook #'(lambda ()
						 (setq-local compile-command (concat
												"racket " (shell-quote-argument buffer-file-name)))))
	(add-hook 'rust-mode-hook
						#'(lambda ()
								(setq-local compile-command
								;; (setq-local compile-command "cargo build && cargo run")
														(concat "rustc " buffer-file-name
																		" && ./"
																		(file-name-sans-extension
																		 (file-name-nondirectory buffer-file-name))))))
	(add-hook 'java-mode-hook #'(lambda ()
						 (setq-local
							compile-command
							(concat "javac " (shell-quote-argument buffer-file-name)
                      " && java " (file-name-sans-extension
																	 (file-name-nondirectory buffer-file-name))))))
	(add-hook 'python-mode-hook #'(lambda ()
						 (setq-local
							compile-command
							(concat "python " (shell-quote-argument buffer-file-name) " < ./in"
                      (file-name-sans-extension
											 (file-name-nondirectory buffer-file-name))))))
	(add-hook 'c++-mode-hook #'(lambda ()
						(setq-local
						 compile-command
							(concat "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion "
											"-O2 -DLOCAL -I/mnt/Data/Documents/problems/include "
											buffer-file-name
											" && ./a.out < ./in"
											(file-name-sans-extension
											 (file-name-nondirectory buffer-file-name))))))
	;; Copy input from clipboard
	(defun paste-input (&optional arg)
		(interactive)
		(find-file (concat
              "in"
              (file-name-sans-extension
							 (file-name-nondirectory buffer-file-name))))
		(erase-buffer)
		(clipboard-yank)
		(basic-save-buffer)
		(kill-current-buffer)
		(message "Populated input file")))

;; File-templates
(with-delayed-execution
	;; https://emacs.stackexchange.com/questions/55754/how-to-run-functions-inside-auto-insert-template
	(defun my/eval-auto-insert-init-form ()
		(goto-char (point-min))
		(cl-letf (((symbol-function '\`) #'progn))
			(while (re-search-forward "`" nil t)
      (let* ((beg (goto-char (match-beginning 0)))
					 (end (with-syntax-table emacs-lisp-mode-syntax-table
							(forward-sexp)
							(point)))
					 (str (eval (read (buffer-substring beg end)))))
			(delete-region beg end)
			(insert str)))))
	(auto-insert-mode)
	(setq auto-insert-directory "~/doom-configs/.emacs.d/templates/"
				auto-insert-query nil)
	(define-auto-insert "\.cpp" ["comp.cpp"
															 my/eval-auto-insert-init-form]))

;;; Keymaps
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'paste-input)
(global-set-key [f4] 'compile)
(global-set-key [f10] 'kill-current-buffer)
(global-set-key (kbd "C-x x") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-t") 'ef-themes-toggle)

;; dired
(with-delayed-execution
  (setq dired-listing-switches "-ahl -v --group-directories-first")
  (define-key dired-mode-map "v" 'dired-x-find-file)
  (define-key dired-mode-map "V" 'dired-view-file)
  (define-key dired-mode-map "j" 'dired-next-line)
  (define-key dired-mode-map "J" 'dired-goto-file)
  (define-key dired-mode-map "k" 'dired-previous-line)
  (define-key dired-mode-map "K" 'dired-do-kill-lines)
  (define-key dired-mode-map "-" 'dired-up-directory)
  (define-key dired-mode-map (kbd "<space>") 'consult-buffer)
  (define-key dired-mode-map (kbd "SPC") 'consult-buffer)
  (define-key dired-mode-map "~"
    #'(lambda () (interactive) (dired "/home/prashant/")))
	(setq dired-dwim-target t
				dired-recursive-deletes 'always
				dired-recursive-copies 'always)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; tabs
(with-eval-after-load 'tab-bar
	(defun +my/tab (tab i)
		(propertize (concat "  " (alist-get 'name tab) "      ")
								'face (funcall tab-bar-tab-face-function tab)))
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-separator ""
        tab-bar-tab-name-format-function #'+my/tab
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        tab-bar-tab-name-truncated-max 12))

;;; Random
(with-delayed-execution
  (save-place-mode 1)
  (global-auto-revert-mode t)
  (winner-mode 1)
  (with-eval-after-load 'winner
    (define-key winner-mode-map (kbd "C-c C-<left>") 'winner-undo)
    (define-key winner-mode-map (kbd "C-c C-<right>") 'winner-redo))
  (with-eval-after-load 'doc-view
    (define-key doc-view-mode-map "j" 'doc-view-next-line-or-next-page)
    (define-key doc-view-mode-map "k" 'doc-view-previous-line-or-previous-page))
  (with-eval-after-load 'eww
    (define-key eww-mode-map "j" 'next-line)
    (define-key eww-mode-map "k" 'previous-line)
    (define-key eww-mode-map "h" 'eww-back-url)
    (define-key eww-mode-map "l" 'eww-forward-url))
  (show-paren-mode)
  (global-hl-line-mode)
	(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
	;; auto pair completion
	(add-hook 'prog-mode-hook (electric-pair-mode t))
	(add-hook 'prog-mode-hook (show-paren-mode t))
  (fset 'yes-or-no-p 'y-or-n-p))

;;; Org
;; (el-get-bundle engrave-faces)
(with-delayed-execution
	(setq org-latex-toc-command "\\tableofcontents \\clearpage"
        org-startup-indented t
        org-startup-folded t
				org-src-preserve-indentation t
				;; \usepackage{listings}
				;; org-latex-listings 'engraved
				org-latex-pdf-process '("tectonic -X compile %f --outdir=%o -Z shell-escape"))
	(defun my/org-inkscape-watcher (fname)
		"Open inkscape and add tex code for importing the figure"
		(interactive "sName: ")
		(insert (shell-command-to-string (concat "inkscape-figures create '" fname
																						 "' ./figures/"))))
	(with-eval-after-load 'org
		(add-hook 'org-mode-hook 'tempel-setup-capf)
		(define-key org-mode-map [f4] #'org-latex-export-to-pdf)
		(define-key org-mode-map [f5] #'my/org-inkscape-watcher)))
;;;; Org-Capture
(with-eval-after-load 'org-capture
  (setq
    +org-capture-readings-file "~/Dropbox/org/links.org"
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
	    ("l" "Personal Log" entry
	     (file +org-capture-log-file)
	     "* %T %?" :prepend t)
	    ("j" "Journal" entry
	     (file+olp+datetree +org-capture-journal-file)
       "* %U %?\n** What happened \n** What is going through your mind? \n** What emotions are you feeling? \n** What thought pattern do you recognize? \n** How can you think about the situation differently? " :prepend t))))

;;;; Org-agenda
(with-eval-after-load 'org-agenda
	(setq org-agenda-start-with-log-mode t
	      org-log-done t
	      org-log-into-drawer t
	      org-agenda-breadcrumbs-separator " ❱ "
	      org-agenda-files '("~/Dropbox/org/todo.org" "~/Dropbox/org/inbox.org")))

;; Terminal session (taken from doom)
(with-delayed-execution-priority-high
	(unless (display-graphic-p)
		(xterm-mouse-mode 1)
		(eldoc-mode 1)
		(turn-on-xclip)
		(etcc-on)))

;; Gnus
(with-delayed-execution
	(load "~/.emacs.d/+gnus"))

(provide 'init)
;;; init.el ends here


;; ------------------------- This contains code for the future.
;; (if (version< emacs-version "29.0")
    ;; (pixel-scroll-mode)
  ;; (pixel-scroll-precision-mode 1)
  ;; (setq pixel-scroll-precision-large-scroll-height 35.0))

;; (defun smart-compile ()
  ;; "runs compile command based on current major mode."
  ;; (interactive)
  ;; (let* ((cmd
          ;; (cond ((bound-and-true-p smart-compile-command) smart-compile-command)
                ;; ((eq major-mode 'js-mode) "npm test")
;; ((eq major-mode 'c++-mode) (concat "zig c++ " (buffer-file-name) "-DLOCAL "
;; "-I/mnt/Data/Documents/problems/include/ -o ")
                ;; ((eq major-mode 'rust-mode) "cargo build")
                ;; ((eq major-mode 'haskell-mode) "cabal run")))
         ;; (default-directory (projectile-project-root)))
    ;; (progn
      ;; (save-some-buffers 1)
      ;; (compile cmd))))
