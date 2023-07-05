;;; init.el --- Description -*- lexical-binding: t; -*-
;; Author: Prashant Tak <prashantrameshtak@gmail.com>
;; Created: June 11, 2023
;; Modified: July 3, 2023
;; Version: 0.0.2
;; This file is not part of GNU Emacs.
;;; Commentary:
;;	Most completion systems follow TnG, tab to choose next and RET to select
;;	Ref 1: https://zenn.dev/takeokunn/articles/56010618502ccc
;;	Ref 2: https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4
;;	Ref 3: https://github.com/doomemacs/doomemacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;;; Code:

;;; Startup hacks
;; (setq comp-deferred-compilation t
;;			 comp-async-report-warnings-errors nil)

;; -----------------------------------------------------------------------------
;; ;; (el-get-bundle benchmark-init)
;; (load "/home/prashant/.emacs.d/el-get/benchmark-init/benchmark-init.el"
;;			 'no-error nil 'no-suffix)
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;; -----------------------------------------------------------------------------
;; ;; (el-get-bundle esup)
;; -----------------------------------------------------------------------------
(setq t0 (current-time)) ;; ymarco

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
(with-delayed-execution
	(setq-default user-full-name "Prashant Tak"
								user-mail-address "prashantrameshtak@gmail.com"
								smtpmail-smtp-server "smtp.gmail.com"
								smtpmail-smtp-service 587
								warning-minimum-level :error
								line-spacing 3
								tab-width 2
								indent-tabs-mode nil
								enable-recursive-minibuffers t
								fill-column 80
								delete-selection-mode t
								mouse-yank-at-point t
								custom-file (make-temp-file "emacs-custom-")
								scroll-margin 14
                maximum-scroll-margin 0.5
								scroll-step 1
								next-screen-context-lines 5
								scroll-conservatively 100000
								scroll-preserve-screen-position 1
								recenter-positions '(5 top bottom)
								bookmark-default-file "~/doom-configs/.emacs.d/bookmarks"
								cursor-in-non-selected-windows nil
								help-window-select t
								large-file-warning-threshold nil
								show-paren-delay 0
								initial-major-mode 'lisp-interaction-mode
								initial-buffer-choice t)
	(advice-add 'bookmark-set-internal :after '(funcall 'bookmark=save))) ;; prot

;;; Package Management
(with-delayed-execution-priority-high
	(load "~/.emacs.d/packages.el" nil t))

;;;; Yay Evil!
(with-delayed-execution
	(setq undo-limit 67108864
				undo-strong-limit 100663296
				undo-outer-limit 1006632960)
	(undo-fu-session-global-mode))

(with-delayed-execution-priority-high
	(define-key global-map (kbd "<escape>") #'keyboard-escape-quit)
	(setq evil-want-keybinding t ;; nil
				evil-split-window-below t
				evil-vsplit-window-right t
				evil-want-C-i-jump nil
				evil-want-C-u-scroll t
				evil-cross-lines t
				evil-move-cursor-back nil
				evil-want-Y-yank-to-eol t
				evil-auto-indent t
				evil-move-beyond-eol t
				evil-shift-width 2
				evil-motion-state-modes nil
				evil-disable-insert-state-bindings t
				evil-undo-system 'undo-fu
				evil-normal-state-tag	 (propertize "▊" 'face '(:foreground "#6272a4"))
				evil-motion-state-tag	 (propertize "▊" 'face '(:foreground "LightSteelBlue1"))
				evil-emacs-state-tag	 (propertize "▊" 'face '(:foreground "dim gray"))
				evil-insert-state-tag	 (propertize "▊" 'face '(:foreground "khaki1"))
				evil-visual-state-tag	 (propertize "▊" 'face '(:foreground "medium spring green")))

	(evil-mode 1)
	(evil-escape-mode)
	(setq-default evil-escape-key-sequence "jk")
	(add-to-list 'evil-highlight-closing-paren-at-point-states 'normal t)
	(evil-define-key '(normal motion) special-mode-map "q" #'quit-window) ;; QoL
	(with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil)))

;;;; Visuals
(with-delayed-execution
	(add-hook 'text-mode-hook #'outli-mode)
	(add-hook 'prog-mode-hook #'outli-mode)
	(setq outli-default-nobar t)

	(if (display-graphic-p)
			(progn
				(add-hook 'text-mode-hook #'olivetti-mode)
				(add-hook 'eww-mode-hook #'olivetti-mode)
				(add-hook 'Info-mode-hook #'olivetti-mode)
				(add-hook 'prog-mode-hook #'olivetti-mode)))
	(with-eval-after-load 'olivetti
		(setq-default olivetti-body-width 90))
	
	(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
	;; (add-hook 'prog-mode-hook #'(lambda ()
	;;															 (setq-local display-line-numbers 'relative)))
	(evil-define-key 'normal 'global (kbd "<leader>tz") #'olivetti-mode))
 
;;;; Modeline
(with-delayed-execution
	(setq-default flymake-mode-line-counter-format
								'("" flymake-mode-line-error-counter
									flymake-mode-line-warning-counter
									flymake-mode-line-note-counter "")
								global-mode-string nil
								flymake-mode-line-format
								'(" " flymake-mode-line-exception flymake-mode-line-counters))
	(setq-default mode-line-end-spaces '(" " ;(vc-mode vc-mode) " "
																			 mode-line-misc-info " %l %p "))
	(defun my/ml-padding ()
		(let ((r-length (length (format-mode-line mode-line-end-spaces))))
			(propertize " "
									'display `(space :align-to (- right ,r-length)))))
	(setq-default mode-line-format
								'((:eval evil-mode-line-tag) "%e "
									(:eval (if (buffer-modified-p)
														 (propertize "%b" 'face '(:slant italic)
																				 'help-echo (buffer-file-name))
													 (propertize "%b" 'help-echo (buffer-file-name))))
									" "
									(:eval (when (bound-and-true-p flymake-mode)
													 flymake-mode-line-format))
									(:eval (my/ml-padding))
									mode-line-end-spaces)))

;;;; Vertico/Marginalia
(with-delayed-execution-priority-high
	(vertico-mode)
	(savehist-mode)
	(keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
	(keymap-set vertico-map "RET" #'vertico-directory-enter)
	(keymap-set vertico-map "TAB" #'vertico-next)
	(keymap-set vertico-map "<backtab>" #'vertico-previous)
	(keymap-set vertico-map "S-TAB" #'vertico-previous)
	(keymap-set vertico-map "C-j" #'vertico-next)
	(keymap-set vertico-map "C-k" #'vertico-previous)
	(setq vertico-scroll-margin 0
				vertico-resize nil
				vertico-cycle t))

(with-delayed-execution
	(marginalia-mode))

;;;; Consult/Orderless
;; orderleass
(with-delayed-execution
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

;; consult
(with-delayed-execution
	(with-eval-after-load 'consult
		(push ".newsrc-dribble" consult-preview-excluded-files)
		(require 'consult-gh)
		(setq consult-gh-show-preview t
					consult-gh-preview-key "M-o"
					consult-gh-preview-buffer-mode 'org-mode))
	(when (executable-find "rg")
		(setq grep-program "rg"))
	(when (executable-find "fd")
		(setq find-program "fd"))
	(setq register-preview-function #'consult-register-format
				xref-show-xrefs-function #'consult-xref
				xref-show-definitions-function #'consult-xref)
	(setq recentf-max-menu-items 10000
				recentf-max-saved-items 10000
				recentf-save-file	 "~/.emacs.d/.recentf"
				recentf-exclude '(".recentf" "\\.gpg\\")
				recentf-filename-handlers '(substring-no-properties
																		abbreviate-file-name))
	(recentf-mode)
	(add-hook 'kill-emacs-hook #'recentf-cleanup)
	(global-set-key (kbd "C-x f") 'consult-recent-file)
	(global-set-key (kbd "C-<return>") 'consult-bookmark)
	(global-set-key (kbd "C-x b") 'consult-buffer)
	(global-set-key [remap list-buffers] 'consult-buffer)
	(global-set-key [remap isearch-forward] 'consult-line))

;;;; Helpful/elisp-demos
(with-delayed-execution
	(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
	(evil-define-key 'normal emacs-lisp-mode-map "K" #'helpful-at-point)
	(evil-define-key '(normal motion) helpful-mode-map
		"K" #'helpful-at-point
		"q" #'(lambda () (interactive) (quit-window t)))
	(global-set-key (kbd "C-h f") #'helpful-callable)
	(global-set-key (kbd "C-h v") #'helpful-variable)
	(global-set-key (kbd "C-h k") #'helpful-key)
	(global-set-key (kbd "C-h '") #'describe-face)
	(global-set-key (kbd "C-h x") #'helpful-command))

;;;; Cape/Corfu
(with-delayed-execution
	(global-corfu-mode)
	(add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
	(add-hook 'evil-insert-state-exit-hook 'corfu-quit)
	(with-eval-after-load 'corfu
		;; TnG completion
		(keymap-set corfu-map "TAB" #'corfu-next)
		(define-key corfu-map [tab] #'corfu-next)
		(keymap-set corfu-map "S-TAB" #'corfu-previous)
		(define-key corfu-map [backtab] #'corfu-previous)

		(setq corfu-cycle t
					corfu-auto t
					corfu-auto-prefix 2
					corfu-auto-delay 0
					corfu-separator 32
					corfu-quit-no-match t
					corfu-quit-at-boundary 'separator
					corfu-preview-current nil
					corfu-popupinfo-delay '(0.2 . 0.1)
					corfu-preselect-first nil
					tab-always-indent 'complete)

			(add-hook 'eshell-mode-hook (lambda ()
																	(setq-local corfu-auto nil)
																	(corfu-mode)))))

(with-delayed-execution
	(defun my/add-capfs ()
		(push 'cape-dabbrev completion-at-point-functions)
		(push 'cape-symbol completion-at-point-functions)
		(push 'cape-keyword completion-at-point-functions)
		(push 'cape-file completion-at-point-functions))
	(add-hook 'prog-mode-hook #'my/add-capfs)
	(add-hook 'text-mode-hook #'my/add-capfs))

(with-delayed-execution
	(setq which-key-idle-delay 0.5)
	(which-key-mode))

;;;; git/tempel
(with-delayed-execution
	(add-hook 'text-mode-hook #'git-gutter-mode)
	(add-hook 'prog-mode-hook #'git-gutter-mode)
	(with-eval-after-load 'git-gutter
		(evil-define-key 'normal 'git-gutter-mode
			(kbd "<localleader>gs") #'git-gutter:stage-hunk
			(kbd "<localleader>gp") #'git-gutter:popup-hunk
			(kbd "<localleader>gj") #'git-gutter:next-hunk
			(kbd "<localleader>gk") #'git-gutter:previous-hunk
			(kbd "<localleader>gr") #'git-gutter:revert-hunk)
		(set-face-foreground 'git-gutter:modified "deep sky blue")
		(set-face-foreground 'git-gutter:deleted "dark orange"))
	(setq git-gutter:update-interval 0.2
				fringes-outside-margins t
				git-gutter:added-sign "│"
				git-gutter:modified-sign "│"
				git-gutter:deleted-sign "│"))

(with-delayed-execution
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

;; tempel (TODO: incorporate aas with tempel?)
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
		(define-key tempel-map (kbd "<tab>") 'tempel-next)
		(define-key tempel-map (kbd "TAB") 'tempel-next)
		(define-key tempel-map (kbd "<backtab>") 'tempel-previous))
	(setq tempel-path "~/.emacs.d/templates.el"))

;;;; eglot
(with-delayed-execution
	(with-eval-after-load 'eldoc-box
		(setq eldoc-box-max-pixel-width 600
					eldoc-box-max-pixel-height 700
					eldoc-box-only-multi-line t)))
(with-delayed-execution
	(evil-define-key '(normal motion) prog-mode-map "X" #'consult-flymake)
	(if (display-graphic-p)
			(evil-define-key '(normal motion) prog-mode-map "K" #'eldoc-box-help-at-point)
		(evil-define-key '(normal motion) prog-mode-map "K" #'eldoc-doc-buffer))

	(with-eval-after-load 'eglot
		(if (display-graphic-p)
				(setq eldoc-echo-area-prefer-doc-buffer t
							eldoc-echo-area-use-multiline-p nil))
		(add-hook 'eglot-managed-mode-hook 'tempel-setup-capf)
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

;;;; Dirvish
(with-delayed-execution
	(dirvish-override-dired-mode)
	;; (add-hook 'dirvish-override-dired-mode-hook 'dirvish-emerge-mode)
	(evil-define-key 'normal dirvish-mode-map "q" #'dirvish-quit)
	;; FIXME: dirvish dir preview
	(dirvish-define-preview ls (file)
		(when (file-directory-p file)
			`(shell . ("ls" "-ahl" "--almost-all" "--group-directories-first" ,file))))
	(push 'ls dirvish-preview-dispatchers)
	(setq dirvish-attributes '(vc-state git-msg))
	(setq dirvish-default-layout '(0 0.11 0.55)
				dirvish-reuse-session nil
				dirvish-emerge-groups
				'(("Recent files" (predicate . recent-files-2h))
					("Documents" (extensions "pdf" "tex" "bib" "epub"))
					("Video" (extensions "mp4" "mkv" "webm"))
					("Pictures" (extensions "jpg" "png" "svg" "gif"))
					("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
					("Archives" (extensions "gz" "rar" "zip")))))

;;;; Leetcode
(with-delayed-execution
	(defun set-exec-path-from-shell-PATH ()
		"Set up Emacs' `exec-path' and PATH environment variable to match
		 that used by the user's shell."
		(interactive)
		(let ((path-from-shell (replace-regexp-in-string
														"[ \t\n]*$" "" (shell-command-to-string
																						"$SHELL --login -c 'echo $PATH'"
																						))))
			(setq exec-path (split-string path-from-shell path-separator))))

	(set-exec-path-from-shell-PATH)

	(with-eval-after-load 'leetcode
		(setq leetcode-prefer-language "python3"
					leetcode-save-solutions t
					leetcode-directory "/mnt/Data/Documents/problems/leetcode")
		(keymap-set leetcode--problems-mode-map "j" #'next-line)
		(define-key leetcode-solution-mode-map (kbd "\C-q") #'leetcode-quit)
		(keymap-set leetcode--problems-mode-map "k" #'previous-line)
		(keymap-set leetcode--problems-mode-map "q" #'leetcode-quit))

	(defun my/leetcode-problem-at-point ()
		"Open leetcode problem by name using string at point. Run after M-x leetcode"
		(interactive)
		(let ((prob (replace-regexp-in-string "\\\"" "" (thing-at-point 'string))))
			(tab-new)
			(leetcode-show-problem-by-slug
			 (leetcode--slugify-title prob))))
	(with-eval-after-load 'org
		(define-key org-mode-map (kbd "C-l") #'my/leetcode-problem-at-point)))

;;; Compilation
(with-delayed-execution
	(with-eval-after-load 'compile
		(setq fancy-compilation-override-colors nil)
		(fancy-compilation-mode)
		(setq compilation-scroll-output 'first-error
					compilation-always-kill t
					compilation-environment '("TERM=xterm-256color"))) ;; flat
	(with-eval-after-load 'smart-compile
		(push '("\\.rkt\\'" . "racket %F") smart-compile-alist)
		(push '("\\.rs\\'" . "rustc %F && ./%n") smart-compile-alist)
		;; (push '("\\.rs\\'" . "cargo build && cargo run") smart-compile-alist)
		(push '("\\.java\\'" . "javac %F && java ./%n") smart-compile-alist)
		(push '("\\.py\\'" . "python %F < in%n") smart-compile-alist)
		(push '("\\.cpp\\'" .
						"g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion -O2 -DLOCAL -I/mnt/Data/Documents/problems/include %F && ./a.out < in%n")
					smart-compile-alist))
	;; Copy input from clipboard
	(defun paste-input (&optional arg)
		(interactive)
		(find-file (concat "in" (file-name-sans-extension
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
															 my/eval-auto-insert-init-form])
	(define-auto-insert "\.py" ["comp.py"
															my/eval-auto-insert-init-form]))

;;; Key binds / Key maps
(with-delayed-execution-priority-high
	(evil-set-leader 'normal (kbd "SPC")) ;; SPC is leader
	(evil-set-leader 'normal "'" t) ;; ' is localleader
	(evil-define-key 'normal 'global
		(kbd "<leader>r") #'query-replace-regexp
		(kbd "<leader>SPC") #'consult-buffer
		(kbd "<leader>fr") #'consult-recent-file
		(kbd "<leader>fs") #'save-buffer
		(kbd "<leader>qq") #'save-buffers-kill-emacs
		(kbd "<leader>fp") #'(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
		(kbd "<leader>gg") #'magit
		(kbd "<leader>x") #'org-capture
		(kbd "<leader>oa") #'org-agenda
		(kbd "<leader>om") #'gnus
		(kbd "<leader>ot") #'eshell)
	(evil-define-key 'visual 'global (kbd ", TAB") #'untabify)
	(evil-define-key 'normal 'global
		")" #'evil-next-close-paren
		"(" #'evil-previous-open-paren
		"-" #'dirvish
		"+" #'er/expand-region
		(kbd "C-f") #'find-file
		";" #'evil-ex
		":" #'evil-repeat-find-char
		(kbd "C-t") #'tab-new
		(kbd "C-w") #'tab-close
		(kbd "A-f") #'fill-paragraph
		(kbd "gcc") #'comment-line)
	(evil-define-key '(normal motion) Info-mode-map
		"RET" #'Info-follow-nearest-node
		(kbd "<ret>") #'Info-follow-nearest-node
		"h" #'Info-backward-node
		"l" #'Info-follow-nearest-node
    "n" #'Info-forward-node
    "p" #'Info-backward-node
    "J" #'Info-next
    "K" #'Info-prev
		"u" #'Info-up
		"U" #'Info-top-node
		"gt" 'Info-toc
		"g?" #'Info-summary
		"q" #'quit-window)
	(global-set-key [f2] #'save-buffer)
	(global-set-key [f3] #'paste-input)
	(global-set-key [f4] #'smart-compile)
	(global-set-key [f8] #'window-toggle-side-windows)
	(global-set-key [f9] #'mark-whole-buffer)
	(global-set-key [f10] #'kill-current-buffer)
	(global-set-key (kbd "C-x x") #'org-capture)
	(global-set-key (kbd "C-c a") #'org-agenda)
	(global-set-key (kbd "C-c m") #'gnus)
	(global-set-key (kbd "C-'") #'er/expand-region)
	(global-set-key (kbd "C-;") #'eval-expression)
	(global-set-key (kbd "C-t") #'tab-new)
	(global-set-key (kbd "C-w") #'tab-close)
	(global-set-key (kbd "M-<down>") #'scroll-other-window)
	(global-set-key (kbd "M-<up>") #'scroll-other-window-down)
	;; window management under alt
	(global-set-key (kbd "M-k") #'windmove-up)
	(global-set-key (kbd "M-j") #'windmove-down)
	(global-set-key (kbd "M-h") #'windmove-left)
	(global-set-key (kbd "M-l") #'windmove-right)
	(global-set-key (kbd "M-r") #'winner-redo)
	(global-set-key (kbd "M-u") #'winner-undo)
	(global-set-key (kbd "M-d") #'delete-window)
	(global-set-key (kbd "M-c") #'delete-other-windows)
	(global-set-key (kbd "M-s") #'split-window-below)
	(global-set-key (kbd "M-v") #'split-window-right))

;;; dired/tabs/windows
;; dired
(with-delayed-execution
	(setq dired-listing-switches
				"-l --almost-all --human-readable --sort=version --group-directories-first")
	(evil-define-key 'normal dired-mode-map
		"O" 'browse-url-of-dired-file
		"h" 'dired-up-directory
		"l" 'dired-find-file
		"q" #'dirvish-quit)
	(define-key dired-mode-map "-" 'dired-up-directory)
	(define-key dired-mode-map "~"
		#'(lambda () (interactive) (dired "/home/prashant/")))
	(define-key dired-mode-map "."
		#'(lambda () (interactive) (dired "/home/prashant/Dotfiles/")))
	(setq dired-dwim-target t
				dired-auto-revert-buffer t
				dired-kill-when-opening-new-dired-buffer t
				dired-recursive-deletes 'always
				dired-recursive-copies 'always))

;; tabs (FIXME: new tab not cleanly init)
(with-eval-after-load 'tab-bar
	(defun +my/tab (tab i)
		(propertize (concat "  " (alist-get 'name tab) "  ")
								'face (funcall tab-bar-tab-face-function tab)))
	(setq tab-bar-close-button-show nil
				tab-bar-new-button-show nil
				tab-bar-separator ""
				tab-bar-tab-name-format-function #'+my/tab
				tab-bar-new-tab-choice "*scratch*"
				tab-bar-tab-name-function 'tab-bar-tab-name-truncated
				tab-bar-tab-name-truncated-max 12))

;; Window configuration (prot)
(with-delayed-execution
	(defun prot/dired-vc-left()
		(interactive)
		(let ((dir (if (eq (vc-root-dir) nil)
									 (dired-noselect default-directory)
								 (dired-noselect (vc-root-dir)))))
			(display-buffer-in-side-window
			 dir `((side . left)
						 (slot . 0)
						 (window-width . 0.2)
						 (window-parameters . ((no-delete-other-windows . t)
																	 (mode-line-format . (" %b"))))))
			(windmove-left)))
	(evil-define-key 'normal 'global (kbd "<leader>op") #'prot/dired-vc-left)
	(setq display-buffer-alist
				'(("\\*e?shell\\*"
					 (display-buffer-in-side-window)
					 (window-height . 0.25)
					 (side . bottom)
					 (slot . -1))
					("\\*\\(Faces\\|compilation\\|git-gutter\\:diff\\)\\*"
					 (display-buffer-in-side-window)
					 (window-height . 0.25)
					 (side . bottom)
					 (slot . 1)))))

;;; Newsticker
;; (fixme: keybinds, layout)
(with-delayed-execution
	(setq newsticker-groups '("PSA" "Gluer" "Matklad" "Lobsters" "DDW" "Arch" "HLTV"))
	(setq newsticker-automatically-mark-visited-items-as-old t)
	(setq newsticker-url-list
				'(("HLTV" "https://www.hltv.org/rss/news")
					("Arch" "https://archlinux.org/feeds/news/")
					("DDW" "https://drewdevault.com/blog/index.xml")
					("Lobsters" "https://lobste.rs/rss")
					("Matklad" "https://matklad.github.io/feed.xml")
					("Gluer" "https://gluer.org/blog/atom.xml")
					("PSA" "https://psa.wf/feed/"))))

;;; Org
(with-delayed-execution
	(setq org-latex-toc-command "\\tableofcontents \\clearpage"
				org-directory "~/Dropbox/org/"
				org-ellipsis "▼"
				org-pretty-entities t
				org-startup-indented t
				org-startup-folded t
				org-src-preserve-indentation t
				;; \usepackage{listings}
				;; org-latex-listings 'engraved
				;; ;; org-latex-src-block-backend 'engraved
				org-latex-pdf-process '("tectonic -X compile %f --outdir=%o -Z shell-escape"))
	(defun my/org-inkscape-watcher (fname)
		"Open inkscape and add tex code for importing the figure"
		(interactive "sName: ")
		(insert (shell-command-to-string (concat "inkscape-figures create '" fname
																						 "' ./figures/"))))
	(with-eval-after-load 'org
		(add-hook 'org-mode-hook 'tempel-setup-capf)
		(add-hook 'org-mode-hook 'visual-line-mode)
		(define-key org-mode-map [f4] #'org-latex-export-to-pdf)
		(define-key org-mode-map [f5] #'my/org-inkscape-watcher)))
;;;; Org-Capture
(with-delayed-execution
	(with-eval-after-load 'org-capture
		(add-hook 'org-capture-mode-hook 'evil-insert-state)
		(setq +org-capture-readings-file "~/Dropbox/org/links.org"
					+org-capture-log-file "~/Dropbox/org/log.org"
					+org-capture-todo-file "~/Dropbox/org/inbox.org"
					+org-capture-journal-file "~/Dropbox/org/journal.org"
					org-capture-templates
					'(("t" "Personal todo" entry
						 (file+headline +org-capture-todo-file "todo")
						 "* TODO %?\n%i\n%a%f" :prepend t)
						("n" "Personal notes" entry
						 (file+headline +org-capture-notes-file "Notes")
						 "* %u %?\n%i\n%a" :prepend t)
						("r" "Readings" entry
						 (file+headline +org-capture-readings-file "Readings")
						 "* %?" :prepend t)
						("l" "Personal Log" entry
						 (file +org-capture-log-file)
						 "* %T %?")
						("j" "Journal" entry
						 (file+olp+datetree +org-capture-journal-file)
						 "* %U %?\n** What happened \n
** What is going through your mind? \n
** What emotions are you feeling? \n
** What thought pattern do you recognize? \n
** How can you think about the situation differently? " :prepend t)))))

;;;; Org-agenda
(with-delayed-execution
	(with-eval-after-load 'org-agenda
		(setq org-agenda-start-with-log-mode t
					org-log-done t
					org-log-into-drawer t
					org-agenda-breadcrumbs-separator " ❱ "
					org-agenda-files '("~/Dropbox/org/todo.org" "~/Dropbox/org/inbox.org"))))

;;; Random
(with-delayed-execution
	;; pdf-tools
	(push (expand-file-name "el-get/pdf-tools/lisp" user-emacs-directory) load-path)
	(require 'pdf-tools)
	(push '("\\.pdf\\'" . pdf-view-mode) auto-mode-alist)
	;; FIXME: 
	(add-hook 'pdf-view-mode-hook 'evil-normal-state)
	(evil-define-key 'normal pdf-view-mode-map
		"j" 'pdf-view-next-line-or-next-page
		"k" 'pdf-view-previous-line-or-previous-page
		"J" 'pdf-view-next-page-command
		"K" 'pdf-view-previous-page-command
		"gg" 'pdf-view-goto-page
		"w" 'pdf-view-fit-width-to-window
		"f" 'pdf-view-fit-height-to-window
		"sb" 'pdf-view-set-slice-from-bounding-box
		"sr" 'pdf-view-reset-slice
		"zm" 'pdf-view-themed-minor-mode
		"o" 'pdf-outline)
	;; ref: noctuid
	(defun noct-maybe-sudo-edit ()
		"If the current file is exists and is unwritable, edit it as root with sudo."
		(interactive)
		(let* ((file (or buffer-file-name
										 (when (derived-mode-p 'dired-mode 'wdired-mode)
											 default-directory)))
					 (parent (file-name-directory file))
					 ;; don't try to lookup password with auth-source
					 auth-sources)
			(when (and file
								 (not (file-writable-p file))
								 (or (file-exists-p file)
										 ;; might want to create a file
										 (and (file-exists-p parent)
													(not (file-writable-p parent))))
								 ;; don't want to edit Emacs source files as root
								 (not (string-match "/usr/share/emacs/.*" file)))
				(let ((method (file-remote-p default-directory 'method))
							(user (file-remote-p default-directory 'user))
							(host (file-remote-p default-directory 'host))
							(localname (file-remote-p file 'localname)))
					(find-file (if method
												 (concat "/" method ":" user "@" host
																 "|sudo:" host ":" localname)
											 (concat "/sudo:root@localhost:" file)))))))
	(evil-define-key 'normal 'global (kbd "<leader>fu") #'noct-maybe-sudo-edit)
 
	(defun +setup-text-mode-left-margin () ;; teco
		(interactive)
		(when (and (derived-mode-p 'text-mode)
							 (eq (current-buffer)
									 (window-buffer (frame-selected-window))))
			(setq left-margin-width (if display-line-numbers
																	0 2))
			(set-window-buffer (get-buffer-window (current-buffer))
												 (current-buffer))))
	(add-hook 'window-configuration-change-hook #'+setup-text-mode-left-margin)
	(add-hook 'display-line-numbers-mode-hook #'+setup-text-mode-left-margin)
	(add-hook 'text-mode-hook #'+setup-text-mode-left-margin)

	(set-display-table-slot standard-display-table 'truncation 32) ;; hides $
	(set-display-table-slot standard-display-table 'wrap 32) ;; hides \
	(save-place-mode 1)
	(global-auto-revert-mode t)
	(setq global-auto-revert-non-file-buffers t
				auto-revert-verbose nil)
	(winner-mode 1)
	(push '("README\\.md\\'" . gfm-mode) auto-mode-alist)
	(push '("rc\\'" . conf-unix-mode) auto-mode-alist)

	;; eww
	(with-eval-after-load 'shr
		(require 'shr-tag-pre-highlight)
		(add-to-list 'shr-external-rendering-functions
								 '(pre . shr-tag-pre-highlight)))

	(eval-after-load 'eww
		(evil-define-key 'normal 'eww-mode-map
			"q" #'quit-window
			"H" #'eww-back-url))

	(with-delayed-execution
		(blink-cursor-mode -1)
		(setq suggest-key-bindings nil))
	(show-paren-mode)
	(global-hl-line-mode)
	(add-hook 'after-save-hook
						'executable-make-buffer-file-executable-if-script-p)

	;; auto pair completion
	(add-hook 'prog-mode-hook (electric-pair-mode t))
	(add-hook 'prog-mode-hook (show-paren-mode t))
	(fset 'yes-or-no-p 'y-or-n-p))

;; Terminal session (taken from doom)
(with-delayed-execution
	(unless (display-graphic-p)
		(eldoc-mode +1)
		(corfu-terminal-mode +1))
	(xterm-mouse-mode 1)
	(turn-on-xclip)
	(etcc-on))

;; Gnus
(with-delayed-execution
	(require 'nice-citation)
	(load "~/.emacs.d/+gnus" nil t))

;;; Config performance measure
(let ((elapsed (float-time (time-subtract (current-time) t0))))
	(makunbound 't0)
	(message "Spent %.3fs in init.el" elapsed))

(provide 'init)
;;; init.el ends here

;; ------------------------- This contains code for the future.
;; (if (version< emacs-version "29.0")
;;		 (pixel-scroll-mode)
;;	 (pixel-scroll-precision-mode 1)
;;	 (setq dired-mouse-drag-files t)
;;	 (setq mouse-drag-and-drop-region-cross-program t)
;;	 (setq pixel-scroll-precision-large-scroll-height 35.0))
