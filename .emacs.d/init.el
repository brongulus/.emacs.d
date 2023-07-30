;;; init.el --- Description -*- lexical-binding: t; -*-
;; Author: Prashant Tak <prashantrameshtak@gmail.com>
;; Created: June 11, 2023
;; Modified: July 10, 2023
;; Version: 0.0.3
;; This file is not part of GNU Emacs.
;;; Commentary:
;;  Look into context-menu-mode but without tooltip!
;;  TODO: use-package ify everything
;;  Most completion systems follow TnG, tab to choose next and RET to select
;;  Ref 1: https://zenn.dev/takeokunn/articles/56010618502ccc
;;  Ref 2: https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4
;;  Ref 3: https://github.com/doomemacs/doomemacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;;; Code:

;;; Startup hacks
;; -----------------------------------------------------------------------------
;; ;; (el-get-bundle benchmark-init)
;; (load "/home/prashant/.emacs.d/el-get/benchmark-init/benchmark-init.el"
;;       'no-error nil 'no-suffix)
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;; -----------------------------------------------------------------------------
;; ;; (el-get-bundle esup)
;; -----------------------------------------------------------------------------
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
                tab-width 2
                indent-tabs-mode nil
                enable-recursive-minibuffers t
                fill-column 80
                delete-selection-mode t
                mouse-yank-at-point t
                custom-file (make-temp-file "emacs-custom-")
                ;; scroll-margin 5
                maximum-scroll-margin 0.5
                scroll-step 1
                next-screen-context-lines 5
                scroll-conservatively 100000
                scroll-preserve-screen-position 1
                pixel-scroll-precision-large-scroll-height 35.0
                recenter-positions '(5 top bottom)
                bookmark-default-file "~/doom-configs/.emacs.d/bookmarks"
                bookmark-save-flag 1
                Man-notify-method 'aggressive
                cursor-in-non-selected-windows nil
                help-window-select t
                large-file-warning-threshold nil
                show-paren-delay 0))

;;; Package Management
(with-delayed-execution-priority-high
  ;(load "~/.emacs.d/packages.el" nil t);; Package.el setup
  (require 'package)
  (push '("gnu" . "https://elpa.gnu.org/packages/") package-archives)
  (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (package-initialize)
  
  (eval-and-compile
    (setq package-native-compile t
          package-install-upgrade-built-in t
          use-package-always-ensure t
          use-package-always-defer t
          use-package-expand-minimally t))

  ;; vc-use-package (in-built in 30)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package)

  (use-package on ;; provides hooks akin to doom
    :vc (:fetcher gitlab :repo "ajgrf/on.el"))
  (use-package compat) ;; for minad's pkgs
  (use-package nerd-icons)) ;; (nerd-icons-install-fonts)

;;;; Yay Evil!
(with-delayed-execution
  (use-package undo-fu)
  (use-package undo-fu-session
    :after undo-fu)
  (setq undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960)
  (undo-fu-session-global-mode))

(with-delayed-execution-priority-high
  (use-package evil
    :demand t
    :defer nil
    :init
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
          evil-normal-state-tag  (propertize "▊" 'face '(:inherit mode-line))
          evil-motion-state-tag  (propertize "▊" 'face '(:inherit mode-line))
          evil-emacs-state-tag   (propertize "▊" 'face '(:inherit mode-line-inactive))
          evil-insert-state-tag  (propertize "▊" 'face '(:inherit minibuffer-prompt))
          evil-visual-state-tag  (propertize "▊" 'face '(:inherit success)))
    :config
    (evil-mode 1)
    (add-to-list 'evil-highlight-closing-paren-at-point-states 'normal t)
    (evil-define-key '(normal motion) special-mode-map "q" #'quit-window) ;; QoL
    (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil)))

  (use-package evil-escape
    :after evil
    :config
    (evil-escape-mode 1)
    (setq-default evil-escape-key-sequence "jk"
                  evil-escape-delay 0.15)))

;;;; Vertico/Marginalia
(with-delayed-execution-priority-high
  (use-package vertico)
  (vertico-mode)
  (savehist-mode)
  (keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "TAB" #'vertico-next)
  (keymap-set vertico-map "<backtab>" #'vertico-previous)
  (keymap-set vertico-map "S-TAB" #'vertico-previous)
  (keymap-set vertico-map "M-TAB" #'vertico-previous)
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous)
  (setq vertico-scroll-margin 0
        vertico-resize nil
        vertico-cycle t))

(with-delayed-execution
  (use-package marginalia)
  (use-package nerd-icons-completion)
  (marginalia-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode))

;;;; Visuals
(with-delayed-execution
  ;; info-fontification (FIXME)
  ;; (use-package info+
    ;; :vc (:fetcher "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/info%2B.el"))
  ;; (autoload 'info "info+" nil t) ;; FIXME:
  (setq Info-use-header-line nil
        Info-breadcrumbs-in-header-flag nil
        Info-breadcrumbs-in-mode-line-mode t)
  (add-hook 'Info-mode-hook
            #'(lambda () (setq-local evil-normal-state-cursor 'hbar)))

  ;; olivetti
  (use-package olivetti)
  (when (display-graphic-p)
      (progn
        (add-hook 'text-mode-hook #'olivetti-mode)
        (add-hook 'eww-mode-hook #'olivetti-mode)
        (add-hook 'Info-mode-hook #'olivetti-mode)
        (add-hook 'prog-mode-hook #'olivetti-mode)))
  (with-eval-after-load 'olivetti
    (setq-default olivetti-body-width 90))
  
  ;; outli
  (use-package outli
    :vc (:fetcher github :repo jdtsmith/outli))
  (add-hook 'text-mode-hook #'outli-mode)
  (add-hook 'prog-mode-hook #'outli-mode)
  (setq outli-default-nobar t)
 
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (evil-define-key 'normal 'global
    (kbd "<leader>ll") '(lambda () (interactive)
                          (setq-local display-line-numbers 'relative))
    (kbd "<leader>zz") #'olivetti-mode))
 
;;;; Modeline
(with-delayed-execution
  (setq-default flymake-mode-line-counter-format
                '("" flymake-mode-line-error-counter
                  flymake-mode-line-warning-counter
                  flymake-mode-line-note-counter "")
                global-mode-string nil
                flymake-mode-line-format
                '(" " flymake-mode-line-exception flymake-mode-line-counters))
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     (if (display-graphic-p)
                         (format "%s" (nerd-icons-codicon "nf-cod-git_pull_request"))
                       (format "%s " (nerd-icons-codicon "nf-cod-git_pull_request")))
                     vc-mode)))
        (setq vc-mode noback))))
  (setq-default mode-line-end-spaces '(" " mode-line-misc-info "  "
                                       (vc-mode vc-mode) " %l %p "))
  (defun my/ml-padding ()
    (let ((r-length (length (format-mode-line mode-line-end-spaces))))
      (propertize " "
                  'display `(space :align-to (- right ,r-length)))))
  (setq-default mode-line-format
                '((:eval evil-mode-line-tag) "%e "
                  (:eval (nerd-icons-icon-for-buffer))
                  (:eval (if (buffer-modified-p)
                             (propertize " %b " 'face '(:slant italic)
                                         'help-echo (buffer-file-name))
                           (propertize " %b " 'help-echo (buffer-file-name))))
                  (:eval (when (and (bound-and-true-p flymake-mode)
                                    (mode-line-window-selected-p))
                           flymake-mode-line-format))
                  (:eval (when (mode-line-window-selected-p)
                           (my/ml-padding)))
                  (:eval (when (mode-line-window-selected-p)
                           mode-line-end-spaces)))))

;;;; Consult/Orderless
;; orderless
(with-delayed-execution
  (use-package orderless)
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
  (use-package consult)
  (use-package consult-gh
    :vc (:fetcher github :repo armindarvish/consult-gh))
  (use-package consult-recoll)
  (with-eval-after-load 'consult-recoll
    (defun my/open-mpv (file)
      (async-shell-command
       (format "setsid -f mpv %s" (shell-quote-argument file)) nil nil))
    (push '("video/x-matroska" . my/open-mpv) consult-recoll-open-fns)
    (push '("video/mp4" . my/open-mpv) consult-recoll-open-fns))
  (with-eval-after-load 'consult
    (setq consult-locate-args "locate --ignore-case --regex")
    (require 'consult-gh)
    (push "\\.newsrc-dribble" consult-preview-excluded-files)
    (push "\\.pdf" consult-preview-excluded-files)
    (setq consult-preview-key '(:debounce 1.0 any)
          consult-gh-show-preview t
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
        recentf-save-file  "~/.emacs.d/.recentf"
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
  (use-package helpful)
  (use-package elisp-demos)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  (evil-define-key 'normal emacs-lisp-mode-map "K" #'helpful-at-point)
  (evil-define-key '(normal motion) helpful-mode-map
    "K" #'helpful-at-point)
    ;; "q" #'kill-buffer-and-window)
  (global-set-key (kbd "C-h Q") #'help-quick)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h '") #'describe-face)
  (global-set-key (kbd "C-h x") #'helpful-command))

;;;; Cape/Corfu
(with-delayed-execution
  (use-package corfu)
  (use-package popon)
  (use-package corfu-terminal)
  (use-package kind-icon)
  (use-package cape)
  (global-corfu-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (add-hook 'evil-insert-state-exit-hook 'corfu-quit)
  (with-eval-after-load 'corfu
    ;; TnG completion
    (keymap-set corfu-map "TAB" #'corfu-next)
    (define-key corfu-map [tab] #'corfu-next)
    (keymap-set corfu-map "M-TAB" #'corfu-previous)
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
                                  (corfu-mode)))
      ;; kind-icon to nerd-icons
      (require 'kind-icon)
      (load "~/.emacs.d/+icons" nil t)
      ;; (advice-add 'load-theme :before #'kind-icon-reset-cache) ;; FIXME:
      (push 'kind-icon-margin-formatter corfu-margin-formatters)))

(with-delayed-execution
  (defun my/add-capfs ()
    (push 'cape-dabbrev completion-at-point-functions)
    (push 'cape-keyword completion-at-point-functions)
    (push 'cape-file completion-at-point-functions))
  (add-hook 'prog-mode-hook #'my/add-capfs)
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda () (push 'cape-symbol completion-at-point-functions)))
  (add-hook 'text-mode-hook #'my/add-capfs))

(with-delayed-execution
  (use-package which-key)
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

;;;; git/tempel/indent
(with-delayed-execution
  (use-package indent-bars ;; bad bad bad
    :vc (:fetcher github :repo "jdtsmith/indent-bars")
    :hook ((prog-mode conf-mode) . indent-bars-mode)
    :config
    (setq indent-bars-color '(highlight :face-bg t)
          indent-bars-pattern "."
          indent-bars-width-frac 0.2
          indent-bars-pad-frac 0.2
          indent-bars-color-by-depth nil
          indent-bars-highlight-current-depth '(:face default :blend 0.4)))
  
  (use-package git-gutter)
  (add-hook 'after-change-major-mode-hook #'git-gutter-mode)
  (with-eval-after-load 'modus-themes
    (set-face-background 'modus-themes-fringe-yellow unspecified)
    (set-face-background 'modus-themes-fringe-green unspecified)
    (set-face-background 'modus-themes-fringe-red unspecified))
  (with-eval-after-load 'git-gutter
    (evil-define-key 'normal 'git-gutter-mode
      (kbd "<localleader>gs") #'git-gutter:stage-hunk
      (kbd "<localleader>gp") #'git-gutter:popup-hunk
      (kbd "<localleader>gj") #'git-gutter:next-hunk
      (kbd "<localleader>gk") #'git-gutter:previous-hunk
      (kbd "<localleader>gr") #'git-gutter:revert-hunk)
    (set-face-foreground 'git-gutter:added "chartreuse")
    (set-face-foreground 'git-gutter:modified "deep sky blue")
    (set-face-foreground 'git-gutter:deleted "dark orange"))
  (setq git-gutter:update-interval 0.2
        fringes-outside-margins t
        git-gutter:added-sign "│"
        git-gutter:modified-sign "│"
        git-gutter:deleted-sign "│"))

(with-delayed-execution
  (use-package git-gutter)
  (use-package transient)
  (use-package ghub)
  (use-package magit-popup)
  (use-package with-editor)
  (use-package magit)
  (use-package forge)
  (autoload 'magit "magit" nil t)
  ;; git-timemachine
  (autoload 'magit-file-dispatch "magit" nil t)
  (autoload 'magit-ediff-show-unstaged "magit" nil t)
  (evil-define-key 'normal magit-blob-mode-map
    (kbd "p") #'magit-blob-previous
    (kbd "n") #'magit-blob-previous
    (kbd "g[") #'magit-blob-previous
    (kbd "g]") #'magit-blob-next)
  (global-set-key (kbd "C-c g") 'magit)
  (setq forge-owned-accounts '(("brongulus")))
  (add-hook 'magit-mode-hook #'(lambda () (require 'forge))))

;; tempel (TODO: incorporate aas with tempel?)
(with-delayed-execution
  (use-package tempel)
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

;;;; eglot/eldoc
(with-delayed-execution
  (use-package rustic)
  (use-package zig-mode)
  (use-package eldoc-box)

  (with-eval-after-load 'eldoc-box
    (setq eldoc-box-max-pixel-width 600
          eldoc-box-max-pixel-height 700
          eldoc-box-only-multi-line t)))

;;;;; eldoc/flymake
(with-delayed-execution
  (use-package flymake-collection ;; alternateved
    :after flymake
    :functions (flymake-collection-hook-setup)
    :init (flymake-collection-hook-setup)
    :custom (flymake-collection-hook-ignored-modes
             '(eglot--managed-mode)))
  (setq flymake-suppress-zero-counters t)
  (defun flymake-eldoc-function (report-doc &rest _) ;; lcolonq
    "Document diagnostics at point.Intended for
     `eldoc-documentation-functions' (which see)."
    (let ((diags (flymake-diagnostics (point))))
      (when diags
        (funcall report-doc
                 (mapconcat #'flymake-diagnostic-text diags "\n")
                 :sort 'error))))
  (with-eval-after-load 'eldoc
    (add-to-list 'eldoc-documentation-functions 'flymake-eldoc-function))
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'prog-mode-hook #'flymake-mode)

  (evil-define-key '(normal motion) prog-mode-map
    "X" #'consult-flymake
    "L" #'xref-find-definitions
    "H" #'xref-pop-marker-stack)
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-idle-delay 0.3
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-display-truncation-message nil)
  (if (display-graphic-p)
      (progn
        (defun eldoc-box-scroll-up ()
          "Scroll up in `eldoc-box--frame'"
          (interactive)
          (with-current-buffer eldoc-box--buffer
            (with-selected-frame eldoc-box--frame
              (scroll-down 3))))
        (defun eldoc-box-scroll-down ()
          "Scroll down in `eldoc-box--frame'"
          (interactive)
          (with-current-buffer eldoc-box--buffer
            (with-selected-frame eldoc-box--frame
              (scroll-up 3))))
        (evil-define-key 'insert prog-mode-map
          (kbd "C-l") #'eldoc-box-help-at-point
          (kbd "C-j") 'eldoc-box-scroll-down
          (kbd "C-k") 'eldoc-box-scroll-up)
        (evil-define-key '(normal motion) eglot-mode-map
          "K" #'eldoc-box-help-at-point
          (kbd "C-j") 'eldoc-box-scroll-down
          (kbd "C-k") 'eldoc-box-scroll-up))
    (progn
      (defun my/eldoc-open-switch ()
        (interactive)
        (eldoc-doc-buffer)
        (windmove-down))
      (evil-define-key '(normal motion) prog-mode-map
        "K" #'my/eldoc-open-switch)
      (evil-define-key 'insert prog-mode-map
        (kbd "C-l") #'my/eldoc-open-switch)))

;;;;; eglot
  (with-eval-after-load 'eglot
    (evil-define-key 'normal eglot-mode-map
      (kbd "<leader>ca") 'eglot-code-actions
      (kbd "<leader>ci") 'eglot-inlay-hints-mode)
    (add-hook 'eglot-managed-mode-hook 'tempel-setup-capf)
    ;; pacman -S clang pyright rust-analyzer
    ;; yay -S jdtls jdk-openjdk jre-openjdk
    ;; rustup component add rust-analyzer
    ;; npm install typescript-language-server typescript-eslint-language-service -D
    ;; yay -S gopls
    (push '((c++-mode c-mode) "clangd"
            "--completion-style=detailed"
            "--header-insertion-decorators=0")
          eglot-server-programs)
    (push '(java-mode "jdtls") eglot-server-programs)
    (push '(rust-mode "rust-analyzer") eglot-server-programs))
  (setq eglot-autoshutdown t
        read-process-output-max (* 1024 1024) ;; 1mb for LSP blobs
        rustic-lsp-client 'eglot)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'zig-mode-hook 'eglot-ensure)
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure))

;;;; tree-sitter
;; Run treesit-install-language-grammar
(with-delayed-execution
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
  (push '("\\.ts\\'" . typescript-ts-mode) auto-mode-alist)
  (push '("\\.go\\'" . go-ts-mode) auto-mode-alist)
  (push '("\\.py\\'" . python-ts-mode) auto-mode-alist))
;;; gdb ; TODO: enable tool-bar?
(with-delayed-execution
  (eval-after-load "gud"
    '(progn
       (setq gdb-debuginfod-enable-setting nil)
       (define-key gud-mode-map (kbd "<up>") 'comint-previous-input)
       (define-key gud-mode-map (kbd "<down>") 'comint-next-input)))
  (defun my/gdb-setup-windows ()
    "Lay out the window pattern for option `gdb-many-windows'
    | Source(1) | IO (4) | Local(3) |
    |           |        |          |
    |--------------------+----------|
    | GUD Interaction(0) | BP (2)   |"
    (gdb-get-buffer-create 'gdb-locals-buffer)
    ;; (gdb-get-buffer-create 'gdb-stack-buffer)
    (gdb-get-buffer-create 'gdb-breakpoints-buffer)
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (let ((win0 (selected-window))
          (win2 (split-window nil (/ (* (window-width) 2) 3) 'right))
          (win1 (split-window nil ( / (* (window-height) 1) 4) 'above)))
      (select-window win2)
      (gdb-set-window-buffer (gdb-breakpoints-buffer-name))
      (let ((win3 (split-window nil (/ (window-height) 3) 'above)))
        (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3))
      (select-window win1)
      (set-window-buffer win1 (or (gdb-get-source-buffer)
                                  (list-buffers-noselect)))
      (setq gdb-source-window-list (list (selected-window)))
      ;; (gdb-set-window-buffer (gdb-stack-buffer-name))
      (let ((win4 (if (< (window-width) 95)
                      (split-window nil (/ (* (window-height) 3) 4) 'below)
                    (split-window-right))))
        (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win4))
      (select-window win0)))
  (advice-add 'gdb-setup-windows :override #'my/gdb-setup-windows)
  (add-hook 'gdb-mode-hook #'gdb-many-windows))

;;; Leetcode
(with-delayed-execution
  (use-package log4e)
  (use-package aio)
  (use-package graphql)
  (use-package leetcode)
  (use-package ht)
  (use-package quickrun)
  (use-package oj
    :vc (:fetcher github :repo conao3/oj.el))
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
  (use-package smart-compile
    :vc (:fetcher github :repo "zenitani/elisp"))
  (use-package fancy-compilation)
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

;;;; Misc
(with-delayed-execution
  (use-package sicp)
  (use-package expand-region)
  (use-package multiple-cursors)
  (evil-define-key 'insert 'global
    (kbd "C-d") 'mc/mark-next-like-this
    (kbd "C-v") 'mc/skip-to-next-like-this
    (kbd "M-f") 'mc/mark-all-like-this
    (kbd "M-e") 'mc/edit-lines))

;;; Key binds / Key maps
(with-delayed-execution-priority-high
  (evil-set-leader 'normal (kbd "SPC")) ;; SPC is leader
  (evil-set-leader 'normal "'" t) ;; ' is localleader
  (evil-define-key 'normal 'global
    (kbd "<leader>r") #'query-replace-regexp
    (kbd "<leader>u") #'universal-argument
    (kbd "<leader>SPC") #'consult-buffer
    (kbd "<leader>fr") #'consult-recent-file
    (kbd "<leader>ss") #'consult-recoll
    (kbd "<leader>sa") #'consult-locate
    (kbd "<leader>sb") #'consult-line
    (kbd "<leader>sd") #'consult-find
    (kbd "<leader>sp") #'consult-ripgrep
    (kbd "<leader>sg") #'consult-gh-search-repos
    (kbd "<leader>fs") #'save-buffer
    (kbd "<leader>qq") #'save-buffers-kill-terminal
    (kbd "<leader>fp") #'(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    (kbd "<leader>gg") #'magit
    (kbd "<leader>gh") #'magit-file-dispatch
    (kbd "<leader>gd") #'(lambda () (interactive)
                           (magit-ediff-show-unstaged buffer-file-name))
    (kbd "<leader>x") #'org-capture
    (kbd "<leader>oa") #'org-agenda
    (kbd "<leader>dc") #'org-web-tools-read-url-as-org
    (kbd "<leader>da") #'(lambda () (interactive)
                           (find-file "~/Dropbox/org/links.org")
                           (call-interactively 'org-web-tools-insert-web-page-as-entry)
                           (message "Link successfully added to the archive."))
    (kbd "<leader>or") #'remember
    (kbd "<leader>om") #'mastodon
    (kbd "<leader>on") #'gnus
    (kbd "<leader>ot") #'eshell
    (kbd "<localleader>tt") #'transpose-sexps
    (kbd "<localleader>tw") #'transpose-words
    (kbd "<localleader>tl") #'transpose-lines
    (kbd "<localleader>ts") #'transpose-sentences)
  (evil-define-key 'visual 'global
    (kbd ", TAB") #'untabify
    "T" #'untabify
    "R" #'remember-region)
  (evil-define-key 'insert 'global (kbd "C-o") #'evil-execute-in-normal-state)
  (evil-define-key 'normal 'global
    ")" #'evil-next-close-paren
    "(" #'backward-up-list
    "-" #'dired-jump
    "+" #'er/expand-region
    (kbd "C-f") #'find-file
    ";" #'evil-ex
    ":" #'evil-repeat-find-char
    (kbd "M-.") #'xref-find-definitions
    (kbd "C-t") #'tab-new
    (kbd "C-w") #'tab-close
    ;; (kbd "A-f") #'fill-paragraph
    (kbd "gcc") #'comment-line)
  (evil-define-key '(normal motion) Info-mode-map
    (kbd "RET") #'Info-follow-nearest-node
    "n" #'Info-forward-node
    "p" #'Info-backward-node
    "J" #'evil-scroll-down
    "K" #'helpful-at-point
    "N" #'Info-next
    "P" #'Info-prev
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
  (global-set-key [f12] #'other-window)
  (global-set-key (kbd "C-x x") #'org-capture)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c m") #'gnus)
  (global-set-key (kbd "C-/") #'context-menu-open)
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
  (put 'dired-find-alternate-file 'disabled nil)
  (use-package nerd-icons-dired)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (add-hook 'dired-mode-hook #'(lambda ()
                                 (setq-local truncate-lines t
                                             evil-normal-state-cursor 'hbar)))
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --sort=version --group-directories-first")
  (evil-define-key 'normal dired-mode-map
    (kbd "C-t") 'tab-new
    "O" 'browse-url-of-dired-file
    "h" 'dired-up-directory
    "q" 'kill-this-buffer
    "l" 'dired-find-alternate-file
    (kbd "RET") 'dired-find-alternate-file
    "E" 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map "-" 'dired-up-directory)
  (define-key dired-mode-map (kbd "SPC") nil)
  (define-key dired-mode-map "~"
    #'(lambda () (interactive) (dired "/home/prashant/")))
  (define-key dired-mode-map "."
    #'(lambda () (interactive) (dired "/home/prashant/Dotfiles/")))
  (setq dired-dwim-target t
        dired-auto-revert-buffer t
        dired-mouse-drag-files t
        mouse-drag-and-drop-region-cross-program t
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always))

;; tabs (FIXME: new tab not cleanly init)
(with-delayed-execution
  (with-eval-after-load 'tab-bar
    (defun +my/tab (tab i)
      (propertize (concat "  " (alist-get 'name tab) "  ")
                  'face (funcall tab-bar-tab-face-function tab)))
    (setq tab-bar-close-button-show nil
          tab-bar-new-button-show nil
          tab-bar-show 1
          tab-bar-separator ""
          tab-bar-tab-name-format-function #'+my/tab
          ;; tab-bar-new-tab-choice "*notes*"
          tab-bar-tab-name-function 'tab-bar-tab-name-truncated
          tab-bar-tab-name-truncated-max 12)))

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
        '(("\\*\\(e?shell\\|terminal\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . -1))
          ("\\*\\(Compile-log\\|Locate\\|eldoc\\|compilation\\|git-gutter\\:diff\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 1)))))

;;; Org
(with-delayed-execution
  (use-package org-download)
  (use-package org-web-tools)
  (use-package engrave-faces
    :after org)
  (use-package org-modern
    :after org
    :hook (org-mode . org-modern-mode))
  (setq org-latex-toc-command "\\tableofcontents \\clearpage"
        org-directory "~/Dropbox/org/"
        org-ellipsis "▼"
        org-pretty-entities t
        org-startup-indented t
        org-adapt-indentation t
        org-startup-folded t
        org-src-preserve-indentation t
        org-src-fontify-natively t
        ;; \usepackage{listings}
        org-latex-src-block-backend 'engraved
        org-latex-pdf-process '("tectonic -X compile %f --outdir=%o -Z shell-escape"))
  (defun my/org-inkscape-watcher (fname)
    "Open inkscape and add tex code for importing the figure"
    (interactive "sName: ")
    (insert (shell-command-to-string (concat "inkscape-figures create '" fname
                                             "' ./figures/"))))
  ;; org-dl  (https://emacs.stackexchange.com/questions/71100/pasting-images-from-clipboard-into-orgmode )
  (with-eval-after-load 'org-download
    (evil-define-key 'insert org-mode-map (kbd "C-S-y") 'org-download-clipboard)
    (setq org-download-method 'directory
          org-download-image-dir (concat
                                  (file-name-sans-extension (buffer-file-name)) "-img")
          org-download-image-org-width 800
          org-download-image-attr-list "#+attr_latex: :width 0.6\textwidth"
          org-download-link-format "[[file:%s]]\n"
          org-download-abbreviate-filename-function #'file-relative-name
          org-download-link-format-function
          #'org-download-link-format-function-default))
  
  (with-eval-after-load 'org
    (require 'org-download)
    ;; (add-hook 'org-mode-hook 'org-download-mode)
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

;;;; Org-cv
(with-delayed-execution
  (use-package ox-awesomecv
    :vc (:fetcher gitlab :repo "Titan-C/org-cv")
    :after org
    :init (require 'ox-awesomecv)))
;;; Random
;;;; 4ch?
;; (with-delayed-execution
;;   (use-package! palikar/q4)
;;   (require 'q4))

;;;; ediff (http://yummymelon.com/devnull/surprise-and-emacs-defaults.html )
(with-delayed-execution
  (with-eval-after-load 'ediff
    (setq ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)))
  
;;;; sdcv
;; JMDict: https://github.com/koaeH/JMdict-sdcv
;; Change websters name by editing its .ifo file
(with-delayed-execution
  (use-package lexic)
  (use-package posframe
    :vc (:fetcher github :repo "tumashu/posframe"))
  (use-package sdcv
    :vc (:fetcher github :repo "manateelazycat/sdcv"))
  (with-eval-after-load 'sdcv
    (set-face-attribute
     'sdcv-tooltip-face nil :foreground 'unspecified :background 'unspecified :inherit 'tooltip))
  (setq sdcv-dictionary-data-dir "~/.local/share/stardict/dic/"
        sdcv-tooltip-timeout 30
        sdcv-say-word-p nil
        sdcv-only-data-dir nil
        sdcv-fail-notify-string "No Match Found"
        sdcv-env-lang "en_US.UTF-8"
        sdcv-dictionary-complete-list
        '("Sanseido"
          "org.edrdg.jmdict-jpn-eng"
          "Websters1913")
        sdcv-dictionary-simple-list
        '("Sanseido"
          "org.edrdg.jmdict-jpn-eng"
          "Websters1913")))

;;;; pdf-tools
(with-delayed-execution
 (use-package pdf-tools)
  (push '("\\.pdf\\'" . pdf-view-mode) auto-mode-alist)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-page-to-window)
  (evil-define-key 'normal pdf-view-mode-map
    "q" 'kill-this-buffer
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
    "o" 'pdf-outline))

;;;; Nov
(with-delayed-execution
  (use-package nov)
  (add-hook 'nov-mode-hook #'olivetti-mode)
  (setq nov-header-line-format nil)
  (evil-define-key 'visual nov-mode-map
    "K" #'sdcv-search-pointer+)
  (evil-define-key 'normal nov-mode-map
    "K" #'sdcv-search-pointer+
    "n" #'nov-scroll-up
    "p" #'nov-scroll-down)
  (add-hook 'nov-mode-hook
            (lambda ()
              (setq-local evil-normal-state-cursor 'hbar
                          ;; global-hl-line-mode nil
                          ;; scroll-margin 100
                          mode-line-format nil
                          olivetti-body-width 120)
              (face-remap-add-relative
               'variable-pitch :family "Noto Serif JP" :height 150)))
  (push '("\\.epub\\'" . nov-mode) auto-mode-alist))

;;;; Misc
(with-delayed-execution
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
 
  (set-display-table-slot standard-display-table 'truncation 32) ;; hides $
  (set-display-table-slot standard-display-table 'wrap 32) ;; hides \
  (save-place-mode 1)
  (pixel-scroll-precision-mode 1)
  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (winner-mode 1)
  (setq markdown-command "multimarkdown"
        markdown-fontify-code-blocks-natively t
        markdown-max-image-size '(800 . 800)
        markdown-display-remote-images t)
  (add-hook 'markdown-mode-hook #'markdown-toggle-inline-images)
  (push '("\\.md\\'" . gfm-view-mode) auto-mode-alist)
  (push '("rc\\'" . conf-unix-mode) auto-mode-alist))

;;;; mastodon
(with-delayed-execution
  (use-package ts)
  (use-package persist)
  (use-package request)
  (use-package mastodon
    :vc (mastodon.el :url "https://codeberg.org/martianh/mastodon.el.git"))
  (use-package mastodon-alt
    :vc (:fetcher github :repo rougier/mastodon-alt))
  (setq mastodon-instance-url "https://emacs.ch"
        mastodon-auth-source-file "~/.authinfo"
        mastodon-alt-tl-box-boosted nil
        mastodon-alt-tl-box-prefix ""
        mastodon-active-user "brongulus")
  (when (display-graphic-p)
      (add-hook 'mastodon-mode-hook #'olivetti-mode))
  (add-hook 'mastodon-mode-hook #'mastodon-alt-tl-activate)
  (autoload 'mastodon "mastodon-alt" nil t)
  (with-eval-after-load 'mastodon-tl
    (push '(reply "" . "R") mastodon-tl--symbols)
    (push '(boost "" . "B") mastodon-tl--symbols)
    (push '(favourite "" . "F") mastodon-tl--symbols)
    (push '(bookmark "" . "K") mastodon-tl--symbols)
    (evil-make-overriding-map mastodon-mode-map 'normal))
  (evil-define-key 'normal mastodon-mode-map
    "q" #'kill-current-buffer))
  
;;;; eww
(with-delayed-execution
  (use-package language-detection)
  (use-package shr-tag-pre-highlight)
  (with-eval-after-load 'shr
    (require 'shr-tag-pre-highlight)
    (add-to-list 'shr-external-rendering-functions
                 '(pre . shr-tag-pre-highlight)))

  (defun my/eww-redirect-urls (url)
    (replace-regexp-in-string "www.reddit.com" "old.reddit.com" url))
  (setq eww-header-line-format nil
        eww-auto-rename-buffer 'title)
  (with-eval-after-load 'eww
    (push 'my/eww-redirect-urls eww-url-transformers)
    (evil-define-key 'normal eww-mode-map
      "Y" #'eww-copy-page-url
      "q" #'kill-buffer-and-window
      "H" #'eww-back-url)))

;;;; Misc-boogaloo
(with-delayed-execution
  (use-package xclip)
  (use-package evil-terminal-cursor-changer)
  (blink-cursor-mode -1)
  (show-paren-mode)
  (global-hl-line-mode)
  (setq suggest-key-bindings nil
        executable-prefix-env t)
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
  (xclip-mode 1)
  (etcc-on))

;;;; Gnus
(with-delayed-execution-priority-high
 (use-package nice-citation
   :vc (:fetcher github :repo damiencollard/nice-citation))
  (require 'nice-citation)
  (load "~/.emacs.d/+gnus" nil t))

;; (with-delayed-execution
;;   (el-get 'sync))

(provide 'init)
;;; init.el ends here

;; ------------------------- This contains code for the future.
;; (use-package project
;;   :pin gnu
;;   :bind (("C-c k" . #'project-kill-buffers)
;;          ("C-c m" . #'project-compile)
;;          ("C-x f" . #'find-file)
;;          ("C-c f" . #'project-find-file)
;;          ("C-c F" . #'project-switch-project))
;;   :custom
;;   ;; This is one of my favorite things: you can customize
;;   ;; the options shown upon switching projects.
;;   (project-switch-commands
;;    '((project-find-file "Find file")
;;      (magit-project-status "Magit" ?g)
;;      (deadgrep "Grep" ?h)))
;;   (compilation-always-kill t)
;;   (project-vc-merge-submodules nil)
;;   )
