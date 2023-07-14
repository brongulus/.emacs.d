;;; init.el --- Description -*- lexical-binding: t; -*-
;; Author: Prashant Tak <prashantrameshtak@gmail.com>
;; Created: June 11, 2023
;; Modified: July 10, 2023
;; Version: 0.0.3
;; This file is not part of GNU Emacs.
;;; Commentary:
;;  Look into context-menu-mode but without tooltip!
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
                scroll-margin 5
                maximum-scroll-margin 0.5
                scroll-step 1
                next-screen-context-lines 5
                scroll-conservatively 100000
                scroll-preserve-screen-position 1
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
  ;(load "~/.emacs.d/packages.el" nil t)
  (push (expand-file-name "el-get/el-get" user-emacs-directory) load-path)

  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

  (with-eval-after-load 'el-get-git
    (setq el-get-git-shallow-clone t))
  
  (push "~/.emacs.d/el-get-user/recipes" el-get-recipe-path)
  (setq el-get-is-lazy t)

  (el-get-bundle elpa:evil)
  (el-get-bundle compat) ;; for minad's pkgs
  (el-get-bundle evil-escape)
  (el-get-bundle nerd-icons)) ;; (nerd-icons-install-fonts)

;;;; Yay Evil!
(with-delayed-execution
  (el-get-bundle undo-fu)
  (el-get-bundle undo-fu-session)
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
        evil-normal-state-tag  (propertize "▊" 'face '(:inherit mode-line))
        evil-motion-state-tag  (propertize "▊" 'face '(:inherit mode-line))
        evil-emacs-state-tag   (propertize "▊" 'face '(:inherit mode-line-inactive))
        evil-insert-state-tag  (propertize "▊" 'face '(:inherit minibuffer-prompt))
        evil-visual-state-tag  (propertize "▊" 'face '(:inherit success)))

  (evil-mode 1)
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (add-to-list 'evil-highlight-closing-paren-at-point-states 'normal t)
  (evil-define-key '(normal motion) special-mode-map "q" #'quit-window) ;; QoL
  (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil)))

;;;; Vertico/Marginalia
(with-delayed-execution-priority-high
  (el-get-bundle vertico)
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
  (el-get-bundle marginalia)
  (el-get-bundle nerd-icons-completion)
  (marginalia-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode))

;;;; Visuals
(with-delayed-execution
  ;; outli
  (el-get-bundle jdtsmith/outli)
  (add-hook 'text-mode-hook #'outli-mode)
  (add-hook 'prog-mode-hook #'outli-mode)
  (setq outli-default-nobar t)

  ;; info-fontification
  (el-get-bundle info+)
  (autoload 'info "info+" nil t)
  (setq Info-use-header-line nil
        Info-breadcrumbs-in-header-flag nil
        Info-breadcrumbs-in-mode-line-mode t)
  (add-hook 'Info-mode-hook
            #'(lambda () (setq-local evil-normal-state-cursor 'hbar)))

  ;; olivetti
  (el-get-bundle olivetti)
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
  ;;                               (setq-local display-line-numbers 'relative)))
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
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     (format "%s " (nerd-icons-codicon "nf-cod-git_pull_request"))
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
                  (:eval (when (bound-and-true-p flymake-mode)
                           flymake-mode-line-format))
                  (:eval (my/ml-padding))
                  mode-line-end-spaces)))

;;;; Consult/Orderless
;; orderless
(with-delayed-execution
  (el-get-bundle orderless)
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
  (el-get-bundle consult)
  (el-get-bundle armindarvish/consult-gh)
  (with-eval-after-load 'consult
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
  (el-get-bundle f)
  (el-get-bundle s)
  (el-get-bundle elisp-refs)
  (el-get-bundle helpful)
  (el-get-bundle elisp-demos)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  (evil-define-key 'normal emacs-lisp-mode-map "K" #'helpful-at-point)
  (evil-define-key '(normal motion) helpful-mode-map
    "K" #'helpful-at-point
    "q" #'kill-buffer-and-window)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h '") #'describe-face)
  (global-set-key (kbd "C-h x") #'helpful-command))

;;;; Cape/Corfu
(with-delayed-execution
  (el-get-bundle corfu)
  (el-get-bundle popon)
  (el-get-bundle corfu-terminal)
  (el-get-bundle kind-icon)
  (el-get-bundle cape)
  (global-corfu-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (add-hook 'evil-insert-state-exit-hook 'corfu-quit)
  (with-eval-after-load 'corfu
    ;; TnG completion
    (keymap-set corfu-map "C-n" #'next-line)
    (keymap-set corfu-map "C-p" #'previous-line)
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
  (el-get-bundle which-key)
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

;;;; git/tempel/indent
(with-delayed-execution
  (el-get-bundle highlight-indent-guides)
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  (with-eval-after-load 'highlight-indent-guides
    (add-hook 'emacs-lisp-mode-hook #'highlight-indent-guides-auto-set-faces))
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  (add-hook 'conf-mode-hook #'highlight-indent-guides-mode)
  (el-get-bundle git-gutter)
  (add-hook 'after-change-major-mode-hook #'git-gutter-mode)
  (with-eval-after-load 'git-gutter
    (evil-define-key 'normal 'git-gutter-mode
      (kbd "<localleader>gs") #'git-gutter:stage-hunk
      (kbd "<localleader>gp") #'git-gutter:popup-hunk
      (kbd "<localleader>gj") #'git-gutter:next-hunk
      (kbd "<localleader>gk") #'git-gutter:previous-hunk
      (kbd "<localleader>gr") #'git-gutter:revert-hunk)
    (with-eval-after-load 'modus-themes
      (set-face-background 'modus-themes-fringe-yellow nil)
      (set-face-background 'modus-themes-fringe-green nil)
      (set-face-background 'modus-themes-fringe-red nil))
    (set-face-foreground 'git-gutter:added "chartreuse")
    (set-face-foreground 'git-gutter:modified "deep sky blue")
    (set-face-foreground 'git-gutter:deleted "dark orange"))
  (setq git-gutter:update-interval 0.2
        fringes-outside-margins t
        git-gutter:added-sign "│"
        git-gutter:modified-sign "│"
        git-gutter:deleted-sign "│"))

(with-delayed-execution
  (el-get-bundle git-gutter)
  (el-get-bundle magit/transient)
  (el-get-bundle magit/ghub)
  (el-get-bundle magit/magit-popup)
  (el-get-bundle magit/with-editor)
  (el-get-bundle magit/magit)
  (el-get-bundle magit/forge)
  (push (locate-user-emacs-file "el-get/transient/lisp") load-path)
  (push (locate-user-emacs-file "el-get/ghub/lisp") load-path)
  (push (locate-user-emacs-file "el-get/magit-pop") load-path)
  (push (locate-user-emacs-file "el-get/with-editor/lisp") load-path)
  (push (locate-user-emacs-file "el-get/magit/lisp") load-path)
  (push (locate-user-emacs-file "el-get/forge/lisp") load-path)
  (autoload 'magit "magit" nil t)
  ;; git-timemachine
  (autoload 'magit-file-dispatch "magit" nil t)
  (evil-define-key 'normal magit-blob-mode-map
    (kbd "g[") #'magit-blob-previous
    (kbd "g]") #'magit-blob-next)
  (global-set-key (kbd "C-c g") 'magit)
  (setq forge-owned-accounts '(("brongulus")))
  (add-hook 'magit-mode-hook #'(lambda () (require 'forge))))

;; tempel (TODO: incorporate aas with tempel?)
(with-delayed-execution
  (el-get-bundle tempel)
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
  (el-get-bundle! project
    :url "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/progmodes/project.el")
  (el-get-bundle rustic)
  (el-get-bundle reformatter)
  (el-get-bundle zig-mode)
  (el-get-bundle typescript-mode)
  (el-get-bundle go-mode)
  (el-get-bundle external-completion)
  (el-get-bundle eldoc-box)
  (el-get-bundle! eglot)

  (with-eval-after-load 'eldoc-box
    (setq eldoc-box-max-pixel-width 600
          eldoc-box-max-pixel-height 700
          eldoc-box-only-multi-line t)))

;;;;; eldoc
(with-delayed-execution
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
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure))

;;; Leetcode
(with-delayed-execution
  (el-get-bundle log4e)
  (el-get-bundle aio)
  (el-get-bundle graphql)
  (el-get-bundle leetcode)
  (el-get-bundle ht)
  (el-get-bundle quickrun)
  (el-get-bundle conao3/oj.el)
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
  (el-get-bundle! smart-compile
    :url "https://raw.githubusercontent.com/zenitani/elisp/master/smart-compile.el")
  (el-get-bundle fancy-compilation)
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
  (el-get-bundle sicp)
  (el-get-bundle expand-region)
  (el-get-bundle multiple-cursors)
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
    (kbd "<leader>sd") #'consult-find
    (kbd "<leader>sp") #'consult-ripgrep
    (kbd "<leader>sg") #'consult-gh-search-repos
    (kbd "<leader>fs") #'save-buffer
    (kbd "<leader>qq") #'save-buffers-kill-terminal
    (kbd "<leader>fp") #'(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    (kbd "<leader>gg") #'magit
    (kbd "<leader>gh") #'magit-file-dispatch
    (kbd "<leader>x") #'org-capture
    (kbd "<leader>oa") #'org-agenda
    (kbd "<leader>or") #'remember
    (kbd "<leader>om") #'mastodon
    (kbd "<leader>on") #'gnus
    (kbd "<leader>ot") #'eshell)
  (evil-define-key 'visual 'global
    (kbd ", TAB") #'untabify
    "T" #'untabify
    "R" #'remember-region)
  (evil-define-key 'insert 'global (kbd "C-o") #'evil-execute-in-normal-state)
  (evil-define-key 'normal 'global
    ")" #'evil-next-close-paren
    "(" #'evil-previous-open-paren
    "-" #'dired-jump
    "+" #'er/expand-region
    (kbd "C-f") #'find-file
    ";" #'evil-ex
    ":" #'evil-repeat-find-char
    (kbd "M-.") #'xref-find-definitions
    (kbd "C-t") #'tab-new
    (kbd "C-w") #'tab-close
    (kbd "A-f") #'fill-paragraph
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
  (el-get-bundle nerd-icons-dired)
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
          ("\\*\\(Compile-log\\|eldoc\\|compilation\\|git-gutter\\:diff\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 1)))))

;;; Org
(with-delayed-execution
  (el-get-bundle org-download)
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
        ;; org-latex-listings 'engraved
        ;; ;; org-latex-src-block-backend 'engraved
        org-latex-pdf-process '("tectonic -X compile %f --outdir=%o -Z shell-escape"))
  (defun my/org-inkscape-watcher (fname)
    "Open inkscape and add tex code for importing the figure"
    (interactive "sName: ")
    (insert (shell-command-to-string (concat "inkscape-figures create '" fname
                                             "' ./figures/"))))
  ;; org-dl  (https://emacs.stackexchange.com/questions/71100/pasting-images-from-clipboard-into-orgmode )
  (autoload 'org "org-download" nil t)
  (with-eval-after-load 'org-download
    (evil-define-key 'insert org-mode-map (kbd "C-S-y") 'org-download-clipboard)
    (setq org-download-method 'directory
          org-download-image-dir (concat
                                  (file-name-sans-extension (buffer-file-name)) "-img")
          org-download-image-org-width 600
          org-download-link-format "[[file:%s]]\n"
          org-download-abbreviate-filename-function #'file-relative-name
          org-download-link-format-function
          #'org-download-link-format-function-default))
  
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
;; ediff (http://yummymelon.com/devnull/surprise-and-emacs-defaults.html )
(with-delayed-execution
  (with-eval-after-load 'ediff
    (setq ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)))
  
;; pdf-tools
(with-delayed-execution
  (el-get-bundle vedang/pdf-tools)
  (push (expand-file-name "el-get/pdf-tools/lisp" user-emacs-directory) load-path)
  (require 'pdf-tools)
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
  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (winner-mode 1)
  (setq markdown-command "multimarkdown"
        markdown-fontify-code-blocks-natively t
        markdown-display-remote-images t)
  (push '("\\.md\\'" . gfm-view-mode) auto-mode-alist)
  (push '("rc\\'" . conf-unix-mode) auto-mode-alist))

;; mastodon
(with-delayed-execution
  (el-get-bundle ts)
  (el-get-bundle persist)
  (el-get-bundle request)
  (el-get-bundle mastodon
    :type git :url "https://codeberg.org/martianh/mastodon.el.git")
  (el-get-bundle rougier/mastodon-alt)
  (push (locate-user-emacs-file "el-get/mastodon/lisp") load-path)
  (setq mastodon-instance-url "https://emacs.ch"
        mastodon-auth-source-file "~/.authinfo"
        mastodon-alt-tl-box-boosted nil
        mastodon-alt-tl-box-prefix ""
        mastodon-active-user "brongulus")
  (if (display-graphic-p)
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
  
;; eww
(with-delayed-execution
  (el-get-bundle language-detection)
  (el-get-bundle shr-tag-pre-highlight)
  (with-eval-after-load 'shr
    (require 'shr-tag-pre-highlight)
    (add-to-list 'shr-external-rendering-functions
                 '(pre . shr-tag-pre-highlight)))

  (setq eww-header-line-format "%t")
  (eval-after-load 'eww
    (evil-define-key 'normal eww-mode-map
      "Y" #'eww-copy-page-url
      "q" #'kill-buffer-and-window
      "H" #'eww-back-url)))

(with-delayed-execution
  (el-get-bundle xclip)
  (el-get-bundle evil-terminal-cursor-changer)
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
  (turn-on-xclip)
  (etcc-on))

;; Gnus
(with-delayed-execution-priority-high
  (el-get-bundle damiencollard/nice-citation) ;; gnus
  (require 'nice-citation)
  (load "~/.emacs.d/+gnus" nil t))

(with-delayed-execution
  (el-get 'sync))

(provide 'init)
;;; init.el ends here

;; ------------------------- This contains code for the future.
;; (if (version< emacs-version "29.0")
;;     (pixel-scroll-mode)
;;   (pixel-scroll-precision-mode 1)
;;   (setq dired-mouse-drag-files t)
;;   (setq mouse-drag-and-drop-region-cross-program t)
;;   (setq pixel-scroll-precision-large-scroll-height 35.0))
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
