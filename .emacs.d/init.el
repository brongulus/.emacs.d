;;; init.el --- Description -*- lexical-binding: t; -*-
;; Author: Prashant Tak <prashantrameshtak@gmail.com>
;; Created: June 11, 2023
;; Modified: June 27, 2023
;; Version: 0.0.1
;; This file is not part of GNU Emacs.
;;; Commentary:
;;  Ref 1: https://zenn.dev/takeokunn/articles/56010618502ccc
;;  Ref 2: https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4
;;  Ref 3: https://robbmann.io/emacsd/
;;  TODO: fix eldoc in terminal, bottom popup in gui,
;;  TODO: lazy load tempel, corfu, eglot
;;  ref: https://github.com/doomemacs/doomemacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;;; Code:

;;; Startup hacks 
;; (setq comp-deferred-compilation t
;;       comp-async-report-warnings-errors nil)

;; ;; (el-get-bundle benchmark-init)
;; (load "/home/prashant/.emacs.d/el-get/benchmark-init/benchmark-init.el"
;;       'no-error nil 'no-suffix)
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

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
                tab-width 2
                indent-tabs-mode nil
                fill-column 80
                delete-selection-mode t
                mouse-yank-at-point t
                custom-file (concat user-emacs-directory "custom.el")
                scroll-margin 10
                scroll-step 1
                next-screen-context-lines 5
                scroll-conservatively 100000
                scroll-preserve-screen-position 1
                recenter-positions '(5 top bottom)
                vc-follow-symlinks t
                bookmark-default-file "~/doom-configs/.emacs.d/bookmarks"
                cursor-in-non-selected-windows nil
                help-window-select t
                large-file-warning-threshold nil
                show-paren-delay 0
                initial-buffer-choice t)
    (advice-add 'bookmark-set-internal :after '(funcall 'bookmark=save))) ;; prot

(with-delayed-execution
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'prog-mode-hook #'(lambda ()
                                (setq-local display-line-numbers 'relative))))

;;; Package Management
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

;;;;; Yay Evil!
(el-get-bundle undo-fu)
(el-get-bundle undo-fu-session)
(with-delayed-execution
  (setq undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960)
  (undo-fu-session-global-mode))

(el-get-bundle elpa:evil)
(with-delayed-execution-priority-high
  (define-key global-map (kbd "<escape>") #'keyboard-escape-quit)
  (setq evil-want-keybinding t ;; nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-C-i-jump nil
        evil-cross-lines t
        evil-move-cursor-back nil
        evil-auto-indent t
        evil-move-beyond-eol t
        evil-shift-width 2
        evil-disable-insert-state-bindings t
        evil-undo-system 'undo-fu
        evil-normal-state-tag  (propertize "⬤" 'face '(:foreground "LightSteelBlue1"))
        evil-emacs-state-tag   (propertize "⬤" 'face '(:foreground "dim gray"))
        evil-insert-state-tag  (propertize "⬤" 'face '(:foreground "khaki1"))
        evil-visual-state-tag  (propertize "⬤" 'face '(:foreground "medium spring green")))

  (evil-mode 1)
  (add-to-list 'evil-highlight-closing-paren-at-point-states 'normal t)
  (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil)))

;;;;; Modeline (Ref: https://github.com/motform/emacs.d/blob/master/init.el)
(with-delayed-execution
  (setq-default flymake-mode-line-counter-format
                '("" flymake-mode-line-error-counter
                  flymake-mode-line-warning-counter
                  flymake-mode-line-note-counter "")
                global-mode-string nil
                flymake-mode-line-format
                '(" " flymake-mode-line-exception flymake-mode-line-counters))
  (setq-default mode-line-end-spaces '("" (vc-mode vc-mode) " "
                                       mode-line-misc-info " %l %p "))
  (defun my/ml-padding ()
    (let ((r-length (length (format-mode-line mode-line-end-spaces))))
      (propertize " "
                  'display `(space :align-to (- right ,r-length)))))
  (setq-default mode-line-format
                '(" %e "
                  (:eval evil-mode-line-tag) " "
                  (:eval (if (buffer-modified-p)
                             (propertize " %b "
                                         'face '(:slant italic :inverse-video t)
                                         'help-echo (buffer-file-name))
                           (propertize " %b " 'help-echo (buffer-file-name))))
                  (:eval (when (bound-and-true-p flymake-mode)
                           flymake-mode-line-format))
                  (:eval (my/ml-padding))
                  mode-line-end-spaces)))

;;;;; Vertico/Marginalia
(el-get-bundle compat) ;; for minad's pkgs
(el-get-bundle vertico)
(with-delayed-execution-priority-high
  (vertico-mode)
  (savehist-mode)
  (keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
  (keymap-set vertico-map "TAB" #'vertico-insert)
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous)
  (setq vertico-scroll-margin 0
        vertico-resize nil
        vertico-cycle t))

(el-get-bundle marginalia)
(with-delayed-execution
  (marginalia-mode))

;;;;; Consult/Orderless
(el-get-bundle orderless)
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
  
(el-get-bundle consult)
(with-delayed-execution
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
  (evil-define-key 'normal 'global
    (kbd "SPC") 'consult-buffer
    "?" 'consult-recent-file)
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
  (evil-define-key 'normal emacs-lisp-mode-map "K" #'helpful-at-point)
  (with-eval-after-load 'helpful
    (evil-define-key 'normal helpful-mode-map
      "K" #'helpful-at-point
      "q" #'(lambda () (interactive) (quit-window t)))) ;; hate unused buffers
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h '") #'describe-face)
  (global-set-key (kbd "C-h x") #'helpful-command))

;;;;; Cape and Corfu
(el-get-bundle corfu)
(with-delayed-execution
  (global-corfu-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (add-hook 'evil-insert-state-exit-hook 'corfu-quit)
  (with-eval-after-load 'corfu
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
                (corfu-mode)))))

(el-get-bundle cape)
(with-delayed-execution
  (defun my/add-capfs ()
    (push 'cape-file completion-at-point-functions)
    (push 'cape-dabbrev completion-at-point-functions)
    (push 'cape-keyword completion-at-point-functions))
  (add-hook 'prog-mode-hook #'my/add-capfs)
  (add-hook 'text-mode-hook #'my/add-capfs))

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
    (set-face-foreground 'git-gutter:modified "deep sky blue")
    (set-face-foreground 'git-gutter:deleted "dark orange"))
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

(with-delayed-execution
  (push (locate-user-emacs-file "el-get/transient/lisp") load-path)
  (push (locate-user-emacs-file "el-get/ghub/lisp") load-path)
  (push (locate-user-emacs-file "el-get/magit-pop") load-path)
  (push (locate-user-emacs-file "el-get/with-editor/lisp") load-path)
  (push (locate-user-emacs-file "el-get/magit/lisp") load-path)
  (push (locate-user-emacs-file "el-get/forge/lisp") load-path)
  (autoload 'magit "magit" nil t)
  (global-set-key (kbd "C-c g") 'magit)
  (setq forge-owned-accounts '(("brongulus"))
        warning-suppress-types '((emacsql)))
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
    (define-key tempel-map (kbd "<tab>") 'tempel-next)
    (define-key tempel-map (kbd "TAB") 'tempel-next)
    (define-key tempel-map (kbd "<backtab>") 'tempel-previous))
  (setq tempel-path "~/.emacs.d/templates.el"))

;;;;; eglot
(el-get-bundle expand-region)
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
  ;; FIXME:
  (evil-define-key 'normal eglot-mode-map "K" 'eldoc-box-help-at-point)
  (evil-define-key 'normal eglot-mode-map "X" 'flymake-show-buffer-diagnostics)
  (with-eval-after-load 'eglot
    (if (display-graphic-p)
        (global-eldoc-mode -1))
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

;;;;; Nice-citation (gnus)
(el-get-bundle! damiencollard/nice-citation)

;;;;; Leetcode
(el-get-bundle log4e)
(el-get-bundle aio)
(el-get-bundle graphql)
(el-get-bundle leetcode)
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

(el-get 'sync)

;;; Compilation
(with-delayed-execution
  (with-eval-after-load 'compile
    (setq compilation-scroll-output 'first-error
          compilation-always-kill t
          compilation-environment '("TERM=xterm-256color"))) ;; flat
  (add-hook 'racket-mode-hook 
            #'(lambda ()
                (setq-local compile-command 
                            (concat
                             "racket " (shell-quote-argument buffer-file-name)))))
  (add-hook 'rust-mode-hook
            #'(lambda ()
                (setq-local compile-command
                ;; (setq-local compile-command "cargo build && cargo run")
                            (concat "rustc " buffer-file-name
                                    " && ./"
                                    (file-name-sans-extension
                                     (file-name-nondirectory buffer-file-name))))))
  (add-hook 'java-mode-hook 
            #'(lambda ()
                (setq-local compile-command
                            (concat "javac " (shell-quote-argument buffer-file-name)
                                    " && java " (file-name-sans-extension
                                                 (file-name-nondirectory buffer-file-name))))))
  (add-hook 'python-mode-hook 
            #'(lambda ()
                (setq-local compile-command
                            (concat "python " (shell-quote-argument buffer-file-name) " < ./in"
                                    (file-name-sans-extension
                                     (file-name-nondirectory buffer-file-name))))))
  (add-hook 'c++-mode-hook 
            #'(lambda ()
                (setq-local compile-command
                            (concat "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion "
                                    "-O2 -DLOCAL -I/mnt/Data/Documents/problems/include "
                                    buffer-file-name
                                    " && ./a.out < ./in"
                                    (file-name-sans-extension
                                     (file-name-nondirectory buffer-file-name))))))
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
                               my/eval-auto-insert-init-form]))

;;; Key binds / Key maps
(with-delayed-execution-priority-high
  (evil-set-leader 'normal "'") ;; ' is leader
  (evil-set-leader 'normal "," t) ;; , is localleader
  (evil-define-key 'normal 'global
    (kbd "<leader>fs") 'save-buffer
    (kbd "<leader>qq") 'save-buffers-kill-emacs
    (kbd "<leader>fp") '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    (kbd "<leader>gg") 'magit
    (kbd "<leader>om") 'gnus
    (kbd "<leader>ot") 'term)
  (evil-define-key 'normal 'global
    ")" 'evil-next-close-paren
    "(" 'evil-previous-open-paren
    "-" 'dired-jump
    "+" 'er/expand-region
    (kbd "C-f") 'find-file
    ";" 'evil-ex
    ":" 'evil-repeat-find-char
    (kbd "A-f") 'fill-paragraph
    (kbd "gcc") 'comment-line 
    (kbd "C-t") 'tab-new)
  (global-set-key [f2] 'save-buffer)
  (global-set-key [f3] 'paste-input)
  (global-set-key [f4] 'compile)
  (global-set-key [f10] 'kill-current-buffer)
  (global-set-key (kbd "C-x x") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c m") 'gnus)
  (global-set-key (kbd "C-'") 'er/expand-region)
  (global-set-key (kbd "C-t") 'tab-new)
  (global-set-key (kbd "C-w") 'tab-close)
  (global-set-key (kbd "M-k") 'windmove-up)
  (global-set-key (kbd "M-j") 'windmove-down)
  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-l") 'windmove-right)
  (global-set-key (kbd "M-d") 'delete-window)
  (global-set-key (kbd "M-m") 'delete-other-windows)
  (global-set-key (kbd "M-s") 'split-window-below)
  (global-set-key (kbd "M-v") 'split-window-right))

;; dired
(with-delayed-execution
  (setq dired-listing-switches "-ahl -v --group-directories-first")
  (evil-define-key 'normal dired-mode-map
    "O" 'browse-url-of-dired-file
    "q" #'(lambda () (interactive) (quit-window t))) ;; hate unused buffers
  (define-key dired-mode-map "-" 'dired-up-directory)
  (define-key dired-mode-map "~"
    #'(lambda () (interactive) (dired "/home/prashant/")))
  (setq dired-dwim-target t
        dired-auto-revert-buffer t
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; tabs
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

;;; Random
(with-delayed-execution
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
  (with-eval-after-load 'winner
    (define-key winner-mode-map (kbd "C-c C-<left>") 'winner-undo)
    (define-key winner-mode-map (kbd "C-c C-<right>") 'winner-redo))
  (with-eval-after-load 'eww
    (add-hook 'eww-mode-hook #'+setup-text-mode-left-margin)
    (evil-define-key 'normal 'eww-mode-map
      "q" #'quit-window
      "H" #'eww-back-url))
  (with-delayed-execution
  (set-cursor-color "white")
  (setq dracula-enlarge-headings nil
        dracula-height-title-1 1.0
        dracula-height-title-2 1.0
        drcaula-height-doc-title 1.0))
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
        org-directory "~/Dropbox/org/"
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
    (set-face-attribute 'org-level-1 nil :height 1.0)
    (set-face-attribute 'org-level-2 nil :height 1.0)
    (set-face-attribute 'org-document-title nil :height 1.0)
    (add-hook 'org-mode-hook 'tempel-setup-capf)
    (define-key org-mode-map [f4] #'org-latex-export-to-pdf)
    (define-key org-mode-map [f5] #'my/org-inkscape-watcher)))
;;;; Org-Capture
(with-delayed-execution
  (with-eval-after-load 'org-capture
    ;; (evil-set-initial-state 'org-capture-mode 'insert)
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
             "* %T %?" :prepend t)
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

;; Terminal session (taken from doom)
(with-delayed-execution
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (eldoc-mode 1)
    (turn-on-xclip)
    (etcc-on)))

;; Gnus
(with-delayed-execution
  (autoload 'gnus "nice-citation" nil t)
  (load "~/.emacs.d/+gnus" nil t))

;;; Config performance measure
(let ((elapsed (float-time (time-subtract (current-time) t0))))
  (makunbound 't0)
  (message "Spent %.3fs in init.el" elapsed))

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