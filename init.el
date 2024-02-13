;;; --------------------------
;;; Defaults? Better? Maybe...
;;; --------------------------
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(set-frame-font "VictorMono Nerd Font Mono 16" nil t)
;; (set-face-attribute
;;  'variable-pitch nil :family "Roboto" :height 180)

(setq-default default-frame-alist
              '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars)
                (left-fringe . 0)
                (right-fringe . 0)
                (internal-border-width . 20)
                (fullscreen . maximized))
              cursor-in-non-selected-windows nil
              bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right
              ;; misc
              line-spacing 3
              cursor-type 'bar
              tab-width 2
              indent-tabs-mode nil
              enable-recursive-minibuffers t
              show-paren-delay 0
              custom-safe-themes t
              ;; tramp
              vc-handled-backends '(Git)
              tramp-default-method "ssh"
              remote-file-name-inhibit-cache nil
              remote-file-name-inhibit-locks t
              tramp-verbose 1
              ;; dired
              dired-dwim-target t
              dired-auto-revert-buffer t
              dired-mouse-drag-files t
              mouse-drag-and-drop-region-cross-program t
              dired-kill-when-opening-new-dired-buffer t
              dired-recursive-deletes 'always
              dired-recursive-copies 'always
              ;; tab-bar
              tab-bar-close-button-show nil
              tab-bar-new-button-show nil
              tab-bar-show 1
              tab-bar-separator "")
              ;; tab-bar-tab-name-function #'dracula-tab-line-name-buffer)

(setq inhibit-startup-screen t
      load-prefer-newer t
      isearch-wrap-pause 'no
      make-backup-files nil
      create-lockfiles nil
      uniquify-buffer-name-style 'forward)

;;; SSH Stuff
(setq project-vc-merge-submodules nil)
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
  "-o ControlPath=\~/.ssh/control/ssh-%%r@%%h:%%p "
  "-o ControlMaster=auto -o ControlPersist=yes"))
(remove-hook 'find-file-hook 'vc-refresh-state)
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(setq xref-search-program 'ripgrep
      xref-auto-jump-to-first-xref nil ; 'move
      xref-show-definitions-function 'xref-show-definitions-completing-read
      xref-show-xrefs-function 'xref-show-definitions-completing-read)

;;; Misc
(fset 'display-startup-echo-area-message'ignore)
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))
(fset 'yes-or-no-p 'y-or-n-p)
(set-display-table-slot standard-display-table 'truncation 32) ;; hides $
(set-display-table-slot standard-display-table 'wrap 32) ;; hides \
(save-place-mode 1)
(blink-cursor-mode -1)
;; (global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(load-theme 'alabaster :no-confirm)

;;; ----------------------------------------------------
;;; Built-in packages (project, recentf, dired, ediff)
;;; ----------------------------------------------------
(use-package project
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m)))

(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :custom
  (recentf-max-menu-items 25)
  (recentf-auto-cleanup 'never)
  :bind ("C-x f" . #'recentf-open))

(global-set-key (kbd "C--") #'(lambda () (interactive) (dired "/data/data/com.termux/files/home/")))
(global-set-key (kbd "C-'") #'(lambda () (interactive)
                                (term "/data/data/com.termux/files/usr/bin/fish")))
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'ediff
    (setq ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq-local display-line-numbers nil))

(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
(kill-buffer))

;;; ------------------------------------------------------------------------
;;; ELPA packages (popper, orderless, meow, vertico, corfu, undo-fu+session)
;;; ------------------------------------------------------------------------
(require 'package)
;; (push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-native-compile t
      package-install-upgrade-built-in t
      package-check-signature nil
      use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Results\\*"
          help-mode
          compilation-mode
          "^\\*term.*\\*$" term-mode)
        popper-mode-line nil)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
    :defer nil
    :init
    (vertico-mode)
    :bind (:map vertico-map
                ("<backspace>" . vertico-directory-delete-char)
                ("RET" . vertico-directory-enter)
                ("TAB" . vertico-next)
                ("<backtab>" . vertico-previous)
                ("S-TAB" . vertico-previous)
                ("M-TAB" . vertico-previous)
                ("C-j" . vertico-next)
                ("C-k" . vertico-previous))
    :config
    (setq vertico-scroll-margin 0
          vertico-resize nil
          vertico-cycle t))

(use-package corfu
  :init (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("M-TAB" . corfu-previous)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :config
  (add-hook 'eshell-mode #'(lambda () (setq-local corfu-auto nil) (corfu-mode)))
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
        tab-always-indent 'complete))

(use-package undo-fu
  :bind (("C-x u" . undo-fu-only-undo)
         ("C-z" . undo-fu-only-redo))
  :config
  (setq undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960))

(use-package undo-fu-session
  :after undo-fu
  :init
  (undo-fu-session-global-mode))

(defun my-jk () ;; src: wasamasa
  (interactive)
  (let* ((initial-key ?j)
         (final-key ?k)
         (timeout 0.5)
         (event (read-event nil nil timeout)))
    (if event ;; timeout met
        (if (and (characterp event) (= event final-key))
            (meow-insert-exit)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(use-package meow
  :demand t
  :config
  (meow-global-mode 1)
  (defun meow-setup()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (add-hook 'meow-insert-exit-hook #'(lambda ()
                                         (when corfu-mode (corfu-quit))))
    (define-key meow-insert-state-keymap (kbd "j") #'my-jk)
    (with-eval-after-load 'dired
      (define-key dired-mode-map "-" 'dired-up-directory))
    (global-set-key (kbd "M-<right>") 'windmove-right)
    (global-set-key (kbd "M-<left>") 'windmove-left)
    (global-set-key (kbd "M-<up>") 'windmove-up)
    (global-set-key (kbd "M-<down>") 'windmove-down)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . meow-comment)
     '("d" . meow-kill)
     '("D" . meow-page-down)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-visit)
     '("t" . meow-till)
     '("u" . undo-fu-only-undo)
     '("U" . meow-page-up)
     '("v" . meow-line)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-delete)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("Z" . undo-fu-only-redo)
     '("+" . meow-block)
     '("-" . dired-jump)
     '("/" . isearch-forward)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup))

(use-package magit
  ;; :commands (magit magit-file-dispatch magit-log-all magit-ediff-show-unstaged)
  :config
  (with-eval-after-load 'transient
    (transient-bind-q-to-quit))
  (setq magit-commit-show-diff nil)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))

;;; -------------------------------------------------
;;; Competitive programming setup (snippets and foxy)
;;; -------------------------------------------------
(define-skeleton cpp-header "Base c++ template for competitive programming." nil
  "#include<bits/stdc++.h>\n"
  "using namespace std;\n"
  "\n#ifdef LOCAL\n"
  "#include \"algo/debug.h\"\n"
  "#else\n"
  "#define debug(...) 42\n"
  "#endif\n"
  "\nint main () {\n"
  "\tios::sync_with_stdio(0);\n"
  "\tcin.tie(0);\n"
  "\t" _ "\n"
  "}")

(define-skeleton cpp-for-loop
  "Insert a C++ for loop with user-defined iterator and termination variable." ""
  > "for (int " (setq iterator (read-char "Iterator variable: ")) " = 0; " iterator " < "
  > (read-char "Termination: ") "; " iterator "++) {\n\t\t"
  > _ "\n}"
  > (indent-according-to-mode)
  > (delete-char -1))

(define-skeleton cpp-tests "Run multiple testcases." ""
  > "int tt = 0; cin >> tt;\n\t"
  > "while(tt--) {\n\t\t"
  > _ "\n}"
  > (indent-according-to-mode)
  > (delete-char -1))

(define-skeleton cpp-all "Run from beginning to end of iterator." ""
  > (setq var (read-from-minibuffer "")) ".begin(), " var ".end()"
  > (forward-char)
  > _)

(setq save-abbrevs nil)
(defun init-c++-abbrevs ()
  (define-abbrev c++-mode-abbrev-table "gtc" "" 'cpp-header)
  (define-abbrev c++-mode-abbrev-table "forl" "" 'cpp-for-loop)
  (define-abbrev c++-mode-abbrev-table "all" "" 'cpp-all)
  (define-abbrev c++-mode-abbrev-table "ttt" "" 'cpp-tests))
(add-hook 'c++-mode-hook 'abbrev-mode)
(add-hook 'c++-mode-hook 'init-c++-abbrevs)

(load "~/.emacs.d/foxy" nil t)
(setq foxy-compile-command "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion -O2 ")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit meow corfu orderless popper)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(widget-button ((t (:foreground unspecified)))))
