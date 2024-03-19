;; -*- coding: utf-8; lexical-binding: t -*-
;;; --------------------------
;;; Commentary: Emacs config.
;;; --------------------------
(use-package use-package
  :no-require
  :config
  (setq use-package-compute-statistics t ;; use-package-report
        use-package-always-ensure t
        use-package-always-defer t
        use-package-enable-imenu-support t
        use-package-expand-minimally t))
  
(use-package emacs
  :no-require
  :bind (("C-h '" . describe-face)
         ("C-x l" . revert-buffer-quick)
         ("<f5>" . (lambda () (interactive)
                     (setq-default display-line-numbers-type 'relative)
                     (hl-line-mode 'toggle)
                     (display-line-numbers-mode 'toggle)))
         ("<f6>" . languid-toggle-theme))
  :config
  (setq-default line-spacing 3
                cursor-type 'bar
                tab-width 2
                indent-tabs-mode nil
                enable-recursive-minibuffers t
                show-paren-delay 0
                custom-safe-themes t
                ring-bell-function 'ignore
                use-short-answers t
                debug-on-error t
                warning-minimum-level :error
                delete-pair-blink-delay 0)
  (setq read-process-output-max (* 2 1024 1024)
        inhibit-startup-screen t
        make-backup-files nil
        create-lockfiles nil
        uniquify-buffer-name-style 'forward
        auto-revert-verbose nil
        Info-use-header-line nil
        outline-minor-mode-cycle nil ;; messes up completion
        tabify-regexp "^\t* [ \t]+"
        electric-pair-skip-self t
        compilation-scroll-output 'first-error)

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook #'ansi-osc-compilation-filter)
  (add-hook 'prog-mode-hook (electric-pair-mode t))
  (add-hook 'prog-mode-hook (show-paren-mode t))
  (add-hook 'prog-mode-hook (which-function-mode))
  
  (copy-face 'default 'fixed-pitch)
  (set-register ?f `(file . ,(locate-user-emacs-file "init.el")))
  (set-register ?c `(file . "~/problems/"))

  (with-eval-after-load 'xref
    (setq xref-search-program 'ripgrep
          xref-auto-jump-to-first-xref nil ; 'move
          xref-show-definitions-function 'xref-show-definitions-completing-read
          xref-show-xrefs-function 'xref-show-definitions-completing-read))
  
  (with-eval-after-load 'minibuffer
    ;; highlight area when yanking/killing
    (defun my/yank-pulse-advice (orig-fn &rest args)
      (let (begin end)
        (setq begin (point))
        (apply orig-fn args)
        (setq end (point))
        (pulse-momentary-highlight-region begin end)))
    (advice-add 'yank :around #'my/yank-pulse-advice)
    (defun my/kill-pulse-advice (orig-fn beg end &rest args)
      (pulse-momentary-highlight-region beg end)
      (apply orig-fn beg end args))
    (advice-add 'kill-ring-save :around #'my/kill-pulse-advice)
    ;; enable some useful modes
    (save-place-mode 1)
    (context-menu-mode 1)
    (savehist-mode)
    (blink-cursor-mode -1))
  ;; Load theme based on the time of the day
  (let ((hour (substring (current-time-string) 11 13)))
    (if (and (string-lessp hour "17") (string-greaterp hour "08"))
        (setq load-theme-light t))) ;; load-theme-light is a languid-theme var
  (load-theme 'languid :no-confirm)
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))
  
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))
;;; ------------------
;;; Built-in packages
;;; ------------------
(use-package package
  :ensure nil
  :init
  (if package-quickstart
      (let ((load-source-file-function nil))
        (package-activate-all))
    (package-initialize))
  :config
  (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (setq package-native-compile t
        package-install-upgrade-built-in t
        package-check-signature nil))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode) ;; dired-hide-dotfiles-mode
  :bind (("C-x d" . dired-vc-left)
         :map dired-mode-map
         ("q" . kill-this-buffer)
         ("RET" . dired-find-alternate-file)
         ("TAB" . dired-insert-subdir)
         ("<backtab>" . dired-kill-subdir)
         ("\\" . dired-up-directory)
         ("E" . wdired-change-to-wdired-mode))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  
  (defun dired-vc-left()
    (interactive)
    (let ((dir (dired-noselect default-directory)))
      (display-buffer-in-side-window
       dir `((side . left)
             (slot . 0)
             (window-width . 0.2)
             (window-parameters . ((mode-line-format . (" %b"))))))
      (setq-local dired-free-space nil)
      (windmove-left)))

  (setq dired-dwim-target t
        dired-auto-revert-buffer t
        dired-mouse-drag-files t
        dired-use-ls-dired nil
        mouse-drag-and-drop-region-cross-program t
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always))

(use-package icomplete
  :init
  (fido-vertical-mode)
  :bind (:map icomplete-fido-mode-map
              ("C-<return>" . icomplete-fido-exit)
              ("<backspace>" . icomplete-fido-backward-updir)
              ("TAB" . icomplete-forward-completions)
              ("<backtab>" . icomplete-backward-completions)
              ("<left>" . backward-char)
              ("<right>" . forward-char)
              :map icomplete-minibuffer-map
              ("C-," . embark-act)
              :map minibuffer-local-map
              ("S-<return>" . newline))
  :hook (icomplete-minibuffer-setup . (lambda ()
                                        (setq-local completion-styles '(orderless basic)
                                                    completion-auto-help nil
                                                    truncate-lines t
                                                    line-spacing nil)))
  :config
  ;; testing completions buffer
  (add-to-list 'display-buffer-alist
               '("\\*Completions\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-parameters (mode-line-format . none))))
  (setq completion-auto-help 'always
        completion-auto-select 'second-tab
        completion-auto-wrap t
        completion-show-help nil
        completions-max-height 15
        completions-detailed t
        ;; completions-header-format nil
        completions-format 'one-column)

  (defun file-capf () ;; src: eshelyaron
    "File completion at point function."
    (pcase (bounds-of-thing-at-point 'filename)
      (`(,beg . ,end)
       (list beg end #'completion-file-name-table
             :annotation-function (lambda (_) " File")
             :exclusive 'no))))

  (add-hook 'completion-at-point-functions #'file-capf)
  
  (setq tab-always-indent 'complete
        icomplete-in-buffer t
        icomplete-scroll t ;nil
        icomplete-delay-completions-threshold 4000
        completions-group t))

(use-package windmove
  :defer 2
  :ensure nil
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings 'shift)
  (windmove-swap-states-default-keybindings 'meta))

(use-package ediff
  :defer 1
  :ensure nil
  :hook ((ediff-before-setup . tab-bar-new-tab)
         (ediff-quit . tab-bar-close-tab))
  :config
  (advice-add 'ediff-quit :around (lambda (&rest args)
                                    (ediff-really-quit args)))
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-diff-options "-w"))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;; TODO: Fix this case. `hs-show-block' needs to be
             ;; called twice to open all folds of the parent
             ;; block.
             (save-excursion (hs-show-block))
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all)))))

(use-package vc
  :defer nil
  :ensure nil
  :bind (("C-x v c" . (lambda (command) (interactive "P")
                        (unless server-mode (server-force-delete) (server-mode))
                        (let ((command (if command command (read-string "Command: git "))))
                          (compile (concat "GIT_EDITOR=\"emacsclient\" bash -c \"git " command "\"")))))
         ("C-x v f" . (lambda () (interactive)
                        (vc-git--pushpull "push" nil '("--force-with-lease"))))
         ("C-x v e" . vc-ediff))
  :config
  (setq vc-handled-backends '(Git)
        vc-find-revision-no-save t
        vc-follow-symlinks t
        project-vc-merge-submodules nil
        diff-default-read-only t
        vc-annotate-background-mode t)
  ;; fixing vc-annotate : vc-annotate-background-mode doesn't play
  ;; well with white fg, so we tweak the faces to have black fg
  (defun vc-annotate-readable (&rest _)
    (dolist (anno-face (seq-filter
                        (lambda (face)
                          (string-prefix-p "vc-annotate-face-" (symbol-name face)))
                        (face-list)))
      (face-remap-add-relative anno-face :foreground "black")))
  
  (with-eval-after-load 'vc-annotate
    (if vc-annotate-background-mode
        (advice-add 'vc-annotate-lines :after #'vc-annotate-readable))
    (define-key vc-annotate-mode-map
                "q" (lambda () (interactive)
                      (kill-this-buffer)
                      (tab-bar-close-tab))))
  ;; vc-annotate messes up the window-arrangement, give it a dedicated tab
  (add-to-list 'display-buffer-alist
               '("^\\*Annotate.*\\*$"
                 (display-buffer-reuse-mode-window display-buffer-in-tab))))
      
(use-package repeat
  :ensure nil
  :hook (after-init . silent-repeat-mode)
  :config
  (defun silent-repeat-mode ()
    (let ((inhibit-message t)
          (message-log-max nil))
      (repeat-mode)))
  (setq repeat-exit-key "RET"))

(use-package isearch
  :ensure nil
  :bind (("C-s" . isearch-forward)
         :repeat-map isearch-repeat-map
         ("s" . isearch-repeat-forward)
         ("r" . isearch-repeat-backward))
  :custom
  (isearch-wrap-pause 'no)
  (isearch-lazy-count t)
  (isearch-allow-scroll 'unlimited)
  (search-whitespace-regexp ".*?"))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-cc-command
          '("gcc" "-fsyntax-only" "-Wall" "-Wextra"
            "-I/usr/local/Cellar/gcc/13.2.0/include/c++/13/x86_64-apple-darwin23"
            "-I/usr/local/Cellar/gcc/13.2.0/include/c++/13/" "-x" "c++" "-"))
  (setq flymake-suppress-zero-counters nil
        flymake-no-changes-timeout nil
        flymake-fringe-indicator-position nil)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (with-eval-after-load 'eldoc
    (setq eldoc-echo-area-prefer-doc-buffer t
          eldoc-idle-delay 0.1
          eldoc-echo-area-use-multiline-p nil
          eldoc-echo-area-display-truncation-message nil)
    ;; Show flymake diagnostics first.
    (setq eldoc-documentation-functions
          (cons #'flymake-eldoc-function
                (remove #'flymake-eldoc-function eldoc-documentation-functions)))))

(use-package recentf
  :ensure nil
  :bind ("C-x f" . #'recentf-open)
  :custom
  (recentf-max-menu-items 25)
  (recentf-auto-cleanup 'never))

(use-package tramp
  :hook (minibuffer-mode . tramp-cleanup-all-connections)
  :config
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=\~/.ssh/control/ssh-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes")
        tramp-default-method "ssh"
        tramp-default-remote-shell "/bin/zsh"
        remote-file-name-inhibit-auto-save-visited t
        remote-file-name-inhibit-cache nil
        remote-file-name-inhibit-locks t
        tramp-verbose 0
        tramp-remote-path '(tramp-own-remote-path)))

(use-package org
  :ensure nil
  :config
  (setq org-directory "~/.emacs.d/org"
        org-agenda-files (list org-directory)
        org-agenda-start-with-log-mode t
        org-log-done t
        org-log-into-drawer t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-export-with-sub-superscripts '{}
        org-pretty-entities t
        org-startup-indented t
        org-adapt-indentation t
        ;; org-startup-folded t
        org-src-preserve-indentation t
        org-src-fontify-natively t)
  (with-eval-after-load 'org-agenda
    (setf (alist-get 'agenda org-agenda-prefix-format
                     nil nil #'equal)
          "  %?-12t% s")))

(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . meow-insert)
  :config
  (add-hook 'org-capture-mode-hook
            (lambda nil
              (setq-local header-line-format nil)))
  (setq org-capture-file
        (concat org-directory "/inbox.org")
        org-capture-templates
        '(("t" "TODO" entry
           (file+headline org-capture-file "TODOs")
           "* TODO %?\n%<%d %b '%g %R>%i %a" :prepend t)
          ("n" "Note" entry
           (file+headline org-capture-file "Notes")
           "* %u %?\n%i %a" :prepend t))))

;;; --------------
;;; ELPA packages
;;; --------------
(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)
         :map meow-motion-state-keymap
         ("`" . popper-cycle)
         :repeat-map popper-repeat-map
         ("`"     . popper-cycle))
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          "magit:.\*"
          "\\*pabbrev suggestions\\*"
          ".*-eat\\*"
          "\\*eldoc\\*"
          "vc-git :.\*"
          "\\*vc-change-log\\*"
          "\\*Flymake diagnostics.\*"
          "\\*Org Select\\*"
          "^CAPTURE.*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Occur\\*"
          compilation-mode
          "^\\*eshell.*\\*$" eshell-mode
          "^\\*term.*\\*$" term-mode)
        popper-mode-line nil
        popper-window-height 0.33))

(use-package orderless
  :after minibuffer
  :custom
  (completion-styles '(orderless basic)))

(use-package avy
  :bind ("C-j" . avy-goto-char-timer)
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :custom
  (avy-single-candidate-jump nil))

(use-package marginalia
  :hook (minibuffer-mode . marginalia-mode)
  :config
  (setq marginalia-annotator-registry
        (assq-delete-all 'file marginalia-annotator-registry)))

(use-package nerd-icons ;; nerd-icons-install-fonts
  :after minibuffer)
(use-package nerd-icons-completion
  :hook ((minibuffer-mode . nerd-icons-completion-mode)
         (marginalia-mode . nerd-icons-completion-marginalia-setup)))

(use-package embark
  :after minibuffer
  :bind ("C-," . embark-act)
  :config
  (setq prefix-help-command #'embark-prefix-help-command
        embark-prompter 'embark-completing-read-prompter
        embark-keymap-prompter-key "'"
        embark-indicators (delete 'embark-mixed-indicator embark-indicators)))

(use-package undo-fu
  :hook (after-init . undo-fu-mode)
  :config
  (setq undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960)
  (define-minor-mode undo-fu-mode ;; doom
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-z")   #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")  #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))

(use-package undo-fu-session
  :hook (undo-fu-mode . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-compression nil))

(use-package eat
  :commands eat-project
  :init (with-eval-after-load 'project
          (add-to-list 'project-switch-commands '(eat-project "Eat" ?t)))
  :bind (("C-." . (lambda () (interactive)
                    (defvar eat-buffer-name)
                    (let ((current-prefix-arg t)
                          (eat-buffer-name (concat
                                            "*" default-directory
                                            "-eat*")))
                     (call-interactively 'eat))))
         (:map eat-semi-char-mode-map
               ("C-u" . eat-self-input)))
  :config
  (setq eat-message-handler-alist
        ;; once eat fish-intergration is merged
        '(("emsg" . (lambda (x)
                      (message x)))
          ("ff" . (lambda (file)
                    (find-file file)))
          ;; FIXME: doesnt work over tramp with large name
          ("ediff" . (lambda (file1 file2)
                       (ediff file1 file2)))))
  
  (setq explicit-shell-file-name "fish"
        eat-kill-buffer-on-exit t
        eat-term-name "xterm-256color"))

(use-package pabbrev ;; completion-preview-mode if emacs>30
  :ensure nil
  :load-path "~/.emacs.d/pabbrev"
  :hook ((prog-mode text-mode) . global-pabbrev-mode)
  ;; :config
  ;; (add-to-list 'completion-at-point-functions #'pabbrev-capf)
  :custom
  (pabbrev-idle-timer-verbose nil)
  (pabbrev-overlay-decorators nil))

(use-package diff-hl
  :hook ((prog-mode . turn-on-diff-hl-mode)
         (prog-mode . diff-hl-show-hunk-mouse-mode))
  :config
  (diff-hl-flydiff-mode t)
  (let* ((width 3)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap 'my:diff-hl-bitmap bitmap 1 width '(top t)))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my:diff-hl-bitmap)))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode)
  :hook (minibuffer-mode . turn-on-solaire-mode))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package which-key
  :hook (minibuffer-mode . which-key-mode))

(use-package deadgrep)

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package meow
  :hook (after-init . (lambda ()
                        (require 'meow)
                        (meow-global-mode)))
  :preface
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
        (insert initial-key))))
  
  (defvar insert-pair-map ;; src: oantolin
    (let ((map (make-sparse-keymap)))
      (define-key map [t] #'insert-pair)
      map))
  (defvar delete-pair-map
    (let ((map (make-sparse-keymap)))
      (define-key map [t] #'delete-pair)
      map))

  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-keypad-leader-dispatch "C-x" ;ctl-x-map
        meow-use-cursor-position-hack t
        meow-use-clipboard t
        meow-esc-delay 0.01)
  (dolist (item '(word line block find till))
    (push `(,item . 0) meow-expand-hint-counts))
  (define-key meow-insert-state-keymap (kbd "j") #'my-jk)
  (define-key meow-normal-state-keymap (kbd "S") insert-pair-map)
  (define-key meow-normal-state-keymap (kbd "D") delete-pair-map)
  (dolist (imode '(eat-mode org-mode eshell-mode log-edit-mode))
    (push `(,imode . insert) meow-mode-state-list))
  (meow-motion-overwrite-define-key
   '("Q" . kill-this-buffer)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("/" . meow-visit)
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("0" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("1" . meow-digit-argument)
   '("-" . negative-argument)
   '("C-;" . meow-reverse)
   '(";" . meow-cancel-selection)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("{" . flymake-goto-prev-error)
   '("}" . flymake-goto-next-error)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . (lambda () (interactive)
             (if (region-active-p)
                 (call-interactively 'string-rectangle)
               (meow-change))))
   '("C" . meow-comment)
   '("d" . meow-kill)
   '("e" . (lambda (n) (interactive "p")
             (let ((meow--selection (push '(expand . word) meow--selection)))
               (meow-next-word n))))
     ;; meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . fff)
   '("ga" . (lambda nil (interactive)
              (org-agenda nil "n")))
   '("gb" . xref-go-back)
   '("gc" . org-capture)
   '("gd" . xref-find-definitions)
   '("gf" . embark-dwim) ;; ffap
   '("gg" . avy-goto-char-timer)
   '("gh" . diff-hl-show-hunk)
   '("gi" . imenu)
   '("gs" . scratch-buffer)
   '("gt" . tab-bar-switch-to-next-tab)
   '("gT" . tab-bar-switch-to-prev-tab)
   '("gx" . flymake-show-buffer-diagnostics)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . down-list)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . (lambda () (interactive)
             (meow-next 1)
             (join-line)))
   '("k" . meow-prev)
   '("K" . (lambda () (interactive)
             (if (derived-mode-p 'emacs-lisp-mode)
                 (describe-symbol (symbol-at-point))
               (eldoc-doc-buffer t))))
   '("l" . meow-right)
   '("L" . up-list)
   ;; '("L" . meow-swap-grab)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . occur)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . kill-this-buffer)
   '("r" . replace-regexp)
   '("R" . kmacro-end-or-call-macro)
   '("s" . kmacro-start-macro)
   '("t" . meow-till)
   '("u" . undo-fu-only-undo)
   '("U" . meow-page-up)
   '("v" . meow-line)
   '("V" . meow-page-down)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-delete)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("zz" . meow-pop-selection)
   '("za" . hs-global-cycle)
   '("zf" . hs-cycle)
   '("Z" . undo-fu-only-redo)
   '("+" . meow-block)
   '("-" . negative-argument)
   '(":" . meow-reverse)
   '("\\" . dired-jump)
   '("*" . isearch-forward-thing-at-point)
   '("&" . align-regexp)
   '("%" . mark-whole-buffer)
   '("/" . meow-visit)
   '("<" . beginning-of-buffer)
   '(">" . end-of-buffer)
   '("\"" . repeat)
   '("'" . (lambda () (interactive)
             (if (region-active-p)
                 (thread-first
                   (meow--make-selection '(expand . char) (mark) (point))
                   (meow--select))
               (thread-first
                 (meow--make-selection '(expand . char) (point) (point))
                 (meow--select)))
             (message "Visual selection mode enabled")))
   '("<escape>" . ignore)))

;;; ------------------
;;; Programming setup
;;; ------------------
(setq treesit-language-source-alist ;; treesit-install-language-grammar
      '((go "https://github.com/tree-sitter/tree-sitter-go")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "typescript/src")))

(push '("\\.rs\\'" . rust-ts-mode) auto-mode-alist)
(push '("\\.go\\'" . go-ts-mode) auto-mode-alist)
(push '("\\.ts\\'" . typescript-ts-mode) auto-mode-alist)
(push '("\\.bin\\'" . hexl-mode) auto-mode-alist)

(use-package eglot
  :bind (:map meow-normal-state-keymap
              ("ga" . eglot-code-actions)
              ("gr" . eglot-rename)
              ("gF" . eglot-format))
  :hook (((rust-ts-mode go-ts-mode) . eglot-ensure)
         (eglot-managed-mode . (lambda ()
                                 (setq eldoc-documentation-strategy
                                       'eldoc-documentation-compose-eagerly))))
  ;; :init (setq eglot-stay-out-of '(flymake))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t
        eglot-inlay-hints-mode nil)
  (add-hook 'rust-ts-mode-hook (lambda () (setq-local tab-width 2)))
  (add-hook 'go-ts-mode-hook
            (lambda () (setq-local tab-width 4)))
  (add-hook 'eglot-managed-mode-hook
            (lambda () (setq-local flymake-cc-command nil)))
  (setq go-ts-mode-indent-offset 4)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (matcher . "CaseSensitive")))))
  (add-to-list 'eglot-server-programs ;; standalone r-a support (from rustic)
               `(rust-ts-mode .
                              ("rust-analyzer" :initializationOptions
                               (:check (:command "clippy")
                                       :detachedFiles
                                       ,(vector (file-local-name
                                                 (file-truename buffer-file-name)))))))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'eglot-format-buffer -10 t))
            nil t)
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (call-interactively
                           'eglot-code-action-organize-imports))
                        nil t))))

(add-hook 'minibuffer-mode-hook
          (lambda ()
            (load "~/.emacs.d/comp" nil t)))

(use-package foxy
  :ensure nil
  :load-path "~/.emacs.d"
  :bind (("C-M-l" . foxy-listen-start)
         ("C-M-c" . foxy-cycle-files)
         ("C-M-b" . foxy-run-all-tests))
  :init
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq-local foxy-compile-command
                          "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion -O2 ")))
  (add-hook 'rust-ts-mode-hook
            (lambda ()
              (setq-local foxy-compile-command "rustc -o a.out ")))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (setq-local foxy-compile-command "go build -o a.out "))))

(setq delete-active-region t)

(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(project-dired "Dired" ?D)))

(defun async-project-find-file (program &rest args)
  "Prompt the user to filter, scroll and select a file from a list of all
project files matching PATTERN."
  (interactive)
  ;; oantolin gawd
  (let ((output-buffer (get-buffer-create "*async-completing-read*"))
        (default-directory (vc-root-dir))
        (make-process-fn (if (file-remote-p default-directory)
                             'tramp-handle-make-process
                           'make-process))
        (update-timer (run-with-timer 0.3 0.3
                                      (lambda () ;; Refresh icomplete by faking change
                                        (when-let ((mini (active-minibuffer-window)))
                                          (with-selected-window mini
                                            (icomplete-exhibit)))))))
    (funcall
     make-process-fn
     :name "project-files"
     :buffer output-buffer
     :sentinel #'ignore
     :noquery t
     :command (cons program args);(list "git" "ls-tree" "-rtd" "--format=%(path)" "HEAD")
     :connection-type 'pipe)
    (unwind-protect
        (completing-read
         "Choose: " (lambda (string pred action)
                      (complete-with-action
                       action
                       (split-string
                        (with-current-buffer output-buffer (buffer-string))
                        "\n" 'omit-nulls)
                       string pred)))
      (cancel-timer update-timer)
      (kill-buffer output-buffer))))

(defun fff (filename)
  (interactive
   (list (async-project-find-file "git" "ls-files")))
  (let ((default-directory (vc-root-dir)))
    (find-file filename)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(deadgrep nerd-icons-completion nerd-icons org-modern which-key solaire-mode esup diff-hl markdown-mode eat with-editor avy undo-fu undo-fu-session embark marginalia meow orderless popper)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
