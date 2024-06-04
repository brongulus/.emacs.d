;;; init.el ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Ah shit, here we go again.
;;; Code:
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
         ("M-o" . project-switch-project)
         ("M-c" . quick-calc)
         ("M-k" . backward-kill-word)
         ("C-d" . delete-backward-char)
         ("C-a" . (lambda nil (interactive)
                    (if (= (point) (progn (beginning-of-line-text) (point)))
                        (beginning-of-line))))
         ("C-x z" . execute-extended-command)
         ("C-x C-m" . execute-extended-command)
         ("C-x C-j" . delete-indentation)
         ("C-h C-o" . describe-symbol)
         ("C-x C-b" . ibuffer)
         ("C-x l" . revert-buffer-quick)
         ("C-x L" . desktop-read)
         ("C-M-r" . raise-sexp)
         ("C-x \\" . align-regexp)
         ("C-x C-z" . restart-emacs)
         ("<f5>" . (lambda () (interactive)
                     (setq-default display-line-numbers-type 'relative)
                     (hl-line-mode 'toggle)
                     (display-line-numbers-mode 'toggle)))
         ("<f6>" . zed-toggle-theme))
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
                display-line-numbers-width 3
                delete-pair-blink-delay 0)
  
  (setq read-process-output-max (* 16 1024)
        process-adaptive-read-buffering nil
        auto-mode-case-fold nil
        idle-update-delay 1.0
        undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960
        save-abbrevs nil
        inhibit-startup-screen t
        make-backup-files nil
        create-lockfiles nil
        uniquify-buffer-name-style 'forward
        auto-revert-verbose nil
        sentence-end-double-space nil
        Info-use-header-line nil
        outline-minor-mode-cycle nil ;; messes up completion
        tabify-regexp "^\t* [ \t]+"
        grep-command "grep --color=always -nHi -r --include=*.* -e \"pattern\" ."
        electric-pair-skip-self t
        compilation-scroll-output 'first-error)
  
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook #'ansi-osc-compilation-filter)
  ;; (push 'check-parens write-file-functions) ;; issue in org
  (add-hook 'prog-mode-hook (electric-pair-mode t))
  (add-hook 'prog-mode-hook (show-paren-mode t))
  (add-hook 'prog-mode-hook (which-function-mode))

  (defun silent-command (fn &rest args)
    (let ((inhibit-message t)
          (message-log-max nil)
          (save-silently t))
      (apply fn args)))

  (copy-face 'default 'fixed-pitch)
  (set-register ?f `(file . ,(locate-user-emacs-file "init.el")))
  (set-register ?c `(file . "~/problems/"))
  (unless (display-graphic-p)
    (set-display-table-slot standard-display-table
                            'vertical-border
                            (make-glyph-code ?│))
    (xterm-mouse-mode))
  
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
    (setq savehist-additional-variables '(kill-ring))
    (setq save-interprogram-paste-before-kill t)
    (subword-mode 1)
    (context-menu-mode 1)
    (savehist-mode)
    (blink-cursor-mode -1)
    (tooltip-mode -1))
  ;; Load theme based on the time of the day
  (let ((hour (substring (current-time-string) 11 13)))
    (if (and (string-lessp hour "17") (string-greaterp hour "08"))
        (setq load-theme-light t))) ;; load-theme-light is a languid-theme var
  (load-theme 'zed :no-confirm)
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
  (if t;package-quickstart
      (let ((load-source-file-function nil))
        (package-activate-all))
    (package-initialize))
  :config
  ;; (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (setq package-native-compile t
        package-install-upgrade-built-in t
        package-check-signature nil))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode) ;; dired-hide-dotfiles-mode
  :bind (("C-x d" . dired-left)
         :map dired-mode-map
         ("q" . kill-this-buffer)
         ("RET" . dired-find-alternate-file)
         ("TAB" . dired-maybe-insert-subdir)
         ("<backspace>" . (lambda nil (interactive)
                            (dired-kill-subdir)
                            (set-mark-command '(4))))
         ("\\" . dired-up-directory)
         ("E" . wdired-change-to-wdired-mode))
  :config
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(project-dired "Dired" ?D)))

  (put 'dired-find-alternate-file 'disabled nil)
  
  (defun dired-left()
    (interactive)
    (let ((dir (dired-noselect default-directory)))
      (display-buffer-in-side-window
       dir `((side . left)
             (slot . 0)
             (window-width . 0.18)
             (window-parameters . ((mode-line-format . (" %b"))))))
      (pop-to-buffer dir)))

  (setq dired-dwim-target t
        dired-auto-revert-buffer t
        dired-mouse-drag-files t
        dired-use-ls-dired nil
        dired-free-space nil
        mouse-drag-and-drop-region-cross-program t
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (with-eval-after-load 'cc-mode
    (defun c-indent-then-complete ()
      (interactive)
      (if (= 0 (c-indent-line-or-region))
          (completion-at-point)))
    (dolist (map (list c-mode-map c++-mode-map))
      (define-key map (kbd "<tab>") #'c-indent-then-complete))))

(use-package xref
  :ensure nil
  :config
  (setq xref-search-program 'ripgrep
        xref-auto-jump-to-first-xref nil ; 'move
        xref-show-definitions-function 'xref-show-definitions-completing-read
        xref-show-xrefs-function 'xref-show-definitions-completing-read))

(use-package icomplete
  :init
  (setq icomplete-in-buffer t
        icomplete-prospects-height 10
        icomplete-show-matches-on-no-input t
        icomplete-scroll t)
  (fido-vertical-mode)
  :bind (:map completion-in-region-mode-map ; icomp-in-buffer
              ("TAB" . icomplete-forward-completions)
              ("<backtab>" . icomplete-backward-completions)
              ("<return>" . icomplete-force-complete-and-exit)
              ("<escape>" . keyboard-quit)
              :map icomplete-fido-mode-map
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
  :hook (icomplete-minibuffer-setup . (lambda nil
                                        (setq-local completion-auto-help nil
                                                    truncate-lines t
                                                    line-spacing nil)))
  :hook (eval-expression-minibuffer-setup . (lambda nil
                                              (setq-local completion-auto-help 'always
                                                          completion-auto-select t)))
  :config
  ;; testing completions buffer
  (add-to-list 'display-buffer-alist
               '("\\*Completions\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-parameters (mode-line-format . none))))
  (setq completion-auto-help 'lazy
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

  ;; (require 'dabbrev)
  ;; (advice-add #'dabbrev-capf :before #'dabbrev--reset-global-variables)
  ;; (add-hook 'completion-at-point-functions #'dabbrev-capf 100)

  (setq tab-always-indent 'complete
        tab-first-completion 'word-or-paren
        icomplete-delay-completions-threshold 4000
        completions-group t))

(use-package windmove
  :defer 2
  :ensure nil
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings 'none)
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
  :hook (after-init . repeat-mode)
  :config
  (advice-add #'repeat-mode :around #'silent-command)
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
  :hook (cc-mode . check-cc-tramp)
  :config
  (defun check-cc-tramp nil
    (if (file-remote-p default-directory)
        (setq-local flymake-cc-command nil)
      (setq flymake-cc-command
            '("gcc" "-fsyntax-only" "-Wall" "-Wextra"
              "-I/usr/local/Cellar/gcc/13.2.0/include/c++/13/x86_64-apple-darwin23"
              "-I/usr/local/Cellar/gcc/13.2.0/include/c++/13/" "-x" "c++" "-"))))
  (setq flymake-suppress-zero-counters t
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
  :bind ("C-x f" . recentf-open)
  :hook (kill-emacs . recentf-cleanup)
  :config
  (add-to-list 'recentf-filename-handlers #'substring-no-properties) ;; doom
  (advice-add #'recentf-load-list :around #'silent-command)
  (advice-add #'recentf-cleanup :around #'silent-command)
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 200
        recentf-auto-cleanup 'never))

(use-package tramp
  :hook (minibuffer-mode . tramp-cleanup-all-connections)
  :config
  ;; (setq tramp-process-connection-type nil)
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=\~/.ssh/control/ssh-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes")
        tramp-default-method "ssh"
        tramp-default-remote-shell "/bin/zsh"
        remote-file-name-inhibit-auto-save-visited t
        remote-file-name-inhibit-cache nil
        remote-file-name-inhibit-locks t
        auto-save-default nil
        tramp-verbose 0
        tramp-remote-path '(tramp-own-remote-path)))

(use-package org
  :ensure nil
  :hook (org-mode . (lambda nil
                      (face-remap-add-relative 'default :height 1.05)))
  :config
  (push 'org-habit org-modules)
  ;; configure <s template for org-src-blocks
  (require 'org-tempo)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<)
                                 t
                               (,electric-pair-inhibit-predicate c))))))
  (setq org-directory "~/.emacs.d/org"
        org-use-sub-superscripts '{}
        ;; org-export-with-sub-superscripts nil
        org-pretty-entities t
        org-startup-indented t
        org-adapt-indentation t
        org-special-ctrl-a/e t
        org-fold-catch-invisible-edits 'show-and-error
        org-src-preserve-indentation t
        org-src-fontify-natively t))

(use-package org-agenda
  :ensure nil
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Calendar\\*"
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (window-parameters (height . 0.33))))

  (setq org-agenda-files (list org-directory)
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-start-with-log-mode t
        org-agenda-show-all-dates nil
        org-log-done t
        org-log-into-drawer t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t)

  (setf (alist-get 'agenda org-agenda-prefix-format
                   nil nil #'equal)
        "  %?-12t% s"))

(use-package org-habit
  :after org-agenda
  :ensure nil
  :config
  ;; (graph (make-string (1+ (- end start)) org-habit-missed-glyph)) ;; BRUH FIXME
  ;; (advice-add 'org-habit-build-graph )

  (setq org-habit-show-habits-only-for-today t
        org-habit-show-done-always-green t
        ;; org-habit-today-glyph ?○ ;; 9675
        ;; org-habit-completed-glyph ?● ;; 9679
        ;; org-habit-missed-glyph ?○ ;; 9675
        org-habit-following-days 1
        org-habit-preceding-days 21))

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
           "* %?\n%i %a" :prepend t)
          ("h" "Habit" entry
           (file+headline org-capture-file "Habits")
           "* TODO %?\n:PROPERTIES:\n:STYLE: habit\n:LOGGING: TODO(!) DONE(!)\n:END:"
           :prepend t))))

(use-package desktop
  ;; session-persistence
  :ensure nil
  ;; :hook (after-init . desktop-save-mode)
  :config
  (dolist (item
           '(alpha background-color background-mode border-width
                   bottom-divider-width cursor-color cursor-type display-type
                   environment font fontsize foreground-color fullscreen
                   fullscreen-restore horizontal-scroll-bars internal-border-width
                   left-fringe line-spacing menu-bar-lines ns-appearance
                   ns-transparent-titlebar powerline-cache right-divider-width
                   right-fringe scroll-bar-height scroll-bar-width tool-bar-lines
                   tool-bar-position vertical-scroll-bars zoom-window-buffers
                   zoom-window-enabled))
    (push `(,item . :never) frameset-filter-alist))
  
  (setq desktop-restore-forces-onscreen nil
        desktop-auto-save-timeout 10)
  (add-hook 'desktop-after-read-hook
            (lambda ()
              (frameset-restore
               desktop-saved-frameset
               :reuse-frames (eq desktop-restore-reuses-frames t)
               :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
               :force-display desktop-restore-in-current-display
               :force-onscreen desktop-restore-forces-onscreen))))

(use-package erc
  ;; auth: machine irc.libera.chat login "USER" password PASSWORD
  :ensure nil
  :commands my/irc
  :hook (erc-join . hl-line-mode)
  :hook (erc-join . (lambda nil
                      (setq-local erc-fill-column (min (- (window-width) 3) 85))))
  :hook (erc-kill-server . (lambda nil
                              (erc-status-sidebar-kill)
                              (tab-bar-close-tab)))
  :custom
  (erc-autojoin-channels-alist '(("libera.chat" "#emacs"))); "##rust")))
  (erc-default-server "irc.libera.chat")
  (erc-nick "brongulus")
  (erc-nickserv-get-password nil)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-fill-column (min (- (window-width) 3) 85))
  (erc-status-side-bar-width 12)
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 14)
  (erc-format-nick-function 'erc-format-@nick)
  (erc-header-line-face-method t)
  (erc-track-position-in-mode-line t)
  (erc-track-showcount t)
  (erc-track-shorten-function nil)
  (erc-track-exclude-server-buffer t)
  (erc-join-buffer 'bury) ; window
  (erc-kill-server-buffer-on-quit t)
  (erc-kill-buffer-on-part t)
  (erc-hide-list '("JOIN" "PART" "QUIT" "353")) ;; 353 hide names
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (defun my/irc nil
    "Setup ERC and connect if not already."
    (interactive)
    (if (get-buffer "Libera.Chat") ;; ERC already active?
        (pop-to-buffer "Libera.Chat")
      (progn
        (tab-bar-new-tab)
        (erc :server "irc.libera.chat" :port 6667 :nick "brongulus" :password nil)
        (erc-track-switch-buffer 1)
        (erc-status-sidebar-open))))
  (erc-services-mode 1)
  (erc-autojoin-mode)
  (erc-track-mode t)
  (erc-timestamp-mode -1)
  (push 'keep-place erc-modules)
  (erc-update-modules))

(use-package gnus
  :ensure nil
  :hook (gnus-exit-gnus . tab-bar-close-tab)
  :hook (gnus-summary-mode . turn-on-gnus-mailing-list-mode)
  :bind (:map gnus-article-mode-map
              ("q" . kill-buffer-and-window)
              ("RET" . gnus-summary-scroll-up)
              :map gnus-summary-mode-map
              ("R" . (lambda nil (interactive)
                       (gnus-summary-mark-article nil ?R))))
  :preface
  (setq gnus-directory (concat user-emacs-directory "/gnus")
        gnus-startup-file (concat user-emacs-directory "/.newsrc")
        gnus-use-dribble-file nil
        gnus-always-read-dribble-file nil)
  :config
  (advice-add 'gnus-splash :before #'tab-bar-new-tab)
  (setq gnus-select-method '(nntp "news.gwene.org"))
  (setq gnus-secondary-select-methods
        '((nnimap "personal"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "993")
                  (nnimap-stream ssl)
                  (nnir-search-engine imap)
                  (nnmail-expiry-target "nnimap+personal:[Imap]/Trash")
                  (nnmail-expiry-wait 'immediate)))
        ;; opts
        gnus-check-new-newsgroups nil ;; disable first time you use gnus
        gnus-asynchronous t
        gnus-use-cache t
        gnus-cache-remove-articles nil
        gnus-large-newsgroup 200
        gnus-blocked-images nil
        gnus-treat-hide-boring-headers t
        mm-text-html-renderer 'shr ;; w3m
        mm-inline-large-images 'resize
        shr-use-colors nil
        shr-max-width fill-column
        shr-indentation 2
        gnus-article-x-face-too-ugly ".*"
        gnus-interactive-exit nil
        gnus-novice-user nil
        gnus-expert-user nil
        gnus-auto-select-first nil
        gnus-summary-display-arrow nil
        gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))
  ;; Better UI
  (gnus-add-configuration
   '(article
     (horizontal 1.0
                 (vertical 25
                           (group 1.0))
                 (vertical 1.0
                           (summary 0.25 point)
                           (article 1.0)))))
  (gnus-add-configuration
   '(summary
     (horizontal 1.0
                 (vertical 25
                           (group 1.0))
                 (vertical 1.0
                           (summary 1.0 point)))))
  (setq gnus-unread-mark #x2022 ;; dot
        gnus-unseen-mark 32 ;; space
        gnus-read-mark 32
        gnus-del-mark ?\
        gnus-ancient-mark 32
        gnus-replied-mark 32
        gnus-cached-mark 32
        gnus-ticked-mark ?!
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-single-indent ""
        gnus-sum-thread-tree-vertical        "│"
        gnus-sum-thread-tree-leaf-with-other "├─►"
        gnus-sum-thread-tree-single-leaf     "╰─►"
        gnus-user-date-format-alist '(((gnus-seconds-today) . " %H:%M")
                                      (t . "%b %d"))
        gnus-topic-line-format (concat "%(%{%n - %A%}%) %v\n")
        gnus-group-uncollapsed-levels 2
        gnus-group-line-format (concat "%S%2y: %(%-40,40c%)\n") ;; %E (gnus-group-icon-list)
        ;;  06-Jan   Sender Name    Email Subject
        gnus-summary-line-format (concat " %0{%U%R%}"
                                         "%1{%&user-date;%}" "%3{ %}" " "
                                         "%4{%-16,16f%}" " "
                                         "%3{ %}" " "
                                         "%1{%B%}" "%S\n"))
  (setq gnus-message-archive-group '((format-time-string "sent.%Y"))))

(use-package gnus-group
  :ensure nil
  :after gnus
  :hook (gnus-group-mode . gnus-topic-mode)
  :config
  ;; mode-line unread indicator
  (defun my/gnus-unread-count ()
    (interactive)
    (let ((uc (cdar gnus-topic-unreads)))
      (if (or (not uc)
              (eq uc 0))
          ""
        (propertize (format " Unread:%s " uc) ;; (char-to-string #x2709)
                    'face 'which-func))))
  (setq global-mode-string
        (append global-mode-string
                (list '(:eval (propertize
                               (my/gnus-unread-count)
                               'help-echo "Gnus - Unread")))))
  (with-eval-after-load 'gnus-topic
    (setq gnus-topic-topology '(("Unread" visible)
                                (("📥 Personal" visible nil nil))
                                (("📰 News" visible nil nil))))
    (setq gnus-topic-alist '(("📥 Personal" ; the key of topic
                              "nnimap+personal:INBOX"
                              "nnimap+personal:[Gmail]/Sent Mail"
                              "nnimap+personal:Sent"
                              "nnimap+personal:sent.2023"
                              "nnimap+personal:[Gmail]/Starred")
                             ("📰 News"
                              "gwene.com.blogspot.petr-mitrichev"
                              "gmane.emacs.announce"
                              "gmane.emacs.devel"
                              "gmane.emacs.gnus.general"
                              "gmane.emacs.gnus.user"
                              "gmane.emacs.tramp"
                              "gmane.emacs.bugs"
                              "gwene.com.youtube.feeds.videos.xml.user.ethoslab"
                              "gmane.comp.web.qutebrowser"
                              "gmane.comp.web.elinks.user"
                              "gwene.app.rsshub.leetcode.articles"
                              "gwene.com.arcan-fe"
                              "gwene.io.github.matklad"
                              "gwene.net.lwn.headlines"
                              "gwene.org.quantamagazine"
                              "gwene.org.bitlbee.news.rss")
                             ("Unread")))))

;;; --------------
;;; ELPA packages
;;; --------------
(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-j"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)
         :repeat-map popper-repeat-map
         ("`"     . popper-cycle))
  :hook ((after-init . popper-mode)
         (after-init . popper-tab-line-mode))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*" "Output\\*$"
          "\\*Async Shell Command\\*"
          "magit:.\*" "\\*pabbrev suggestions\\*"
          ".*-eat\\*" "\\*eldoc\\*" "vc-git :.\*"
          "\\*vc-change-log\\*" "\\*Deletions\\*"
          "\\*Flymake diagnostics.\*" "\\*CDLaTex Help\\*"
          "\\*Process List\\*" "\\*Org Select\\*"
          "^CAPTURE.*" "\\*Warnings\\*"
          "\\*Backtrace\\*" "\\*Occur\\*"
          help-mode compilation-mode
          "^\\*eshell.*\\*$" eshell-mode
          "^\\*term.*\\*$" term-mode)
        popper-mode-line nil
        popper-window-height 0.33))

(use-package orderless
  :after minibuffer
  :hook (completion-in-region-mode . (lambda nil
                                       (setq-local orderless-component-separator "[ -]")))
  :hook (icomplete-minibuffer-setup . (lambda nil
                                        (setq-local completion-styles '(orderless basic))))
  :custom
  (completion-styles '(orderless basic)))

(use-package avy
  :bind ("C-j" . avy-goto-char-timer)
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :custom
  (avy-single-candidate-jump nil))

(use-package embark
  :after minibuffer
  :bind ("C-," . embark-act)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-prompter 'embark-completing-read-prompter
        embark-keymap-prompter-key "'"
        embark-indicators (delete 'embark-mixed-indicator embark-indicators)))

(use-package undo-fu-session
  :hook ((prog-mode conf-mode fundamental-mode text-mode tex-mode) . undo-fu-session-mode)
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
                                            "*" (file-name-nondirectory
                                                 (directory-file-name
                                                  (if (vc-root-dir)
                                                      (vc-root-dir)
                                                    default-directory)))
                                            "-eat*")))
                      (call-interactively 'eat))))
         (:map eat-semi-char-mode-map
               ("C-u" . eat-self-input)
               ("M-j" . popper-toggle)
               ("M-w" . kill-ring-save)))
  :config
  (setq eat-message-handler-alist
        ;; once eat fish-intergration is merged
        '(("emsg" . (lambda (x)
                      (message x)))
          ("ff" . (lambda (file)
                    (find-file file)))
          ;; FIXME: doesnt work over tramp with large name
          ("ediff" . (lambda (file1 file2)
                       (tab-bar-new-tab)
                       (ediff file1 file2)))))

  (setq explicit-shell-file-name "fish"
        eat-kill-buffer-on-exit t
        eat-term-name "xterm-256color"))

(use-package diff-hl
  :hook ((prog-mode . turn-on-diff-hl-mode)
         (prog-mode . (lambda nil
                        (unless (display-graphic-p)
                          (diff-hl-margin-mode))))
         (prog-mode . diff-hl-show-hunk-mouse-mode))
  :config
  (diff-hl-flydiff-mode t)
  (setq diff-hl-margin-symbols-alist nil)
  (dolist (diff '(insert delete change))
    (push `(,diff . "│") diff-hl-margin-symbols-alist))
  (let* ((width 3)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap 'my:diff-hl-bitmap bitmap 1 width '(top t)))
  (setq diff-hl-fringe-bmp-function (lambda (_type _pos) 'my:diff-hl-bitmap)))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode)
  :hook (minibuffer-mode . turn-on-solaire-mode))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(use-package which-key
  :hook (minibuffer-mode . which-key-mode))

(use-package ox-awesomecv
  :ensure nil
  :load-path "~/Documents/org-cv"
  ;; :vc (:fetcher gitlab :repo "Titan-C/org-cv")
  :after org
  :init (require 'ox-awesomecv))

(use-package deadgrep)

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
        meow-keypad-describe-delay 1.0 ;; not interfere with which-key
        meow-use-cursor-position-hack t
        meow-use-clipboard t
        meow-esc-delay 0.01)
  (dolist (item '(word line block find till))
    (push `(,item . 0) meow-expand-hint-counts))
  (define-key meow-insert-state-keymap (kbd "j") #'my-jk)
  (define-key meow-normal-state-keymap (kbd "m s") insert-pair-map)
  (define-key meow-normal-state-keymap (kbd "m d") delete-pair-map)
  
  (dolist (imode '(reb-mode eat-mode eshell-mode log-edit-mode))
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
   '("'" . meow-cancel-selection)
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
             (if (and (region-active-p)
                      (bound-and-true-p rectangle-mark-mode))
                 (call-interactively 'string-rectangle)
               (meow-change))))
   '("M-C" . (lambda () (interactive)
             (if (and (region-active-p)
                      (bound-and-true-p rectangle-mark-mode))
                 (meow-prev 1)
               (rectangle-mark-mode 1))))
   '("C" . (lambda () (interactive)
             (if (and (region-active-p)
                      (bound-and-true-p rectangle-mark-mode))
                 (meow-next 1)
               (rectangle-mark-mode 1))))
   '("d" . (lambda () (interactive)
             (if (region-active-p)
                 (meow-kill)
               (meow-delete))))
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . fff)
   '("ga" . (lambda nil (interactive)
              (org-agenda nil "n")))
   '("gb" . xref-go-back)
   '("gc" . org-capture)
   '("gC" . meow-comment)
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
   '("i" . (lambda nil (interactive)
             (if (and (region-active-p)
                      (bound-and-true-p rectangle-mark-mode))
                 (call-interactively 'string-rectangle)
               (meow-insert))))
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . (lambda () (interactive)
             (meow-next 1)
             (delete-indentation)))
   '("k" . meow-prev)
   '("K" . (lambda () (interactive)
             (if (derived-mode-p 'emacs-lisp-mode)
                 (describe-symbol (symbol-at-point))
               (eldoc-doc-buffer t))))
   '("l" . meow-right)
   '("L" . up-list)
   ;; '("L" . meow-swap-grab)
   '("mm" . (lambda nil (interactive)
              (if (nth 3 (syntax-ppss))
                  (backward-up-list 1 t t)
                (cond ((looking-at "\\s\(\\|\{") (forward-list 1) (backward-char 1))
                      ((looking-at "\\s\)\\|\}") (forward-char 1) (backward-list 1))
                      (t (backward-up-list 1 t t))))))
   '("mi" . meow-inner-of-thing)
   '("ma" . meow-bounds-of-thing)
   '("n" . meow-search)
   '("o" . occur)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . kill-this-buffer)
   '("r" . (lambda nil (interactive)
             (delete-char 1)
             (insert-char (read-char nil t))
             (backward-char 1)))
   '("R" . replace-regexp)
   '("s" . kmacro-start-macro)
   '("S" . kmacro-end-or-call-macro)
   '("t" . meow-till)
   '("u" . undo-only)
   '("U" . meow-page-up)
   '("v" . (lambda () (interactive)
             (if (region-active-p)
                 (thread-first
                   (meow--make-selection '(expand . char) (mark) (point) t)
                   (meow--select))
               (thread-first
                 (meow--make-selection '(expand . char) (point) (point) t)
                 (meow--select)))
             (message "Visual selection mode enabled")))
   '("V" . meow-page-down)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("zz" . (lambda nil (interactive)
              (set-mark-command '(4))))
   '("za" . hs-global-cycle)
   '("zf" . hs-cycle)
   '("Z" . undo-redo)
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
   '("<escape>" . ignore)))

;;; ------------------
;;; Programming setup
;;; ------------------
(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist ;; treesit-install-language-grammar
        '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src"))
        treesit-font-lock-level 4))

(setq major-mode-remap-alist '((c++-mode . c++-ts-mode)
                               (c-mode . c-ts-mode)))

(push '("\\.rs\\'" . rust-ts-mode) auto-mode-alist)
(push '("\\.go\\'" . go-ts-mode) auto-mode-alist)
(push '("\\.ts\\'" . typescript-ts-mode) auto-mode-alist)
(push '("\\.bin\\'" . hexl-mode) auto-mode-alist)
(push '("\\.prototxt\\'" . js-json-mode) auto-mode-alist)

(use-package eglot
  :bind (:map meow-normal-state-keymap
              ;; ("ga" . eglot-code-actions)
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
  :bind (("C-c C-l" . foxy-listen-start)
         ("C-c C-c" . foxy-cycle-files)
         ("C-C C-b" . foxy-run-all-tests))
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

(defun async-project-find-file (program &rest args)
  "Prompt the user to filter & select a file from a list of all files.
The files are returned by calling PROGRAM with ARGS."
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
  "Call `find-file' and search for FILENAME asynchronously."
  (interactive
   (list (async-project-find-file "git" "ls-files")))
  (let ((default-directory (if (vc-root-dir)
                               (vc-root-dir)
                             default-directory)))
    (find-file filename)))

(load "~/.emacs.d/pff")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult cdlatex org-modern inf-ruby popper deadgrep which-key solaire-mode esup diff-hl markdown-mode eat avy undo-fu-session embark marginalia meow orderless)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
