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
         ("M-c" . quick-calc) ; 16#hex
         ("M-k" . kill-word)
         ("M-d" . backward-kill-word)
         ("C-d" . delete-backward-char)
         ("C-a" . (lambda nil (interactive)
                    (if (= (point) (progn (beginning-of-line-text) (point)))
                        (beginning-of-line))))
         ("M-P" . execute-extended-command)
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
                fill-column 80
                indent-tabs-mode nil
                enable-recursive-minibuffers t
                show-paren-delay 0
                show-paren-context-when-offscreen t
                show-paren-when-point-inside-paren t
                custom-safe-themes t
                ring-bell-function 'ignore
                use-short-answers t
                debug-on-error t
                warning-minimum-level :error
                display-line-numbers-width 5
                delete-pair-blink-delay 0)

  (setq read-process-output-max (* 16 1024)
        process-adaptive-read-buffering nil
        switch-to-buffer-obey-display-actions t
        kill-do-not-save-duplicates t
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
        outline-minor-mode-cycle t
        tabify-regexp "^\t* [ \t]+"
        grep-command "grep --color=always -nHi -r --include=*.* -e \"pattern\" ."
        electric-pair-skip-self t
        electric-pair-preserve-balance nil
        shell-command-prompt-show-cwd t
        shell-kill-buffer-on-exit t
        compilation-scroll-output 'first-error)

  (push 'check-parens write-file-functions) ;; issue in org - fixed below
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook #'ansi-osc-compilation-filter)
  (add-hook 'prog-mode-hook (electric-pair-mode t))
  (add-hook 'prog-mode-hook (show-paren-mode t))
  (add-hook 'prog-mode-hook (which-function-mode))
  (add-to-list 'write-file-functions
               '(lambda () (if (not indent-tabs-mode)
                               (untabify (point-min) (point-max)))
                  nil))

  (defun silent-command (fn &rest args)
    "Used to suppress output of FN."
    (let ((inhibit-message t)
          (message-log-max nil)
          (save-silently t))
      (apply fn args)))

  ;; (copy-face 'default 'fixed-pitch)
  (set-register ?f `(file . ,(locate-user-emacs-file "init.el")))
  (set-register ?c `(file . "~/problems/"))
  (with-eval-after-load 'whitespace
    (add-to-list 'whitespace-display-mappings '(newline-mark ?\n [8626 ?\n]) t))
  (unless (display-graphic-p)
    (set-display-table-slot standard-display-table
                            'vertical-border
                            (make-glyph-code ?│))
    (xterm-mouse-mode))

  ;; load theme based on the time of the day
  (let ((hour (substring (current-time-string) 11 13)))
    (if (and (string-lessp hour "17") (string-greaterp hour "08"))
        (setq load-theme-light t))) ;; load-theme-light is a languid-theme var
  (load-theme 'zed :no-confirm)
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))
  ;; Quitly kill term buffers on exit
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))

(use-package emacs
  :ensure nil
  :defer 2
  :hook
  (after-init . (lambda nil
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
                  (global-subword-mode 1)
                  (delete-selection-mode)
                  (context-menu-mode 1)
                  (savehist-mode)
                  (blink-cursor-mode -1)
                  (setq blink-cursor-interval 0.7)
                  (tooltip-mode -1))))

(use-package package
  :ensure nil
  :init
  (if t ;package-quickstart
      (let ((load-source-file-function nil))
        (package-activate-all))
    (package-initialize))
  :config
  (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (setq package-native-compile t
        package-install-upgrade-built-in t
        package-check-signature nil))

;;; ------------------
;;; Built-in packages
;;; ------------------
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
  (defun dired-left()
    (interactive)
    (let ((dir (dired-noselect default-directory)))
      (display-buffer-in-side-window
       dir `((side . left)
             (slot . 0)
             (window-width . 0.18)
             (window-parameters . ((mode-line-format . (" %b"))))))
      (pop-to-buffer dir)))

  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(project-dired "Dired" ?D)))

  (put 'dired-find-alternate-file 'disabled nil)
  
  (setq dired-dwim-target t
        dired-auto-revert-buffer t
        dired-mouse-drag-files t
        dired-use-ls-dired nil
        dired-free-space nil
        mouse-drag-and-drop-region-cross-program t
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always))

(use-package xref
  :ensure nil
  :config
  (setq xref-search-program 'ripgrep
        xref-auto-jump-to-first-xref nil ; 'move
        xref-show-definitions-function 'xref-show-definitions-completing-read
        xref-show-xrefs-function 'xref-show-definitions-completing-read))

(use-package windmove
  :defer 1
  :ensure nil
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings 'none)
  (windmove-swap-states-default-keybindings 'meta))

(use-package ediff
  :defer 1
  :ensure nil
  :hook ((ediff-before-setup . tab-bar-new-tab)
         (ediff-quit . (lambda nil
                         (tab-bar-close-tab)
                         (kill-buffer ediff-registry-buffer))))
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
        flymake-warning-bitmap '(filled-square compilation-warning)
        flymake-error-bitmap '(filled-square compilation-error)
        flymake-note-bitmap '(filled-square compilation-info)
        ;; flymake-show-diagnostics-at-end-of-line t
        flymake-fringe-indicator-position 'right-fringe)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (remove-hook 'flymake-diagnostic-functions 'elisp-flymake-byte-compile))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-display-truncation-message nil)
  ;; FIXME Show flymake diagnostics first.
  (with-eval-after-load 'flymake
    (setq eldoc-documentation-functions
          (cons #'flymake-eldoc-function
                (remove #'flymake-eldoc-function eldoc-documentation-functions)))))

(use-package recentf
  :ensure nil
  :bind ("C-x f" . my/recentf-buffer)
  :hook (kill-emacs . recentf-cleanup)
  :preface
  (defun my/recentf-buffer () ;; src:James dyer
    "Switch to a buffer or open a recent file from a unified interface."
    (interactive)
    (unless (recentf-mode (recentf-mode 1)))
    (let* ((buffers (mapcar #'buffer-name (buffer-list)))
           (recent-files recentf-list)
           (all-options (append buffers recent-files))
           (selection (completing-read "Switch to: "
                                       (lambda (str pred action)
                                         (if (eq action 'metadata)
                                             '(metadata . ((category . file)))
                                           (complete-with-action action all-options str pred)))
                                       nil t nil 'file-name-history)))
      (pcase selection
        ((pred (lambda (sel) (member sel buffers))) (switch-to-buffer selection))
        (_ (find-file selection)))))
  :config
  (add-to-list 'recentf-filename-handlers #'substring-no-properties) ;; doom
  (advice-add #'recentf-load-list :around #'silent-command)
  (advice-add #'recentf-cleanup :around #'silent-command)
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 200
        recentf-auto-cleanup 'never))

(use-package tramp
  :after minibuffer
  :hook (minibuffer-mode . tramp-cleanup-all-connections)
  :config
  (setq tramp-process-connection-type nil)
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
                      (setq-local line-spacing 8)
                      ;; fix for check-parens
                      (modify-syntax-entry ?\) "." org-mode-syntax-table)
                      (modify-syntax-entry ?\( "." org-mode-syntax-table)
                      (modify-syntax-entry ?< "." org-mode-syntax-table)
                      (modify-syntax-entry ?> "." org-mode-syntax-table)))
  :hook (org-mode . visual-line-mode)
  :config
  ;; Taken from rougier: org-outer-indent
  (defun org-outer-indent--compute-prefixes ()
    "Compute prefix strings for regular text and headlines."
    (setq org-indent--heading-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (setq org-indent--inlinetask-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (setq org-indent--text-line-prefixes
          (make-vector org-indent--deepest-level nil))
    ;; Find the lowest headline level
    (let* ((headline-levels (or (org-element-map
                                    (org-element-parse-buffer) 'headline
                                  #'(lambda (item)
                                      (org-element-property :level item)))
                                '()))
           (max-level (seq-max headline-levels))
           ;; We could also iterate over each evel to get maximum length
           ;; Instead, we take the length of the deepest numbered level.
           (line-indentation (+ 3 max-level))
           (headline-indentation))
      (dotimes (level org-indent--deepest-level)
        (setq headline-indentation
              (max 0 (- line-indentation (+ 1 level))))
        (aset org-indent--inlinetask-line-prefixes level
              (make-string line-indentation ?\s))
        (aset org-indent--text-line-prefixes level
              (make-string line-indentation ?\s))
        (aset org-indent--heading-line-prefixes level
              (make-string headline-indentation ?\s))))
    (setq-local org-hide-leading-stars nil))

  (advice-add 'org-indent--compute-prefixes :override
              #'org-outer-indent--compute-prefixes)

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
        org-ellipsis "…"
        org-pretty-entities t
        org-startup-indented t
        org-adapt-indentation t
        org-special-ctrl-a/e t
        org-fold-catch-invisible-edits 'show-and-error
        org-edit-src-content-indentation 0
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

(use-package desktop ;; session-persistence
  :ensure nil
  :hook (window-setup . desktop-save-mode)
  :hook (window-setup . desktop-read)
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
    (let ((unread-count (cdar gnus-topic-unreads)))
      (if (or (not unread-count) (eq unread-count 0))
          ""
        (propertize (format " Unread:%s " unread-count)
                    'face 'which-func))))
  (with-eval-after-load 'gnus-topic
    (setq global-mode-string
          (append global-mode-string
                  (list '(:eval (propertize
                                 (my/gnus-unread-count)
                                 'help-echo "Gnus - Unread")))))
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
                              "gmane.emacs.announce" "gmane.emacs.devel"
                              "gmane.emacs.gnus.general" "gmane.emacs.gnus.user"
                              "gmane.emacs.tramp" "gmane.emacs.bugs"
                              "gwene.com.youtube.feeds.videos.xml.user.ethoslab"
                              "gmane.comp.web.qutebrowser" "gmane.comp.web.elinks.user"
                              "gwene.app.rsshub.leetcode.articles"
                              "gwene.com.arcan-fe" "gwene.io.github.matklad"
                              "gwene.net.lwn.headlines" "gwene.org.quantamagazine"
                              "gwene.org.bitlbee.news.rss")
                             ("Unread")))))

;;; --------------
;;; ELPA packages
;;; --------------
(use-package popper
  :bind (("M-j"   . popper-toggle)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)
         :repeat-map popper-repeat-map
         ("`"     . popper-cycle))
  :hook ((after-init . popper-mode)
         (after-init . popper-tab-line-mode))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*" "Output\\*$"
          "\\*Async Shell Command\\*" "\\*shell.\*"
          "magit:.\*" "\\*pabbrev suggestions\\*"
          ".*-eat\\*" "\\*eldoc\\*" "vc-git :.\*"
          "\\*vc-change-log\\*" "\\*Deletions\\*"
          "\\*Flymake .\*" "\\*CDLaTex Help\\*"
          "\\*Process List\\*" "\\*Org Select\\*"
          "^CAPTURE.*" "\\*Warnings\\*"
          "\\*Backtrace\\*" "\\*Occur\\*"
          help-mode compilation-mode
          "^\\*eshell.*\\*$" eshell-mode
          "^\\*term.*\\*$" term-mode)
        popper-mode-line nil
        popper-window-height 0.33))

(use-package popper-echo
  :after popper
  :ensure nil
  :config
  ;; cleaner tab-line
  (advice-add 'tab-line-tab-name-buffer :around
              (lambda (orig-fn &rest args)
                (string-replace "*" "" (apply orig-fn args))))
  (defun popper-tab-line--format (tab tabs)
    (concat
     (when (display-graphic-p)
       (propertize " " 'face `(:background ,(face-foreground 'vertical-border nil t))
                   'display '(space :width (1))))
     (propertize " "
                 'face (if (eq tab (current-buffer))
                           (if (mode-line-window-selected-p)
                               'tab-line-tab-current 'tab-line-tab)
                         'tab-line-tab-inactive))
     (tab-line-tab-name-format-default tab tabs)

     (when (display-graphic-p)
       (propertize " " 'face `(:background ,(face-foreground 'vertical-border nil t))
                   'display '(space :width (1)))))))

(use-package orderless
  :after minibuffer
  :hook (completion-in-region-mode . (lambda nil
                                       (setq-local orderless-component-separator "[ -]")))
  :hook (icomplete-minibuffer-setup . (lambda nil
                                        (setq-local completion-styles '(orderless basic))))
  :config
  (bind-key "<SPC>" #'self-insert-command minibuffer-local-completion-map)
  :custom
  (completion-styles '(orderless basic)))

(use-package avy
  :bind ("C-j" . avy-goto-char-timer)
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :custom
  (avy-single-candidate-jump nil))

(use-package vertico
  :hook (after-init . vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
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
        vertico-cycle t)

  (defun file-capf () ;; src: eshelyaron
    "File completion at point function."
    (pcase (bounds-of-thing-at-point 'filename)
      (`(,beg . ,end)
       (list beg end #'completion-file-name-table
             :annotation-function (lambda (_) " File")
             :exclusive 'no))))

  (require 'dabbrev)
  (advice-add #'dabbrev-capf :before #'dabbrev--reset-global-variables)

  (add-hook 'completion-at-point-functions #'file-capf)
  ;; (add-hook 'completion-at-point-functions #'dabbrev-capf 100) ; posframe/corfu issue

  (add-to-list 'display-buffer-alist
               '("\\*Completions\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-parameters (mode-line-format . none))))

  (setq tab-always-indent 'complete
        tab-first-completion 'word-or-paren
        completions-group t
        completions-detailed t
        ;; completions-buffer (for eval-expr)
        completion-auto-help 'always
        completion-auto-select 'second-tab
        completion-auto-wrap t
        completion-show-help nil
        completions-max-height 15
        completions-detailed t
        completions-format 'one-column))

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8))
        vertico-posframe-width 80
        vertico-posframe-poshandler 'posframe-poshandler-frame-top-center))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :hook ((corfu-mode . corfu-popupinfo-mode)
         (meow-insert-exit . corfu-quit))
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
        corfu-preselect-first nil))

(use-package undo-fu-session
  :hook ((prog-mode conf-mode fundamental-mode text-mode tex-mode) . undo-fu-session-mode)
  :config
  (setq undo-fu-session-compression nil))

(use-package writeroom-mode
  :config
  (setq writeroom-width 90
         writeroom-global-effects nil
         writeroom-maximize-window nil)
  (add-hook 'writeroom-mode-hook
            (lambda nil
              (if writeroom-mode
                    (add-hook 'post-command-hook #'recenter nil t)
                (remove-hook 'post-command-hook #'recenter t)))))

(use-package eat
  :commands eat-project
  :init (with-eval-after-load 'project
          (add-to-list 'project-switch-commands '(eat-project "Eat" ?t)))
  :bind (("C-." . (lambda () (interactive)
                    (defvar eat-buffer-name)
                    (let ((current-prefix-arg t)
                          (eat-buffer-name
                           (concat "*" (file-name-nondirectory
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
         (prog-mode . diff-hl-margin-mode))
  :config
  (diff-hl-flydiff-mode t)
  (setq vc-git-diff-switches '("--histogram")
        diff-hl-flydiff-delay 0.5
        diff-hl-update-async t
        diff-hl-show-staged-changes nil
        diff-hl-margin-symbols-alist '((insert . "█")
                                       (delete . "█")
                                       (change . "█"))
        diff-hl-draw-borders nil))

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
  :after org)

(use-package deadgrep
  :preface
  (defun deadgrep-current-dir (search-term)
    "Grep for SEARCH-TERM in current directory."
    (interactive "sTerm: ")
    (deadgrep search-term default-directory)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . visual-line-mode)
  (nov-mode . writeroom-mode)
  :config
  (setq nov-text-width 80
        nov-header-line-format nil))

(use-package meow
  :hook (after-init . (lambda ()
                        (require 'meow)
                        (meow-global-mode)))
  :hook (meow-insert-enter . (lambda nil (blink-cursor-mode +1)))
  :hook (meow-insert-exit . (lambda nil (blink-cursor-mode -1)))
  ;; :hook (special-mode . meow-normal-mode)
  :preface
  (defun my-chord (initial-key final-key fn) ;; src: wasamasa
    (interactive)
    (let* ((timeout 0.5)
           (event (read-event nil nil timeout)))
      (if event ;; timeout met
          (if (and (characterp event) (= event final-key))
              (funcall fn)
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
        meow-esc-delay 0.01
        meow-cursor-type-insert '(bar . 3))
  (dolist (item '(word line block find till))
    (push `(,item . 0) meow-expand-hint-counts))
  (define-key meow-insert-state-keymap (kbd "j") (lambda nil (interactive)
                                                   (my-chord ?j ?k 'meow-insert-exit)))
  (define-key meow-normal-state-keymap (kbd "m s") insert-pair-map)
  (define-key meow-normal-state-keymap (kbd "m d") delete-pair-map)

  (dolist (imode '(reb-mode eat-mode shell-mode eshell-mode log-edit-mode))
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
   '("c" . meow-change)
   '("C" . string-rectangle)
   '("d" . (lambda () (interactive)
             (if (region-active-p)
                 (meow-kill)
               (delete-forward-char 1))))
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
   '("gf" . ffap)
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
   '("n" . (lambda nil (interactive)
             (meow--direction-forward)
             (call-interactively 'meow-search)))
   '("N" . (lambda nil (interactive)
             (meow--direction-backward)
             (call-interactively 'meow-search)))
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
   '("V" . rectangle-mark-mode)
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
   '("<" . indent-rigidly-left-to-tab-stop)
   '(">" . indent-rigidly-right-to-tab-stop)
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
          (janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src"))
        treesit-font-lock-level 4))

;; (setq major-mode-remap-alist '((c++-mode . c++-ts-mode)
;;                                (c-mode . c-ts-mode)))
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

(with-eval-after-load 'cc-mode ;; why is this in dired
    (defun c-indent-then-complete ()
      (interactive)
      (if (= 0 (c-indent-line-or-region))
          (completion-at-point)))
    (dolist (map (list c-mode-map c++-mode-map))
      (define-key map (kbd "<tab>") #'c-indent-then-complete)))

(push '("\\.rs\\'" . rust-ts-mode) auto-mode-alist)
(push '("\\.go\\'" . go-ts-mode) auto-mode-alist)
(push '("\\.ts\\'" . typescript-ts-mode) auto-mode-alist)
(push '("\\.bin\\'" . hexl-mode) auto-mode-alist)
(push '("\\.info\\'" . Info-mode) auto-mode-alist)
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
              (add-hook 'after-save-hook
                        (lambda ()
                          (call-interactively
                           'eglot-code-action-organize-imports))
                        nil t))))

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

(use-package janet-ts-mode
  :init
  (unless (package-installed-p 'janet-ts-mode)
    (package-vc-install "https://github.com/sogaiu/janet-ts-mode"))
  :mode ("\\.janet\\'" . janet-ts-mode)
  :config
  (remove-hook 'janet-ts-mode-hook 'treesit-inspect-mode))

;; comp
(add-hook 'minibuffer-mode-hook
          (lambda nil (load "~/.emacs.d/lisp/comp" nil t)))

(use-package foxy
  :ensure nil
  :load-path "~/.emacs.d/lisp"
  :bind (("C-c C-l" . foxy-listen-start)
         ("C-c C-c" . foxy-cycle-files)
         ("C-C C-b" . foxy-run-all-tests))
  :init
  (defvar foxy-compile-commands
    '((rust-ts-mode-hook . "rustc -o a.out ")
      (c++-mode-hook . "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion -O2 ")
      (go-ts-mode-hook . "go build -o a.out ")))
  (dolist (pair foxy-compile-commands)
    (let ((mode (car pair))
          (command (cdr pair)))
      (add-hook mode
                (lambda ()
                  (setq-local compile-command (concat command
                                                      buffer-file-name
                                                      " && ./a.out"))
                  (setq-local foxy-compile-command command))))))

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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy corfu vertico-posframe vertico janet-ts-mode zig-mode which-key undo-fu-session writeroom-mode
         solaire-mode popper orderless nov meow markdown-mode inf-ruby esup eat diff-hl deadgrep cdlatex))
 '(package-vc-selected-packages
   '((janet-ts-mode :vc-backend Git :url "https://github.com/sogaiu/janet-ts-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
