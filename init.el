;; -*- coding: utf-8; lexical-binding: t -*-
;;; --------------------------
;;; Commentary: Emacs config.
;;; --------------------------
;; (when (member "VictorMono Nerd Font Mono" (font-family-list))
;;   (set-frame-font "VictorMono Nerd Font Mono 16" nil t))
(use-package emacs
  :ensure nil
  :bind (("C-h '" . describe-face)
         ("C-x l" . revert-buffer-quick))
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
                initial-major-mode 'fundamental-mode
                debug-on-error t
                warning-minimum-level :error
                delete-pair-blink-delay 0)
  (setq read-process-output-max (* 2 1024 1024)
        inhibit-startup-screen t
        make-backup-files nil
        create-lockfiles nil
        uniquify-buffer-name-style 'forward
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        Info-use-header-line nil
        outline-minor-mode-cycle nil ;; messes up completion
        tabify-regexp "^\t* [ \t]+"
        electric-pair-skip-self t)

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook #'ansi-osc-compilation-filter)
  (add-hook 'prog-mode-hook (electric-pair-mode t))
  (add-hook 'prog-mode-hook (show-paren-mode t))
  
  (copy-face 'default 'fixed-pitch)
  (set-register ?f `(file . ,(locate-user-emacs-file "init.el")))
  (set-register ?c `(file . "~/problems/"))
  (set-display-table-slot standard-display-table 'truncation 32) ;; hides $
  (set-display-table-slot standard-display-table 'wrap 32) ;; hides \

  (with-eval-after-load 'xref
    (setq xref-search-program 'ripgrep
          xref-auto-jump-to-first-xref nil ; 'move
          xref-show-definitions-function 'xref-show-definitions-completing-read
          xref-show-xrefs-function 'xref-show-definitions-completing-read))

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
  
  (with-eval-after-load 'minibuffer
    (save-place-mode 1)
    (context-menu-mode 1)
    (savehist-mode)
    (global-auto-revert-mode t)
    (blink-cursor-mode -1))
  ;; Load theme based on the time of the day
  (let ((hour (substring (current-time-string) 11 13)))
    (if (and (string-lessp hour "17") (string-greaterp hour "08"))
        (setq load-theme-light t)))
  (load-theme 'languid :no-confirm)
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))

  (with-eval-after-load 'ediff
    (setq ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-keep-variants nil)
    (setq-local display-line-numbers nil))
  
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))
;;; -------------------------------------------------------------
;;; Built-in packages (icomplete, project, recentf, dired, ediff)
;;; -------------------------------------------------------------
(use-package package
  :ensure nil
  :init (package-initialize)
  :config
  ;; (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (unless package-archive-contents
    (package-refresh-contents))
  (setq package-native-compile t
        package-install-upgrade-built-in t
        package-check-signature nil
        use-package-compute-statistics t ;; use-package-report
        use-package-always-ensure t
        use-package-always-defer t
        use-package-enable-imenu-support t
        use-package-expand-minimally t))

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
    (let ((dir (if (eq (vc-root-dir) nil)
                   (dired-noselect default-directory)
                 (dired-noselect (vc-root-dir)))))
      (display-buffer-in-side-window
       dir `((side . left)
             (slot . 0)
             (window-width . 0.2)
             (window-parameters . ((mode-line-format . (" %b"))))))
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
              :map icomplete-minibuffer-map
              ("C-," . embark-act)
              :map minibuffer-local-map
              ("S-<return>" . newline))
  :hook (icomplete-minibuffer-setup . (lambda ()
                                        (setq-local completion-styles '(orderless basic)
                                                    truncate-lines t
                                                    line-spacing nil)))
  :config ;; src: https://github.com/JasZhe/vimilla-emacs
  (defun completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
      Use as a value for `completion-in-region-function'."
    (let* ((initial (buffer-substring-no-properties start end))
           (limit (car (completion-boundaries initial collection predicate "")))
           (all (completion-all-completions initial collection predicate (length initial)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all)))
                         (concat (substring initial 0 limit) (car all)))
                        (t
                         (setq completion
                               (catch 'done
                                 (atomic-change-group
                                   (let ((completion
                                          (completing-read "Completion: " collection predicate nil initial)))
                                     (throw 'done completion)))))))))
      (cond (completion (completion--replace start end completion) t)
            (t (message "No completion") nil))))
  
  (setq completion-in-region-function #'completing-read-in-region
        tab-always-indent 'complete
        icomplete-in-buffer t
        icomplete-scroll t ;nil
        icomplete-delay-completions-threshold 4000
        completions-group t))

(use-package windmove
    :defer 2
    :config
    (windmove-default-keybindings 'meta)
    (windmove-swap-states-default-keybindings 'shift))

(use-package vc
  :defer nil
  :ensure nil
  :bind (("C-x v f" . (lambda () (interactive)
                        (vc-git--pushpull "push" nil '("--force-with-lease"))))
         ("C-x v e" . vc-ediff)
         ("C-x v R" . vc-interactive-rebase)
         :map vc-annotate-mode-map
         ("q" . (lambda () (interactive)
                  (kill-this-buffer)
                  (tab-bar-close-tab))))
  :config
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (defun vc-interactive-rebase (branch)
    (interactive "sRebase target branch: ")
    (with-current-buffer (eshell)
      (eshell-return-to-prompt)
      (insert "git rebase -i " branch)
      (eshell-send-input)))

  (push "bldr" vc-directory-exclusion-list)
  (push "external" vc-directory-exclusion-list)
  (setq vc-handled-backends '(Git)
        vc-find-revision-no-save t
        vc-follow-symlinks t
        project-vc-merge-submodules nil
        vc-annotate-background-mode t)
  ;; fixing vc-annotate
  (add-to-list 'display-buffer-alist
               '("^\\*Annotate.*\\*$"
                 (display-buffer-in-new-tab)))
  (defun vc-annotate-readable (&rest _)
    (dolist (anno-face (seq-filter
                        (lambda (face)
                          (string-prefix-p "vc-annotate-face-" (symbol-name face)))
                        (face-list)))
      (face-remap-add-relative anno-face :foreground "black")))
  (advice-add 'vc-annotate :after #'vc-annotate-readable))
      
      
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
  (setopt flymake-cc-command
          '("gcc" "-fsyntax-only" "-Wall" "-Wextra" "-D_GLIBCXX_NO_ASSERT"
            "-I/Users/admin/problems/include" "-x" "c" "-"))
  (setq flymake-suppress-zero-counters nil
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
  :defer 1
  :bind ("C-x f" . #'recentf-open)
  :custom
  (recentf-max-menu-items 25)
  (recentf-auto-cleanup 'never))

(use-package tramp
  :defer 2
  :config
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=\~/.ssh/control/ssh-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes")
        tramp-default-method "ssh"
        remote-file-name-inhibit-auto-save-visited t
        remote-file-name-inhibit-cache nil
        remote-file-name-inhibit-locks t
        tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
;;; ---------------------------------------------------------------------------------
;;; ELPA packages (popper, orderless, marginalia, embark, meow, eat, undo-fu+session)
;;; ---------------------------------------------------------------------------------
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

(use-package embark
  :after minibuffer
  :bind ("C-," . embark-act)
  :config
  (setq prefix-help-command #'embark-prefix-help-command
        embark-prompter 'embark-completing-read-prompter
        embark-keymap-prompter-key "'"
        embark-indicators (delete 'embark-mixed-indicator embark-indicators)))

(use-package undo-fu
  :demand t
  :bind (("C-x u" . undo-fu-only-undo)
         ("C-z" . undo-fu-only-redo))
  :config
  (setq undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960))

(use-package undo-fu-session
  :demand t
  :init
  (unless (string-equal system-type "android")
    (undo-fu-session-global-mode)))

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
  (setq explicit-shell-file-name "fish"
        eat-kill-buffer-on-exit t
        eat-term-name "xterm-256color"))

(use-package with-editor
  :hook ((eshell-mode . with-editor-export-git-editor)
         (eshell-mode . with-editor-export-editor)))

(use-package pabbrev ;; completion-preview-mode if emacs>30
  :ensure nil
  :load-path "~/.emacs.d/pabbrev"
  :hook ((prog-mode text-mode) . global-pabbrev-mode)
  :custom
  (pabbrev-idle-timer-verbose nil)
  (pabbrev-overlay-decorators nil))

(use-package diff-hl
  :hook ((prog-mode . turn-on-diff-hl-mode)
         (prog-mode . diff-hl-show-hunk-mouse-mode)
         (dired-mode . (unless diff-hl-disable-on-remote
                         diff-hl-dired-mode)))
  :config
  (setq diff-hl-disable-on-remote t)
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

(use-package meow
  :demand t
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

  (defun substitute-regexp (substitution) ;; src: Juri Linkov
    "Use s/old/new/g regexp syntax for ‘query-replace’."
    (interactive
     (list
      (read-from-minibuffer "Substitute regexp: " '("s/" . 3) nil nil
                            'query-replace-history nil t)))
    (if (string-match "\\`s/\\(.*\\)/\\(.*\\)/\\([gi]*\\)" substitution)
        (let* ((sregex (match-string 1 substitution))
               (ssubst (match-string 2 substitution))
               (sflags (match-string 3 substitution))
               (case-fold-search (string-match "i" sflags)))
          (perform-replace
           sregex ssubst (not (string-match "g" sflags))
           t nil nil nil
           (if (and transient-mark-mode mark-active) (region-beginning) (goto-char (point-min)))
           (if (and transient-mark-mode mark-active) (region-end) (goto-char (point-max)))))
      (error "Invalid syntax")))

  :hook (after-init . meow-global-mode)
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-keypad-leader-dispatch "C-x" ;ctl-x-map
        meow-use-cursor-position-hack t
        meow-esc-delay 0.01)
  (dolist (item '(word line block find till))
    (push `(,item . 0) meow-expand-hint-counts))
  (define-key meow-insert-state-keymap (kbd "j") #'my-jk)
  (define-key meow-normal-state-keymap (kbd "S") insert-pair-map)
  (define-key meow-normal-state-keymap (kbd "D") delete-pair-map)
  (global-set-key (kbd "C-x SPC") 'meow-M-x)
  (dolist (imode '(eat-mode eshell-mode log-edit-mode))
    (push `(,imode . insert) meow-mode-state-list))
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
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
   '("a" . embark-act)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-comment)
   '("d" . meow-kill)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . recentf-open)
   '("gg" . avy-goto-char-timer)
   '("gh" . diff-hl-show-hunk)
   '("gi" . imenu)
   '("gf" . embark-dwim) ;; ffap
   '("gx" . flymake-show-buffer-diagnostics)
   '("gd" . xref-find-definitions)
   '("gb" . xref-go-back)
   '("gt" . tab-bar-switch-to-next-tab)
   '("gT" . tab-bar-switch-to-prev-tab)
   '("G" . meow-grab)
   '("h" . meow-left)
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
   '("L" . meow-swap-grab)
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
   '("z" . meow-pop-selection)
   '("Z" . undo-fu-only-redo)
   '("+" . meow-block)
   '("-" . negative-argument)
   '(":" . substitute-regexp)
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

;;; -------------------------------------------------
;;; Competitive programming setup (snippets and foxy)
;;; -------------------------------------------------
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
  :init (setq eglot-stay-out-of '(flymake))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t
        eglot-inlay-hints-mode nil)
  (add-hook 'rust-ts-mode-hook (lambda () (setq-local tab-width 4)))
  (add-hook 'go-ts-mode-hook
            (lambda () (setq-local tab-width 4)))
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

(defun my-project-find-file (&optional pattern)
  "Prompt the user to filter, scroll and select a file from a list of all
project files matching PATTERN."
  (interactive)
  (let ((comp-cand '()))
    (let* ((default-directory (vc-root-dir))
           (make-process-fn (if (file-remote-p default-directory)
                                'tramp-handle-make-process
                              'make-process))
           (process-name "my-project-find-file")
           (candidates '()))
      (funcall
       make-process-fn
       :name process-name
       :filter (lambda (_process-buffer output-batch)
                 ;; (when (boundp 'candidates)
                 (let ((candidate-batch (split-string output-batch "\n" t)))
                   (setq candidates (append candidates candidate-batch))
                   (setq comp-cand (seq-filter pattern candidates))));)
       :command (list "git" "ls-files")
       ;; :sentinel (lambda (process event)
       ;;             (completing-read "File: "
       ;;                              candidates
       ;;                              nil
       ;;                              t
       ;;                              pattern))
       :connection-type 'pipe)
      (completing-read "FileOut: "
                       comp-cand)
      ))
  ;; oantolin gawd
  (setq completing-read-function #'async-completing-read)
  (completing-read "File: " (acr-lines-from-process "git" "ls-files"))
  ;; oantolin gawd
  (let ((output-buffer (get-buffer-create "*test*"))
        (update-timer (run-with-timer 1 1
                                      (lambda ()
                                        (insert "@")     ; fake a change
                                        (backward-delete-char 1)
                                        (icomplete-exhibit)))))
    (tramp-handle-start-file-process "test" output-buffer "git" "ls-files")
    (unwind-protect
        (completing-read
         "Choose file: "
         (lambda (string pred action)
           (complete-with-action
            action
            (split-string
             (with-current-buffer output-buffer
               (buffer-string))
             "\n")
            string
            pred)))
      (cancel-timer update-timer)))
  )
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(solaire-mode esup diff-hl markdown-mode eat with-editor avy howm undo-fu undo-fu-session embark marginalia meow orderless popper)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
