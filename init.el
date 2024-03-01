;; -*- coding: utf-8; lexical-binding: t -*-
;;; --------------------------
;;; Code: Emacs config.
;;; --------------------------
;; (when (member "VictorMono Nerd Font Mono" (font-family-list))
;;   (set-frame-font "VictorMono Nerd Font Mono 16" nil t))
(use-package emacs
  :ensure nil
  :defines global-auto-revert-non-file-buffers auto-revert-verbose xref-search-program outline-minor-mode-cycle
  xref-show-definitions-function xref-show-xrefs-function ediff-split-window-function tabify-regexp
  ediff-keep-variants xref-auto-jump-to-first-xref ediff-window-setup-function Info-use-header-line
  :bind ("C-h '" . describe-face)
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
                warning-minimum-level :error)
  (setq read-process-output-max (* 2 1024 1024)
        inhibit-startup-screen t
        load-prefer-newer t
        make-backup-files nil
        create-lockfiles nil
        uniquify-buffer-name-style 'forward
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        Info-use-header-line nil
        outline-minor-mode-cycle t
        tabify-regexp "^\t* [ \t]+")

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'prog-mode-hook (electric-pair-mode t))
  (add-hook 'prog-mode-hook (show-paren-mode t))
  (add-hook 'prog-mode-hook (lambda ()
                              (add-hook 'before-save-hook
                                        (lambda ()
                                          (let ((current-prefix-arg t))
                                            (call-interactively 'untabify)))))
            nil t)
  (set-display-table-slot standard-display-table 'truncation 32) ;; hides $
  (set-display-table-slot standard-display-table 'wrap 32) ;; hides \

  (with-eval-after-load 'xref
    (setq xref-search-program 'ripgrep
          xref-auto-jump-to-first-xref nil ; 'move
          xref-show-definitions-function 'xref-show-definitions-completing-read
          xref-show-xrefs-function 'xref-show-definitions-completing-read))

  (with-eval-after-load 'minibuffer
    (save-place-mode 1)
    (context-menu-mode 1)
    (savehist-mode)
    (global-auto-revert-mode t)
    (blink-cursor-mode -1))
  ;; Load theme based on the time of the day
  (let ((hour (substring (current-time-string) 11 13)))
    (if (and (string-lessp hour "17") (string-greaterp hour "08"))
        (load-theme 'alabaster :no-confirm)
      (load-theme 'dracula :no-confirm)))
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))

  (global-set-key (kbd "C--") #'(lambda () (interactive) (dired "/data/data/com.termux/files/home/")))
  (global-set-key (kbd "C-'") #'(lambda () (interactive)
                                  (term "/data/data/com.termux/files/usr/bin/fish")))
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
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("\\" . dired-up-directory)
              ("E" . wdired-change-to-wdired-mode))
  :config
  (setq-default dired-dwim-target t
                dired-auto-revert-buffer t
                dired-mouse-drag-files t
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
  :functions completing-read-in-region
  :defines completion
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

(use-package vc
  :defer nil
  :ensure nil
  :defines project-vc-merge-submodules vc-annotate-background-mode
  :functions vc-git-push eshell-return-to-prompt eshell-send-input
  :bind (("C-x v f" . (lambda () (interactive)
                        (vc-git-push t)))
         ("C-x v e" . vc-ediff)
         ("C-x v R" . vc-interactive-rebase))
  :config
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (defun vc-interactive-rebase ()
    (interactive)
    (with-current-buffer (eshell)
      (eshell-return-to-prompt)
      (insert "git rebase -i")
      (eshell-send-input)))

  (push "bldr" vc-directory-exclusion-list)
  (push "external" vc-directory-exclusion-list)
  (setq vc-handled-backends '(Git)
        vc-follow-symlinks t
        project-vc-merge-submodules nil
        vc-annotate-background-mode t)
  
  (if (string= (car custom-enabled-themes) "dracula")
      ;; (advice-add 'vc-annotate-lines :after
      ;;             (lambda (&rest args)
      ;;               (let ((limit (car args)))
      ;;                 ;; TODO: move point back
      ;;                 (forward-line (- limit))
      ;;                 (while (< (point) limit)
      ;;                   (let* ((start (point))
      ;;                         (end (progn (forward-line 1) (point)))
      ;;                         (tmp-face (get-text-property (point) 'face)))
      ;;                     (put-text-property start end 'face tmp-face)
      ;;                     )))
      ;;               nil))))
      (add-hook 'vc-annotate-mode-hook
                (lambda ()
                  (face-remap-add-relative 'default :foreground "black")))))

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
  :functions flymake-eldoc-function
  :config
  (setq flymake-suppress-zero-counters t)
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
  :defer 2
  :defines tramp-default-method tramp-ssh-controlmaster-options tramp-verbose tramp-remote-path
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
         ("`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
  :defines popper-reference-buffers popper-mode-line popper-window-height
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          "magit:.\*"
          "\\*pabbrev suggestions\\*"
          "\\*eat\\*"
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
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer))

(use-package marginalia
  :defines marginalia-annotator-registry
  :functions marginalia-mode
  :hook (minibuffer-mode . marginalia-mode)
  :config
  (setq marginalia-annotator-registry
      (assq-delete-all 'file marginalia-annotator-registry)))

(use-package embark
  :after minibuffer
  :bind ("C-," . embark-act)
  :defines embark-indicators embark-prompter
  :config
  (setq embark-prompter 'embark-completing-read-prompter
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
  :functions undo-fu-session-global-mode
  :init
  (unless (string-equal system-type "android")
    (undo-fu-session-global-mode)))

(use-package eat
  :functions eat
  :bind ("C-." . (lambda () (interactive)
                   (eat "fish")))
  :custom
  (eat-kill-buffer-on-exit t))

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

(use-package meow
  :demand t
  :defines meow-keypad-leader-dispatch meow-cheatsheet-layout-qwerty meow-digit-argument meow-mode-state-list meow-normal-state-keymap
  meow-digit-argument meow-expand-hint-counts meow-use-cursor-position-hack meow-cheatsheet-layout meow-insert-state-keymap
  :functions meow-global-mode meow-motion-overwrite-define-key meow-normal-define-key meow-setup meow-insert-exit
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
  
  :init (meow-global-mode 1)
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-keypad-leader-dispatch ctl-x-map
        meow-use-cursor-position-hack t)
  (dolist (item '(word line block find till))
    (push `(,item . 0) meow-expand-hint-counts))
  (define-key meow-insert-state-keymap (kbd "j") #'my-jk)
  (define-key meow-normal-state-keymap (kbd "S") insert-pair-map)
  (define-key meow-normal-state-keymap (kbd "D") delete-pair-map)
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
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-comment)
   '("d" . meow-kill)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . avy-goto-char-timer)
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
   '("%" . mark-whole-buffer)
   '("/" . isearch-forward)
   '(">" . indent-rigidly-right)
   '("<" . indent-rigidly-left)
   '("'" . repeat)
   '("<escape>" . ignore)))

;;; -------------------------------------------------
;;; Competitive programming setup (snippets and foxy)
;;; -------------------------------------------------
(use-package markdown-mode
  :defines markdown-fontify-code-blocks-natively
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package eldoc-box
  :ensure nil
  :bind ("C-l" . eldoc-box-help-at-point)
  :defines eldoc-box-max-pixel-height eldoc-box-max-pixel-width eldoc-box-only-multi-line
  :config
  (setq eldoc-box-max-pixel-width 600
        eldoc-box-max-pixel-height 700
        eldoc-box-only-multi-line t))

(setq treesit-language-source-alist ;; treesit-install-language-grammar
      '((go "https://github.com/tree-sitter/tree-sitter-go")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "typescript/src")))

(push '("\\.rs\\'" . rust-ts-mode) auto-mode-alist)
(push '("\\.go\\'" . go-ts-mode) auto-mode-alist)
;; (with-eval-after-load 'project
;;   (defun project-find-go-module (dir)
;;     (when-let ((root (locate-dominating-file dir "go.mod")))
;;       (cons 'go-module root)))
;;   (cl-defmethod project-root ((project (head go-module)))
;;     (cdr project))
;;   (add-hook 'project-find-functions #'project-find-go-module))

(push '("\\.ts\\'" . typescript-ts-mode) auto-mode-alist)
(push '("\\.bin\\'" . hexl-mode) auto-mode-alist)

(use-package eglot
  :hook (((rust-ts-mode go-ts-mode) . eglot-ensure)
         (eglot-managed-mode . (lambda ()
                                 (setq eldoc-documentation-strategy
                                       'eldoc-documentation-compose-eagerly))))
  :functions eglot-format-buffer jsonrpc--log-event
  :defines go-ts-mode-indent-offset
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t
        eglot-inlay-hints-mode nil)
  (add-hook 'rust-ts-mode-hook (lambda () (setq-local tab-width 4)))
  (add-hook 'go-ts-mode-hook
            (lambda () (setq-local tab-width 4)))
  (setq go-ts-mode-indent-offset 4)
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

(with-eval-after-load 'prog-mode
  (load "~/.emacs.d/comp" nil t))

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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(esup markdown-mode eldoc-box eat with-editor avy howm undo-fu undo-fu-session embark marginalia meow orderless popper)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
