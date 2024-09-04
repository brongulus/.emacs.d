;;; init.el ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Ah shit, here we go again.
;;; Code:

;;; Bootstrap

(define-key global-map (kbd "C-z") (make-sparse-keymap))
(defconst dropbox-dir
  (if is-android
      "/sdcard/Dropbox/"
    "~/Dropbox/"))
(when is-android
  (global-set-key (kbd "C-\\") 'term))

(use-package use-package
  :no-require
  :config
  (setq use-package-compute-statistics t ;; use-package-report
        use-package-always-ensure t
        use-package-always-defer t
        use-package-enable-imenu-support t ;; not working
        use-package-expand-minimally t))

(use-package emacs
  :no-require
  :ensure nil
  :bind-keymap ("C-j" . ctl-x-r-map)
  :bind (("C-h '" . describe-face)
         ("M-c" . quick-calc) ; 16#hex
         ("M-k" . kill-word)
         ("C-d" . delete-backward-char)
         ("C-a" . (lambda nil (interactive)
                    (if (= (point) (progn (beginning-of-line-text) (point)))
                        (beginning-of-line))))
         ("C-z C-z" . (lambda () (interactive)
                        (if (derived-mode-p 'emacs-lisp-mode)
                            (describe-symbol (symbol-at-point))
                          (if (and (display-graphic-p)
                                   (package-installed-p 'eldoc-box))
                              (eldoc-box-help-at-point)
                            (eldoc-doc-buffer t)))))
         ("C-o" . other-window)
         ("C-," . scroll-other-window)
         ("C-." . scroll-other-window-down)
         ("M-s r" . replace-regexp)
         ("M-s f" . ffap)
         ("C-x C-m" . execute-extended-command)
         ("C-x C-j" . (lambda nil (interactive)
                        (let ((current-prefix-arg '(4)))
                          (call-interactively 'delete-indentation))))
         ("C-x C-b" . ibuffer)
         ("C-x k" . kill-current-buffer)
         ("C-M-r" . raise-sexp)
         ("C-x \\" . align-regexp)
         ("C-x C-z" . restart-emacs)
         ("<f7>" . (lambda () (interactive)
                     (hl-line-mode 'toggle)
                     (highlight-indent-guides-mode 'toggle)))
         ("<f6>" . zed-toggle-theme))
  :config
  (setq-default line-spacing 3
                ;; cursor-type 'bar
                tab-width 2
                fill-column 100
                indent-tabs-mode nil
                enable-recursive-minibuffers t
                show-paren-delay 0
                show-paren-context-when-offscreen t
                show-paren-when-point-inside-paren t
                custom-safe-themes t
                ring-bell-function 'ignore
                use-short-answers t
                ;; debug-on-error t ; issues w/ completion in :bind
                warning-minimum-level :error
                display-line-numbers-width 5
                display-line-numbers-grow-only t
                delete-pair-blink-delay 0)

  (setq process-adaptive-read-buffering nil
        kill-do-not-save-duplicates t
        kill-whole-line t
        auto-mode-case-fold nil
        undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960
        enable-local-variables :safe
        scroll-margin 0
        scroll-conservatively 100
        scroll-preserve-screen-position t
        ;; pixel-scroll-precision-large-scroll-height 35.0
        split-height-threshold nil
        split-width-threshold 100
        save-abbrevs nil
        make-backup-files nil
        create-lockfiles nil
        uniquify-buffer-name-style 'forward
        auto-revert-verbose nil
        sentence-end-double-space nil
        Info-use-header-line nil
        outline-minor-mode-cycle nil ;t
        tabify-regexp "^\t* [ \t]+"
        grep-command "grep --color=always -nHi -r --include=*.* -e \"pattern\" ."
        electric-pair-skip-self t
        electric-pair-preserve-balance 'electric-pair-inhibit-predicate
        electric-pair-delete-adjacent-pairs t
        ;; electric-pair-open-newline-between-pairs nil
        electric-pair-skip-whitespace nil
        shell-command-prompt-show-cwd t
        shell-kill-buffer-on-exit t
        set-mark-command-repeat-pop t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)

  ;; surround
  (defvar insert-pair-map ;; src: oantolin
    (let ((map (make-sparse-keymap)))
      (define-key map [t] #'insert-pair)
      map))
  (defvar delete-pair-map
    (let ((map (make-sparse-keymap)))
      (define-key map [t] #'delete-pair)
      map))
  (global-set-key (kbd "M-s s") insert-pair-map)
  (global-set-key (kbd "M-s d") delete-pair-map)
  (defun match-pair nil
    (interactive)
    (if (nth 3 (syntax-ppss))
        (backward-up-list 1 t t)
      (cond ((looking-at "\\s\(\\|\{") (forward-list 1) (backward-char 1))
            ((looking-at "\\s\)\\|\}") (forward-char 1) (backward-list 1))
            (t (backward-up-list 1 t t)))))
  (global-set-key (kbd "M-s m") #'match-pair)
  
  ;; Copy-line if region not selected
  (defun slick-copy nil
    (interactive)
    (if mark-active
        (call-interactively 'kill-ring-save)
      (message "Copied current line")
      (copy-region-as-kill (line-beginning-position 1)
                           (line-end-position 1))))

  (defun my/select-current-line-and-forward-line (arg) ;src: kaushal modi
    "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
    (interactive "p")
    (when (not (use-region-p))
      (forward-line 0)
      (set-mark-command nil))
    (forward-line arg))
  
  ;; (global-set-key (kbd "M-w") 'slick-copy)
  (global-set-key (kbd "M-l") #'my/select-current-line-and-forward-line)
  
  (defun silent-command (fn &rest args)
    "Used to suppress output of FN."
    (let ((inhibit-message t)
          (message-log-max nil)
          (save-silently t))
      (apply fn args)))

  (set-register ?f `(file . ,(locate-user-emacs-file "init.el")))
  (set-register ?c `(file . "~/problems/"))
  
  (add-hook 'prog-mode-hook (which-function-mode))
  ;; load theme based on the time of the day
  (let ((hour (substring (current-time-string) 11 13)))
    (if (and (string-lessp hour "17") (string-greaterp hour "08")
             (display-graphic-p))
        ;; load-theme-light is a zed-theme var
        (setq load-theme-light t)
      (when (eq system-type 'darwin)
        (modify-all-frames-parameters '((ns-appearance . dark))))))
  (load-theme 'zed :no-confirm)

  (unless (package-installed-p 'meow)
    (define-key key-translation-map (kbd "ESC") (kbd "C-g")))
  
  (define-advice load-theme (:before (&rest _args) theme-dont-propagate)
    "Discard all themes before loading new."
    (mapc #'disable-theme custom-enabled-themes))

  (define-advice kill-region (:before (&rest _args) unix-werase) ; src: wiki
    "When called interactively with no active region, delete a single word
backwards instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (save-excursion (backward-word 1) (point)) (point)))))

  (dolist (func '(yank yank-pop))
    (eval
     `(define-advice ,func (:after (&rest _args) yank-indent) ; src: magnars
        "Indent yanked text (with prefix arg don't indent)."
        (let ((transient-mark-mode nil))
          (indent-region (region-beginning) (region-end) nil)))))
  
  (define-advice term-handle-exit (:after (&rest _args) term-kill-on-exit)
    "Kill the buffer after the term exits."
    (kill-buffer (current-buffer))))

(use-package emacs
  :no-require
  :ensure nil
  :bind (("C-x w w" . my-min-max-window)
         ("<f5>" . my-switch-to-alternate-file))
  :bind (:map special-mode-map
              ("Q" . (lambda nil (interactive)
                       (quit-window t)))
              :map prog-mode-map
              ("C-c C-c" . compile)
              ("C-c C-r" . recompile))
  :defer 2
  :hook
  (after-init . (lambda nil
                  ;; random functions
                  (defvar my-min-max-window nil)
                  (defun my-min-max-window()
                    "Toggle full view of selected window."
                    (interactive)
                    (if (and (one-window-p) my-min-max-window)
                        (window-state-put my-min-max-window)
                      (setq my-min-max-window (window-state-get))
                      (delete-other-windows)))

                  (defun my-switch-to-alternate-file ()
                    "Switch to or create an alternate file based on the major mode."
                    (interactive)
                    (let* ((filename (buffer-file-name))
                           (extension (file-name-extension filename))
                           (basename (file-name-sans-extension filename))
                           (alternate-file
                            (cond
                             ((derived-mode-p 'c-mode 'c++-mode)
                              (if (string= extension "h")
                                  (concat basename ".c")
                                (concat basename ".h")))
                             ((derived-mode-p 'go-mode)
                              (if (string-match-p "_test\\.go\\'" filename)
                                  (concat (replace-regexp-in-string "_test\\'" ""
                                                                    basename)
                                          ".go")
                                (concat basename "_test.go")))
                             (t (message "No alternate file for current mode"))))
                           (alternate-buffer (find-file-noselect alternate-file)))
                      (if (file-exists-p alternate-file)
                          (switch-to-buffer alternate-buffer)
                        (progn
                          (message "Creating new file: %s" alternate-file)
                          (switch-to-buffer (find-file-noselect alternate-file))))))

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
                  
                  ;; misc modes and hooks
                  (put 'narrow-to-region 'disabled nil)
                  (add-hook 'debugger-mode-hook #'visual-line-mode)
                  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
                  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
                  (add-hook 'compilation-filter-hook #'ansi-osc-compilation-filter)
                  (add-hook 'prog-mode-hook (electric-pair-mode t))
                  (add-hook 'minibuffer-setup-hook (lambda () (electric-pair-mode 0)))
                  (add-hook 'minibuffer-exit-hook (lambda () (electric-pair-mode 1)))
                  (add-hook 'prog-mode-hook (show-paren-mode t))
                  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
                  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
                  (add-to-list 'write-file-functions
                               '(lambda ()
                                  (when (eq major-mode 'emacs-lisp-mode)
                                    (check-parens))
                                  nil))
                  (add-hook 'help-fns-describe-function-functions
                            #'shortdoc-help-fns-examples-function)
                  (with-eval-after-load 'whitespace
                    (add-to-list 'whitespace-display-mappings
                                 '(newline-mark ?\n [8626 ?\n]) t))
                  (unless (display-graphic-p)
                    (set-display-table-slot standard-display-table
                                            'vertical-border
                                            (make-glyph-code ?│))
                    (xterm-mouse-mode))
                  ;; enable some useful modes
                  (tab-bar-mode +1)
                  ;; (pixel-scroll-precision-mode 1)) ;; modeline flickering
                  (save-place-mode 1)
                  (when (string> emacs-version "29.4")
                    (which-key-mode 1))
                  (setq savehist-additional-variables '(register-alist kill-ring))
                  (setq save-interprogram-paste-before-kill t)
                  (global-auto-revert-mode 1)
                  (global-subword-mode 1)
                  (delete-selection-mode) ;; pending-delete-mode
                  (context-menu-mode 1)
                  (savehist-mode 1)
                  (blink-cursor-mode 1)
                  (setq blink-cursor-interval 0.7)
                  (tooltip-mode -1))))

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

;;; Completion

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

(use-package vertico
  :hook (after-init . vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (:map vertico-map
              ("<escape>" . minibuffer-keyboard-quit)
              ("<backspace>" . vertico-directory-delete-char)
              ("RET" . vertico-directory-enter)
              ("C-<return>" . vertico-exit-input)
              ("M-`" . vertico-multiform-vertical)
              ("TAB" . vertico-next)
              ("<backtab>" . vertico-previous)
              ("S-TAB" . vertico-previous)
              ("M-TAB" . vertico-previous)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :custom
  (vertico-multiform-categories
   '((command flat)
     (buffer flat)
     (project-file flat)
     (file flat)))
  :config
  (vertico-multiform-mode)
  
  (setq vertico-scroll-margin 0
        vertico-resize nil
        vertico-cycle t)

  (setq tab-always-indent 'complete
        tab-first-completion 'word-or-paren
        completions-group t
        completions-sort 'historical
        completions-detailed t))

(use-package cape
  :hook (eglot-managed-mode . (lambda nil
                                (setq-local completion-at-point-functions
                                            (list (cape-capf-super
                                                   #'eglot-completion-at-point
                                                   #'cape-abbrev
                                                   #'cape-file
                                                   #'cape-dabbrev)))))
  :hook (emacs-lisp-mode . (lambda nil
                             (setq-local completion-at-point-functions
                                         (list (cape-capf-super
                                                #'elisp-completion-at-point
                                                #'cape-dabbrev)
                                               #'cape-file))))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-abbrev))

(use-package completion-preview
  :if (string> emacs-version "29.4")
  :ensure nil
  :hook ((prog-mode comint-mode eshell-mode)
         . completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ([tab] . completion-preview-complete)
              ("TAB" . completion-preview-complete)
              ("M-i" . completion-at-point)
              ("C-<return>" . completion-preview-insert))
  :config
  (push 'org-self-insert-command completion-preview-commands))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :hook ((corfu-mode . corfu-popupinfo-mode)
         (meow-insert-exit . corfu-quit))
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :config
  (dolist (key '("C-f" "C-b" "C-a" "C-e"))
    (define-key corfu-map key nil))
  (keymap-unset corfu-map "<remap> <next-line>")
  (keymap-unset corfu-map "<remap> <previous-line>")
  (add-hook 'eshell-mode #'(lambda () (setq-local corfu-auto nil) (corfu-mode)))
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  (setq completion-ignore-case t)
  (setq corfu-cycle t
        corfu-auto nil;t
        corfu-auto-prefix 2
        corfu-auto-delay 0.1
        corfu-separator 32
        corfu-max-width 80
        corfu-preselect 'prompt
        corfu-quit-no-match t
        corfu-quit-at-boundary 'separator
        corfu-preview-current nil
        corfu-popupinfo-delay '(0.5 . 0.1)
        corfu-preselect-first nil)
  (use-package corfu-terminal
    :when (not (display-graphic-p))
    :hook ((corfu-mode . corfu-terminal-mode))))

;;; Editing

(use-package easy-kill
  ;; use M-w ? for help!
  :bind (([remap kill-ring-save] . #'easy-kill)
         ([remap mark-sexp]      . #'easy-mark)))

(use-package macrursors
  ;; use C-; (macrursors-end) to do edits
  :vc (:url "https://github.com/karthink/macrursors"
            :rev :newest)
  :init
  (define-prefix-command 'macrursors-mark-map)
  :bind-keymap ("C-;" . macrursors-mark-map)
  :bind (("M-d" . (lambda nil ; mark-word-then-select-next
                    (interactive)
                    (if (or (use-region-p)
                            defining-kbd-macro)
                        (progn
                          (call-interactively 'macrursors-mark-next-instance-of)
                          (if (use-region-p)
                              (deactivate-mark)))
                      (let ((start-of-word (save-excursion (backward-word) (point))))
                        (unless (eq (point) start-of-word)
                          (backward-word)))
                      (mark-word))))
         ("M-'" . macrursors-mark-all-instances-of)
         ("M-\\" . (lambda nil (interactive) ; toggle secondary selection
                     (if (secondary-selection-exist-p)
                         (macrursors-select-clear)
                       (macrursors-select))))
         ("M-<down>" . macrursors-mark-next-line)
         ("M-<up>" . macrursors-mark-previous-line)
         ("M-n" . macrursors-mark-next-instance-of)
         ("M-p" . macrursors-mark-previous-instance-of)
         ("M-u" . (lambda nil ; macrursors-undo-last
                    (interactive)
                    (and macrursors--overlays
                         (delete-overlay (car macrursors--overlays)))
                    (cl-callf cdr macrursors--overlays)))
         
         :map macrursors-mark-map
         ("M-a" . macrursors-mark-all-instances-of)
         ("C-g" . macrursors-early-quit)
         
         :repeat-map macrursors-mark-repeat-map
         ("n" . macrursors-mark-next-instance-of)
         ("p" . macrursors-mark-previous-instance-of)
         ("<down>" . macrursors-mark-next-line)
         ("<up" . macrursors-mark-previous-line)
         
         :map macrursors-mode-map
         ("C-'" . macrursors-hideshow)
         
         :map isearch-mode-map
         ("C-;" . macrursors-mark-from-isearch)
         ("M-<down>" . macrursors-mark-next-from-isearch)
         ("M-<up>" . macrursors-mark-previous-from-isearch)
         
         :repeat-map isearch-repeat-map
         ("<down>" . macrursors-mark-next-from-isearch)
         ("<up>" . macrursors-mark-previous-from-isearch))
  :config
  (set-face-attribute 'macrursors-cursor-face nil
                      :inverse-video nil :inherit 'cursor)
  (dolist (mode '(corfu-mode beacon-mode))
    (add-hook 'macrursors-pre-finish-hook mode)
    (add-hook 'macrursors-post-finish-hook mode))
  (when (featurep 'meow)
    (add-hook 'macrursors-mode-hook #'meow-insert)))

(use-package move-text
  :bind (("M-P" . #'move-text-up)
         ("M-N" . #'move-text-down)))

(use-package puni
  :if (display-graphic-p) ; FIXME puni is messing terminal
  ;; TODO: https://karthinks.com/software/a-consistent-structural-editing-interface/
  ;; https://countvajhula.com/2021/09/25/the-animated-guide-to-symex/
  :hook ((emacs-lisp-mode go-ts-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :bind (("C-=" . puni-expand-region)
         :map puni-mode-map
         ("C-w" . nil) ;delete-backword
         ("M-w" . nil) ;easy-kill
         ("M-d" . nil) ;macrursors
         ("C-d" . nil) ;delete-back-char
         ("C-k" . my-puni-kill-line)
         ("s-s" . puni-splice)
         ("M-]" . puni-slurp-forward)
         ("M-[" . puni-barf-forward)
         ("M-}" . puni-barf-backward)
         ("M-{" . puni-slurp-backward)
         :repeat-map puni-mode-repeat-map
         ("-" . puni-contract-region)
         ("=" . puni-expand-region))
  :config
  (defun my-puni-kill-line ()
    "Kill a line forward while keeping expressions balanced.
If nothing can be deleted, kill backward.  If still nothing can be
deleted, kill the pairs around point."
    (interactive)
    (let ((bounds (puni-bounds-of-list-around-point)))
      (if (eq (car bounds) (cdr bounds))
          (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
            (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill))
        (if (eq (point) (cdr bounds))
            (puni-backward-kill-line)
          (puni-kill-line))))))

(use-package devil
  :hook (after-init . global-devil-mode)
  :config
  (push '("%k v") devil-repeatable-keys)
  (assoc-delete-all "%k SPC" devil-special-keys)
  (devil-set-key (kbd "'")))

(use-package avy
  :bind ("C-'" . avy-goto-char-timer)
  :bind (:map isearch-mode-map
              ("M-s M-s" . avy-isearch))
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :config
  (defun avy-action-add-cursor (pt) ; src: karthik
    (require 'macrursors)
    (unwind-protect
        (progn
          (macrursors--add-overlay-at-point pt)
          (kmacro-keyboard-quit)
          (avy-resume))
      (macrursors-start)))

  (push '(?\; . avy-action-add-cursor) avy-dispatch-alist)
  :custom
  (avy-single-candidate-jump t)
  (avy-background t))

(use-package deadgrep
  :bind ("M-s d" . deadgrep)
  :preface
  (defun deadgrep-current-dir (search-term)
    "Grep for SEARCH-TERM in current directory."
    (interactive "sTerm: ")
    (deadgrep search-term default-directory)))

(use-package embark
  :bind ("C-c C-o" . embark-act)
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-prompter 'embark-completing-read-prompter
        embark-keymap-prompter-key "'"
        embark-indicators (delete 'embark-mixed-indicator embark-indicators)))

;;; Built-ins

(use-package xref
  :ensure nil
  :bind (:map xref--transient-buffer-mode-map
              ("TAB" . xref-show-location-at-point))
  :config
  (setq xref-search-program (if (executable-find "rg")
                                'ripgrep
                              'grep)
        xref-auto-jump-to-first-xref nil ; 'move
        xref-show-definitions-function 'xref-show-definitions-buffer-at-bottom
        xref-show-xrefs-function 'xref-show-definitions-buffer-at-bottom))

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
  :bind (("C-z a" . hs-global-cycle)
         ("C-z f" . hs-cycle))
  :config
  (defun hs-cycle (&optional level) ; src: Karthik
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

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :config
  (advice-add #'repeat-mode :around #'silent-command)
  (setq repeat-exit-key "RET"))

(use-package isearch
  :ensure nil
  :bind (("C-s" . isearch-forward-regexp)
         :map isearch-mode-map
         ("M-/" . isearch-complete)
         :repeat-map isearch-repeat-map
         ("s" . isearch-repeat-forward)
         ("r" . isearch-repeat-backward))
  :custom
  (isearch-wrap-pause 'no)
  (isearch-lazy-count t)
  (isearch-allow-scroll 'unlimited)
  (isearch-regexp-lax-whitespace t)
  (search-whitespace-regexp ".*?"))

(use-package replace
  :ensure nil
  :bind (("M-s a" . multi-occur-in-matching-buffers))
  :hook (occur-mode . (lambda nil
                        (setq-local truncate-lines t
                                    mouse-wheel-tilt-scroll t
                                    mouse-wheel-flip-direction t
                                    mouse-wheel-scroll-amount-horizontal 2)))
  :config
  (add-hook 'occur-hook
            (lambda nil
              (rename-buffer (format "*Occur: %s*" (car regexp-history)))))
  
  (add-to-list 'display-buffer-alist
               '("\\*Occur: .\*"
                 (display-buffer-reuse-window)
                 (window-parameters (width . 0.50)
                                    (direction . right))))
  
  (setq list-matching-lines-buffer-name-face 'tab-bar
        list-matching-lines-prefix-face 'vertical-border
        list-matching-lines-default-context-lines 2)

  (defun noccur-project (regexp &optional nlines directory-to-search) ;; src: noccur (async?)
    (interactive (occur-read-primary-args))
    (let* ((default-directory
            (or directory-to-search (read-directory-name "Search in directory: ")))
           (files (mapcar #'find-file-noselect
                          (noccur--find-files regexp))))
      (when (package-installed-p 'diff-hl)
        (let ((diff-hl-flydiff-mode -1))
          (multi-occur files regexp nlines)))))

  (defun noccur--find-files (regexp)
    (let* ((listing-command (if (locate-dominating-file default-directory ".git")
                                "git ls-files -z"
                              "find . -type f -print0"))
           (command (format "%s | xargs -0 rg -l \"%s\""
                            listing-command
                            regexp)))
      (split-string (shell-command-to-string command) "\n")))
  
  (defun clean-occur-context-line (orig-fun &rest args) ; src: GPT
    "Advice for `occur-context-lines` to change the separator."
    (let ((result (apply orig-fun args)))
      (cl-destructuring-bind (output-line after-lines) result
        (setq output-line
              (replace-regexp-in-string
               "-------\n" ;; Old separator
               (propertize (concat (make-string (window-total-width) ?─) "\n")
                           'face list-matching-lines-prefix-face)
               output-line))
        (list output-line after-lines))))

  (defvar occur-engine-prev-linum -1)
  (defun occur-engine-add-prefix (lines &optional prefix-face)
    (let ((curr-line (- 1 list-matching-lines-default-context-lines)))
      ;; FIXME after match for last result
      (mapcar
       (lambda (line)
         (let* ((linum (if (and (> occur-engine-prev-linum -1)
                                (< occur-engine-prev-linum (current-line)))
                           (+ occur-engine-prev-linum
                              (1+ list-matching-lines-default-context-lines)
                              curr-line)
                         (+ (current-line) curr-line)))
                (line-prefix (if prefix-face
                                 (propertize (format "%7d:" linum)
                                             'font-lock-face prefix-face)
                               (format "%7d:" linum))))
           (setq curr-line (1+ curr-line))
           (if (> curr-line 0)
               (setq occur-engine-prev-linum (current-line)))
           (concat line-prefix line "\n")))
       lines)))
  
  (advice-add 'occur-context-lines :around #'clean-occur-context-line))

(use-package flymake
  :ensure nil
  :bind ("M-s x" . flymake-show-buffer-diagnostics)
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
  (setq flymake-no-changes-timeout nil
        flymake-suppress-zero-counters t
        flymake-fringe-indicator-position nil
        flymake-margin-indicator-position nil
        flymake-show-diagnostics-at-end-of-line t)

  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-idle-delay 0.3
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-display-truncation-message nil))

(use-package recentf
  :ensure nil
  :bind ("C-x f" . my/recentf-buffer)
  :hook (kill-emacs . recentf-cleanup)
  :preface
  (defun my/recentf-buffer () ;; src:James dyer
    "Switch to a buffer or open a recent file from a unified interface."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
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
        auto-save-default nil
        tramp-verbose 0
        tramp-remote-path '(tramp-own-remote-path)))

;;; Window Management

(use-package windmove
  :defer 1
  :ensure nil
  :bind (("M-<right>" . windmove-swap-states-right)
         ("M-<left>" . windmove-swap-states-left))
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings 'none))

(use-package popper
  :bind (("M-j"   . popper-toggle)
         ("C-`"   . popper-cycle)
         ("M-`" . popper-toggle-type)
         :repeat-map popper-repeat-map
         ("`"     . popper-cycle))
  :hook ((after-init . popper-mode)
         (after-init . popper-tab-line-mode))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*" "Output\\*$" "\\*xref\\*"
          "\\*Async Shell Command\\*" "\\*shell.\*"
          "magit:.\*" "\\*pabbrev suggestions\\*"
          ".*-eat\\*" "\\*eldoc\\*" "vc-git :.\*"
          "\\*vc-change-log\\*" "\\*Deletions\\*"
          "\\*Flymake .\*" "\\*CDLaTex Help\\*"
          "\\*Process List\\*" "\\*Org Select\\*"
          "^CAPTURE.*" "\\*Org Agenda\\*"
          "\\*Warnings\\*" "\\*Backtrace\\*"
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
    (let ((tab-vbar (when (display-graphic-p)
                      (propertize " "
                                  'face `(:background ,(face-foreground 'vertical-border))
                                  'display '(space :width (1)))))
          (tab-face (if (eq tab (current-buffer))
                        (if (mode-line-window-selected-p)
                            'tab-line-tab-current 'tab-line-tab)
                      'tab-line-tab-inactive)))
      (concat
       tab-vbar
       (propertize " " 'face tab-face)
       (when (package-installed-p 'nerd-icons)
         (with-eval-after-load 'nerd-icons
           (with-current-buffer tab
             (propertize (format "%s " (nerd-icons-icon-for-buffer))
                         'face tab-face))))
       (tab-line-tab-name-format-default tab tabs)
       tab-vbar))))

(use-package transpose-frame
  :bind (("C-x 5 t" . transpose-frame)
         ("C-x 5 r" . rotate-frame)
         ("C-x 5 f" . flip-frame)
         ("C-x 5 l" . flop-frame)
         :repeat-map ctl-x-5-repeat-map
         ("t" . transpose-frame)
         ("r" . rotate-frame)
         ("f" . flip-frame)
         ("l" . flop-frame)))

;;; Environment

(use-package exec-path-from-shell
  :if (display-graphic-p)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package xclip
  :unless (display-graphic-p)
  :init (xclip-mode 1))

(use-package kkp
  :unless (display-graphic-p)
  :hook (after-init . global-kkp-mode))

(use-package undo-fu-session
  :hook ((prog-mode conf-mode fundamental-mode text-mode tex-mode) . undo-fu-session-mode)
  :config
  (setq undo-fu-session-compression nil))

;;; Visual Niceties

(use-package eldoc-box
  :after eldoc
  :commands eldoc-box-help-at-point
  :config
  (setq eldoc-box-max-pixel-width 600
        eldoc-box-max-pixel-height 700
        eldoc-box-only-multi-line t))

(use-package sideline-flymake
  :disabled t
  :if (string< emacs-version "30.0")
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line)
  (setq sideline-backends-right '(sideline-flymake)
        sideline-flymake-show-backend-name t))

(use-package writeroom-mode
  :bind ("<f9>" . writeroom-mode)
  :hook ((nov-mode Info-mode Man-mode eww-mode) . writeroom-mode)
  :hook (org-mode . org-writer-mode)
  :config
  (defun org-writer-mode nil
    ;; visual-line-mode & variable-pitch-mode
    (add-to-list ;; TODO: Add line-height support as well
     'window-size-change-functions
     (lambda (window)
       (when (eq major-mode 'org-mode)
         (let* ((window (selected-window))
                (width (window-total-width window))
                (width-ten (* 10 (round (/ (* 1.6 width) 10))))
                (adaptive-height (min (max 170 width-ten) 220))
                (wheight (window-total-height window)))
           (face-remap-add-relative 'default :height
                                    (if (> wheight 35)
                                        adaptive-height
                                      160))
           (setq-local line-spacing 0.5)))))
    (setq-local mode-line-format nil
                writeroom-restore-window-config t
                writeroom-maximize-window t)
    (writeroom-mode)
    (setq-local cursor-type 'bar
                scroll-preserve-screen-position t
                scroll-conservatively 101
                maximum-scroll-margin 0.5
                scroll-margin 28))
  
  (setq writeroom-width (min 100 (max 90 (- (window-width) 10)))
        writeroom-mode-line t
        writeroom-restore-window-config t
        writeroom-global-effects nil
        writeroom-maximize-window nil))

(use-package nerd-icons
  :defer 0.2
  :config
  ;; causing issues in emacs -nw
  ;; (when (not (find-font (font-spec :name nerd-icons-font-family)))
  ;;   (nerd-icons-install-fonts t))
  (when (custom-theme-enabled-p 'zed)
    (setq zed-tab-back-button
          (nerd-icons-octicon "nf-oct-arrow_left" :v-adjust 0.2)
          zed-tab-forward-button
          (nerd-icons-octicon "nf-oct-arrow_right" :v-adjust 0.2)))
  (push '(eat-mode nerd-icons-devicon "nf-dev-terminal") nerd-icons-mode-icon-alist)
  (use-package nerd-icons-corfu
    :init
    (with-eval-after-load 'corfu
      (push 'nerd-icons-corfu-formatter corfu-margin-formatters)))
  (use-package nerd-icons-dired
    :hook (dired-mode . nerd-icons-dired-mode)
    :config
    (define-advice nerd-icons-dired--setup
        (:before (&rest _args) fix-pr22-18)
      (when (derived-mode-p 'dired-mode)
        (setq-local tab-width 1)
        (with-eval-after-load 'dired-subtree
          (advice-add 'dired-subtree-insert :around #'nerd-icons-dired--refresh-advice)
          (advice-add 'dired-subtree-remove :around #'nerd-icons-dired--refresh-advice))
        (with-eval-after-load 'wdired
          (advice-add 'wdired-abort-changes :around #'nerd-icons-dired--refresh-advice))))
    (define-advice nerd-icons-dired--teardown
        (:before (&rest _args) fix-pr22-18)
      (advice-remove 'dired-subtree-insert #'nerd-icons-dired--refresh-advice)
      (advice-remove 'dired-subtree-remove #'nerd-icons-dired--refresh-advice)
      (advice-remove 'wdired-abort-changes #'nerd-icons-dired--refresh-advice)))
  (use-package nerd-icons-ibuffer
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode)))

(use-package info-colors
  :vc (:url "https://github.com/ubolonton/info-colors")
  :hook
  (Info-selection . info-colors-fontify-node))

(use-package shr-tag-pre-highlight
  ;; FIXME complains if mode isnt available
  :defer 1
  :config
  (with-eval-after-load 'nov
    (advice-add 'shr--set-target-ids :around
                (lambda (orig-fn &rest args)
                  (if (and (get-buffer-window)
                           (bufferp (point)))
                      (apply orig-fn args)
                    nil)))
    (add-to-list 'nov-shr-rendering-functions
                 '(pre . shr-tag-pre-highlight) t))
  ;; eww
  (setq eww-auto-rename-buffer 'title)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package highlight-indent-guides
  :config
  (set-face-attribute 'highlight-indent-guides-character-face nil
                      :inherit 'vertical-border)
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-auto-enabled nil
        highlight-indent-guides-responsive nil))

(use-package diff-hl
  :hook (((prog-mode conf-mode) . turn-on-diff-hl-mode)
         ((prog-mode conf-mode) . diff-hl-margin-mode))
  :config
  (diff-hl-flydiff-mode t)
  (when (package-installed-p 'magit)
    (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (setq vc-git-diff-switches '("--histogram")
        diff-hl-flydiff-delay 0.5
        diff-hl-update-async t
        diff-hl-show-staged-changes nil
        diff-hl-margin-symbols-alist '((insert . "█")
                                       (delete . "█")
                                       (change . "█"))
        diff-hl-draw-borders nil))

;;; Apps

(use-package desktop
  :ensure nil
  :if (display-graphic-p)
  :hook (window-setup . desktop-save-mode)
  :hook (window-setup . desktop-read)
  :config
  (advice-add 'desktop-read :around
              (lambda (orig &rest args)
                (let ((start-time (current-time)))
                  (prog1
                      (apply orig args)
                    (message "Desktop restored in %.2fs"
                             (float-time
                              (time-subtract (current-time) start-time)))))))
  (dolist (item
           '(alpha background-color background-mode border-width tab-bar
                   bottom-divider-width cursor-color cursor-type display-type
                   environment font fontsize foreground-color fullscreen
                   fullscreen-restore horizontal-scroll-bars internal-border-width
                   left-fringe line-spacing menu-bar-lines ns-appearance
                   ns-transparent-titlebar powerline-cache right-divider-width
                   right-fringe scroll-bar-height scroll-bar-width tool-bar-lines
                   tool-bar-position vertical-scroll-bars zoom-window-buffers
                   zoom-window-enabled))
    (push `(,item . :never) frameset-filter-alist))
  (push '(flymake-mode nil) desktop-minor-mode-table) ;; fix elisp-flymake-byte-compile
  
  (setq desktop-restore-forces-onscreen nil
        desktop-auto-save-timeout 10
        desktop-files-not-to-save
        (format "%s\\|%s"
                desktop-files-not-to-save
                ".*\\.el\\.gz\\|inbox.org"))
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
              ("C-<return>" . gnus-summary-scroll-down)
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
        gnus-auto-select-next 'quietly
        gnus-summary-display-arrow nil
        gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))
  ;; Better UI
  (gnus-add-configuration
   '(article
     (horizontal 1.0
                 (vertical 1.0
                           (summary 0.25 point)
                           (article 1.0)))))
  (gnus-add-configuration
   '(summary
     (horizontal 1.0
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
  :ensure nil     ; use G R to subscribe to rss feeds
  :after gnus
  :hook (gnus-group-mode . gnus-topic-mode)
  :config
  (with-eval-after-load 'gnus-cite
    (defun gnus-clean-citation nil
      (save-excursion
        (let ((replacement "▎ "))
          (put-text-property 0 2 'face 'font-lock-comment-face replacement)
          (replace-regexp-in-region
           "\\(>[ ]?\\)" replacement (point-min) (point-max)))
        (replace-regexp-in-region "\\([^\s\n]\\)▎ " "\\1>" (point-min) (point-max))))
    
    (nconc gnus-treatment-function-alist
           '((t gnus-clean-citation))))

  (add-hook 'gnus-article-mode-hook
            (lambda nil
              (setq left-margin-width 4)))
  
  ;; mode-line unread indicator
  (defun my/gnus-unread-count ()
    (interactive)
    (let ((unread-count (cdar gnus-topic-unreads)))
      (if (or (not unread-count) (eq unread-count 0))
          ""
        (propertize (format " Unread:%s " unread-count)
                    'face 'which-func))))
  (with-eval-after-load 'gnus-topic
    (setq-local global-mode-string
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
                              "nnrss:Prot Codelog" "nnrss:HLTV.org"
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

(use-package vc
  :defer 1
  :ensure nil
  :bind (("C-x v f" . (lambda () (interactive)
                        (vc-git--pushpull "push" nil '("--force-with-lease"))))
         ("C-x v e" . vc-ediff))
  :config
  (setq vc-handled-backends '(Git)
        vc-display-status 'no-backend
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
                      (kill-current-buffer)
                      (tab-bar-close-tab))))
  ;; vc-annotate messes up the window-arrangement, give it a dedicated tab
  (add-to-list 'display-buffer-alist
               '("^\\*Annotate.*\\*$"
                 (display-buffer-reuse-mode-window display-buffer-in-tab))))

(use-package magit
  :commands (magit magit-file-dispatch magit-log-all magit-ediff-show-unstaged)
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (transient-bind-q-to-quit)
  (setq magit-display-buffer-function
        (lambda (buffer) ;; display diffs to the left, easier to use scroll-other-window
          (with-current-buffer buffer
            (if (eq major-mode 'magit-diff-mode)
                (display-buffer buffer
                                '(display-buffer-in-direction
                                  (direction . leftmost)))
              (magit-display-buffer-traditional buffer))))
        magit-refresh-status-buffer nil
        magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)

  (add-hook 'magit-status-mode-hook ;; src: doom (Might break magit on tramp)
            (lambda ()
              (when-let (path (executable-find magit-git-executable t))
                (setq-local magit-git-executable path))))

  (when (featurep 'meow)
    (add-hook 'git-commit-mode-hook #'meow-insert))
  
  (defun mu-magit-kill-buffers () ;src: manuel-uberti
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  :bind (:map dired-mode-map
              ("q" . kill-current-buffer)
              ("O" . dired-do-open)
              ("RET" . dired-find-alternate-file)
              ("TAB" . dired-maybe-insert-subdir)
              ("<backspace>" . (lambda nil (interactive)
                                 (dired-kill-subdir)
                                 (set-mark-command '(4))))
              ("<backtab>" . (lambda nil (interactive)
                               (dired-kill-subdir)
                               (set-mark-command '(4))))
              ("\\" . dired-up-directory)
              ("E" . wdired-change-to-wdired-mode)
              ("z" . find-grep-dired))
  :config
  (define-key dired-mode-map "?" dired-mode-map) ;src: amno1
  (add-hook 'dired-mode-hook ;; solaire
            (lambda nil
              (dolist (face '(default fringe))
                (face-remap-add-relative
                 face :background (if load-theme-light
                                      "#EEEEEE"
                                    "#30343D")))))
  (when (eq system-type 'darwin)
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil
          dired-listing-switches
          "-l --almost-all --human-readable --group-directories-first"))
  
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

(use-package dired-sidebar
  :bind ("C-x d" . dired-sidebar-toggle-sidebar)
  :bind (:map dired-sidebar-mode-map
              ("C-o" . other-window)
              ("l" . windmove-right)
              ("\\" . dired-sidebar-up-directory))
  :config
  (setq dired-sidebar-window-fixed nil
        dired-sidebar-mode-line-format
        '("%e" mode-line-buffer-identification)))

(use-package eat
  :unless is-android
  :commands eat-project
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(eat-project "Eat" ?t)))
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  :bind (("C-\\" . (lambda () (interactive)
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
               ("C-o" . other-window)
               ("M-j" . popper-toggle)
               ("M-w" . kill-ring-save)))
  :config
  (setq eat-message-handler-alist
        ;; once eat fish-intergration is merged
        '(("emsg" . (lambda (x)
                      (message x)))
          ("ff" . (lambda (file)
                    (find-file file)))
          ;; FIXME doesnt work over tramp with large name
          ("ediff" . (lambda (file1 file2)
                       (tab-bar-new-tab)
                       (ediff file1 file2)))))

  (setq explicit-shell-file-name "fish"
        eat-kill-buffer-on-exit t
        eat-term-name "xterm-256color"))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . visual-line-mode)
  :config
  (add-hook 'nov-mode-hook
            (lambda nil ;; thx teco
              (setq-local
               global-mode-string
               (append global-mode-string
                       (list '(:eval
                               (propertize
                                (format " %d/%d"
                                        (1+ nov-documents-index)
                                        (length nov-documents))
                                'face font-lock-comment-face)))))))
  (setq nov-text-width (min 80 (window-width))
        nov-header-line-format nil))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook ((pdf-view-mode . pdf-view-fit-page-to-window)
         (pdf-view-mode . pdf-view-themed-minor-mode))
  :config ;; pdf-tools-install
  (add-hook 'pdf-view-mode-hook
            (lambda nil
              (setq-local
               global-mode-string
               (append global-mode-string
                       (list '(:eval
                               (propertize
                                (format " %d/%d"
                                        (pdf-view-current-page)
                                        (pdf-cache-number-of-pages))
                                'face font-lock-comment-face))))))))

;;; Org

(use-package auctex
  :config ; ref: tectonic docs
  (setq TeX-engine-alist '((default
                            "Tectonic"
                            "tectonic -X compile -f plain %T"
                            "tectonic -X watch"
                            nil)))
  (setq LaTeX-command-style '(("" "%(latex)"))
        TeX-process-asynchronous t
        TeX-check-TeX nil
        TeX-engine 'default)
  (let ((tex-list (assoc "TeX" TeX-command-list))
        (latex-list (assoc "LaTeX" TeX-command-list)))
    (setf (cadr tex-list) "%(tex)"
          (cadr latex-list) "%l")))

(use-package org
  :ensure nil
  :bind (("C-x y" . yank-media)
         :map org-mode-map
         ("C-'" . avy-goto-char-timer))
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode))
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
    ;; Find the lowest headline level (FIXME)
    (let* (;; (headline-levels (or (org-element-map
           ;;                          (org-element-parse-buffer) 'headline
           ;;                        #'(lambda (item)
           ;;                            (org-element-property :level item)))
           ;;                      '()))
           ;; (max-level (seq-max (if headline-levels
           ;;                         headline-levels
           ;;                       '(0))))
           (line-indentation (+ 3 4))
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
                             (if (or (char-equal c ?\[)
                                     (char-equal c ?<))
                                 t
                               (,electric-pair-inhibit-predicate c))))))

  (setq org-directory (concat dropbox-dir "org")
        org-use-sub-superscripts '{}
        ;; org-export-with-sub-superscripts nil
        org-ellipsis "…"
        org-pretty-entities t
        org-startup-indented t
        org-startup-truncated nil
        org-adapt-indentation t
        org-special-ctrl-a/e t
        org-fold-catch-invisible-edits 'show-and-error
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        ;; tectonic
        org-highlight-latex-and-related '(latex)
        org-preview-latex-default-process 'tectonic
        org-preview-latex-process-alist
        '((tectonic :programs
                    ("tectonic" "convert")
                    :description "pdf > png"
                    :message "you need install the programs: tectonic and imagemagick."
                    :image-input-type "pdf"
                    :image-output-type "png"
                    :image-size-adjust (1.0 . 1.0)
                    :latex-compiler
                    ("tectonic -Z shell-escape-cwd=%o -Z continue-on-errors --outfmt pdf --outdir %o %f")
                    :image-converter
                    ("magick convert -density %D -trim -antialias %f -quality 300 %O")))
        org-latex-compiler "tectonic"
        org-latex-pdf-process
        '("tectonic -X compile -Z shell-escape -Z continue-on-errors --outdir=%o %f")))

(use-package org-agenda
  :ensure nil
  :bind ("C-c o a" . (lambda nil (interactive)
                       (org-agenda nil "n")))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Calendar\\*"
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (window-parameters (height . 0.33))))

  (setq org-agenda-files (list org-directory)
        org-agenda-inhibit-startup t
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-start-with-log-mode t
        org-agenda-show-all-dates nil
        org-log-done t
        org-log-into-drawer t
        org-agenda-include-deadlines t)

  (defun elegant-agenda--title nil ;; src: elegant-agenda-mode
    (when-let ((title (when (and org-agenda-redo-command
                                 (stringp (cadr org-agenda-redo-command)))
                        (format "─  %s "
                                (mapconcat
                                 #'identity
                                 (split-string-and-unquote
                                  (cadr org-agenda-redo-command) "")
                                 ""))))
               (width (window-width)))
      (face-remap-set-base 'header-line :height 1.4)
      (setq-local header-line-format
                  (format "%s %s" title (make-string (- width (length title)) ?─ t)))))
  
  (add-hook 'org-agenda-finalize-hook #'elegant-agenda--title)
  
  (setq org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-todo-keyword-format "%-1s"
        org-agenda-use-time-grid t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-scheduled-leaders '("" "")
        org-agenda-deadline-leaders '("" "")
        org-agenda-todo-keyword-format ""
        org-agenda-block-separator (string-to-char " ")
        org-agenda-current-time-string "← now ─────────"
        org-agenda-time-grid
        '((daily today require-timed remove-matched)
          (800 1200 1600 2000)
          "       " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-prefix-format
        '((agenda . " %i %-12b%t%s")
          (todo . " %i %?-12b"))))

(use-package org-habit
  :after org-agenda
  :ensure nil
  :config
  (setq org-habit-show-habits-only-for-today t
        org-habit-show-done-always-green t
        org-habit-show-all-today t
        org-habit-missed-glyph ?◌;; 9676
        org-habit-completed-glyph ?● ;; 9679
        org-habit-today-glyph ?○ ;; 9675
        org-habit-following-days 1
        org-habit-preceding-days 21)
  
  (defun add-missed-day-glyph (graph)
    (dotimes (i (length graph))
      (when (char-equal ?\s (aref graph i))
        (let* ((face (get-char-property i 'face graph))
               (rep-str (propertize (char-to-string org-habit-missed-glyph)
                                    'face face)))
          (aset graph i (string-to-char rep-str)))))
    graph)
  
  (advice-add 'org-habit-build-graph :filter-return #'add-missed-day-glyph))

(use-package org-capture
  :ensure nil
  :bind ("C-c o c" . org-capture)
  ;; :hook (org-capture-mode . meow-insert)
  :config
  (add-hook 'org-capture-mode-hook
            (lambda nil
              (setq-local header-line-format nil)))
  (setq org-capture-file
        (concat org-directory "/inbox.org")
        org-capture-templates
        '(("t" "TODO" entry
           (file+headline org-capture-file "Tasks")
           "* TODO %?\n%<%d %b '%g %R>%i %a" :prepend t)
          ("n" "Note" entry
           (file+headline org-capture-file "Notes")
           "* %?\n%i %a" :prepend t)
          ("h" "Habit" entry
           (file+headline org-capture-file "Habit")
           "* TODO %?\n:PROPERTIES:\n:STYLE: habit\n:END:"
           :prepend t))))

(use-package ox-awesomecv
  :ensure nil
  :disabled t ; installation error
  :vc (:url "https://gitlab.com/Titan-C/org-cv")
  :after org)

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks 'just-brackets)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star nil
        org-modern-hide-stars nil
        org-modern-todo nil
        org-modern-list
        '((?+ . "◦") (?- . "◦"))))

(use-package denote
  :bind (("C-c n n" . denote)
         ("C-c n r" . denote-rename-file-using-front-matter)
         ("C-c n j" . denote-journal-extras-new-entry)
         ("C-c n l" . denote-link-after-creating)
         ("C-c n o" . denote-open-or-create))
  :hook (dired-mode . denote-dired-mode-in-directories)
  :hook (dired-sidebar . denote-dired-mode-in-directories)
  :hook (org-mode . denote-rename-buffer-mode)
  :config
  (define-advice org-mark-ring-goto (:before (&rest _args) close-window)
    "Close current window before going back."
    (when (window-parent)
      (delete-window)))

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("d" "Denote" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  
  (denote-rename-buffer-mode 1)
  
  (setq denote-directory (concat dropbox-dir "denote/")
        denote-dired-directories-include-subdirectories t
        denote-dired-directories
        `(,denote-directory)))

(use-package denote-refs
  :ensure nil
  :vc (:url "https://codeberg.org/akib/emacs-denote-refs")
  :bind ("C-c n b" . denote-refs-mode))

(use-package deft
  :bind (("<f8>" . deft)
         ([remap deft-complete] . #'deft-use-denote)
         :map deft-mode-map
         ("TAB" . deft-open-file-other-window)
         ([tab] . deft-open-file-other-window))
  :config
  (defun deft-use-denote nil
    (interactive)
    (denote (deft-whole-filter-regexp)))
  
  (setq deft-use-filename-as-title nil
        deft-recursive t
        deft-auto-save-interval -1.0
        deft-directory (concat dropbox-dir "denote/")
        deft-strip-summary-regexp
        (format "%s\\|%s"
                deft-strip-summary-regexp
                "^#\\+.*")
        deft-use-filter-string-for-filename t))

;;; Meow

(use-package meow
  :hook (after-init . (lambda ()
                        (require 'meow)
                        (meow-global-mode)))
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
  
  (autoload 'viper-ex "viper")
  
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
  
  (dolist (imode '(reb-mode eat-mode shell-mode eshell-mode
                            deft-mode magit-log-edit-mode log-edit-mode))
    (push `(,imode . insert) meow-mode-state-list))

  (add-hook 'ediff-mode-hook #'meow-motion-mode)
  
  (meow-motion-overwrite-define-key
   '("Q" . kill-current-buffer)
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
   ;; '("C-;" . meow-reverse)
   '(";" . meow-cancel-selection)
   ;; '("'" . meow-reverse)
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
   '("F" . find-file)
   '("ga" . (lambda nil (interactive)
              (org-agenda nil "n")))
   '("gb" . xref-go-back)
   '("gc" . org-capture)
   '("gC" . meow-comment)
   '("gd" . xref-find-definitions)
   '("gf" . ffap)
   '("gg" . avy-goto-char-timer)
   '("gh" . diff-hl-show-hunk)
   '("gk" . beginning-of-buffer)
   '("gj" . end-of-buffer)
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
               (if (and (display-graphic-p)
                        (package-installed-p 'eldoc-box))
                   (eldoc-box-help-at-point)
                 (eldoc-doc-buffer t)))))
   '("l" . meow-right)
   '("L" . up-list)
   ;; '("L" . meow-swap-grab)
   '("mm" . match-pair)
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
   '("Q" . kill-current-buffer)
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
   '(":" . viper-ex)
   '("\\" . dired-jump)
   '("*" . isearch-forward-thing-at-point)
   '("&" . align-regexp)
   '("%" . match-pair)
   '("/" . meow-visit)
   '("<" . indent-rigidly-left-to-tab-stop)
   '(">" . indent-rigidly-right-to-tab-stop)
   '("\"" . repeat)
   '("<escape>" . ignore)))

;;; Programming setup

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist ;; treesit-install-language-grammar
        '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src"))
        treesit-font-lock-level 4))

(setq major-mode-remap-alist '((c++-mode . c++-ts-mode)
                               (c-mode . c-ts-mode)))
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

(with-eval-after-load 'cc-mode
  (add-hook 'c++-mode-hook
            (lambda nil
              (local-set-key (kbd "C-c C-c") #'compile)))
  (defun c-indent-then-complete ()
    (interactive)
    (if (= 0 (c-indent-line-or-region))
        (completion-at-point)))
  (dolist (map (list c-mode-map c++-mode-map c-mode-base-map))
    (define-key map (kbd "<tab>") #'c-indent-then-complete)
    (define-key map "C-c C-r" #'recompile)))

(nconc auto-mode-alist
       '(("\\.rs\\'" . rust-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.lua\\'" . lua-ts-mode)
         ("\\.bin\\'" . hexl-mode)
         ("\\.info\\'" . Info-mode)))

(use-package eglot
  :bind ("M-+" . eglot-code-actions)
  ;; :bind (:map meow-normal-state-keymap
  ;;             ("gr" . eglot-rename)
  ;;             ("gF" . eglot-format))
  :hook (((rust-ts-mode go-ts-mode) . eglot-ensure)
         (eglot-managed-mode . (lambda ()
                                 (setq eldoc-documentation-strategy
                                       'eldoc-documentation-compose-eagerly))))
  ;; :init (setq eglot-stay-out-of '(flymake))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-config 0
        eglot-autoshutdown t
        eglot-inlay-hints-mode nil)
  (add-hook 'rust-ts-mode-hook (lambda () (setq-local tab-width 2)))
  (add-hook 'go-ts-mode-hook (lambda () (setq-local tab-width 4)))

  (defun my-eglot-organize-imports ()
    (interactive)
    (ignore-errors
      (eglot-code-actions nil nil "source.organizeImports" t)))
  (defun my-eglot-setup ()
    (interactive)
    (when (not (eq major-mode 'sql-mode))
      (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
      (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  
  (add-hook 'eglot-managed-mode-hook
            (lambda nil
              (setq-local flymake-cc-command nil)
              (my-eglot-setup)))
  (setq go-ts-mode-indent-offset 4)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (setq-default eglot-workspace-configuration
                '((:gopls . ((staticcheck . t)
                             (matcher . "CaseSensitive")))))
  (defvar ra-setup
    `(:rust-analyzer
      (:procMacro (:attributes (:enable t)
                               :enable t)
                  :rootPath ,(file-local-name (file-truename buffer-file-name))
                  :detachedFiles ,(vector (file-local-name
                                           (file-truename buffer-file-name)))
                  :diagnostics (:disabled ["unresolved-proc-macro"
                                           "unresolved-macro-call"])))))
;; (add-to-list 'eglot-server-programs ;; standalone r-a support (from rustic)
;;              `(rust-ts-mode . ("rust-analyzer"
;;                                :initializationOptions
;;                                ,ra-setup)))

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode))

(use-package janet-ts-mode
  :vc (:url "https://github.com/sogaiu/janet-ts-mode")
  :mode ("\\.janet\\'" . janet-ts-mode)
  :config
  (remove-hook 'janet-ts-mode-hook 'treesit-inspect-mode))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(use-package foxy
  :ensure nil
  :load-path "~/.emacs.d/lisp"
  :bind (("C-c l" . foxy-listen-start)
         ("C-c ]" . foxy-cycle-files)
         ("C-c [" . (lambda nil (interactive)
                      (foxy-cycle-files -1)))
         ("C-c b" . foxy-run-all-tests))
  :init
  (defvar foxy-compile-commands
    '((rust-ts-mode-hook . "rustc -o a.out ")
      (c++-mode-hook . "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion -O2 -I/Users/admin/problems/include ")
      (c++-ts-mode-hook . "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion -O2 -I/Users/admin/problems/include ")
      ;; (go-ts-mode-hook . "go build -o a.out ")
      ))
  (dolist (pair foxy-compile-commands)
    (let ((mode (car pair))
          (command (cdr pair)))
      (add-hook mode
                (lambda ()
                  (setq-local compile-command (concat command
                                                      buffer-file-name
                                                      " && ./a.out"))
                  (setq-local foxy-compile-command command))))))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex avy cape cdlatex corfu-terminal deadgrep deft denote-refs devil diff-hl dired-sidebar
            easy-kill eat eldoc-box embark exec-path-from-shell highlight-indent-guides info-colors
            janet-ts-mode kkp macrursors magit markdown-mode meow move-text nerd-icons
            nerd-icons-corfu nerd-icons-dired nerd-icons-ibuffer nix-mode nov orderless org-appear
            org-modern pdf-tools popper puni shr-tag-pre-highlight transpose-frame undo-fu-session
            vertico writeroom-mode xclip zig-mode))
 '(package-vc-selected-packages
   '((dired-preview :url "https://github.com/protesilaos/dired-preview")
     (ox-awesomecv :url "https://gitlab.com/Titan-C/org-cv")
     (janet-ts-mode :url "https://github.com/sogaiu/janet-ts-mode")
     (info-colors :url "https://github.com/ubolonton/info-colors")
     (macrursors :url "https://github.com/karthink/macrursors"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(widget-button ((t (:foreground "white" :background "grey50")))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
