;;; init.el ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Ah shit, here we go again.
;;; Code:

;;; Bootstrap
;; (load "~/.emacs.d/lisp/benchmarking" nil :no-message)

(define-key global-map (kbd "C-z") (make-sparse-keymap))
(defconst dropbox-dir
  (if is-android
      "/sdcard/Dropbox/"
    "~/Dropbox/"))
(when is-android
  (setq touch-screen-precision-scroll t)
  (global-set-key (kbd "C-\\") (lambda nil (interactive)
                                 (term "fish"))))

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
         ("C-o" . other-window)
         ("C-," . my/scroll-other-window)
         ("C-." . my/scroll-other-window-down)
         ("C-<" . my/xref-other-go-back)
         ("C->" . my/xref-other-go-fwd)
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
         ("C-z C-s" . suspend-frame)
         ("C-s-f" . toggle-frame-fullscreen) ;; macos
         ("<f7>" . (lambda () (interactive)
                     (hl-line-mode 'toggle)
                     (highlight-indent-guides-mode 'toggle)))
         ("<f6>" . zed-toggle-theme))
  :config
  (defun my/scroll-other-window nil
    (interactive)
    (let ((mode (with-current-buffer (window-buffer (other-window-for-scrolling))
                  major-mode)))
      (with-selected-window (other-window-for-scrolling)
        (cond
         ((eq mode 'pdf-view-mode)
          (pdf-view-scroll-up-or-next-page))
         ((eq mode 'Info-mode)
          (Info-scroll-up))
         (t (scroll-up-command 5))))))
  
  (defun my/scroll-other-window-down nil
    (interactive)
    (let ((mode (with-current-buffer (window-buffer (other-window-for-scrolling))
                  major-mode)))
      (with-selected-window (other-window-for-scrolling)
        (cond
         ((eq mode 'pdf-view-mode)
          (pdf-view-scroll-down-or-previous-page))
         ((eq mode 'Info-mode)
          (Info-scroll-down))
         (t (scroll-down-command 5))))))

  (defun my/xref-other-go-back nil
    (interactive)
    (with-selected-window (other-window-for-scrolling)
      (xref-go-back)))
  
  (defun my/xref-other-go-fwd nil
    (interactive)
    (with-selected-window (other-window-for-scrolling)
      (call-interactively 'xref-find-definitions)))
  
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
                debug-on-error t ; issues w/ completion in :bind
                warning-minimum-level :error
                delete-by-moving-to-trash t ; never change this :'(
                display-line-numbers-width 5
                display-line-numbers-grow-only t
                delete-pair-blink-delay 0
                mouse-wheel-tilt-scroll t
                mouse-wheel-flip-direction t
                mouse-wheel-scroll-amount-horizontal 4)

  (setq process-adaptive-read-buffering nil
        kill-do-not-save-duplicates t
        kill-whole-line t
        auto-mode-case-fold nil
        undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960
        enable-local-variables :safe
        scroll-margin 0
        scroll-conservatively 101
        scroll-preserve-screen-position t
        ;; pixel-scroll-precision-large-scroll-height 35.0
        vc-follow-symlinks t
        split-height-threshold nil
        split-width-threshold 120
        save-abbrevs nil
        make-backup-files nil
        backup-directory-alist `(("." . "~/.emacs.d/backup/"))
        create-lockfiles nil
        uniquify-buffer-name-style 'forward
        auto-revert-verbose nil
        sentence-end-double-space nil
        Info-use-header-line nil
        outline-minor-mode-cycle nil ;t
        tabify-regexp "^\t* [ \t]+"
        grep-command "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)"
        grep-command-position 27
        electric-pair-skip-self t
        electric-pair-preserve-balance 'electric-pair-inhibit-predicate
        electric-pair-delete-adjacent-pairs t
        ;; electric-pair-open-newline-between-pairs nil
        electric-pair-skip-whitespace nil
        set-mark-command-repeat-pop t)

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
  ;; load theme based on terminal
  (let ((color (shell-command-to-string
                "kitty @ --to=\"unix:/tmp/$(ls /tmp | grep mykitty)\" get-colors | grep ^background | awk '{printf $2}'")))
    (when (string= color "#f7f7f7")
      ;; load-theme-light is a zed-theme var
      (setq load-theme-light t)
      (when (eq system-type 'darwin)
        (modify-all-frames-parameters '((ns-appearance . light))))))
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
        (when (and (derived-mode-p 'prog-mode)
                   (not (eq major-mode 'yaml-ts-mode)))
          (let ((transient-mark-mode nil))
            (indent-region (region-beginning) (region-end) nil))))))

  (define-advice rectangle--default-line-number-format
      (:filter-return (str) rect-number-period)
    "Add a period after the inserted number"
    (replace-regexp-in-string " $" ". " str))
  
  (define-advice term-handle-exit (:after (&rest _args) term-kill-on-exit)
    "Kill the buffer after the term exits."
    (kill-buffer (current-buffer))))

(use-package emacs
  :no-require
  :ensure nil
  :bind (("C-x w w" . my-min-max-window)
         ("M-#" . mac-dict)
         ("<f5>" . my-switch-to-alternate-file))
  :bind (:map special-mode-map
              ("Q" . (lambda nil (interactive)
                       (quit-window t))))
  :defer 2
  :hook
  (after-init . (lambda nil
                  ;; random functions
                  (defun mac-dict (beg end)
                    "Look up a word in macOS Dictionary.app"
                    (interactive "r")
                    (thread-last (buffer-substring-no-properties beg end)
                                 url-hexify-string
                                 (concat "dict:///")
                                 browse-url))
                  
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
                             (t (error "No alternate file for current mode"))))
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
                  ;; install-info --dir-file=./dir --info-file=
                  (push "~/.emacs.d/info" Info-directory-list)
                  (put 'help-fns-edit-variable 'disabled nil) ;; FIXME
                  (put 'narrow-to-region 'disabled nil)
                  (add-hook 'debugger-mode-hook #'visual-line-mode)
                  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
                  (add-hook 'prog-mode-hook (electric-pair-mode t))
                  (add-hook 'minibuffer-setup-hook (lambda () (electric-pair-mode 0)))
                  (add-hook 'minibuffer-exit-hook (lambda () (electric-pair-mode 1)))
                  (add-hook 'prog-mode-hook (show-paren-mode t))
                  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
                  (add-hook 'yaml-ts-mode-hook #'display-line-numbers-mode)
                  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
                  ;; (add-hook 'tab-bar-mode-hook #'tab-bar-history-mode) ;; slows init
                  (add-to-list 'write-file-functions
                               '(lambda ()
                                  (when (eq major-mode 'emacs-lisp-mode)
                                    (check-parens))
                                  nil))
                  (with-eval-after-load 'whitespace
                    (add-to-list 'whitespace-display-mappings
                                 '(newline-mark ?\n [8626 ?\n]) t))
                  (unless (display-graphic-p)
                    (set-display-table-slot standard-display-table
                                            'vertical-border
                                            (make-glyph-code ?│))
                    (set-display-table-slot standard-display-table
                                            'truncation
                                            (make-glyph-code ? ))
                    (set-display-table-slot standard-display-table
                                            'wrap
                                            (make-glyph-code ? ))
                    (xterm-mouse-mode))
                  ;; enable some useful modes
                  (tab-bar-mode +1)
                  ;; (pixel-scroll-precision-mode 1) ;using ultra-scroll-mode instead
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
                  (setq blink-cursor-interval 0.8)
                  (tooltip-mode -1))))

(load "~/.emacs.d/lisp/dev-conf" nil :no-message)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;; Completion

(use-package compile
  :ensure nil
  :bind (:map prog-mode-map
              ("C-c C-c" . compile)
              ("C-c C-r" . recompile))
  :hook ((compilation-filter . ansi-color-compilation-filter)
         (compilation-filter . ansi-osc-compilation-filter))
  :hook (compilation-mode .
                          (lambda nil
                            "Enable comint mode to allow for providing program input."
                            (comint-mode)
                            (setq-local buffer-read-only nil)))
  :hook (compilation-finish-functions
         .
         (lambda (buffer status)
           "Reset comint mode so that we get the compilation-mode goodness."
           (setq-local buffer-read-only t)
           (compilation-minor-mode)
           (when (featurep 'meow)
             (meow-motion-mode t))))
  :config
  (with-eval-after-load 'sh-script
    (define-key sh-mode-map (kbd "C-c C-c") #'executable-interpret))
  ;; completing-read for compile
  (defun compilation-read-command-with-autocomplete (command)
    "Use `completing-read` to add autocomplete powers to compilation read"
    (completing-read "Compile command: " compile-history
                     nil nil command
                     (if (equal (car compile-history) command)
                         '(compile-history . 1)
                       'compile-history)))
  (advice-add
   #'compilation-read-command
   :override #'compilation-read-command-with-autocomplete)
  
  :init ;TODO
  (setq shell-command-prompt-show-cwd t
        shell-kill-buffer-on-exit t
        shell-file-name (car (process-lines "which" "fish"))
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (with-eval-after-load 'executable ; src: eshel
    (define-advice executable-interpret (:before (&rest _) ensure-executable)
      (unless (file-exists-p buffer-file-name)
        (basic-save-buffer))
      (executable-make-buffer-file-executable-if-script-p)))
  ;; compile commands
  (defvar compile-commands
    '(;; (go-ts-mode-hook . "go build -o a.out ")
      ;; (rust-ts-mode-hook . "rustc -o a.out ")
      (c++-mode-hook
       . (concat "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion \
 -O2 -I/Users/admin/problems/include " buffer-file-name " && ./a.out"))
      (c++-ts-mode-hook
       . (concat "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion \
 -O2 -I/Users/admin/problems/include " buffer-file-name " && ./a.out"))
      (go-ts-mode-hook . (concat "go run " buffer-file-name))
      (rust-ts-mode-hook . "cargo run ")
      (janet-ts-mode-hook . (concat "janet " buffer-file-name))
      (ruby-mode-hook . (concat "ruby " buffer-file-name))
      (sh-mode-hook . (concat "." buffer-file-name))))

  (dolist (pair compile-commands)
    (let ((mode (car pair))
          (command (cdr pair)))
      (add-hook mode
                (lambda nil
                  (setq-local compile-command command)
                  (setq-local foxy-compile-command command))))))

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

(use-package icomplete
  :ensure nil
  :init (fido-vertical-mode)
  :bind (:map icomplete-fido-mode-map
              ("C-<return>" . icomplete-fido-exit)
              ("TAB" . icomplete-forward-completions)
              ("<backtab>" . icomplete-backward-completions)
              ("<escape>" . minibuffer-keyboard-quit))
  :hook (icomplete-minibuffer-setup . (lambda nil
                                        (setq-local truncate-lines t
                                                    line-spacing nil)))
  :config
  (defun crm-indicator (args)
    (cons (format "%s %s"
                  (propertize (format "[CRM%s]"
                                      (replace-regexp-in-string
                                       "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                                       crm-separator))
                              'face 'success)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  
  (setq tab-always-indent 'complete
        tab-first-completion 'word-or-paren
        completions-group t
        completions-sort 'historical
        completions-detailed t))

(use-package cape
  :hook (eglot-managed-mode
         . (lambda nil
             (setq-local completion-at-point-functions
                         (list (cape-capf-super
                                #'eglot-completion-at-point
                                #'cape-abbrev
                                #'cape-file
                                #'cape-dabbrev)))))
  :hook (emacs-lisp-mode
         . (lambda nil
             (setq-local completion-at-point-functions
                         (list (cape-capf-super
                                #'elisp-completion-at-point
                                #'cape-dabbrev)
                               #'cape-file))))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-abbrev))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-t" . consult-line)
         ("M-s i" . consult-imenu)
         ([remap project-find-regexp] . consult-ripgrep)
         ;; ([remap project-find-file] . consult-find)
         ([remap flymake-show-buffer-diagnostics] . consult-flymake))
  :config
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" ?g))
    (map-delete project-switch-commands 'project-find-regexp))

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-eglot
  :hook (eglot-managed-mode
         . (lambda nil
             (local-set-key (kbd "C-M-.") #'consult-eglot-symbols))))

(use-package completion-preview
  :disabled t
  :if (string> emacs-version "29.4")
  :ensure nil
  :hook ((prog-mode comint-mode eshell-mode)
         . completion-preview-mode)
  :bind (("M-i" . completion-at-point)
         :map completion-preview-active-mode-map
         ([tab] . completion-preview-complete)
         ("TAB" . completion-preview-complete)
         ("M-i" . completion-at-point)
         ("C-<return>" . completion-preview-insert))
  :config
  (push 'org-self-insert-command completion-preview-commands))

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
  (when (featurep 'meow) ;; FIXME
    (add-hook 'macrursors-mode-hook #'meow-insert))
  :bind-keymap ("C-;" . macrursors-mark-map)
  :bind (("M-d" . (lambda nil
                    "Macrurors mark word if no selection, otherwise select next."
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
         ("M-\\" . (lambda nil (interactive)
                     "Macrursors toggle secondary selection."
                     (if (secondary-selection-exist-p)
                         (macrursors-select-clear)
                       (macrursors-select))))
         ("M-<down>" . macrursors-mark-next-line)
         ("M-<up>" . macrursors-mark-previous-line)
         ("M-n" . macrursors-mark-next-instance-of)
         ("M-p" . macrursors-mark-previous-instance-of)
         ("M-u" . (lambda nil
                    "Macrursors undo last cursor."
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
    (add-hook 'macrursors-post-finish-hook mode)))

(use-package puni
  :if (display-graphic-p) ; FIXME puni is messing terminal
  ;; TODO: https://karthinks.com/software/a-consistent-structural-editing-interface/
  ;; https://countvajhula.com/2021/09/25/the-animated-guide-to-symex/
  :hook ((emacs-lisp-mode go-ts-mode rust-ts-mode
                          tex-mode eval-expression-minibuffer-setup)
         . puni-mode)
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
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer avy-parens-jump)
  :config
  (defun avy-parens-jump () ; src: elken
    "Jump to parens inside a defun."
    (interactive)
    (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'defun)))
      (avy-jump "(+" :beg beg :end end)))
  
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

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :hook (after-init . ultra-scroll-mode))

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
  :hook (prog-mode . (lambda nil
                       (unless (eq major-mode 'templ-ts-mode)
                         (hs-minor-mode))))
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
                                (< occur-engine-prev-linum (array-current-line)))
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
               (setq occur-engine-prev-linum (array-current-line)))
           (concat line-prefix line "\n")))
       lines)))
  
  (advice-add 'occur-context-lines :around #'clean-occur-context-line))

(use-package flymake
  :ensure nil
  :bind ("M-s x" . flymake-show-buffer-diagnostics)
  :hook (emacs-lisp-mode . flymake-mode)
  :hook (cc-mode . check-cc-tramp)
  :config
  (defun check-cc-tramp nil
    (if (file-remote-p default-directory)
        (setq-local flymake-cc-command nil)
      (setq flymake-cc-command
            '("gcc" "-fsyntax-only" "-Wall" "-Wextra"
              "-I/usr/local/Cellar/gcc/13.2.0/include/c++/13/x86_64-apple-darwin23"
              "-I/usr/local/Cellar/gcc/13.2.0/include/c++/13/" "-x" "c++" "-"))))
  (setq flymake-no-changes-timeout 2
        flymake-suppress-zero-counters t
        flymake-fringe-indicator-position nil
        flymake-margin-indicator-position nil
        flymake-show-diagnostics-at-end-of-line 'short)

  (add-hook 'yaml-ts-mode-hook
            (lambda nil
              (setq-local flymake-show-diagnostics-at-end-of-line nil)))

  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-idle-delay 0.3
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-display-truncation-message nil))

;; (use-package help-fns ;; FIXME
;;   :ensure nil
;;   :config
;;   (add-hook 'help-fns-describe-function-functions
;;             #'shortdoc-help-fns-examples-function)
;;   :custom
;;   (help-enable-variable-value-editing t))

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
  ;; :hook (minibuffer-mode . tramp-cleanup-all-connections)
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
  :ensure nil
  :bind (("M-<right>" . windmove-swap-states-right)
         ("M-<left>" . windmove-swap-states-left)
         ("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<down>" . windmove-down)
         ("S-<up>" . windmove-up))
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings 'shift))

(use-package popper
  :bind (("M-j"   . popper-toggle)
         ("C-`"   . popper-cycle)
         ("M-`" . popper-toggle-type)
         :repeat-map popper-repeat-map
         ("`"     . popper-cycle))
  :hook ((after-init . popper-mode)
         (popper-mode . popper-tab-line-mode))
  :init
  (setq display-buffer-alist
        `(;; no window
          ("\\`\\*Async Shell Command\\*\\'"
           (display-buffer-no-window))
          ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\|diff-hl-show-.*\\)\\*\\'"
           (display-buffer-no-window)
           (allow-no-window . t))))
  (setq popper-reference-buffers
        '("\\*Messages\\*" "\\*(?!EGLOT).*Output\\*$" "\\*xref\\*"
          "\\*Async Shell Command\\*" "\\*shell.\*"
          "\\*pabbrev suggestions\\*" ;"magit:.\*"
          ".*-eat\\*" "\\*eldoc\\*" "vc-git :.\*"
          "\\*vc-change-log\\*" "\\*Deletions\\*"
          "\\*Flymake .\*" "\\*CDLaTex Help\\*"
          "\\*Process List\\*" "\\*Org Select\\*"
          "^CAPTURE.*" "\\*Org Agenda\\*" "\\*kubel stderr\\*"
          "\\*Warnings\\*" "\\*Backtrace\\*" "\\*edit .\*"
          help-mode compilation-mode comint-mode
          "^\\*eshell.*\\*$" eshell-mode grep-mode
          "^\\*term.*\\*$" term-mode)
        popper-mode-line nil
        popper-window-height 0.33))

(use-package popper-echo
  :after popper
  :ensure nil
  :config
  ;; (defun popper--delete-popup (win) ;; FIXME
  ;;   (message "Attempt to delete popper window"))
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
  :hook (after-init . xclip-mode))

(use-package kkp ;; FIXME tmux
  :unless (display-graphic-p)
  :hook (after-init . global-kkp-mode))

(use-package undo-fu-session
  :hook ((prog-mode conf-mode fundamental-mode text-mode tex-mode) . undo-fu-session-mode)
  :config
  (setq undo-fu-session-compression nil))

;;; Visual Niceties

(use-package writeroom-mode
  :bind ("<f9>" . writeroom-mode)
  :hook ((nov-mode markdown-mode Info-mode Man-mode eww-mode) . writeroom-mode)
  :hook (org-mode . org-writer-mode)
  :config
  (defun org-writer-mode nil
    ;; visual-line-mode & variable-pitch-mode
    (add-to-list ;; TODO: Add line-height support as well
     'window-size-change-functions
     (lambda (window)
       (when (eq major-mode 'org-mode)
         (let* ((window (selected-window))
                (lower-limit (if (eq system-type 'darwin)
                                 170
                               130))
                (upper-limit (+ lower-limit 50))
                (scaling-factor (if (eq system-type 'darwin)
                                    1.6
                                  1.4))
                (width (window-total-width window))
                (width-ten (* 10 (round (/ (* scaling-factor width) 10))))
                (adaptive-height (min (max lower-limit width-ten) upper-limit))
                (wheight (window-total-height window))
                (writeroom-width (min 100 (max 90 (- (window-width) 10)))))
           (writeroom-mode)
           (face-remap-add-relative 'default :height
                                    (if (> wheight 35)
                                        adaptive-height
                                      lower-limit))
           (setq-local line-spacing 0.6)))))
    (setq-local cursor-type 'bar
                scroll-preserve-screen-position t
                scroll-conservatively 101
                maximum-scroll-margin 0.5
                scroll-margin 28))
  
  (setq writeroom-width 0.85;(min 100 (max 90 (- (window-width) 10)))
        writeroom-mode-line t
        writeroom-bottom-divider-width 0
        writeroom-restore-window-config t
        writeroom-global-effects nil
        writeroom-maximize-window nil))

(use-package nerd-icons
  :commands nerd-icons-octicon nerd-icons-codicon
  :config
  ;; causing issues in emacs -nw
  ;; (when (not (find-font (font-spec :name nerd-icons-font-family)))
  ;;   (nerd-icons-install-fonts t))
  (defun my/add-tab-bar-arrow-glyphs (theme)
    (when (custom-theme-enabled-p 'zed)
      (setq zed-tab-back-button
            (nerd-icons-octicon "nf-oct-arrow_left" :v-adjust 0.3)
            zed-tab-forward-button
            (nerd-icons-octicon "nf-oct-arrow_right" :v-adjust 0.3))))
  (my/add-tab-bar-arrow-glyphs 'theme)
  (add-hook 'enable-theme-functions 'my/add-tab-bar-arrow-glyphs)
  (push '(eat-mode nerd-icons-devicon "nf-dev-terminal") nerd-icons-mode-icon-alist))

(use-package nerd-icons-corfu
  :init
  (with-eval-after-load 'corfu
    (push 'nerd-icons-corfu-formatter corfu-margin-formatters)))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package shr-tag-pre-highlight
  ;; FIXME complains if mode isnt available
  :init
  (setq eww-auto-rename-buffer 'title
        eww-header-line-format nil)
  :config
  ;; Add advice to indent code blocks (claude)
  (advice-add 'shr-tag-pre-highlight :around
              (lambda (orig-fun &rest args)
                "Advice to indent pre blocks by 2 spaces."
                (let ((start (point)))
                  (apply orig-fun args)
                  (save-excursion
                    (goto-char start)
                    (while (< (point) (point-max))
                      (unless (looking-at-p "^$") ; Skip empty lines
                        (insert "  "))
                      (forward-line 1))))))
  
  ;; use highlight in nov mode as well!
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
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package highlight-indent-guides
  :hook (yaml-ts-mode . highlight-indent-guides-mode)
  :hook (yaml-ts-mode . hl-line-mode)
  :config
  (set-face-attribute 'highlight-indent-guides-character-face nil
                      :inherit 'vertical-border)
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-auto-enabled nil
        highlight-indent-guides-responsive nil))

;;; Apps

(use-package desktop
  :ensure nil
  :if (display-graphic-p)
  :hook (window-setup . desktop-save-mode)
  ;; :hook (window-setup . desktop-read)
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

(use-package vc
  ;; :defer nil
  :ensure nil
  ;; :hook (log-edit . log-edit-show-diff) ;; FIXME
  :hook (log-edit-done . (lambda nil
                           (when-let ((buf (get-buffer "*vc-diff*")))
                             (kill-buffer buf))))
  :bind (("C-x v f" . (lambda () (interactive)
                        (vc-git--pushpull "push" nil '("--force-with-lease"))))
         ("C-x v e" . vc-ediff))
  :config
  (setq vc-handled-backends '(Git)
        vc-display-status 'no-backend
        vc-find-revision-no-save t
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
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m)))
  :bind (:map magit-status-mode-map
              ("x" . magit-discard))
  :config
  (transient-bind-q-to-quit)
  (setq magit-display-buffer-function
        (lambda (buffer) ;; display diffs to the left, easier to use scroll-other-window
          (with-current-buffer buffer
            (if (eq major-mode 'magit-status-mode)
                (progn
                  (tab-bar-new-tab)
                  (display-buffer buffer '(display-buffer-same-window)))
              (magit-display-buffer-traditional buffer))))
        magit-refresh-status-buffer nil
        magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)

  (add-hook 'magit-revision-mode-hook #'toggle-truncate-lines)
  (add-hook 'magit-status-mode-hook ;; src: doom (Might break magit on tramp)
            (lambda ()
              (when-let (path (executable-find magit-git-executable t))
                (setq-local magit-git-executable path))))

  (when (featurep 'meow)
    (add-hook 'git-commit-mode-hook #'meow-insert))
  
  (defun magit-forge-kill-buffers () ;src: manuel-uberti
    "Restore window configuration and kill all Magit & Forge buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers))
          (forge-buffers
           (seq-filter (lambda (buf)
                         (with-current-buffer buf
                           (member major-mode '(forge-pullreq-mode
                                                forge-issue-mode
                                                forge-topics-mode
                                                forge-post-mode
                                                forge-motifications-mode
                                                forge-repository-list-mode))))
                       (buffer-list))))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)
      (mapc #'kill-buffer forge-buffers))
    (tab-bar-close-tab))

  (bind-key "q" #'magit-forge-kill-buffers magit-status-mode-map))

(use-package forge
  ;; run `forge-pull' before!
  ;; remember to generate auth-token (info "(forge) Setup for Githubcom")
  ;; src: karthik
  ;; `magit-clone' or 'git clone' a repo. The remote is named origin.
  ;; Add it to forge's database, pulling individual or all topics (N a)
  ;; Contribute to or create issues with forge. (N f t, then C-c C-r or C-c C-e to reply.)
  ;; Fork the repo to my account with `forge-fork'. My 'fork to' should be
  ;; username to fork to and remote is named copy, or fork.
  ;; Work on the local repo as usual, push to copy/fork.
  ;; Create a PR using forge. (source is fork, target is origin/master)
  ;; To mark a PR as draft: C-c C-e d when creating it
  ;; In case commits dont show for PR: run `forge-add-pullreq-refspec' (forge/issues/320)
  :after magit)

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
  (when (eq system-type 'darwin)
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil
          dired-listing-switches
          "-l --almost-all --human-readable --group-directories-first"))
  
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(project-dired "Dired" ?D)))

  (put 'dired-find-alternate-file 'disabled nil)
  
  (setq dired-dwim-target t
        dired-omit-verbose nil
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
  (add-hook 'dired-sidebar-mode-hook
            (lambda nil
              (setq-local line-spacing 5)
              (hl-line-mode t)
              (dolist (face '(default fringe))
                (face-remap-add-relative
                 face :background (face-background 'help-key-binding)
                 :family "Fira Sans" :weight 'regular))))

  (when (display-graphic-p)
    (setq dired-sidebar-subtree-line-prefix "    "))
  (setq dired-sidebar-window-fixed nil
        dired-sidebar-use-omit-mode-integration nil ;; t breaks
        dired-sidebar-theme 'nerd-icons
        dired-sidebar-use-custom-modeline nil
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
               ("M-`" . popper-toggle-type)
               ("C-`" . popper-cycle)
               ("M-w" . kill-ring-save)
               ("M-v" . scroll-down-command)))
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
  (setq nov-text-width (min 80 (window-width))
        nov-header-line-format nil))

;; (use-package pdf-tools
;;   :if (display-graphic-p)
;;   :mode ("\\.pdf\\'" . pdf-view-mode)
;;   :magic ("%PDF" . pdf-view-mode)
;;   :hook ((pdf-view-mode . pdf-view-fit-page-to-window)
;;          (pdf-view-mode . pdf-view-themed-minor-mode)
;;          (pdf-view-mode . pdf-outline-minor-mode))
;;   :hook (pdf-view-mode . (lambda nil
;;                            (blink-cursor-mode -1)
;;                            (when (featurep 'meow)
;;                              (setq-local meow-cursor-type-motion nil))
;;                            (setq-local mouse-wheel-tilt-scroll t
;;                                        mouse-wheel-flip-direction t
;;                                        mouse-wheel-scroll-amount-horizontal 2))))

;;; Org

(use-package auctex)
;;   :config ; ref: tectonic docs
;;   (setq TeX-engine-alist '((default
;;                             "Tectonic"
;;                             "tectonic -X compile -f plain %T"
;;                             "tectonic -X watch"
;;                             nil)))
;;   (setq LaTeX-command-style '(("" "%(latex)"))
;;         TeX-process-asynchronous t
;;         TeX-check-TeX nil
;;         TeX-engine 'default)
;;   (let ((tex-list (assoc "TeX" TeX-command-list))
;;         (latex-list (assoc "LaTeX" TeX-command-list)))
;;     (setf (cadr tex-list) "%(tex)"
;;           (cadr latex-list) "%l")))

(load "~/.emacs.d/lisp/org-conf" nil :no-message)
(load "~/.emacs.d/lisp/gnus-conf" nil :no-message)

(use-package ox-awesomecv
  :ensure nil
  :disabled t ; installation error
  :vc (:url "https://gitlab.com/Titan-C/org-cv" :rev "24bcd82")
  :after org)

(use-package ox-hugo
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
        org-modern-todo nil)
  (if is-android
      (setq org-modern-list nil)
    (setq org-modern-list
          '((?+ . "◦") (?- . "◦")))))

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

(use-package howm
  :init
  (define-key global-map (kbd "C-x ;") (make-sparse-keymap))
  (setq howm-directory "~/Dropbox/denote"
        howm-home-directory howm-directory
        howm-file-name-format "%Y%m%dT%H%M%S.org"
        howm-view-title-header "#+title:" ; "*"
        howm-prefix (kbd "C-x ;"))

  (setopt howm-view-title-regexp
        "^#?\\+?[tT][iI][tT][lL][eE]:\\( +\\(.*\\)\\|\\)$")
  (setopt howm-view-title-regexp-grep
          "^(#?\\+?[tT][iI][tT][lL][eE]:) +")
  
  :bind* ("C-x ; ;" . howm-menu)
  :bind (:map howm-menu-mode-local-map
              ("<backtab>" . action-lock-goto-previous-link)
              :map howm-view-summary-mode-map
              ("<backtab>" . howm-view-summary-previous-section))
  :hook ((howm-menu howm-view-summary-mode) . meow-motion-mode)
  :config
  (setq howm-view-summary-sep "│"
        howm-menu-reminder-format "❱ %s │ %s")
  (advice-add 'howm-menu-copy-skel
              :filter-args
              (lambda (args)
                (message "Hi")
                (list
                 (replace-regexp-in-string
                  "^-\\{2,\\}$"
                  "─────────────────────────────────────────────────"
                  (car args)))))

  (defvar howm-view-title-regexp)
  (defun howm-cut-title (str)
    "Remove `howm-view-title-header' plus whitespace from STR."
    (let ((begin (when (string-match howm-view-title-regexp str)
                   (match-beginning 2))))
      (if begin (substring str begin) str)))

  (advice-add 'howm-view-item-summary :filter-return 'howm-cut-title)
  (setq howm-menu-list-format
        (let* ((path (format-time-string howm-file-name-format))
               (width (length (file-name-sans-extension
                               (file-name-nondirectory path)))))
          (concat "❱ %-" (format "%s" width) "s │ %s"))))
;; howm-template "* %title%cursor\n%date %file\n\n"



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
  
  (dolist (imode '(reb-mode eat-mode shell-mode eshell-mode term-mode comint-mode
                            deft-mode magit-log-edit-mode log-edit-mode))
    (push `(,imode . insert) meow-mode-state-list))

  (add-hook 'ediff-mode-hook #'meow-motion-mode)
  
  (meow-motion-overwrite-define-key
   '("Q" . kill-current-buffer)
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
   '("g;" . meow-reverse)
   '("ga" . (lambda nil (interactive)
              (org-agenda nil "n")))
   '("gb" . xref-go-back)
   '("gc" . org-capture)
   '("gC" . meow-comment)
   '("gd" . xref-find-definitions)
   '("gf" . ffap)
   '("gg" . avy-goto-char-timer)
   '("gp" . avy-parens-jump)
   '("gh" . diff-hl-show-hunk)
   '("gk" . beginning-of-buffer)
   '("gj" . end-of-buffer)
   '("gi" . eglot-find-implementation)
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
   '("K" . my/eldoc-get-help)
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
   '("Q" . kill-buffer-and-window)
   '("r" . (lambda nil (interactive)
             (delete-char 1)
             (insert-char (read-char nil t))
             (backward-char 1)))
   '("R" . replace-regexp)
   '("s" . isearch-forward-regexp)
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
  :hook (prog-mode . my/setup-install-grammars)
  :config
  (defun my/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (go "https://github.com/tree-sitter/tree-sitter-go")
               (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
               (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")
               (helm "https://github.com/ngalaiko/tree-sitter-go-template"
                     "master" "dialects/helm/src")
               (templ "https://github.com/vrischmann/tree-sitter-templ")
               (gotmpl "https://github.com/ngalaiko/tree-sitter-go-template")
               (rust "https://github.com/tree-sitter/tree-sitter-rust")
               (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
               (json "https://github.com/tree-sitter/tree-sitter-json")
               (janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")
               (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                           "master" "typescript/src")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (setq treesit-font-lock-level 4))

;; (load "~/.emacs.d/lisp/helm-ts-mode.el")

(setq major-mode-remap-alist '((c++-mode . c++-ts-mode)
                               (c-mode . c-ts-mode)
                               (yaml-mode . yaml-ts-mode)))
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
         ("\\go\\.mod\\'"  . go-mod-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.lua\\'" . lua-ts-mode)
         ("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerignore\\'" . dockerfile-ts-mode)
         ("\\.bin\\'" . hexl-mode)
         ("\\.info\\'" . Info-mode)))

(use-package eglot
  :bind ("M-+" . eglot-code-actions)
  :bind (:map meow-normal-state-keymap
              ("gr" . eglot-rename)
              ("gF" . eglot-format))
  :hook ((rust-ts-mode go-ts-mode yaml-ts-mode) . eglot-ensure)
  :hook ((rust-ts-mode go-ts-mode)
         . (lambda ()
             (setq-local tab-width 4)
             (hl-line-mode t)
             (highlight-indent-guides-mode t)))
  :hook (rust-ts-mode . (lambda nil
                          (add-to-list 'process-environment "CARGO_TERM_COLOR=always" :append)))
  :custom
  (eglot-sync-connect nil) ;; speeds up file opening
  ;; :init (setq eglot-stay-out-of '(flymake))
  :config
  (with-eval-after-load 'project
    (setq project-vc-extra-root-markers '("go.mod" "Cargo.toml"))
    (setq project-vc-ignores '("**/vendor/**")))
  
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-config 0
        eglot-autoshutdown t
        eglot-inlay-hints-mode nil
        go-ts-mode-indent-offset 4
        eglot-ignored-server-capabilities '(:inlayHintProvider
                                            :colorProvider
                                            :foldingRangeProvider))
  
  (add-to-list 'eglot-server-programs '(helm-ts-mode "helm_ls" "serve"))

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
              (setq eldoc-documentation-strategy
                    'eldoc-documentation-compose-eagerly)
              (setq-local flymake-cc-command nil)
              (my-eglot-setup)))
  
  (setq-default eglot-workspace-configuration
                '((:gopls . ((staticcheck . t) ; go install staticcheck
                             (linksInHover . :json-false)
                             (matcher . "CaseSensitive")))
                  ;; https://github.com/joaotavora/eglot/discussions/918
                  ;; https://www.reddit.com/r/emacs/comments/11xjpeq
                  (:yaml . (:schemaStore
                            (:enable t :url "https://schemastore.org/api/json/catalog.json")
                            ;; (:enable t :url "https://json.schemastore.org/chart.json")
                            :format (:enable t :proseWrap t :printWidth 80)
                            :validate t
                            :hover t
                            :completion t
                            :schemas (:Kubernetes "/*")))))
  ;; https://gist.github.com/casouri/0ad2c6e58965f6fd2498a91fc9c66501
  (add-to-list 'eglot-server-programs ;; standalone r-a support (from rustic)
               `(rust-ts-mode . ("rust-analyzer"
                                 :initializationOptions
                                 (:check (:command "clippy"))))))

(load "~/.emacs.d/lisp/snippets" nil :no-message)

(use-package kubed
  :bind-keymap ("C-c k" . kubed-prefix-map))

(use-package kubel)

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest :branch "main"))

(use-package direnv
  :hook (prog-mode. direnv-mode)
  :config
  (setq direnv-always-show-summary nil)
  (push 'comint-mode direnv-non-file-modes))

;; (use-package flymake-golangci
;;   :vc (:url "https://github.com/storvik/flymake-golangci")
;;   :hook ((eglot-managed-mode . (lambda ()
;;                                  (when (derived-mode-p '(go-mode go-ts-mode))
;;                                    (flymake-golangci-load))))
;;          (go-mode . flymake-golangci-load)
;;          (go-ts-mode . flymake-golangci-load)))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

(use-package janet-ts-mode
  :vc (:url "https://github.com/sogaiu/janet-ts-mode")
  :mode ("\\.janet\\'" . janet-ts-mode)
  :config
  (remove-hook 'janet-ts-mode-hook 'treesit-inspect-mode))

;; (use-package templ-ts-mode
;;   :vc (:url "https://github.com/danderson/templ-ts-mode")
;;   :mode ("\\.tpl\\'" . templ-ts-mode))

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(use-package foxy
  :ensure nil
  :load-path "~/.emacs.d/lisp"
  :bind (("C-c l" . foxy-listen-start)
         ("C-c ]" . foxy-cycle-files)
         ("C-c [" . (lambda nil (interactive)
                      (foxy-cycle-files -1)))
         ("C-c b" . foxy-run-all-tests)))

(when (file-exists-p custom-file)
  (load custom-file nil :no-message))

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; Overlay testing
;; (let* ((str (format-mode-line "%b"))
;;        (len (length str))
;;        (tr (- (nth 2 (window-body-edges)) 1)) ; FIXME

;;        (ovl (make-overlay tr tr)))
;;   (overlay-put ovl 'before-string str)
;;   (overlay-put ovl 'face 'tab-bar))
