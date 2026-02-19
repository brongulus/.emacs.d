;;; init.el --- 🦬 -*- lexical-binding: t; -*
;; (load "~/.emacs.d/lisp/benchmarking.el" :noerr :no-message)
;;; Better defaults ---
(setq-default auto-save-default nil auto-save-list-file-prefix nil
              create-lockfiles nil
              custom-file "/tmp/emacs-custom"
              cursor-in-non-selected-windows nil
              frame-resize-pixelwise t make-backup-files nil
              eldoc-echo-area-use-multiline-p nil
              imenu-flatten t inhibit-startup-screen t
              mode-line-collapse-minor-modes '(not flymake-mode)
              mode-line-end-spaces nil mode-line-compact t
              copy-region-blink-delay 0 display-line-numbers-width 4
              truncate-lines nil tab-bar-show nil
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              warning-minimum-level :error
              use-short-answers t uniquify-buffer-name-style 'forward
              tab-width 4 line-spacing 5 indent-tabs-mode nil
              fill-column 140 text-scale-mode-step 1.3)
(setq enable-recursive-minibuffers t savehist-additional-variables '(register-alist kill-ring)
	  minibuffer-default-prompt-format " [%s]" minibuffer-visible-completions t
	  read-buffer-completion-ignore-case t read-file-name-completion-ignore-case t
	  org-fontify-quote-and-verse-blocks t org-fontify-whole-heading-line t
	  org-pretty-entities t org-src-fontify-natively t
	  treesit-enabled-modes t treesit-font-lock-level 4 go-ts-mode-indent-offset 4
	  isearch-lax-whitespace t isearch-lazy-count t
	  isearch-repeat-on-direction-change t isearch-wrap-pause 'no-ding
	  search-whitespace-regexp ".*?" copy-region-blink-predicate 'always
	  dired-kill-when-opening-new-dired-buffer t delete-by-moving-to-trash t
      help-window-select t kill-region-dwim 'emacs-word
      eglot-ignored-server-capabilities '(:inlayHintProvider :workspace.didChangeWatchedFiles)
	  eglot-autoshutdown t jsonrpc-event-hook nil flymake-mode-line-title nil
	  require-final-newline t resize-mini-windows t
      maximum-scroll-margin 0.5 scroll-margin 9999 scroll-conservatively 101
      scroll-preserve-screen-position t doc-view-continuous t
      ring-bell-function 'ignore tab-always-indent 'complete
      vc-allow-rewriting-published-history t vc-follow-symlinks t
	  vc-git-diff-switches '("--patch-with-stat" "--histogram")
	  vc-git-shortlog-switches '("--stat")
      project-vc-extra-root-markers '("Cargo.toml" "build.zig" "go.work")
      eshell-hist-ignoredups 'erase eshell-history-size 20000
      eshell-save-history-on-exit t eshell-glob-case-insensitive t
      grep-command "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)"
      grep-command-position 27 ido-enable-flex-matching t ido-everywhere t
      ido-ignore-buffers
      '("\\` " "\\*Messages\\*" "\\*scratch\\*" "\\*Completions\\*" "\\*Native-compile-Log\\*"
        "\\*Async-native-compile-log\\*" "\\*EGLOT.*events\\*" "\\*Flymake.*\\*"
        "\\*Buffer List\\*" "\\*Help\\*" "\\*Minibuf-.*\\*" "\\*vc-.*\\*")
      ido-create-new-buffer 'always ido-use-virtual-buffers 'auto
      ido-show-dot-for-dired t ido-max-prospects 6
      ido-auto-merge-work-directories-length -1)
(unless (eq system-type 'android) (setq shell-file-name "~/.nix-profile/bin/fish"))
(put 'narrow-to-region 'disabled nil)
(setcdr (assq 'continuation fringe-indicator-alist) '(nil nil))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(blink-cursor-mode -1) (tooltip-mode -1) (menu-bar-mode -1) (scroll-bar-mode -1) (tool-bar-mode -1)
(kill-ring-deindent-mode 1) (save-place-mode 1) (global-visual-line-mode 1) (repeat-mode 1)
(add-hook 'emacs-startup-hook
          (lambda () (ido-mode 'both) (which-key-mode 1) (global-auto-revert-mode 1)
            (savehist-mode 1) (viper-mode)))
;;; Keys ---
(setq viper-mode t viper-expert-level 5
      viper-inhibit-startup-message t viper-want-ctl-h-help t
      viper-want-emacs-keys-in-insert t viper-want-emacs-keys-in-vi t
      viper-ex-style-editing nil viper-insert-state-cursor-color nil)
(with-eval-after-load 'viper
  (dolist (key '("\C-b" "\C-d" "\C-e" "\C-f" "\C-u" "\C-y" "\C-v"))
    (define-key viper-vi-basic-map key nil))
  (define-key viper-vi-basic-map (kbd "SPC") ctl-x-map)
  (dolist (binding '(("g" . nil) ("x" . sel-line) ("-" . negative-argument) ("y" . kill-ring-save)
                     ("C-\\" . epop) ("R" . replace-regexp) ("=" . mark-inner) ("d" . del-vi)
                     ("g i" . eglot-find-implementation) ("g r" . xref-find-references)
                     ("C" . string-rectangle) ("p" . yank) ("+" . eglot-rename) ("g s" . imenu)
                     ("z f" . hs-toggle-hiding) ("z c" . hs-hide-all) ("z s" . hs-show-all)
                     ("<" . beginning-of-buffer) (">" . end-of-buffer) ("o" . other-window)
                     ("v" . set-mark-command) ("s" . isearch-forward-regexp) ("u" . undo-only)
                     ("Z" . undo-redo) ("," . my-scroll-other-down) ("." . my-scroll-other-up)))
    (keymap-set viper-vi-basic-map (car binding) (cdr binding))))
(dolist (binding '(("C-x c c" . compile) ("C-x c r" . recompile) ("C-h '" . describe-face)
                   ("C-x C-m" . execute-extended-command) ("C-x k" . kill-current-buffer)
                   ("C-o" . other-window) ("<escape>" . keyboard-escape-quit) ("C-\\" . epop)
                   ("C-x ;" . comment-line) ("C-x x c" . save-buffers-kill-emacs) ("M-;" . eval-expression)
                   ("C-," . my-scroll-other-down) ("M-j" . window-toggle-side-windows)
                   ("C-<tab>" . tab-next) ("C-S-<tab>" . tab-previous) ("C-x x f" . find-file)
                   ("C-x x s" . save-buffer) ("C-x x e" . eval-defun) ("C-x x z" . restart-emacs)
                   ("C-." . my-scroll-other-up) ("C-x x x" . flymake-show-project-diagnostics)))
  (keymap-global-set (car binding) (cdr binding)))
(keymap-global-set "C-x m" esc-map)
(keymap-global-set "<f6>" #'(lambda nil (interactive) (invert-face 'default)))
(keymap-global-set "j" #'(lambda nil (interactive)
                           (let* ((event (read-event nil nil 0.4)))
                             (if event (if (and (characterp event) (= event ?k))
                                           (viper-change-state-to-vi)
                                         (insert ?j) (push event unread-command-events))
                               (insert ?j)))))
(keymap-set vc-prefix-map "f"
            (lambda () (interactive) (vc-git--pushpull "push" nil '("--force-with-lease"))))
(keymap-set vc-prefix-map "e" #'vc-ediff)
;;; Visuals ---
(dolist (face '(default fixed-pitch variable-pitch))
  (set-face-attribute face nil :font "Input Mono Narrow" :height (if (eq system-type 'android) 150 140)))
(dolist (face '(vertical-border font-lock-comment-face))
  (set-face-attribute face nil :foreground 'unspecified :inherit '(shadow default)))
(set-face-attribute 'fringe nil :background 'unspecified)
(let ((common (list :background 'unspecified :foreground 'unspecified
                    :inverse-video (not (display-graphic-p)) :height 140
                    :box '(:line-width 1 :style flat-button)
                    :overline (face-foreground 'shadow))))
  (apply #'set-face-attribute 'mode-line nil
         :inherit 'default common)
  (apply #'set-face-attribute 'mode-line-inactive nilq 
         :inherit 'shadow common))
(set-face-attribute 'default nil :foreground "#222323" :background "#eae8e1")
(add-hook 'post-command-hook
          (lambda () (unless (eq (buffer-modified-p) (bound-and-true-p curs-mod))
                       (set-cursor-color (if (setq curs-mod (buffer-modified-p)) "coral3" "#00c2ff")))))
(dolist (spec '((font-lock-string-face :foreground nil) (show-paren-match :background t)))
  (let* ((face (car spec)) (prop (cadr spec)) (invert (caddr spec))
         (dark "#26BF96") (light "#0C9671"))
    (custom-set-faces `(,face ((((background dark))  ,prop ,(if invert light dark))
                               (((background light)) ,prop ,(if invert dark light)))))))
(set-face-attribute 'font-lock-variable-name-face nil :foreground 'unspecified)
(dolist (face '(minibuffer-prompt font-lock-keyword-face
                                  font-lock-function-name-face font-lock-type-face))
  (set-face-attribute face nil :foreground 'unspecified :weight 'bold))
(custom-set-faces '(eglot-highlight-symbol-face
                    ((((background dark))  :background "#2a2c2c")
                     (((background light)) :background "#d9d7d0"))))
(defvar tab-bar--tab-keymaps
  (let ((v (make-vector 20 nil)))
    (dotimes (i 20 v)
      (let ((m (make-sparse-keymap)))
        (define-key m [mode-line mouse-1]
                    `(lambda () (interactive) (tab-bar-select-tab ,(1+ i))))
        (aset v i m)))))
(setq mode-line-front-space
      '(:eval (when (> (length (tab-bar-tabs)) 1)
                (propertize
                 (concat " "
                         (mapconcat
                          (lambda (i) (propertize (if (= i (tab-bar--current-tab-index)) "⦿" "○")
                                                  'mouse-face 'mode-line-highlight
                                                  'local-map (aref tab-bar--tab-keymaps i)))
                          (number-sequence 0 (1- (length (tab-bar-tabs)))) " ")
                         " ")))))
(setq-default mode-line-format (remove '(project-mode-line project-mode-line-format) mode-line-format))
(setq mode-line-misc-info nil)
(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        '(:eval (when (and (bound-and-true-p eglot--managed-mode) (eglot-managed-p)) eglot-mode-line-progress))))
(with-eval-after-load 'viper
  (let ((cell (memq 'mode-line-modes mode-line-format)))
    (setcar cell 'viper-mode-string) (setcdr cell (cons 'mode-line-format-right-align (cons 'mode-line-modes (cdr cell))))))
;;; Programming stuff ---
(dolist (fn '(electric-pair-local-mode hs-minor-mode display-line-numbers-mode
                                       completion-preview-mode goto-address-mode))
  (add-hook 'prog-mode-hook fn))
(add-hook 'text-mode-hook #'goto-address-mode)
(with-eval-after-load 'which-func ; disabled because this causes scroll slowdown
  (setq which-func-format (list (cadr which-func-format)) which-func-unknown "")
  (set-face-attribute 'which-func nil :foreground 'unspecified :weight 'bold))
(dolist (mode '(rust-ts-mode-hook go-ts-mode-hook python-mode-hook c++-mode-hook))
  (add-hook mode #'(lambda nil (run-with-timer 0.1 nil #'eglot-ensure))))
(dolist (mode-hook '(conf-mode-hook yaml-ts-mode-hook))
  (add-hook mode-hook #'display-line-numbers-mode)
  (add-hook mode-hook #'completion-preview-mode))
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map
              "C-s" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map
              "C-r" #'completion-preview-prev-candidate))
(with-eval-after-load 'eshell
  (defun eshell-insert-history () (interactive) ; src: habrams
         (let ((cmd (completing-read "Eshell history: "
                                     (delete-dups (ring-elements eshell-history-ring)))))
           (when cmd (kill-line 0) (insert cmd))))
  (add-hook 'eshell-mode-hook #'completion-preview-mode)
  (when (not (or (getenv "GCTL_SESSION_ID") (getenv "TERM_SESSION_ID")))
    (setenv "GCTL_SESSION_ID" (string-trim (shell-command-to-string "uuidgen"))))
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (eshell/addpath (concat (getenv "GOPATH") "/bin")))
(add-hook #'eshell-mode-hook 
          (lambda nil (define-key eshell-hist-mode-map (kbd "C-r") #'eshell-insert-history)))
;;; Miscellaneous ---
(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook #'viper-mode)
  (setq eww-header-line-format nil)
  (setq eww-auto-rename-buffer 'title))
(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map (kbd "SPC") ctl-x-map)
  (define-key doc-view-mode-map (kbd "j") #'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "k") #'doc-view-scroll-down-or-previous-page))

(setopt minibuffer-completion-auto-choose t completion-ignore-case t
        completions-format 'one-column completions-header-format nil
        completion-show-help nil completions-max-height 13
        completion-preview-minimum-symbol-length 2
        completions-sort 'historical completions-detailed t
        completion-eager-update t completion-auto-help 'visible
        completion-styles '(partial-completion basic flex))
(define-key minibuffer-local-completion-map [remap previous-line] #'minibuffer-previous-completion)
(define-key minibuffer-local-completion-map [remap next-line]     #'minibuffer-next-completion)
(add-to-list 'display-buffer-alist
             '("\\*\\(Completions\\|xref\\|Occur.*\\|eldoc\\|vc.*-log\\|compilation.*\\|Flymake.*\\)\\*"
               (display-buffer-in-side-window) (side . bottom) (window-height . 0.30)
               (window-parameters . ((mode-line-format . none)))))
;;; Sensible changes ---
(defun prot-quit (&optional interactive)
  "A sensible `keyboard-quit'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (when interactive (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ((derived-mode-p 'completion-list-mode) (delete-completion-window))
          ((unwind-protect (keyboard-quit)
             (when interactive (setq this-command 'keyboard-quit)))))))
(define-key (current-global-map) [remap keyboard-quit] #'prot-quit)

(defun my/show-paren-data ()
  (or (and (boundp 'treesit-show-paren-data) (treesit-show-paren-data))
      (when-let* ((open (cond ((eq (car (syntax-after (point))) 4) (point))
                              ((eq (car (syntax-after (1- (point)))) 5)
                               (save-excursion (backward-sexp) (point)))
                              ((nth 1 (syntax-ppss)))))
                  (end (save-excursion (goto-char open) (forward-sexp) (point)))
                  ((> end open)))
        (list open (1+ open) (1- end) end))))
(add-hook 'prog-mode-hook (lambda () (setq-local show-paren-data-function #'my/show-paren-data)))

(defun zen-buffer-apply-margins () "Apply zen margins to all windows."
       (walk-windows
        (lambda (win)
          (with-current-buffer (window-buffer win)
            (when (or (derived-mode-p '(prog-mode text-mode)) (eq major-mode 'eww-mode))
              (let* ((special-modes (member major-mode '(org-mode markdown-ts-mode)))
                     (margin (max 0 (/ (- (window-total-width win) fill-column) 2)))
                     (lmargin (if special-modes (max 0 (- margin 10)) margin)))
                (if (> (window-total-width win) 140)
                    (progn (visual-line-mode 1) (set-window-margins win lmargin margin)
                           (when special-modes (text-scale-set 1) (setq-local line-spacing 0.6)))
                  (progn (set-window-margins win nil)
                         (when special-modes (text-scale-set 0) (setq-local line-spacing 5))))))))
        nil t))
(add-hook 'window-configuration-change-hook #'zen-buffer-apply-margins)

(defun epop nil (interactive)
       (defvar eshell-buffer-name)
       (let ((display-buffer-alist `(("\\*eshell-pop\\*"
                                      (display-buffer-in-side-window)
                                      (side . bottom) (slot . -2) (window-height . 0.30))))
             (eshell-buffer-name "*eshell-pop*") (inhibit-message t))
         (eshell))
       (setq-local mode-line-format nil))
(defun my-scroll-other-down nil (interactive)
       (let ((mode (with-current-buffer (window-buffer (other-window-for-scrolling)) major-mode)))
         (with-selected-window (other-window-for-scrolling)
           (cond ((eq mode 'Info-mode) (Info-scroll-up))
                 ((eq mode 'doc-view-mode) (doc-view-scroll-up-or-next-page 5))
                 (t (scroll-up-command 5))))))
(defun my-scroll-other-up nil (interactive)
       (let ((mode (with-current-buffer (window-buffer (other-window-for-scrolling)) major-mode)))
         (with-selected-window (other-window-for-scrolling)
           (cond ((eq mode 'Info-mode) (Info-scroll-down))
                 ((eq mode 'doc-view-mode) (doc-view-scroll-down-or-previous-page 5))
                 (t (scroll-down-command 5))))))
(defun mark-inner () (interactive)
       (condition-case nil (if (nth 3 (syntax-ppss)) ; string or list
                               (let ((start (nth 8 (syntax-ppss))))
                                 (goto-char start) (set-mark (point)) (forward-sexp))
                             (backward-up-list) (set-mark (point))
                             (down-list) (up-list) (backward-down-list))
         (error (message "No inner list or string found."))))
(defun sel-line (arg) (interactive "p")
       (or arg (setq arg 1))
       (when (not (use-region-p)) (forward-line 0) (set-mark-command nil))
       (forward-line arg))
(defun del-vi nil (interactive)
       (if (use-region-p) (call-interactively 'kill-region) (delete-char 1)))
