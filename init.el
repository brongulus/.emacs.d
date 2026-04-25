;;; init.el - 🦬 ---  -*- lexical-binding: t; -*-
;; (load "~/.emacs.d/lisp/benchmarking.el" :noerr :no-message)
;;; Better defaults ---
(setq-default auto-save-file-name-transforms `((".*" "~/.emacs.d/backup/" t))
              backup-directory-alist `(("." . "~/.emacs.d/backup/"))
              create-lockfiles nil custom-file "/tmp/emacs-custom"
              warning-minimum-level :error cursor-in-non-selected-windows nil
              frame-resize-pixelwise t show-paren-mode nil
              eldoc-echo-area-use-multiline-p nil imenu-flatten t
              inhibit-startup-screen t display-line-numbers-width 4
              display-line-numbers-widen t truncate-lines nil tab-bar-show nil
              sentence-end-double-space nil ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain use-short-answers t
              uniquify-buffer-name-style 'forward c-basic-offset 4 tab-width 4 line-spacing '(3 . 3)
              indent-tabs-mode nil split-width-threshold 130 fill-column 140 text-scale-mode-step 1.3)
(setq enable-recursive-minibuffers t savehist-additional-variables '(register-alist kill-ring)
	  minibuffer-default-prompt-format " [%s]" minibuffer-visible-completions t
	  read-buffer-completion-ignore-case t read-file-name-completion-ignore-case t
      org-modules nil org-pretty-entities t org-src-fontify-natively t
      org-src-content-indentation 0 org-src-preserve-indentation t
	  org-fontify-quote-and-verse-blocks t org-fontify-whole-heading-line t
	  treesit-enabled-modes t treesit-font-lock-level 2 go-ts-mode-indent-offset 4
      save-interprogram-paste-before-kill t delete-pair-push-mark t ispell-program-name "aspell"
      delete-pair-blink-delay t lazy-highlight-initial-delay 0  isearch-regexp-lax-whitespace t
      isearch-lazy-count t isearch-repeat-on-direction-change t isearch-wrap-pause 'no-ding
	  search-whitespace-regexp ".*?" dired-kill-when-opening-new-dired-buffer t
      dired-listing-switches "-l -v --almost-all --human-readable --group-directories-first"
      delete-by-moving-to-trash t help-window-select t kill-region-dwim 'emacs-word
      eglot-ignored-server-capabilities
      '(:inlayHintProvider :workspace.didChangeWatchedFiles :colorProvider :codeLensProvider
                           :foldingRangeProvider :semanticTokensProvider :documentHighlightProvider)
      eglot-sync-connect 0 eglot-autoshutdown t jsonrpc-event-hook nil
      maximum-scroll-margin 0.5 scroll-margin 9999 scroll-conservatively 101
      scroll-preserve-screen-position t fast-but-imprecise-scrolling t doc-view-continuous t
      require-final-newline t resize-mini-windows t ring-bell-function 'ignore tab-always-indent 'complete
      diff-font-lock-syntax nil vc-allow-rewriting-published-history t vc-follow-symlinks t vc-make-backup-files t
      vc-display-status 'no-backend vc-git-diff-switches '("--patch-with-stat" "--histogram")
      project-vc-extra-root-markers '("Cargo.toml" "build.zig" "go.work" "CMakeLists.txt")
      eshell-banner-message "" eshell-hist-ignoredups 'erase eshell-history-size 20000
      eshell-save-history-on-exit t eshell-glob-case-insensitive t eshell-scroll-to-bottom-on-input 'this
      grep-command "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)"
      grep-command-position 27 ido-enable-flex-matching t ido-everywhere nil
      ido-ignore-buffers
      '("\\` " "\\*Messages\\*" "\\*scratch\\*" "\\*Completions\\*" "\\*Native-compile-Log\\*"
        "\\*Async-native-compile-log\\*" "\\*EGLOT.*events\\*" "\\*Flymake.*\\*" "\\*MPC.*\\*"
        "\\*Buffer List\\*" "\\*Help\\*" "\\*Minibuf-.*\\*" "\\*vc-.*\\*" "\\#.*")
      ido-create-new-buffer 'always ido-use-virtual-buffers 'auto recentf-max-saved-items 200
      ido-show-dot-for-dired t ido-max-window-height 1 ido-auto-merge-work-directories-length -1
      ido-separator " • " icomplete-separator " • " icomplete-tidy-shadowed-file-names t
      Info-default-directory-list '("~/.emacs.d/info") Info-use-header-line nil)
(if (not (eq system-type 'android)) (setq shell-file-name "/opt/homebrew/bin/fish")
  (setq-default fill-column 120 line-spacing '(4 . 4)))
(put 'narrow-to-region 'disabled nil)
(setcdr (assq 'continuation fringe-indicator-alist) '(nil nil))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(blink-cursor-mode -1) (tooltip-mode -1) (menu-bar-mode -1) (scroll-bar-mode -1)
(tool-bar-mode -1) (line-number-mode -1) (kill-ring-deindent-mode 1)
(global-visual-line-mode 1) (global-visual-wrap-prefix-mode 1) (electric-pair-mode 1)
(add-hook 'emacs-startup-hook
          (lambda () (ido-mode 'buffer) (global-auto-revert-mode 1) (viper-mode) (fido-mode)
            (repeat-mode 1) (save-place-mode 1) (delete-selection-mode 1) (savehist-mode 1)))
(setq fido-non-vertical-fns
      '(find-file find-file-other-window execute-extended-command project-switch-to-buffer))
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda nil
            (unless (memq this-command fido-non-vertical-fns) (setq-local icomplete-vertical-mode t))
            (setq-local icomplete-prospects-height (if icomplete-vertical-mode 11 1))))
(dolist (hook '(text-mode-hook eshell-mode-hook)) (add-hook hook #'goto-address-mode))
;;; Keys ---
(setq viper-mode t viper-expert-level 5 viper-ex-style-motion nil
      viper-inhibit-startup-message t viper-want-ctl-h-help t
      viper-want-emacs-keys-in-insert t viper-want-emacs-keys-in-vi t
      viper-ex-style-editing nil viper-insert-state-cursor-color nil)
(with-eval-after-load 'viper-cmd
  (setq viper-insert-basic-map (make-sparse-keymap))
  (define-key viper-insert-basic-map viper-toggle-key 'viper-escape-to-vi)
  (advice-add 'viper-adjust-keys-for :after
              (lambda (state)
                (when (memq state '(insert-state replace-state))
                  (define-key viper-insert-basic-map [backspace] nil)
                  (define-key viper-replace-map [backspace] nil)))
              '((name . viper-remove-backspace-override))))
(with-eval-after-load 'viper
  (advice-add 'viper-post-command-sentinel :override #'ignore)
  (defun viper-set-insert-cursor-type nil (setq cursor-type '(bar . 3)))
  (define-key viper-minibuffer-map "\C-j" #'icomplete-fido-exit)
  (define-key viper-vi-basic-map "c" #'(lambda nil (interactive) (del-vi) (viper-change-state-to-insert)))
  (dolist (key '("\C-b" "\C-d" "\C-e" "\C-f" "\C-u" "\C-y" "\C-v"))
    (define-key viper-vi-basic-map key nil))
  (define-key viper-vi-basic-map (kbd "SPC") ctl-x-map)
  (define-key viper-vi-basic-map (kbd "m") (make-sparse-keymap))
  (define-key viper-vi-basic-map "ms" insert-pair-map)
  (dolist (binding '(("g" . nil) ("x" . sel-line) ("-" . negative-argument) ("y" . kill-ring-save)
                     ("C-\\" . epop) ("R" . replace-regexp) ("=" . mark-inner) ("d" . del-vi)
                     ("g i" . eglot-find-implementation) ("g r" . xref-find-references) (";" . prot-quit)
                     ("C" . string-rectangle) ("p" . yank) ("+" . eglot-rename) ("_" . eglot-code-actions)
                     ("z f" . hs-toggle-hiding) ("z c" . hs-hide-all) ("z s" . hs-show-all)
                     ("[" . previous-error) ("]" . next-error) ("#" . definition-at-point) ("m d" . my/delete-pair)
                     ("g s" . imenu) ("q" . quit-window) ("j" . next-line) ("k" . previous-line)
                     ("<" . beginning-of-buffer) (">" . end-of-buffer) ("o" . other-window)
                     ("v" . set-mark-command) ("s" . isearch-forward-regexp) ("u" . undo-only)
                     ("U" . undo-redo) ("," . my-scroll-other-down) ("." . my-scroll-other-up)
                     ("@" . eww-open-in-new-buffer) ("g a" . beginning-of-defun) ("g e" . end-of-defun)
                     ("&" . align-regexp) ("(" . flymake-goto-prev-error) (")" . flymake-goto-next-error)
                     ("g z" . pop-to-mark-command) ("g /" . xref-find-definitions-other-window)
                     ("K" . my/eldoc-get-help) ("*" . isearch-forward-symbol-at-point)))
    (keymap-set viper-vi-basic-map (car binding) (cdr binding)))
  ;; selection-first word movements (meow/kak style)
  (defun my-viper-select-thing (thing n)
    "Select the next N THINGs, advancing on repeat."
    (let* ((fwd (> n 0))
           (skip (let ((s (if (eq thing 'word) "^w" "^w_")))
                   (if fwd (lambda () (skip-syntax-forward s)) (lambda () (skip-syntax-backward s))))))
      (when (and (region-active-p) (eq last-command this-command))
        (let ((b (bounds-of-thing-at-point thing)))
          (when b (goto-char (if fwd (cdr b) (car b)))))
        (funcall skip))
      (dotimes (i (abs n))
        (or (bounds-of-thing-at-point thing) (funcall skip))
        (when-let* ((b (bounds-of-thing-at-point thing)))
          (when (= i 0) (set-mark (if fwd (car b) (cdr b))))
          (goto-char (if fwd (cdr b) (car b)))
          (activate-mark)
          (when (< i (1- (abs n))) (funcall skip))))))
  (dolist (p '(("w" . (symbol . 1)) ("e" . (word . 1)) ("b" . (word . -1))
               ("W" . (symbol . 1)) ("E" . (symbol . 1)) ("B" . (symbol . -1))))
    (let ((thing (cadr p)) (dir (cddr p)))
      (keymap-set viper-vi-basic-map (car p)
                  (lambda (n) (interactive "p") (my-viper-select-thing thing (* n dir)))))))

(unless (display-graphic-p)
  (add-hook 'viper-vi-state-hook (lambda () (send-string-to-terminal "\e[2 q")))
  (add-hook 'viper-insert-state-hook (lambda () (send-string-to-terminal "\e[6 q")))
  (add-hook 'viper-replace-state-hook (lambda () (send-string-to-terminal "\e[4 q")))
  (add-hook 'kill-emacs-hook (lambda () (send-string-to-terminal "\e[2 q"))))
(dolist (binding '(("C-x c c" . compile) ("C-x c r" . recompile) ("C-x c ." . compile-at-root)
                   ("C-h '" . describe-face) ("C-x C-m" . execute-extended-command) ("C-\\" . epop)
                   ("C-x k" . kill-current-buffer) ("M-o" . other-window) ("<escape>" . keyboard-escape-quit)
                   ("C-x ;" . comment-line) ("C-x x c" . save-buffers-kill-emacs) ("s-o" . other-window)
                   ("C-x x b" . ibuffer) ("M-;" . eval-expression) ("C-/" . undo-only)
                   ("C-," . my-scroll-other-down) ("M-j" . window-toggle-side-windows)
                   ("C-<tab>" . tab-next) ("C-S-<tab>" . tab-previous) ("C-x x f" . find-file)
                   ("C-x x s" . save-buffer) ("C-x x e" . eval-defun) ("C-x x z" . restart-emacs)
                   ("C-." . my-scroll-other-up) ("C-x x x" . flymake-show-project-diagnostics)))
  (keymap-global-set (car binding) (cdr binding)))
(keymap-global-set "C-x m" esc-map)
(keymap-global-set "C-x 6" #'(lambda nil (interactive) (invert-face 'default)))

(defun my-chord (initial-key final-key fn)
  (interactive) ;; src: wasamasa
  (let ((event (read-event nil nil 0.4)))
    (cond ((and event (characterp event) (= event final-key)) (call-interactively fn))
          (event (insert initial-key) (push event unread-command-events))
          (t (insert initial-key)))))
(keymap-global-set "j" #'(lambda nil (interactive) (my-chord ?j ?k 'viper-change-state-to-vi)))
(keymap-set vc-prefix-map "f" (lambda () (interactive) (vc-git--pushpull "push" nil '("--force-with-lease"))))
(keymap-set vc-prefix-map "e" #'vc-ediff)
(with-eval-after-load 'dired (keymap-set dired-mode-map "SPC" ctl-x-map))
(with-eval-after-load 'doc-view
  (keymap-set doc-view-mode-map "SPC" ctl-x-map)
  (keymap-set doc-view-mode-map "j" #'doc-view-next-line-or-next-page)
  (keymap-set doc-view-mode-map "k" #'doc-view-previous-line-or-previous-page))
;;; Visuals ---
(dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
  (set-face-attribute face nil :font "Input Mono Narrow" :height (if (eq system-type 'android) 160 140)))
(dolist (set '(cjk-misc han kana)) (set-fontset-font t set "Noto Sans Mono CJK JP" nil 'prepend))
(set-face-attribute 'vertical-border nil :foreground 'unspecified :inherit '(shadow default))
(set-face-attribute 'font-lock-comment-face nil :foreground 'unspecified :inherit 'shadow)
(set-face-attribute 'fringe nil :background 'unspecified)
(set-face-attribute 'default nil :foreground "#212121" :background "#eae8e1")
(custom-set-faces '(font-lock-string-face ((((background dark))  :foreground "#deb07a")
                                           (((background light)) :foreground "sienna"))))
(custom-set-faces '(bold ((((background dark)) :foreground "#fafbfc" :weight bold)
                          (((background light)) :weight bold))))
(add-hook 'post-command-hook
          (lambda () (unless (eq (buffer-modified-p) (bound-and-true-p curs-mod))
                       (set-cursor-color (if (setq curs-mod (buffer-modified-p)) "coral3" "#00c2ff")))))
(dolist (face '(font-lock-type-face font-lock-constant-face viper-minibuffer-insert
                                    font-lock-keyword-face font-lock-variable-name-face))
  (custom-set-faces `(,face ((t nil)))))
(set-face-attribute 'font-lock-builtin-face nil :foreground 'unspecified :slant 'italic)
(set-face-attribute 'error nil :foreground "Coral3")
(custom-set-faces '(success ((t :foreground "ForestGreen"))))
(set-face-attribute 'nobreak-space nil :underline nil)

(dolist (face '(eshell-prompt minibuffer-prompt font-lock-function-name-face line-number-current-line))
  (custom-set-faces `(,face ((t :foreground unspecified :inherit bold)))))
(dotimes (i 9) (let ((face (intern (format "outline-%d" (1+ i)))))
                 (custom-set-faces `(,face ((t :height 1.1 :inherit bold))))))
(custom-set-faces '(highlight ((((background dark))  :background "#393939")
                               (((background light)) :background "#d9d7d0"))))
(custom-set-faces '(eglot-highlight-symbol-face ((t :inherit (highlight default)))))
(dolist (face '(org-block org-block-begin-line org-block-end-line))
  (custom-set-faces `(,face ((t :inherit (highlight default) :extend t)))))
(custom-set-faces '(isearch ((t :inverse-video t))))
(dolist (face '(lazy-highlight org-code org-verbatim org-table)) (custom-set-faces `(,face ((t :inherit highlight)))))
(custom-set-faces '(completions-common-part ((t :underline t :weight bold))))
(custom-set-faces '(org-table ((t :foreground unspecified))))
(custom-set-faces '(link ((t :foreground "DodgerBlue" :underline t))))
(custom-set-faces '(hs-ellipsis ((t :box unspecified :underline t))))
(custom-set-faces '(compilation-info ((t :foreground "#448c27" :inherit bold))))
(set-face-attribute 'region nil :background "lightgoldenrod2" :foreground "#202225" :extend nil)
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME\\|HACK\\|TODO\\|WIP\\|BUG\\)\\( \\|:\\)" 1 'match t)
                   (";" . 'shadow)))))
;;; Programming stuff ---
(dolist (fn '(hs-minor-mode display-line-numbers-mode ; hl-line-mode which-function-mode
                            show-paren-mode completion-preview-mode goto-address-mode))
  (add-hook 'prog-mode-hook fn))
(with-eval-after-load 'eglot
  (setq python-flymake-command '("ruff" "check" "--output-format=concise" "--stdin-filename" "stdin" "-"))
  (add-to-list 'eglot-server-programs '((python-ts-mode python-mode) . ("uvx" "ty" "server")))
  (defun my-eglot-organize-imports () (interactive)
         (ignore-errors (eglot-code-actions nil nil "source.organizeImports" t))))
(add-hook 'eglot-managed-mode-hook
          (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)
            (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
            (when (eq major-mode 'python-mode)
              (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
            (when (eq major-mode 'go-ts-mode)
              (setq eldoc-documentation-functions
                    (remove #'eglot-signature-eldoc-function eldoc-documentation-functions)))))
(with-eval-after-load 'which-func ; disabled because this causes scroll slowdown
  (setq which-func-format (list (cadr which-func-format)) which-func-unknown "" hich-func-update-delay 1)
  (set-face-attribute 'which-func nil :foreground 'unspecified :inherit 'mode-line))
(dolist (mode '(rust-ts-mode-hook go-ts-mode-hook python-mode-hook c++-mode-hook)) (add-hook mode #'eglot-ensure))
(dolist (mode-hook '(conf-mode-hook yaml-ts-mode-hook)) (add-hook mode-hook #'display-line-numbers-mode))
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "C-s" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "C-r" #'completion-preview-prev-candidate))
(with-eval-after-load 'compile
  (setq compilation-scroll-output 'first-error)
  (push 'go-test compilation-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-test
                 . (".*?\\([[:alnum:]_./-]+\\.go\\):\\([0-9]+\\)\\(?:\\(?::\\([0-9]+\\)\\)?\\| \\+0x[0-9a-f]+\\)"
                    1 2 3 nil 1)))
  (add-hook 'compilation-filter-hook
            (lambda nil (goto-address-mode -1)
              (unless (eq major-mode 'grep-mode) (ansi-color-compilation-filter) (ansi-osc-compilation-filter)))))
(add-hook 'eshell-mode-hook #'compilation-shell-minor-mode)
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
(setq inferior-lisp-program "clojure")
;;; Miscellaneous ---
(setq shr-max-image-proportion 0.5 shr-use-colors nil)
(defun my-shr-tag-render (tag face-spec)
  (let ((default-renderer (intern (format "shr-tag-%s" tag))))
    (lambda (dom) (let ((start (point)))
                    (funcall default-renderer dom) (add-face-text-property start (point) face-spec)))))
(with-eval-after-load 'shr
  (set-face-attribute 'shr-mark nil :foreground 'unspecified :background 'unspecified)
  (setq shr-external-rendering-functions
        `((pre        . ,(my-shr-tag-render 'pre        '(:inherit highlight :extend t)))
          (blockquote . ,(my-shr-tag-render 'blockquote '(:slant italic)))
          (h1         . ,(my-shr-tag-render 'h1         '(:inherit bold :height 1.3)))
          (h2         . ,(my-shr-tag-render 'h2         '(:inherit bold :height 1.2)))
          (h3         . ,(my-shr-tag-render 'h3         '(:inherit bold :height 1.2))))))
(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook #'viper-mode)
  (setq eww-header-line-format nil eww-auto-rename-buffer 'title
        eww-default-download-directory "~/Downloads/eww/" browse-url-new-window-flag t))
(defun compile-at-root nil (interactive) "Run compile command at project root."
       (let ((default-directory (project-root (project-current nil))))
         (call-interactively 'compile)))
(setq completion-ignore-case t completion-auto-help nil ;'visible
      completion-styles '(initials partial-completion basic flex))
(add-to-list 'display-buffer-alist
             '("\\*\\(Completions\\|xref\\|Occur.*\\|compilation.*\\|Flymake.*\\|vc-git :.*\\)\\*"
               (display-buffer-in-side-window) (side . bottom) (window-height . 0.25)
               (window-parameters . ((mode-line-format . none)))))
(defun my/display-buffer-adaptive (buffer alist)
  (let ((side (if (< (frame-width) 160) 'bottom 'right))
        (size-param (if (< (frame-width) 160) '(window-height . 0.25) '(window-width . 82))))
    (display-buffer-in-side-window buffer (append `((side . ,side) ,size-param) alist))))
(add-to-list 'display-buffer-alist
             '("\\*\\(Dictionary\\|eldoc\\)\\*" my/display-buffer-adaptive
               (body-function . select-window) (window-parameters . ((split-window . #'ignore)))))
(defvar insert-pair-map ;; src: oantolin
  (let ((map (make-sparse-keymap))) (define-key map [t] #'insert-pair) map))
(defun my/delete-pair () (interactive)
       (if (use-region-p) (progn (goto-char (region-beginning)) (delete-pair))
         (mark-inner) (my/delete-pair)))
(setq dictionary-server "localhost")
(defun definition-at-point nil (interactive)
       (if (use-region-p)
           (dictionary-new-search (cons (buffer-substring-no-properties (mark) (point)) dictionary-default-dictionary))
         (dictionary-lookup-definition)))
(custom-set-faces '(dictionary-word-definition-face ((t :family unspecified))))
;;; Sensible changes ---
(defun prot-quit (&optional interactive) "A sensible `keyboard-quit'."
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
  (let ((open (cond ((eq (car (syntax-after (point))) 4) (point))
                    ((eq (car (syntax-after (1- (point)))) 5)
                     (save-excursion (backward-sexp) (point)))
                    ((nth 1 (syntax-ppss))))))
    (save-excursion
      (when open (goto-char open))
      (if (fboundp 'treesit-show-paren-data) (treesit-show-paren-data) (show-paren--default)))))
(add-hook 'prog-mode-hook (lambda nil (setq-local show-paren-data-function #'my/show-paren-data)))

(defvar zen-enabled-modes '(Info-mode diff-mode eww-mode dired-mode gnus-article-mode gnus-group-mode erc-mode))
(defun zen-buffer-apply-margins nil "Apply zen margins to all windows."
       (walk-windows
        (lambda (win)
          (with-current-buffer (window-buffer win)
            (when (or (derived-mode-p '(prog-mode text-mode)) (member major-mode zen-enabled-modes))
              (let* ((special-modes (member major-mode '(org-mode markdown-ts-mode)))
                     (margin (max 0 (/ (- (window-total-width win) fill-column) 2)))
                     (lmargin (if special-modes (max 0 (- margin 10)) margin)))
                (if (> (window-total-width win) 140)
                    (progn (visual-line-mode 1) (set-window-margins win lmargin margin)
                           (when special-modes (text-scale-set 1) (setq-local line-spacing '(0.3 . 0.3))))
                  (progn (set-window-margins win nil)
                         (when special-modes (text-scale-set 0) (setq-local line-spacing '(3 . 3)))))))))
        nil t))
(add-hook 'window-configuration-change-hook #'zen-buffer-apply-margins)

(defun epop nil (interactive) (defvar eshell-buffer-name)
       (let* ((display-buffer-alist `(("\\*eshell-pop.*\\*"
                                       (display-buffer-in-side-window)
                                       (side . bottom) (slot . -2) (window-height . 0.25))))
              (dir (if-let* ((proj (project-current)))
                       (file-name-nondirectory (directory-file-name (project-root proj)))
                     default-directory))
              (eshell-buffer-name (concat "*eshell-pop:*" dir))
              (inhibit-message t) (mode-line-format nil))
         (eshell)))
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
(defun mark-inner nil (interactive)
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
(defun file-capf ()
  "File completion at point function. src: eshelyaron."
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (when bounds
      (list (car bounds) (cdr bounds) #'completion-file-name-table
            :annotation-function (lambda (_) " File")
            :exclusive 'no))))
(add-hook 'completion-at-point-functions #'file-capf)
;; vc
(define-advice ediff-vc-internal (:around (orig-fun &rest args) custom-quit)
  (apply orig-fun args) (switch-to-buffer "*Ediff Control Panel*")
  (define-key ediff-mode-map (kbd "q")
              (lambda () (interactive)
                (let ((rev (if (string-match-p "\\*vc-\\|\\*ediff-revision" (buffer-name ediff-buffer-A))
                               ediff-buffer-B ediff-buffer-A))
                      (a ediff-buffer-A) (b ediff-buffer-B))
                  (ediff-really-quit nil) (kill-buffer rev) (switch-to-buffer (if (eq rev b) a b))))))
(add-hook 'ediff-before-setup-hook #'tab-bar-new-tab)
(add-hook 'ediff-quit-hook (lambda nil (tab-bar-close-tab) (kill-buffer ediff-registry-buffer)))
(with-eval-after-load 'ediff (advice-add 'ediff-quit :around (lambda (&rest args) (ediff-really-quit args))))
(with-eval-after-load 'smerge-mode
  (define-key ctl-x-map (kbd ",") smerge-basic-map)
  (repeat-mode 1) (setq diff-refine 'navigation)
  (map-keymap (lambda (_key cmd)
                (when (symbolp cmd) (put cmd 'repeat-map 'smerge-basic-map)))
              smerge-basic-map))
(with-eval-after-load 'log-edit
  (define-advice log-edit-show-files (:after (&rest _args) show-diff)
    (let ((orig-window (selected-window)))
      (log-edit-show-diff) (select-window orig-window))
    (setq-local other-window-scroll-buffer (get-buffer "*vc-diff*")))
  (defun my/vc-cleanup-buffers ()
    (dolist (buf '("*log-edit-files*" "*vc-diff*" "*vc*")) (when-let* ((b (get-buffer buf))) (kill-buffer b))))
  (dolist (fn '(log-edit-done log-edit-kill-buffer)) (advice-add fn :after #'my/vc-cleanup-buffers)))

(defun ediff-pr--blob (ref file)
  "Read-only buffer with FILE at REF."
  (with-current-buffer (get-buffer-create (format "*%s:%s*" ref (file-name-nondirectory file)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ignore-errors (vc-git-command t 0 nil "show" (concat ref ":" file)))
      (setq buffer-file-name file) (set-auto-mode)
      (setq buffer-file-name nil buffer-read-only t))
    (current-buffer)))
(defun ediff-pr (pr-number base-branch)
  "Review PR-NUMBER against BASE-BRANCH with ediff."
  (interactive "nPR number: \nsBase branch (default main): ")
  (require 'project)
  (let* ((default-directory (project-root (project-current)))
         (base (concat "origin/" (if (string-empty-p base-branch) "main" base-branch))))
    (message "Fetching PR #%s..." pr-number)
    (with-temp-buffer
      (vc-git-command t 0 nil "fetch" "origin" (if (string-empty-p base-branch) "main" base-branch))
      (vc-git-command t 0 nil "fetch" "origin" (format "pull/%s/head" pr-number)))
    (cl-flet ((diff-files (&rest args)
                (split-string (with-temp-buffer
                                (apply #'vc-git-command t 0 nil "diff" "--name-only"
                                       (append args (list (concat base "..FETCH_HEAD"))))
                                (string-trim (buffer-string)))
                              "\n" t)))
      (let* ((files (diff-files))
             (added (diff-files "--diff-filter=A"))
             (deleted (diff-files "--diff-filter=D")))
        (if (null files) (message "No changed files in PR %s" pr-number)
          (with-current-buffer (get-buffer-create (format "*PR #%s:%s*" pr-number default-directory))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "PR #%s — %d files  (TAB/S-TAB to navigate)\n\n" pr-number (length files)))
              (dolist (f files)
                (insert-text-button f 'face (cond ((member f added) 'success) ((member f deleted) 'error) (t 'button))
                                    'action
                                    (lambda (btn)
                                      (ediff-buffers (ediff-pr--blob base (button-label btn))
                                                     (ediff-pr--blob "FETCH_HEAD" (button-label btn)))
                                      (let ((o (make-overlay (button-start btn) (button-end btn))))
                                        (overlay-put o 'face 'shadow)))
                                    'follow-link t)
                (insert "\n"))
              (goto-char (point-min))
              (special-mode) (local-set-key "SPC" #'ctl-x-map))
            (switch-to-buffer (current-buffer))))))))
;; org
;; (run-with-idle-timer 5 nil #'require 'org)
(with-eval-after-load 'org
  (require 'org-tempo)
  (with-eval-after-load 'org-src
    (nconc org-src-lang-modes
           '(("rust" . rust-ts) ("python" . python-ts) ("go" . go-ts) ("bash" . bash-ts)
             ("typescript" . typescript-ts) ("javascript" . js-ts) ("json" . json-ts)
             ("yaml" . yaml-ts) ("toml" . toml-ts) ("c" . c-ts) ("cpp" . c++-ts))))
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t) (shell . t) (python . t) (emacs-lisp . t)))
  (setq org-confirm-babel-evaluate nil))
;;; Nov.el alternative (claude)
(defun epub--xml (path)
  (with-temp-buffer (insert-file-contents path) (libxml-parse-xml-region (point-min) (point-max))))
(defun epub-open (&optional file) (interactive "fEPUB: ")
       (require 'shr)
       (let* ((file (or file buffer-file-name))
              (dir (make-temp-file "epub-" t))
              (_ (call-process "unzip" nil nil nil "-qq" "-od" dir file))
              (opf-path (dom-attr (car (dom-by-tag (epub--xml (expand-file-name "META-INF/container.xml" dir)) 'rootfile)) 'full-path))
              (opf-dir (file-name-directory (expand-file-name opf-path dir)))
              (opf (epub--xml (expand-file-name opf-path dir)))
              (manifest (mapcar (lambda (i) (cons (dom-attr i 'id) (expand-file-name (url-unhex-string (dom-attr i 'href)) opf-dir)))
                                (dom-by-tag opf 'item))))
         (let ((inhibit-read-only t)) (erase-buffer)
              (dolist (f (delq nil (mapcar (lambda (r) (cdr (assoc (dom-attr r 'idref) manifest))) (dom-by-tag opf 'itemref))))
                (when (string-match-p "\\.x?html?\\'" f)
                  (shr-insert-document
                   (with-temp-buffer (insert-file-contents f)
                                     (while (re-search-forward "\\(src\\|href\\)=\"\\([^\"]+\\)\"" nil t)
                                       (let ((v (match-string 2)))
                                         (unless (string-match-p "^[a-z]+://" v)
                                           (replace-match
                                            (format "%s=\"file://%s\"" (match-string 1)
                                                    (expand-file-name (url-unhex-string v) (file-name-directory f))) t t))))
                                     (libxml-parse-html-region (point-min) (point-max)))))))
         (goto-char (point-min)) (eww-mode) (viper-mode) (set-buffer-modified-p nil)
         (setq buffer-file-name file default-directory (file-name-directory file))
         (setq-local revert-buffer-function #'ignore write-contents-functions '(ignore))
         (add-hook 'after-change-functions (lambda (&rest _) (set-buffer-modified-p nil)) nil t)
         (add-hook 'kill-buffer-hook (lambda () (delete-directory dir t)) nil t)))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . epub-open))
;;; Mode-line ---
(setq-default mode-line-collapse-minor-modes '(not flymake-mode defining-kbd-macro)
              mode-line-end-spaces nil mode-line-compact t flymake-mode-line-title nil)
(let ((common (list :background 'unspecified :foreground 'unspecified
                    :inverse-video (not (display-graphic-p))
                    :height (if (eq system-type 'android) 160 140)
                    :box '(:line-width 1 :style flat-button)
                    :overline (face-foreground 'shadow))))
  (apply #'set-face-attribute 'mode-line-active nil :inherit 'default common)
  (apply #'set-face-attribute 'mode-line-inactive nil :inherit 'shadow common))
(defvar my/tab-keymaps (let ((v (make-vector 20 nil)))
                         (dotimes (i 20 v) (let ((m (make-sparse-keymap)))
                                             (define-key m [mode-line mouse-1]
                                                         `(lambda () (interactive) (tab-bar-select-tab ,(1+ i))))
                                             (aset v i m)))))
(defun my/tab-bar--update-indicator (&rest _)
  (let* ((tabs (tab-bar-tabs)) (n (length tabs)) (cur (tab-bar--current-tab-index tabs)))
    (setq-default mode-line-front-space
                  (if (> n 1)
                      (concat " " (mapconcat (lambda (i) (propertize (if (= i cur) "⦿" "○")
                                                                      'mouse-face 'mode-line-highlight
                                                                      'local-map (aref my/tab-keymaps i)))
                                             (number-sequence 0 (1- n)) " ") " ") ""))))
(setq-default mode-line-front-space "")
(dolist (fn '(tab-bar-new-tab tab-bar-close-tab)) (advice-add fn :after #'my/tab-bar--update-indicator))
(with-eval-after-load 'tab-bar (add-hook 'tab-bar-tab-post-select-functions #'my/tab-bar--update-indicator))
(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:eval (when (and (not (display-graphic-p)) (boundp 'viper-mode-string)) (concat " " viper-mode-string)))
                (:propertize " %+  " display (min-width (6.0)))
                (:eval (propertize "%b" 'face 'bold 'help-echo (buffer-file-name)))
                (:eval (propertize (string-trim-left (format-mode-line vc-mode))))
                (:propertize "   "   display (min-width (4.0))) mode-line-position
                mode-line-format-right-align
                mode-line-modes mode-line-misc-info mode-line-end-spaces))
(with-eval-after-load 'viper
  (setq global-mode-string '((:eval (unless (derived-mode-p 'prog-mode) (format-time-string "%a %H:%M"))))))
(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        '((which-function-mode (which-func-mode (which-func--use-mode-line ("" which-func-format " "))))
          (global-mode-string ("" global-mode-string))
          (:eval (when (and (bound-and-true-p eglot--managed-mode) (eglot-managed-p)) eglot-mode-line-progress)))))
;;; Apps ---
(setq erc-kill-queries-on-quit t erc-kill-server-buffer-on-quit t erc-join-buffer 'buffer
      erc-fill-function 'erc-fill-static erc-fill-static-center 18
      erc-prompt-for-password nil erc-use-auth-source-for-nickserv-password t
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE" "353" "366")
      erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#emacs-social" "##rust"
                                     "#zig" "#janet" "#clojure" "#racket" "#ocaml")))
(defun my-erc-tls () (interactive) (erc-tls :server "irc.libera.chat" :port 6697 :nick "brongulus"))
(with-eval-after-load 'erc
  (dolist (mod '(keep-place log nicks services xdcc)) (push mod erc-modules))
  (with-eval-after-load 'erc-track
    (define-key erc-track-minor-mode-map "\C-j" #'erc-track-switch-buffer))
  (erc-fill-mode 1) (erc-timestamp-mode -1) (erc-update-modules))
(setq gnus-directory "~/.emacs.d/gnus" gnus-startup-file "~/.emacs.d/.newsrc"
      gnus-use-dribble-file nil gnus-always-read-dribble-file nil
      gnus-interactive-exit nil gnus-widen-article-window t
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-use-adaptive-scoring '(word line) gnus-summary-expunge-below 0
      gnus-select-method '(nntp "news.gwene.org") gnus-group-uncollapsed-levels 2
      gnus-sum-thread-tree-indent " " gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-root "" gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-single-leaf     "╰─►"
      gnus-user-date-format-alist '(((gnus-seconds-today) . " %H:%M") (t . "%b %d"))
      gnus-topic-line-format "%(%{%n - %A%}%) %v\n" gnus-group-line-format "%S%4y: %(%-40,40c%)\n"
      gnus-summary-line-format (concat " %0{%U%R%}" "%1{%&user-date;%}" "%3{ %}" " "
                                       "%4{%-16,16f%}" " " "%3{ %}" " " "%1{%B%}" "%S\n"))
(with-eval-after-load 'gnus
  (advice-add 'gnus-splash :before #'tab-bar-new-tab)
  (add-hook 'gnus-after-exiting-gnus-hook #'tab-bar-close-tab)
  (with-eval-after-load 'gnus-sum
    (define-key gnus-summary-mode-map (kbd "RET") #'gnus-summary-select-article-buffer))
  (add-hook 'gnus-summary-prepare-hook (lambda () (setq-local truncate-lines t)))
  (with-eval-after-load 'gnus-art
    (dolist (binding '(("q" . quit-window) ("j" . next-line) ("k" . previous-line)))
      (keymap-set gnus-article-mode-map (car binding) (cdr binding))))
  (add-hook 'gnus-article-mode-hook
            (lambda () (setq-local browse-url-browser-function #'eww-browse-url))))
(with-eval-after-load 'mpc
  (setq mpc-browser-tags '(Directory) mpc-mpd-music-directory "~/Downloads/music"
        mpc-songs-format "%-5{Time} %25{Title} %20{Album} %20{Artist}")
  (advice-add 'mpc :before (lambda (&rest _args) (tab-bar-new-tab)))
  (advice-add 'mpc :after (lambda (&rest _args) (call-interactively 'window-layout-transpose)))
  (advice-add 'mpc-quit :after (lambda (&rest _args) (tab-bar-close-tab)))
  (defun my-mpc-tagbrowser-toggle ()
    "Toggle directory at point."
    (interactive)
    (let ((name (buffer-substring (line-beginning-position) (line-end-position)))
          (prop (if (stringp mpc-tag) (intern mpc-tag) mpc-tag))
          (proc (mpc-proc)))
      (if (not (member name (process-get proc prop)))
          (process-put proc prop (cons name (process-get proc prop)))
        (let ((new (delete name (process-get proc prop))))
          (setq name (concat name "/"))
          (process-put proc prop (delq nil (mapcar (lambda (x) (if (string-prefix-p name x) nil x)) new)))))
      (mpc-tagbrowser-refresh)))
  (dolist (binding '(("<f7>" . mpc-prev) ("<f8>" . mpc-toggle-play) ("<f9>" . mpc-next)))
    (keymap-global-set (car binding) (cdr binding)))
  (dolist (map (list mpc-tagbrowser-dir-mode-map mpc-status-mode-map mpc-songs-mode-map))
    (define-key map (kbd "SPC") ctl-x-map)
    (define-key map (kbd "p") 'mpc-toggle-play)
    (define-key map (kbd "U") 'mpc-update))
  (define-key mpc-tagbrowser-mode-map (kbd "TAB") 'my-mpc-tagbrowser-toggle)
  (define-key mpc-tagbrowser-mode-map (kbd "RET") 'mpc-play-at-point))
;;; eldoc-box --- I need this man... ;-;
(with-eval-after-load 'eglot (load "~/.emacs.d/eldoc-box" :noerr :no-message))
(setq eldoc-box-clear-with-C-g t)
(defun my/eldoc-get-help () (interactive)
       (if (derived-mode-p 'emacs-lisp-mode) (describe-symbol (symbol-at-point))
         (if (and (display-graphic-p) (symbolp 'eldoc-box-help-at-point))
             (eldoc-box-help-at-point)
           (eldoc-doc-buffer t))))
(with-eval-after-load 'eldoc
  (with-eval-after-load 'eldoc-box
    (setq eldoc-box-max-pixel-width 800 eldoc-box-max-pixel-height 700 eldoc-box-only-multi-line t)))
;;; mark-multiple clone --- This was an experiment to see how far opus 4.6 can go
(load "~/.emacs.d/mini-mark-multiple" :noerr :no-message)
(define-key (current-global-map) (kbd "M-p") #'mmm/mark-previous-like-this)
(define-key (current-global-map) (kbd "M-n") #'mmm/mark-next-like-this)
(define-key (current-global-map) (kbd "M-'") #'mmm/mark-all-like-this)
(define-key (current-global-map) (kbd "M-r") #'mmm/mark-all-in-defun)
