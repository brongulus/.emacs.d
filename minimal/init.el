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
      org-edit-src-content-indentation 0 org-src-preserve-indentation t
	  org-fontify-quote-and-verse-blocks t org-fontify-whole-heading-line t
	  treesit-enabled-modes t treesit-font-lock-level 2 go-ts-mode-indent-offset 4
	  isearch-regexp-lax-whitespace t isearch-lazy-count t lazy-highlight-initial-delay 0
	  isearch-repeat-on-direction-change t isearch-wrap-pause 'no-ding
	  search-whitespace-regexp ".*?" dired-kill-when-opening-new-dired-buffer t
      dired-listing-switches "-l --almost-all --human-readable --group-directories-first"
      delete-by-moving-to-trash t help-window-select t kill-region-dwim 'emacs-word
      eglot-ignored-server-capabilities
      '(:inlayHintProvider :workspace.didChangeWatchedFiles :colorProvider :codeLensProvider
                           :foldingRangeProvider :semanticTokensProvider :documentHighlightProvider)
      eglot-sync-connect 0 eglot-autoshutdown t jsonrpc-event-hook nil
      maximum-scroll-margin 0.5 scroll-margin 9999 scroll-conservatively 101
      scroll-preserve-screen-position t fast-but-imprecise-scrolling t doc-view-continuous t
      require-final-newline t resize-mini-windows t ring-bell-function 'ignore tab-always-indent 'complete
      diff-font-lock-syntax nil vc-allow-rewriting-published-history t vc-follow-symlinks t
      vc-display-status 'no-backend vc-git-diff-switches '("--patch-with-stat" "--histogram")
      project-vc-extra-root-markers '("Cargo.toml" "build.zig" "go.work")
      eshell-banner-message "" eshell-hist-ignoredups 'erase eshell-history-size 20000
      eshell-save-history-on-exit t eshell-glob-case-insensitive t eshell-scroll-to-bottom-on-input 'this
      grep-command "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)"
      grep-command-position 27 ido-enable-flex-matching t ido-everywhere t
      ido-ignore-buffers
      '("\\` " "\\*Messages\\*" "\\*scratch\\*" "\\*Completions\\*" "\\*Native-compile-Log\\*"
        "\\*Async-native-compile-log\\*" "\\*EGLOT.*events\\*" "\\*Flymake.*\\*" "\\*MPC.*\\*"
        "\\*Buffer List\\*" "\\*Help\\*" "\\*Minibuf-.*\\*" "\\*vc-.*\\*")
      ido-create-new-buffer 'always ido-use-virtual-buffers 'auto
      ido-show-dot-for-dired t ido-max-prospects 6 ido-auto-merge-work-directories-length -1
      Info-default-directory-list '("~/.emacs.d/info") Info-use-header-line nil)
(if (not (eq system-type 'android)) (setq shell-file-name "~/.nix-profile/bin/fish")
  (setq-default fill-column 120 line-spacing '(4 . 4)))
(put 'narrow-to-region 'disabled nil)
(setcdr (assq 'continuation fringe-indicator-alist) '(nil nil))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(blink-cursor-mode -1) (tooltip-mode -1) (menu-bar-mode -1) (scroll-bar-mode -1)
(tool-bar-mode -1) (line-number-mode -1) (kill-ring-deindent-mode 1)
(global-visual-line-mode 1) (global-visual-wrap-prefix-mode 1) (electric-pair-mode 1)
(add-hook 'emacs-startup-hook
          (lambda () (ido-mode 'both) (global-auto-revert-mode 1) (viper-mode) (fido-vertical-mode)
            (repeat-mode 1) (save-place-mode 1) (delete-selection-mode 1) (savehist-mode 1)))
(defun disable-fido-vertical (orig-fun &rest args)
  (icomplete-vertical-mode -1) (unwind-protect (apply orig-fun args) (fido-vertical-mode 1)))
(dolist (fn '(find-file find-file-other-window read-extended-command project-switch-to-buffer))
  (advice-add fn :around #'disable-fido-vertical))
(dolist (hook '(text-mode-hook eshell-mode-hook)) (add-hook hook #'goto-address-mode))
;;; Keys ---
(setq viper-mode t viper-expert-level 5 viper-ex-style-motion nil
      viper-inhibit-startup-message t viper-want-ctl-h-help t
      viper-want-emacs-keys-in-insert t viper-want-emacs-keys-in-vi t
      viper-ex-style-editing nil viper-insert-state-cursor-color nil)
(with-eval-after-load 'viper
  (advice-add 'viper-post-command-sentinel :override #'ignore)
  (defun viper-set-insert-cursor-type nil (setq cursor-type '(bar . 3)))
  (define-key viper-insert-basic-map "\C-d" #'viper-del-backward-char-in-insert)
  (dolist (key '("\C-b" "\C-d" "\C-e" "\C-f" "\C-u" "\C-y" "\C-v"))
    (define-key viper-vi-basic-map key nil))
  (define-key viper-vi-basic-map (kbd "SPC") ctl-x-map)
  (dolist (binding '(("g" . nil) ("x" . sel-line) ("-" . negative-argument) ("y" . kill-ring-save)
                     ("C-\\" . epop) ("R" . replace-regexp) ("=" . mark-inner) ("d" . del-vi)
                     ("g i" . eglot-find-implementation) ("g r" . xref-find-references)
                     ("C" . string-rectangle) ("p" . yank) ("+" . eglot-rename) ("g s" . imenu)
                     ("z f" . hs-toggle-hiding) ("z c" . hs-hide-all) ("z s" . hs-show-all)
                     ("[" . previous-error) ("]" . next-error)
                     ("q" . quit-window) ("j" . next-line) ("k" . previous-line)
                     ("<" . beginning-of-buffer) (">" . end-of-buffer) ("o" . other-window)
                     ("v" . set-mark-command) ("s" . isearch-forward-regexp) ("u" . undo-only)
                     ("Z" . undo-redo) ("," . my-scroll-other-down) ("." . my-scroll-other-up)
                     ("g a" . beginning-of-defun) ("g e" . end-of-defun)
                     ("(" . flymake-goto-prev-error) (")" . flymake-goto-next-error)
                     ("g z" . pop-to-mark-command) ("g /" . xref-find-definitions-other-window)
                     ("K" . eldoc-doc-buffer) ("*" . isearch-forward-symbol-at-point)))
    (keymap-set viper-vi-basic-map (car binding) (cdr binding))))
(unless (display-graphic-p)
  (add-hook 'viper-vi-state-hook (lambda () (send-string-to-terminal "\e[2 q")))
  (add-hook 'viper-insert-state-hook (lambda () (send-string-to-terminal "\e[6 q")))
  (add-hook 'viper-replace-state-hook (lambda () (send-string-to-terminal "\e[4 q")))
  (add-hook 'kill-emacs-hook (lambda () (send-string-to-terminal "\e[2 q"))))
(dolist (binding '(("C-x c c" . compile) ("C-x c r" . recompile) ("C-h '" . describe-face)
                   ("C-x C-m" . execute-extended-command) ("C-x k" . kill-current-buffer)
                   ("M-o" . other-window) ("<escape>" . keyboard-escape-quit) ("C-\\" . epop)
                   ("C-x ;" . comment-line) ("C-x x c" . save-buffers-kill-emacs)
                   ("C-x x b" . ibuffer) ("M-;" . eval-expression)
                   ("C-," . my-scroll-other-down) ("M-j" . window-toggle-side-windows)
                   ("C-<tab>" . tab-next) ("C-S-<tab>" . tab-previous) ("C-x x f" . find-file)
                   ("C-x x s" . save-buffer) ("C-x x e" . eval-defun) ("C-x x z" . restart-emacs)
                   ("C-." . my-scroll-other-up) ("C-x x x" . flymake-show-project-diagnostics)))
  (keymap-global-set (car binding) (cdr binding)))
(keymap-global-set "C-x m" esc-map)
(keymap-global-set "C-x 6" #'(lambda nil (interactive) (invert-face 'default)))
(keymap-global-set "j" #'(lambda nil (interactive)
                           (let* ((event (read-event nil nil 0.4)))
                             (if event (if (and (characterp event) (= event ?k))
                                           (viper-change-state-to-vi)
                                         (insert ?j) (push event unread-command-events))
                               (insert ?j)))))
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
(set-face-attribute 'vertical-border nil :foreground 'unspecified :inherit '(shadow default))
(set-face-attribute 'font-lock-comment-face nil :foreground 'unspecified :inherit 'shadow)
(set-face-attribute 'fringe nil :background 'unspecified)
(set-face-attribute 'default nil :foreground "#202225" :background "#eae8e1")
(custom-set-faces '(bold ((((background dark)) :foreground "#fafbfc" :weight bold)
                          (((background light)) :weight bold))))
(add-hook 'post-command-hook
          (lambda () (unless (eq (buffer-modified-p) (bound-and-true-p curs-mod))
                       (set-cursor-color (if (setq curs-mod (buffer-modified-p)) "coral3" "#00c2ff")))))
(custom-set-faces '(font-lock-string-face ((((background dark))  :foreground "#deb07a")
                                           (((background light)) :foreground "#0031a9"))))
(dolist (face '(font-lock-type-face font-lock-constant-face viper-minibuffer-insert
                                    font-lock-keyword-face font-lock-variable-name-face))
  (custom-set-faces `(,face ((t :foreground unspecified :background unspecified)))))
(set-face-attribute 'font-lock-builtin-face nil :foreground 'unspecified :slant 'italic)
(set-face-attribute 'error nil :foreground "Coral3")
(custom-set-faces '(ido-subdir ((t :inherit font-lock-string-face))))
(dolist (face '(eshell-prompt minibuffer-prompt font-lock-function-name-face line-number-current-line))
  (custom-set-faces `(,face ((t :foreground unspecified :inherit bold)))))
(dotimes (i 9) (let ((face (intern (format "outline-%d" (1+ i)))))
                 (custom-set-faces `(,face ((t :height 1.1 :inherit bold))))))
(custom-set-faces '(highlight ((((background dark))  :background "#32353a")
                               (((background light)) :background "#d9d7d0"))))
(custom-set-faces '(eglot-highlight-symbol-face ((t :inherit (highlight default)))))
(dolist (face '(org-block org-block-begin-line org-block-end-line))
  (custom-set-faces `(,face ((t :inherit (highlight default) :extend t)))))
(dolist (face '(org-code org-verbatim)) (custom-set-faces `(,face ((t :inherit highlight)))))
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
  (defun my-eglot-organize-imports () (interactive)
         (ignore-errors (eglot-code-actions nil nil "source.organizeImports" t))))
(add-hook 'eglot-managed-mode-hook
          (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)
            (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
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
(setq shr-max-image-proportion 0.5)
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
  (setq eww-header-line-format nil eww-auto-rename-buffer 'title))
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

(defun my/show-paren-data nil
  (or (and (boundp 'treesit-show-paren-data) (treesit-show-paren-data))
      (when-let* ((open (cond ((eq (car (syntax-after (point))) 4) (point))
                              ((eq (car (syntax-after (1- (point)))) 5)
                               (save-excursion (backward-sexp) (point)))
                              ((nth 1 (syntax-ppss)))))
                  (end (save-excursion (goto-char open) (forward-sexp) (point)))
                  ((> end open)))
        (list open (1+ open) (1- end) end))))
(add-hook 'prog-mode-hook (lambda nil (setq-local show-paren-data-function #'my/show-paren-data)))

(defun zen-buffer-apply-margins nil "Apply zen margins to all windows."
       (walk-windows
        (lambda (win)
          (with-current-buffer (window-buffer win)
            (when (or (derived-mode-p '(prog-mode text-mode))
                      (member major-mode '(Info-mode diff-mode eww-mode dired-mode)))
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
;; org
(run-with-idle-timer 5 nil #'require 'org)
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
;;; Mode-line
(setq-default mode-line-collapse-minor-modes '(not flymake-mode defining-kbd-macro)
              mode-line-end-spaces nil mode-line-compact t flymake-mode-line-title nil)
(let ((common (list :background 'unspecified :foreground 'unspecified
                    :inverse-video (not (display-graphic-p))
                    :height (if (eq system-type 'android) 160 140)
                    :box '(:line-width 1 :style flat-button)
                    :overline (face-foreground 'shadow))))
  (apply #'set-face-attribute 'mode-line-active nil :inherit 'default common)
  (apply #'set-face-attribute 'mode-line-inactive nil :inherit 'shadow common))
(defvar tab-bar--tab-keymaps
  (let ((v (make-vector 20 nil)))
    (dotimes (i 20 v) (let ((m (make-sparse-keymap)))
                        (define-key m [mode-line mouse-1] `(lambda () (interactive) (tab-bar-select-tab ,(1+ i))))
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
(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:eval (unless (display-graphic-p) (concat " " viper-mode-string)))
                (:propertize " %+  " display (min-width (6.0))) "%b"
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
;;; Apps
(setq erc-kill-queries-on-quit t erc-kill-server-buffer-on-quit t erc-join-buffer 'buffer
      erc-fill-function 'erc-fill-static erc-fill-static-center 18
      erc-prompt-for-password nil erc-use-auth-source-for-nickserv-password t
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE" "353" "366")
      erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#emacs-social" "##rust"
                                     "#zig" "#janet" "#racket" "#ocaml")))
(with-eval-after-load 'erc
  (dolist (mod '(keep-place log nicks services xdcc)) (push mod erc-modules))
  (define-key erc-track-minor-mode-map "\C-j" #'erc-track-switch-buffer)
  (erc-fill-mode 1) (erc-timestamp-mode -1) (erc-update-modules))
(setq gnus-directory (concat "~/.emacs.d" "/gnus")
      gnus-startup-file (concat "~/.emacs.d" "/.newsrc")
      gnus-use-dribble-file nil gnus-always-read-dribble-file nil
      gnus-use-adaptive-scoring '(word line) gnus-summary-expunge-below 0
      gnus-select-method '(nntp "news.gwene.org") gnus-group-uncollapsed-levels 2
      gnus-sum-thread-tree-false-root "" gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "" gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-single-leaf     "╰─►"
      gnus-user-date-format-alist '(((gnus-seconds-today) . " %H:%M") (t . "%b %d"))
      gnus-topic-line-format (concat "%(%{%n - %A%}%) %v\n")
      gnus-group-line-format (concat "%S%4y: %(%-40,40c%)\n") ;; %E (gnus-group-icon-list)
      ;;  06-Jan   Sender Name    Email Subject
      gnus-summary-line-format (concat " %0{%U%R%}" "%1{%-4,4i%}" " " "%1{%&user-date;%}"
                                       "%3{ %}" " " "%4{%-16,16f%}" " " "%3{ %}" " "
                                       "%1{%B%}" "%S\n"))
(with-eval-after-load 'gnus
  (add-hook 'gnus-article-mode-hook
            (lambda () (setq-local browse-url-browser-function #'eww-browse-url)))
  (advice-add 'gnus-splash :before #'tab-bar-new-tab)
  (add-hook 'gnus-after-exiting-hook #'tab-bar-close-tab))
(with-eval-after-load 'mpc
  (setq mpc-browser-tags '(Directory) mpc-mpd-music-directory "~/Downloads/music")
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
          (process-put
           proc prop (delq nil (mapcar (lambda (x) (if (string-prefix-p name x) nil x)) new)))))
      (mpc-tagbrowser-refresh)))
  (dolist (map (list mpc-tagbrowser-dir-mode-map mpc-status-mode-map mpc-songs-mode-map))
    (define-key map (kbd "SPC") ctl-x-map)
    (define-key map (kbd "p") 'mpc-toggle-play)
    (define-key map (kbd "U") 'mpc-update))
  (define-key mpc-tagbrowser-mode-map (kbd "TAB") 'my-mpc-tagbrowser-toggle)
  (define-key mpc-tagbrowser-mode-map (kbd "RET") 'mpc-play-at-point))
