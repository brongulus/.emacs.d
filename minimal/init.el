;;; init.el --- 🦬 -*- lexical-binding: t; -*
(setcdr (assq 'continuation fringe-indicator-alist) '(nil nil))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(blink-cursor-mode -1) (tooltip-mode -1) (menu-bar-mode -1) (scroll-bar-mode -1) (tool-bar-mode -1)
(which-key-mode 1) (global-auto-revert-mode 1) (kill-ring-deindent-mode 1) (save-place-mode 1)
(global-visual-line-mode 1) (global-goto-address-mode 1)
;;; Keys ---
(setq viper-mode t viper-expert-level 5
      viper-inhibit-startup-message t
      viper-want-ctl-h-help t
      viper-want-emacs-keys-in-insert t
      viper-want-emacs-keys-in-vi t)
(add-hook 'emacs-startup-hook #'viper-mode)
(with-eval-after-load 'viper
  (dolist (pair '((viper-exec-Yank . kill-ring-save) (viper-exec-Delete . kill-region)))
    (advice-add (car pair) :around
                (lambda (fn m-com com)
                  (if (use-region-p) (funcall (cdr pair) (region-beginning) (region-end))
                    (funcall fn m-com com)))))
  (dolist (key '("\C-b" "\C-d" "\C-e" "\C-f" "\C-u" "\C-y" "\C-v"))
    (define-key viper-vi-basic-map key nil))
  (define-key viper-vi-basic-map (kbd "SPC") ctl-x-map)
  (dolist (binding
           '(("z f" . hs-toggle-hiding) ("z c" . hs-hide-all) ("z s" . hs-show-all)
             ("<" . beginning-of-buffer) (">" . end-of-buffer) ("o" . other-window)
             ("v" . set-mark-command) ("s" . isearch-forward-regexp) ("u" . undo-only)
             ("Z" . undo-redo) ("," . my-scroll-other-down) ("." . my-scroll-other-up)))
    (keymap-set viper-vi-basic-map (car binding) (cdr binding))))
(dolist (binding
         '(("<escape>" . keyboard-escape-quit)
           ("C-x C-m" . execute-extended-command) ("C-x k" . kill-current-buffer)
           ("C-o" . other-window) ("C-h '" . describe-face)
           ("C-x ;" . comment-line) ("C-x x c" . save-buffers-kill-emacs)
           ("C-x x f" . find-file) ("C-x x s" . save-buffer)
           ("C-x x e" . eval-defun) ("C-x x z" . restart-emacs)
           ("C-x x x" . flymake-show-buffer-diagnostics)))
  (keymap-global-set (car binding) (cdr binding)))
(keymap-global-set "C-x m" esc-map)
(keymap-global-set "<f6>" #'(lambda nil (interactive) (invert-face 'default)))
;;; Visuals ---
(dolist (face '(default fixed-pitch variable-pitch))
  (set-face-attribute face nil :font "Input Mono Narrow" :height 140))
(add-hook 'post-command-hook
          #'(lambda nil (set-cursor-color (if (buffer-modified-p) "coral3" "#00c2ff"))))
(dolist (face '(vertical-border font-lock-comment-face))
  (set-face-attribute face nil :foreground 'unspecified :inherit '(shadow default)))
(set-face-attribute 'fringe nil :background 'unspecified)
(let ((common (list :background 'unspecified :foreground 'unspecified
                    :inverse-video (not (display-graphic-p)) :height 140
                    :box '(:line-width 1 :style flat-button)
                    :overline (face-foreground 'shadow))))
  (apply #'set-face-attribute 'mode-line nil
         :inherit 'default common)
  (apply #'set-face-attribute 'mode-line-inactive nil
         :inherit 'shadow common))
(set-face-attribute 'default nil :background "#222323" :foreground "#eae8e1")
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
                    ((((background dark))  :background "grey10")
                     (((background light)) :background "grey95"))))
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
;;; Programming stuff ---
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(dolist (mode '(rust-ts-mode-hook go-ts-mode-hook python-mode-hook c++-mode-hook))
  (add-hook mode #'(lambda nil (run-with-timer 0.3 nil #'eglot-ensure))))
(setq eglot-mode-line-format '(eglot-mode-line-progress))
(dolist (mode-hook '(prog-mode-hook conf-mode-hook yaml-ts-mode-hook))
  (add-hook mode-hook #'display-line-numbers-mode)
  (add-hook mode-hook #'completion-preview-mode))
(when (executable-find "rg")
  (setq grep-command "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)"
        grep-command-position 27))
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map
              "C-s" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map
              "C-r" #'completion-preview-prev-candidate))
(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook #'completion-preview-mode)
  (when (not (or (getenv "GCTL_SESSION_ID") (getenv "TERM_SESSION_ID")))
    (setenv "GCTL_SESSION_ID" (string-trim (shell-command-to-string "uuidgen"))))
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (eshell/addpath (concat (getenv "GOPATH") "/bin")))
;;; Better defaults ---
(setq-default auto-save-default nil
              auto-save-list-file-prefix nil
              create-lockfiles nil
              custom-file "/tmp/emacs-custom"
              cursor-in-non-selected-windows nil
              frame-resize-pixelwise t
              inhibit-startup-screen t
              make-backup-files nil
              mode-line-collapse-minor-modes t
              mode-line-end-spaces nil
              mode-line-compact t
              copy-region-blink-delay 0
              display-line-numbers-width 4
              truncate-lines nil
              tab-bar-show nil
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              warning-minimum-level :error
              use-short-answers t
              tab-width 4
              line-spacing 5
              indent-tabs-mode nil)
(setq enable-recursive-minibuffers t
	  minibuffer-default-prompt-format " [%s]"
	  minibuffer-visible-completions t
	  read-buffer-completion-ignore-case t
	  read-file-name-completion-ignore-case t
	  org-fontify-quote-and-verse-blocks t
	  org-fontify-whole-heading-line t
	  org-pretty-entities t
	  org-src-fontify-natively t
	  ;; treesit-enabled-modes t
	  treesit-font-lock-level 4
	  isearch-lax-whitespace t
	  isearch-lazy-count t
	  isearch-repeat-on-direction-change t
	  isearch-wrap-pause 'no-ding
	  search-whitespace-regexp ".*?"
	  copy-region-blink-predicate 'always
	  dired-kill-when-opening-new-dired-buffer t
      delete-by-moving-to-trash t
      help-window-select t
      kill-region-dwim 'emacs-word
	  eglot-autoshutdown t
	  eglot-ignored-server-capabilities '(:inlayHintProvider)
	  jsonrpc-event-hook nil
	  require-final-newline t
	  resize-mini-windows t
      show-paren-when-point-in-periphery t
      maximum-scroll-margin 0.5
	  scroll-margin 9999
      scroll-conservatively 101
      scroll-preserve-screen-position t
      doc-view-continuous t
      ring-bell-function 'ignore
	  tab-always-indent 'complete
      vc-allow-rewriting-published-history t
	  vc-follow-symlinks t
	  vc-git-diff-switches '("--patch-with-stat" "--histogram")
	  vc-git-shortlog-switches '("--stat"))
(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook #'viper-mode)
  (setq eww-header-line-format nil)
  (setq eww-auto-rename-buffer 'title))
;;; Completion ---
(setopt minibuffer-completion-auto-choose t
        completion-ignore-case t
        completions-format 'one-column
        completions-header-format nil
        completion-show-help nil
        completions-max-height 13
        completions-sort 'historical
        completions-detailed t
        completion-eager-update t
        completion-auto-help 'visible
        completion-styles '(partial-completion basic flex))
(define-key minibuffer-local-completion-map [remap previous-line] #'minibuffer-previous-completion)
(define-key minibuffer-local-completion-map [remap next-line]     #'minibuffer-next-completion)
(add-to-list 'display-buffer-alist
             '("\\*\\(Completions\\|Flymake diagnostics.*\\)\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-parameters . ((mode-line-format . none)))))
(setq ido-enable-flex-matching t ido-everywhere t
      ido-ignore-buffers
      '("\\` " "\\*Messages\\*" "\\*scratch\\*" "\\*Completions\\*" "\\*Native-compile-Log\\*"
        "\\*Async-native-compile-log\\*" "\\*EGLOT.*events\\*" "\\*Flymake.*\\*"
        "\\*Buffer List\\*" "\\*Help\\*" "\\*Minibuf-.*\\*" "\\*vc-.*\\*")
      ido-create-new-buffer 'always
      ido-use-virtual-buffers 'auto
      ido-show-dot-for-dired t ido-max-prospects 6
      ido-auto-merge-work-directories-length -1)
(ido-mode 'both)
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
      (when-let* ((open (and (not (nth 4 (syntax-ppss))) (nth 1 (syntax-ppss))))
                  (end (save-excursion (goto-char open)
                                       (ignore-errors (forward-sexp) (point))))
                  ((> end open)))
        (list open (1+ open) (1- end) end))))
(add-hook 'prog-mode-hook
          (lambda () (when (treesit-parser-list)
                       (setq-local show-paren-data-function #'my/show-paren-data))))
(setq show-paren-data-function #'my/show-paren-data)

(setq-default fill-column 140 text-scale-mode-step 1.3)
(defun zen-buffer-apply-margins () "Apply zen margins to all windows."
       (walk-windows
        (lambda (win)
          (with-current-buffer (window-buffer win)
            (when (derived-mode-p 'prog-mode 'text-mode)
              (let* ((special-modes (or (eq major-mode 'org-mode) (eq major-mode 'markdown-mode)))
                     (margin (max 0 (/ (- (window-total-width win) fill-column) 2)))
                     (lmargin (if special-modes (max 0 (- margin 10)) margin)))
                (if (> (window-total-width win) fill-column)
                    (progn (visual-line-mode 1) (set-window-margins win lmargin margin)
                           (when special-modes (text-scale-set 1) (setq-local line-spacing 0.6)
                                 (setq markdown-marginalize-headers-margin-width (- lmargin 4))))
                  (progn (set-window-margins win nil)
                         (when special-modes (text-scale-set 0) (setq-local line-spacing 5))))))))
        nil t))
(add-hook 'window-configuration-change-hook #'zen-buffer-apply-margins)

(defun my-scroll-other-down nil (interactive)
       (let ((mode (with-current-buffer (window-buffer (other-window-for-scrolling))
                     major-mode)))
         (with-selected-window (other-window-for-scrolling)
           (cond ((eq mode 'Info-mode) (Info-scroll-up))
                 ((eq mode 'doc-view-mode) (doc-view-scroll-up-or-next-page 5))
                 (t (scroll-up-command 5))))))
(defun my-scroll-other-up nil (interactive)
       (let ((mode (with-current-buffer (window-buffer (other-window-for-scrolling))
                     major-mode)))
         (with-selected-window (other-window-for-scrolling)
           (cond ((eq mode 'Info-mode) (Info-scroll-down))
                 ((eq mode 'doc-view-mode) (doc-view-scroll-down-or-previous-page 5))
                 (t (scroll-down-command 5))))))
