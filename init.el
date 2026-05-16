;;; init.el - 🦬 ---  -*- lexical-binding: t; -*-
;; (load "~/.emacs.d/lisp/benchmarking.el" :noerr :no-message)

;;; Initialisation

(setq-default auto-save-file-name-transforms `((".*" "~/.emacs.d/backup/" t))
              backup-directory-alist `(("." . "~/.emacs.d/backup/"))
              create-lockfiles nil custom-file "/tmp/emacs-custom"
              warning-minimum-level :error)

(put 'narrow-to-region 'disabled nil)
(if (eq system-type 'android)
    (setq-default fill-column 120 line-spacing '(4 . 4))
  (setq shell-file-name "/opt/homebrew/bin/fish"))

(add-hook 'emacs-startup-hook
          (lambda () (ido-mode 'buffer) (global-auto-revert-mode 1) (fido-mode)
            (repeat-mode 1) (save-place-mode 1) (delete-selection-mode 1) (savehist-mode 1)
            (global-visual-line-mode 1) (global-visual-wrap-prefix-mode 1)
            (window-divider-mode 1) (electric-pair-mode 1) (kill-ring-deindent-mode 1)))
(run-with-idle-timer 0.1 nil #'viper-mode)

;;; Appearance

;;;; Frame and chrome

(setq-default cursor-in-non-selected-windows nil
              frame-resize-pixelwise t
              inhibit-startup-screen t
              window-divider-default-right-width 1
              window-divider-default-bottom-width 0
              window-divider-default-places 'right-only)
(blink-cursor-mode -1) (tooltip-mode -1) (menu-bar-mode -1) (scroll-bar-mode -1)
(tool-bar-mode -1) (line-number-mode -1)
(setcdr (assq 'continuation fringe-indicator-alist) '(nil nil))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;;;; Fonts

(dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
  (custom-set-faces `(,face ((t :family "Input Mono Narrow")))))
(set-face-attribute 'default nil :height (if (eq system-type 'android) 160 140))
(dolist (set '(cjk-misc han kana)) (set-fontset-font t set "Noto Sans Mono CJK JP" nil 'prepend))

;;;; Theme

(setq custom-theme-directory "~/.emacs.d/themes/" custom-safe-themes t)
(define-advice load-theme (:around (orig-fun &rest args) theme-dont-propagate)
  (put 'user 'theme-settings nil)
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fun args)
  (custom-set-faces '(fringe ((t :background unspecified)))))

(defun solaire-background () "Remap faces to use solaire background."
       (dolist (face '(default fringe header-line margin))
         (face-remap-add-relative face 'highlight)))

(deftheme untitled-plain "An industrial subtly washed theme.")
(apply #'custom-theme-set-faces 'untitled-plain
       `((region ((t :background "#fedf7b" :foreground "#0f0e0d" :extend nil)))
         (header-line ((t :overline ,(face-foreground 'shadow))))
         (highlight ((((background dark))  :background "#303030")
                     (((background light)) :background "#bfc0c1")))
         (font-lock-string-face ((((background dark))  :foreground "#eeb43d")
                                 (((background light)) :foreground "#084095")))
         (font-lock-builtin-face ((t :slant italic)))
         (font-lock-function-name-face ((t :inherit bold)))
         (shadow ((t :foreground "#828386")))
         (error ((t :foreground "Coral3")))
         (success ((t :foreground "ForestGreen")))
         ,@(mapcar (lambda (f) `(,f ((t nil))))
                   '(fringe shr-mark font-lock-type-face font-lock-constant-face viper-minibuffer-insert org-agenda-done
                            font-lock-keyword-face font-lock-variable-name-face dictionary-word-definition-face org-table
                            speedbar-selected-face markdown-ts-list-marker markdown-ts-table-delimiter-cell))
         ,@(mapcar (lambda (i) `(,(intern (format "outline-%d" i)) ((t :height 1.1 :inherit bold))))
                   (number-sequence 1 9))
         ,@(mapcar (lambda (f) `(,f ((t :inherit (highlight default) :extend t))))
                   '(org-block org-block-begin-line org-block-end-line diff-header markdown-ts-code-block))
         ,@(mapcar (lambda (f) `(,f ((t :inherit highlight))))
                   '(lazy-highlight org-code org-verbatim org-agenda-clocking speedbar-highlight-face))
         ,@(mapcar (lambda (f) `(,f ((t :inherit font-lock-string-face :weight bold))))
                   '(minibuffer-prompt dired-directory woman-bold Man-overstrike speedbar-directory-face fixed-pitch-serif))
         ,@(mapcar (lambda (f) `(,f ((t :inherit shadow))))
                   '(vertical-border font-lock-comment-face org-time-grid speedbar-file-face))
         (link ((t :underline t))) ;:foreground "#0965ef"
         (hs-ellipsis ((t :underline t)))
         (nobreak-space ((t :underline nil)))
         (diff-file-header ((t :inherit (highlight bold))))
         (line-number-current-line ((t :weight bold :inherit default)))
         (eglot-highlight-symbol-face ((t :inherit (highlight default))))
         (isearch ((t :inverse-video t)))
         (completions-common-part ((t :underline t :weight bold)))
         (org-document-info ((t :height 1.1 :inherit bold)))
         (org-document-title ((t :height 1.2 :inherit bold)))
         (org-agenda-structure ((t :height 1.2 :inherit default)))
         (org-agenda-date ((t :weight bold :slant italic)))
         (compilation-info ((t :foreground "#448c27" :inherit bold)))
         (which-func ((t :inherit mode-line)))
         (eww-form-text ((t :box (:line-width 1) :inherit (highlight default))))
         (eww-form-submit ((t :box (:line-width 2 :style released-button) :inherit (highlight default))))
         ;; diff colors for light background taken from doric-marble
         (ediff-current-diff-A ((((background light)) :background "#eac0bf" :extend t)))
         (ediff-current-diff-B ((((background light)) :background "#bde0c2" :extend t)))
         (ediff-fine-diff-A ((((background light)) :background "#e05fa1209f9e" :weight bold)))
         (ediff-fine-diff-B ((((background light)) :background "#a187d39fa8af" :weight bold)))
         (diff-removed ((((background light)) :background "#ed05c713c62b" :extend t)))
         (diff-added ((((background light)) :background "#c45de3fcc8e1" :extend t)))
         (diff-refine-removed ((((background light)) :background "#e05fa1209f9e" :weight bold)))
         (diff-refine-added ((((background light)) :background "#a187d39fa8af" :weight bold)))
         ,@(let ((common `(:inverse-video ,(not (display-graphic-p))
                                          :height ,(if (eq system-type 'android) 160 140)
                                          :overline ,(face-foreground 'shadow)
                                          :box (:line-width 2 :style flat-button))))
             `((mode-line-active ((t :inherit default ,@common)))
               (mode-line-inactive ((t :inherit shadow ,@common)))))))
(set-face-attribute 'default nil :foreground "#d8d8da" :background "#0f0e0d")
(enable-theme 'untitled-plain)
(keymap-global-set "C-x 6"
                   #'(lambda () (interactive)
                       (invert-face 'default) (frame-set-background-mode nil)
                       (enable-theme 'untitled-plain)))

;;;; Cursor colour on modification

(add-hook 'post-command-hook (lambda () (set-cursor-color (if (buffer-modified-p) "coral3" "#00c2ff"))))

;;;; Keyword highlighting in prog buffers

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME\\|HACK\\|TODO\\|WIP\\|BUG\\)\\( \\|:\\)" 1 'match t)
                   (";" . 'shadow)))))

;;;; Mode-line

(setq-default mode-line-collapse-minor-modes
              '(not flymake-mode defining-kbd-macro text-scale-mode)
              mode-line-collapse-minor-modes-to ""
              mode-line-modes-delimiters '("" . "")
              mode-line-end-spaces nil
              flymake-mode-line-title nil)

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
                (:eval (when (and (not (display-graphic-p)) (boundp 'viper-mode-string))
                         (concat " " viper-mode-string)))
                " %+  "
                (:eval (propertize "%b" 'face 'bold 'help-echo (buffer-file-name)))
                (:eval (propertize (string-trim-left (format-mode-line vc-mode))))
                "    " mode-line-position
                mode-line-format-right-align
                mode-line-modes mode-line-misc-info
                mode-line-end-spaces ""))

(with-eval-after-load 'viper
  (setq global-mode-string
        '((:eval (unless (derived-mode-p 'prog-mode)
                   (format-time-string "%a %H:%M "))))))

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        '((which-function-mode
           (which-func-mode
            (which-func--use-mode-line ("" which-func-format " "))))
          (global-mode-string ("" global-mode-string))
          (:eval (when (and (bound-and-true-p eglot--managed-mode)
                            (eglot-managed-p))
                   eglot-mode-line-progress)))))

;;; General editing

;;;; Options

(setq-default sentence-end-double-space nil
              truncate-lines nil
              tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil
              fill-column 130
              line-spacing '(3 . 3)
              text-scale-mode-step 1.3
              split-width-threshold (- fill-column 20)
              use-short-answers t
              require-final-newline t
              resize-mini-windows t
              ring-bell-function 'ignore
              tab-always-indent 'complete
              save-interprogram-paste-before-kill t
              delete-pair-push-mark t
              delete-pair-blink-delay t
              kill-region-dwim 'emacs-word
              help-window-select t)

;;;; Show paren

(setq-default show-paren-mode nil)

(defun my/show-paren-data ()
  (let ((open (cond ((eq (car (syntax-after (point))) 4) (point))
                    ((eq (car (syntax-after (point))) 5)
                     (ignore-errors (save-excursion (forward-char) (backward-list) (point))))
                    ((eq (car (syntax-after (1- (point)))) 5)
                     (ignore-errors (save-excursion (backward-list) (point))))
                    ((nth 1 (syntax-ppss))))))
    (save-excursion
      (when open (goto-char open))
      (if (fboundp 'treesit-show-paren-data) (treesit-show-paren-data) (show-paren--default)))))

(add-hook 'prog-mode-hook (lambda nil (setq-local show-paren-data-function #'my/show-paren-data)))

;;;; Zen margins

(defvar zen-enabled-modes '(Info-mode diff-mode eww-mode dired-mode gnus-article-mode Man-mode org-agenda-mode
                                      vc-dir-mode gnus-group-mode erc-mode woman-mode))

(defun zen-buffer-apply-margins nil "Apply zen margins to all windows."
       (walk-windows
        (lambda (win)
          (with-current-buffer (window-buffer win)
            (when (or (derived-mode-p '(prog-mode text-mode)) (member major-mode zen-enabled-modes))
              (let* ((special-modes (member major-mode '(Info-mode org-mode markdown-ts-mode)))
                     (winw (window-total-width win))
                     (fill (if (eq major-mode 'eww-mode) 120 fill-column))
                     (margin (max 0 (/ (- winw fill) 2)))
                     (lmargin (if (eq major-mode 'org) (max 0 (- margin 5)) margin)))
                (if (> winw fill-column)
                    (progn (visual-line-mode 1) (set-window-margins win lmargin margin)
                           (when special-modes (text-scale-set 1) (setq-local line-spacing '(0.3 . 0.3))))
                  (progn (set-window-margins win nil 1)
                         (when special-modes
                           (set-window-margins win (if (eq major-mode 'org-mode) 0 5) 5)
                           (text-scale-set 0) (setq-local line-spacing '(3 . 3)))))))))
        nil t))

(add-hook 'window-configuration-change-hook #'zen-buffer-apply-margins)

;;;; Sensible keyboard-quit

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

;;;; Small editing functions

(defun my/delete-pair () (interactive)
       (if (use-region-p) (progn (goto-char (region-beginning)) (delete-pair))
         (mark-inner) (my/delete-pair)))

(defun mark-inner nil (interactive)
       (condition-case nil (if (nth 3 (syntax-ppss))
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

;;; Navigation and scrolling

(setq-default maximum-scroll-margin 0.5
              scroll-margin 9999
              scroll-conservatively 101
              scroll-preserve-screen-position t
              fast-but-imprecise-scrolling t)

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

;;; Completion and minibuffer

(setq enable-recursive-minibuffers t
      savehist-additional-variables '(register-alist kill-ring)
      minibuffer-default-prompt-format " [%s]"
      minibuffer-visible-completions t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t
      completion-auto-help nil
      completion-styles '(initials partial-completion basic flex))

(with-eval-after-load 'recentf ; silence recentf
  (dolist (recentf-fn '(recentf-load-list recentf-cleanup))
    (advice-add recentf-fn :around
                (lambda (fn &rest args)
                  (let ((inhibit-message t) (message-log-max nil) (save-silently t))
                    (apply fn args))))))

;;;; Ido and fido

(setq ido-enable-flex-matching t ido-everywhere nil
      ido-ignore-buffers
      '("\\` " "\\*Messages\\*" "\\*Completions\\*" "\\*Native-compile-Log\\*" "\\*Buffer List\\*"
        "\\*Async-native-compile-log\\*" "\\*EGLOT.*events\\*" "\\*Flymake.*\\*" "\\*MPC.*\\*"
        "\\*Help\\*" "\\*Minibuf-.*\\*" "\\*vc-.*\\*" "\\*changes to.*" "^\\#.*")
      ido-create-new-buffer 'always ido-use-virtual-buffers 'auto recentf-max-saved-items 200
      ido-show-dot-for-dired t ido-max-window-height 1 ido-auto-merge-work-directories-length -1
      ido-separator " • " icomplete-separator " • " icomplete-tidy-shadowed-file-names t)

(setq fido-non-vertical-fns
      '(find-file find-file-other-window execute-extended-command project-switch-to-buffer))

(add-hook 'icomplete-minibuffer-setup-hook
          (lambda nil
            (unless (memq this-command fido-non-vertical-fns) (setq-local icomplete-vertical-mode t))
            (setq-local icomplete-prospects-height (if icomplete-vertical-mode 11 1))))

;;; Keybindings

;;;; Viper

(setq viper-mode t viper-expert-level 5 viper-ex-style-motion nil
      viper-inhibit-startup-message t viper-want-ctl-h-help t
      viper-want-emacs-keys-in-insert t viper-want-emacs-keys-in-vi t
      viper-ex-style-editing nil viper-insert-state-cursor-color nil
      viper-vi-state-cursor-color nil viper-emacs-state-cursor-color nil)

(with-eval-after-load 'viper-cmd
  (setq viper-insert-basic-map (make-sparse-keymap))
  (define-key viper-insert-basic-map viper-toggle-key 'viper-escape-to-vi)
  (advice-add 'viper-adjust-keys-for :after
              (lambda (state)
                (when (memq state '(insert-state replace-state))
                  (define-key viper-insert-basic-map [backspace] nil)
                  (define-key viper-replace-map [backspace] nil)))
              '((name . viper-remove-backspace-override))))

(defvar insert-pair-map ;; src: oantolin
  (let ((map (make-sparse-keymap))) (define-key map [t] #'insert-pair) map))

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
                     ("g A" . (lambda nil (interactive) (org-agenda nil "n"))) ("g c" . org-capture)
                     ("g z" . pop-to-mark-command) ("g /" . xref-find-definitions-other-window)
                     ("K" . my/eldoc-get-help) ("*" . isearch-forward-symbol-at-point)))
    (keymap-set viper-vi-basic-map (car binding) (cdr binding)))

  ;; Selection-first word movements (meow/kak style)
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

;;;; Terminal cursor shapes

(unless (display-graphic-p)
  (dolist (p '((viper-vi-state-hook . "\e[2 q") (viper-insert-state-hook . "\e[6 q")
               (viper-replace-state-hook . "\e[4 q") (kill-emacs-hook . "\e[2 q")))
    (add-hook (car p) (let ((s (cdr p))) (lambda () (send-string-to-terminal s))))))

;;;; Global bindings

(dolist (binding '(("C-x c c" . compile) ("C-x c r" . recompile) ("C-x c ." . compile-at-root)
                   ("C-h '" . describe-face) ("C-x C-m" . execute-extended-command) ("C-\\" . epop)
                   ("C-x k" . kill-current-buffer) ("M-o" . other-window) ("s-o" . other-window)
                   ("C-x ;" . comment-line) ("C-x x c" . save-buffers-kill-emacs)
                   ("C-x C-b" . ibuffer) ("M-;" . eval-expression) ("C-/" . undo-only)
                   ("C-," . my-scroll-other-down) ("C-." . my-scroll-other-up) ("C-x d" . speedbar)
                   ("M-j" . window-toggle-side-windows) ("<escape>" . keyboard-escape-quit)
                   ("C-<tab>" . tab-next) ("C-S-<tab>" . tab-previous) ("C-x x f" . find-file)
                   ("C-x x s" . save-buffer) ("C-x x e" . eval-defun) ("C-x x z" . restart-emacs)
                   ("C-x x x" . flymake-show-project-diagnostics)))
  (keymap-global-set (car binding) (cdr binding)))

(keymap-global-set "C-x m" esc-map)

;;;; Chord (jk to escape)

(defun my-chord (initial-key final-key fn)
  (interactive) ;; src: wasamasa
  (let ((event (read-event nil nil 0.4)))
    (cond ((and event (characterp event) (= event final-key)) (call-interactively fn))
          (event (insert initial-key) (push event unread-command-events))
          (t (insert initial-key)))))

(keymap-global-set "j" #'(lambda nil (interactive) (my-chord ?j ?k 'viper-change-state-to-vi)))

;;;; VC prefix map extras

(keymap-set vc-prefix-map "f"
            (lambda () (interactive)
              (vc-git--pushpull "push" nil '("--force-with-lease"))))
(keymap-set vc-prefix-map "e" #'vc-ediff)

;;;; Mode-specific SPC as ctl-x

(with-eval-after-load 'dired (keymap-set dired-mode-map "SPC" ctl-x-map))
(with-eval-after-load 'doc-view
  (keymap-set doc-view-mode-map "SPC" ctl-x-map)
  (keymap-set doc-view-mode-map "j" #'doc-view-next-line-or-next-page)
  (keymap-set doc-view-mode-map "k" #'doc-view-previous-line-or-previous-page))

;;; Search and grep

(setq lazy-highlight-initial-delay 0
      isearch-regexp-lax-whitespace t
      isearch-lazy-count t
      isearch-repeat-on-direction-change t
      isearch-wrap-pause 'no-ding
      search-whitespace-regexp ".*?"
      grep-command
      "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)"
      grep-command-position 27)

;;; Files and projects

(setq dired-kill-when-opening-new-dired-buffer t
      dired-listing-switches
      "-l -v --almost-all --human-readable --group-directories-first"
      dired-dwim-target t
      dired-auto-revert-buffer 'dired-buffer-stale-p
      delete-by-moving-to-trash t
      doc-view-continuous t
      vc-allow-rewriting-published-history t
      vc-follow-symlinks t
      vc-make-backup-files t
      vc-find-revision-no-save t
      vc-display-status 'no-backend
      vc-git-diff-switches '("--patch-with-stat" "--histogram" "-w")
      project-vc-extra-root-markers '("Cargo.toml" "build.zig" "go.work" "CMakeLists.txt"))

(add-hook 'dired-mode-hook (lambda () (setq mode-name "Dired")))

(setq speedbar-prefer-window t
      speedbar-use-images t
      speedbar-show-unknown-files t
      speedbar-window-default-width 30)

(with-eval-after-load 'ezimage
  (dolist (var '(ezimage-page ezimage-directory-plus ezimage-directory-minus ezimage-page-plus ezimage-page-minus     
                              ezimage-box-plus ezimage-box-minus ezimage-tag ezimage-label ezimage-checkout))
    (set var "")))

(with-eval-after-load 'speedbar
  (define-key speedbar-file-key-map (kbd "q") #'speedbar-window)
  (advice-add 'speedbar-window-mode :after
              (lambda (&rest _)
                (when (window-live-p speedbar--window)
                  (select-window speedbar--window))))
  (advice-add 'speedbar-set-mode-line-format :override (lambda () nil)))

(add-hook 'speedbar-mode-hook
          (lambda nil (solaire-background) (setq-local mode-line-format nil)))

;;; Windows and buffers

(setq-default uniquify-buffer-name-style 'forward
              tab-bar-show nil
              display-line-numbers-width 4
              display-line-numbers-widen t
              imenu-flatten t)

(add-to-list
 'display-buffer-alist
 '((or "\\*Completions\\*" "\\*xref\\*" "\\*Occur.*\\*" "\\*compilation.*\\*" "\\*Flymake.*\\*"
       "\\*vc-git :.*\\*" "\\*vc-change-log\\*" "\\*Org Select\\*" "\\CAPTURE-.*")
   (display-buffer-in-side-window)
   (side . bottom) (window-height . 0.28)
   (window-parameters . ((mode-line-format . none)))))

(defun my/display-buffer-adaptive (buffer alist)
  (let ((side (if (< (frame-width) 160) 'bottom 'right))
        (size-param (if (< (frame-width) 160)
                        '(window-height . 0.28)
                      '(window-width . 82))))
    (display-buffer-in-side-window
     buffer (append `((side . ,side) ,size-param) alist))))

(add-to-list
 'display-buffer-alist
 '("\\*\\(Dictionary\\|eldoc\\|Help\\)\\*"
   my/display-buffer-adaptive
   (body-function . select-window)
   (window-parameters . ((split-window . #'ignore) (mode-line-format . none)))))

;;; Programming

;;;; Options

(setq-default eldoc-echo-area-use-multiline-p nil
              treesit-enabled-modes t
              treesit-font-lock-level 2
              go-ts-mode-indent-offset 4
              diff-font-lock-syntax nil
              ispell-program-name "aspell"
              inferior-lisp-program "clojure"
              eglot-ignored-server-capabilities
              '(:inlayHintProvider
                :workspace.didChangeWatchedFiles
                :colorProvider :codeLensProvider
                :foldingRangeProvider
                :semanticTokensProvider
                :documentHighlightProvider)
              eglot-sync-connect 0
              eglot-autoshutdown t
              eglot-extend-to-xref t
              xref-auto-jump-to-first-xref t
              xref-auto-jump-to-first-definition t
              jsonrpc-event-hook nil)

;;;; Prog-mode hooks

(dolist (fn '(hs-minor-mode display-line-numbers-mode show-paren-mode
                            completion-preview-mode goto-address-mode))
  (add-hook 'prog-mode-hook fn))

(dolist (hook '(text-mode-hook eshell-mode-hook))
  (add-hook hook #'goto-address-mode))

(dolist (mode-hook '(conf-mode-hook yaml-ts-mode-hook))
  (add-hook mode-hook #'display-line-numbers-mode))

(add-to-list 'auto-mode-alist
             '("\\.log\\'" . (lambda () (display-line-numbers-mode))))

;;;; Eglot

(with-eval-after-load 'eglot
  (setq python-flymake-command
        '("ruff" "check" "--output-format=concise"
          "--stdin-filename" "stdin" "-"))
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) . ("uvx" "ty" "server")))
  (defun my-eglot-organize-imports () (interactive)
         (ignore-errors (eglot-code-actions nil nil "source.organizeImports" t))))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format-buffer nil t)
            (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
            (when (eq major-mode 'python-mode)
              (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
            (when (eq major-mode 'go-ts-mode)
              (setq eldoc-documentation-functions
                    (remove #'eglot-signature-eldoc-function eldoc-documentation-functions)))))

(dolist (mode '(rust-ts-mode-hook go-ts-mode-hook python-mode-hook c++-mode-hook))
  (add-hook mode #'eglot-ensure))

;;;; Which-func

(with-eval-after-load 'which-func ; disabled because this causes scroll slowdown
  (setq which-func-format (list (cadr which-func-format))
        which-func-unknown ""
        hich-func-update-delay 1))

;;;; Completion-preview

(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map
              "C-s" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map
              "C-r" #'completion-preview-prev-candidate))

;;;; Compilation

(defun compile-at-root nil (interactive) "Run compile command at project root."
       (let ((default-directory (project-root (project-current nil))))
         (call-interactively 'compile)))

(with-eval-after-load 'compile
  (setq compilation-scroll-output 'first-error)
  (push 'go-test compilation-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-test
                 . (".*?\\([[:alnum:]_./-]+\\.go\\):\\([0-9]+\\)\\(?:\\(?::\\([0-9]+\\)\\)?\\| \\+0x[0-9a-f]+\\)"
                    1 2 3 nil 1)))
  (add-hook 'compilation-filter-hook
            (lambda nil (goto-address-mode -1)
              (unless (eq major-mode 'grep-mode)
                (ansi-color-compilation-filter)
                (ansi-osc-compilation-filter)))))

;;;; Org

(setq org-directory (concat "~/Dropbox/" "org") org-agenda-files (list org-directory)
      org-modules nil org-pretty-entities t org-src-fontify-natively t
      org-adapt-indentation t org-startup-indented t org-startup-truncated nil
      org-src-content-indentation 0 org-src-preserve-indentation t
      org-fontify-quote-and-verse-blocks t org-fontify-whole-heading-line t
      org-special-ctrl-a/e nil org-M-RET-may-split-line '((item . nil)))

(with-eval-after-load 'org
  (defun org-outer-indent--compute-prefixes () ; src: rougier
    "Compute prefix strings with outer-aligned stars."
    (setq org-indent--heading-line-prefixes (make-vector org-indent--deepest-level nil)
          org-indent--inlinetask-line-prefixes (make-vector org-indent--deepest-level nil)
          org-indent--text-line-prefixes (make-vector org-indent--deepest-level nil))
    (let ((indent 7))  ; (+ 3 4)
      (dotimes (n org-indent--deepest-level)
        (aset org-indent--heading-line-prefixes n (make-string (max 0 (- indent (1+ n))) ?\s))
        (aset org-indent--inlinetask-line-prefixes n (make-string indent ?\s))
        (aset org-indent--text-line-prefixes n (make-string indent ?\s)))
      (setq-local org-hide-leading-stars nil)))

  (advice-add 'org-indent--compute-prefixes :override #'org-outer-indent--compute-prefixes)

  (require 'org-tempo)
  (with-eval-after-load 'org-src
    (nconc org-src-lang-modes
           '(("rust" . rust-ts) ("python" . python-ts) ("go" . go-ts) ("bash" . bash-ts)
             ("typescript" . typescript-ts) ("javascript" . js-ts) ("json" . json-ts)
             ("yaml" . yaml-ts) ("toml" . toml-ts) ("c" . c-ts) ("cpp" . c++-ts))))
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t) (shell . t) (python . t) (emacs-lisp . t)))
  (setq org-confirm-babel-evaluate nil))

(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook
            (lambda nil (setq-local header-line-format nil)))
  (setq org-capture-file (concat org-directory "/inbox.org")
        org-joural-file (concat org-directory "/journal.org")
        org-capture-templates
        '(("t" "TODO" entry (file+headline org-capture-file "Tasks")
           "* TODO %?\n%<%d %b '%g %R>" :prepend t)
          ("n" "Note" entry (file+headline org-capture-file "Notes")
           "* %?\n" :prepend t)
          ;; https://www.twelvety.net/2024/12/styling-a-markdown-one-line-journal-in-emacs
          ("j" "Journal" plain (file+datetree org-joural-file)
           "%<%d %b, %a> | %?" :tree-type month :empty-lines 1)
          ("h" "Habit" entry (file+headline org-capture-file "Habit")
           "* TODO %?\n:PROPERTIES:\n:STYLE: habit\n:END:" :prepend t))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-ignore-properties '(effort appt stats category)
        org-agenda-dim-blocked-tasks nil
        org-agenda-use-tag-inheritance nil
        org-agenda-inhibit-startup t
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-start-with-log-mode t
        org-agenda-log-mode-add-notes nil
        org-agenda-remove-tags t
        org-agenda-show-all-dates nil
        org-agenda-start-on-weekday 0
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-include-deadlines t)

  (setq my/org-grid-w 31)
  (defun my/org-agenda-clean-clockin (orig-fun &rest args)
    "Reformat clock entries to show time ranges after task name."
    (let ((result (apply orig-fun args)))
      (when (and result (stringp result))
        (cl-flet ((fmt (prefix time dur task pad-w &optional mark)
                    (let* ((full (concat (or mark "") task))
                           (pad (make-string (max 0 (- pad-w (length full))) ?┄ t)))
                      (format "%s%s %7s %s %s" prefix time dur full pad))))
          (cond
           ((string-match "\\([0-9]+:[0-9]+\\)-\\([0-9]+:[0-9]+\\)Clocked:\\s-+(\\([^)]+\\))\\(.+\\)$" result)
            (fmt (substring result 0 (match-beginning 1))
                 (match-string 1 result)
                 (concat "(" (match-string 3 result) ")")
                 (string-trim (match-string 4 result))
                 (1- my/org-grid-w)))
           ((string-match "\\([0-9]+:[0-9]+\\)\\s-+Closed:\\s-+\\(.+\\)$" result)
            (fmt (substring result 0 (match-beginning 1))
                 (match-string 1 result) ""
                 (string-trim (match-string 2 result))
                 (+ 4 my/org-grid-w) "✓ "))
           ((string-match "\\([0-9]+:[0-9]+\\)\\s-+Clocked:\\s-+\\(.+\\)$" result)
            (fmt (substring result 0 (match-beginning 1))
                 (match-string 1 result) ""
                 (string-trim (match-string 2 result))
                 (1- my/org-grid-w)))
           (t result))))))
  (advice-add 'org-agenda-format-item :around #'my/org-agenda-clean-clockin)

  (setq org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-todo-keyword-format "%-1s"
        org-agenda-use-time-grid t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-scheduled-leaders '("" "")
        org-agenda-deadline-leaders '("" "" "")
        org-agenda-todo-keyword-format ""
        org-agenda-block-separator (string-to-char " ")
        org-agenda-current-time-string
        (concat "← now " (make-string (- my/org-grid-w 6) ?─ t))
        org-agenda-time-grid
        `((daily today require-timed remove-matched)
          (800 1200 1600 2000)
          ,(make-string 9 ?  t) ,(make-string my/org-grid-w ?┄ t))
        org-agenda-prefix-format
        '((agenda . " %i %-16b%t%s")
          (todo . " %i %?-16b"))))

;;; Shells

;;;; Eshell

(setq pcomplete-termination-string ""
      eshell-aliases-file "~/.config/alias"
      eshell-banner-message ""
      eshell-hist-ignoredups 'erase
      eshell-history-size 20000
      eshell-save-history-on-exit t
      eshell-glob-case-insensitive t
      eshell-status-in-mode-line nil
      eshell-scroll-to-bottom-on-input 'this)

(defun epop nil (interactive) (defvar eshell-buffer-name)
       (let* ((display-buffer-alist `(("\\*eshell-pop.*\\*"
                                       (display-buffer-in-side-window)
                                       (side . bottom) (slot . -2) (window-height . 0.28))))
              (dir (if-let* ((proj (project-current)))
                       (file-name-nondirectory (directory-file-name (project-root proj)))
                     default-directory))
              (eshell-buffer-name (concat "*eshell-pop:*" dir))
              (inhibit-message t))
         (eshell) (setq-local mode-line-format nil header-line-format "")
         (solaire-background)))

(defun my-eshell-read-aliases-list ()
  "Read in an aliases list from `eshell-aliases-file' using bash format."
  (interactive)
  (when (and eshell-aliases-file
             (file-readable-p eshell-aliases-file))
    (setq eshell-command-aliases-list
          (with-temp-buffer
            (let (eshell-command-aliases-list)
              (insert-file-contents eshell-aliases-file)
              (while (not (eobp))
                (if (re-search-forward
                     "^alias\\s-+\\(\\S-+\\)=\'\\(.+\\)\'$")
                    (setq eshell-command-aliases-list
                          (cons (list (match-string 1)
                                      (concat (match-string 2) " $*"))
                                eshell-command-aliases-list)))
                (forward-line 1))
              eshell-command-aliases-list))))
  (my-eshell-only-aliases))
(advice-add 'eshell-read-aliases-list :override #'my-eshell-read-aliases-list)

(defun my-eshell-only-aliases ()
  (dolist (a '(("source" ". $1") ("mkcd" "mkdir -p $1 && cd $1") ("k" "kubectl $*")
               ("clear" "clear t") ("d" "dired-other-window $1") ("dired" "dired $1")
               ("ff" "find-file $1") ("jq" "jq -M $*")
               ("rg" "rg --color=never --no-line-number $*")
               ("gd" "vc-diff") ("glog" "vc-print-root-log")
               ("groot" "cd ${git rev-parse --show-toplevel}")
               ("gk" "export KUBECONFIG=${gardenctl kubectl-env zsh | sed -n \"s/export KUBECONFIG='\\(.*\\)'/\\1/p\" | sed 's/;//g'}")))
    (push a eshell-command-aliases-list)))

(defun eshell--k8s-context-and-namespace ()
  (when-let* ((kubeconfig (getenv "KUBECONFIG"))
              ((not (string-empty-p (string-trim kubeconfig))))
              (kc (concat "kubectl --kubeconfig=" kubeconfig " "))
              (context (string-trim (or (eshell-command-result (concat kc "config current-context")) "")))
              ((not (string-empty-p context)))
              ((not (string-match "error" context)))
              (ns (or (string-trim (or (eshell-command-result
                                        (concat kc (format
                                                    "config view -o 'jsonpath={.contexts[?(@.name==\"%s\")].context.namespace}'"
                                                    context)))
                                       ""))
                      "default")))
    (format "(%s|%s) " context (if (string-empty-p ns) "default" ns))))

(defun eshell--git-prompt ()
  (cl-flet ((git (cmd)
              (with-temp-buffer
                (if (zerop (apply #'call-process "git" nil t nil cmd))
                    (format " (%s)" (string-trim (buffer-string)))
                  ""))))
    (let* ((git-dir (locate-dominating-file default-directory ".git"))
           (rebasing (and git-dir
                          (or (file-exists-p (expand-file-name ".git/rebase-merge" git-dir))
                              (file-exists-p (expand-file-name ".git/rebase-apply" git-dir)))
                          (not (string-empty-p (git '("rev-parse" "--verify" "REBASE_HEAD"))))))
           (merging (not (string-empty-p (git '("rev-parse" "--verify" "MERGE_HEAD"))))))
      (cond (rebasing " (REBASE-i)")
            (merging " (MERGE-i)")
            (t (git '("symbolic-ref" "-q" "--short" "HEAD")))))))

(add-hook 'eshell-mode-hook #'compilation-shell-minor-mode)
(with-eval-after-load 'eshell
  (setq eshell-highlight-prompt nil
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function
        (lambda ()
          (let ((prompt (concat (propertize (or (eshell--k8s-context-and-namespace) "") 'font-lock-face 'font-lock-string-face)
                                (propertize (abbreviate-file-name (eshell/pwd)) 'font-lock-face 'bold-italic)
                                (propertize (eshell--git-prompt) 'font-lock-face 'font-lock-comment-face)
                                (if (zerop eshell-last-command-status)
                                    (propertize " λ" 'font-lock-face 'success)
                                  (propertize (format " [%s] λ" eshell-last-command-status) 'font-lock-face 'warning))
                                " ")))
            (add-text-properties 0 (length prompt) '(read-only t rear-nonsticky (read-only)) prompt)
            prompt)))
  (defun eshell-insert-history () (interactive) ; src: habrams
         (let ((cmd (completing-read "Eshell history: "
                                     (delete-dups (ring-elements eshell-history-ring)))))
           (when cmd (kill-line 0) (insert cmd))))
  (add-hook 'eshell-mode-hook #'completion-preview-mode)
  (add-hook 'eshell-mode-hook
            (lambda () ; lidl with-editor
              (require 'server) (unless (server-running-p) (server-start))
              (setenv "GIT_EDITOR" "emacsclient")))
  (add-hook 'server-visit-hook
            (lambda () (local-set-key (kbd "C-c C-c") #'server-edit)))
  (when (not (or (getenv "GCTL_SESSION_ID") (getenv "TERM_SESSION_ID")))
    (setenv "GCTL_SESSION_ID" (string-trim (shell-command-to-string "uuidgen"))))
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (eshell/addpath (concat (getenv "GOPATH") "/bin")))

(add-hook #'eshell-mode-hook
          (lambda nil
            (define-key eshell-hist-mode-map
                        (kbd "C-r") #'eshell-insert-history)))

;;; Version control

;;;; Ediff

(setq-default ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain)

(define-advice ediff-vc-internal
    (:around (orig-fun &rest args) custom-quit)
  (apply orig-fun args)
  (switch-to-buffer "*Ediff Control Panel*")
  (define-key ediff-mode-map (kbd "q")
              (lambda () (interactive)
                (let ((rev (if (string-match-p
                                "\\*vc-\\|\\*ediff-revision"
                                (buffer-name ediff-buffer-A))
                               ediff-buffer-B ediff-buffer-A))
                      (a ediff-buffer-A) (b ediff-buffer-B))
                  (ediff-really-quit nil)
                  (kill-buffer rev)
                  (switch-to-buffer (if (eq rev b) a b))))))

(add-hook 'ediff-before-setup-hook #'tab-bar-new-tab)
(add-hook 'ediff-quit-hook
          (lambda nil
            (tab-bar-close-tab)
            (kill-buffer ediff-registry-buffer)))
(with-eval-after-load 'ediff
  (advice-add 'ediff-quit :around
              (lambda (&rest args) (ediff-really-quit args))))

;;;; Smerge

(with-eval-after-load 'smerge-mode
  (define-key ctl-x-map (kbd ",") smerge-basic-map)
  (repeat-mode 1) (setq diff-refine 'navigation)
  (map-keymap (lambda (_key cmd)
                (when (symbolp cmd) (put cmd 'repeat-map 'smerge-basic-map)))
              smerge-basic-map))

;;;; Log-edit

(with-eval-after-load 'log-edit
  (define-advice log-edit-show-files (:after (&rest _args) show-diff)
    (let ((orig-window (selected-window)))
      (log-edit-show-diff) (select-window orig-window))
    (setq-local other-window-scroll-buffer (get-buffer "*vc-diff*")))
  (defun my/vc-cleanup-buffers ()
    (dolist (buf '("*log-edit-files*" "*vc-diff*" "*vc*"))
      (when-let* ((b (get-buffer buf))) (kill-buffer b))))
  (dolist (fn '(log-edit-done log-edit-kill-buffer))
    (advice-add fn :after #'my/vc-cleanup-buffers)))

;;;; PR review with ediff (claude)

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

;;; Browsing and web

(setq shr-max-image-proportion 0.5 shr-use-colors nil)

(defun my-shr-tag-render (tag face-spec &optional predicate) ; src: takeonrules
  (let ((default-renderer (intern (format "shr-tag-%s" tag))))
    (lambda (dom) (let ((start (point)))
                    (funcall default-renderer dom)
                    (when (or (null predicate) (funcall predicate dom))
                      (add-face-text-property start (point) face-spec))))))

(defun my-table-is-data-p (dom)
  "Return non-nil if DOM looks like a data table, not a layout table."
  (or (equal (dom-attr dom 'role) "grid") (equal (dom-attr dom 'role) "table")
      (dom-attr dom 'summary) (dom-by-tag dom 'th)))

(with-eval-after-load 'shr
  (setq shr-external-rendering-functions
        `((pre        . ,(my-shr-tag-render 'pre        '(:inherit highlight :extend t)))
          (table      . ,(my-shr-tag-render 'table      '(:inherit mode-line-active) #'my-table-is-data-p))
          (blockquote . ,(my-shr-tag-render 'blockquote '(:inherit font-lock-string-face :slant italic)))
          (h1         . ,(my-shr-tag-render 'h1         '(:inherit bold :height 1.3)))
          (h2         . ,(my-shr-tag-render 'h2         '(:inherit bold :height 1.2)))
          (h3         . ,(my-shr-tag-render 'h3         '(:inherit bold :height 1.2))))))

(setq browse-url-handlers '(("youtu\\(?:\\.be\\|be\\.com\\)" .
                             (lambda (url &rest _)
                               (call-process-shell-command
                                (concat "nohup mpv " (shell-quote-argument url) " >/dev/null 2>&1 &"))))))

(autoload 'eww-open-in-new-buffer "eww")
(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook #'viper-mode)
  (defun my/eww-redirect-urls (url)
    (replace-regexp-in-string "://\\(www\\.\\)?reddit\\.com" "://old.reddit.com" url)
    (replace-regexp-in-string "://\\(www\\.\\)?x\\.com" "://nitter.net" url))
  (push 'my/eww-redirect-urls eww-url-transformers)
  (setq eww-header-line-format nil
        eww-auto-rename-buffer 'title
        eww-default-download-directory "~/Downloads/eww/"
        browse-url-new-window-flag t
        eww-use-browse-url
        "\\`mailto:\\|youtu\\(?:\\.be\\|be\\.com\\)"))

;;;; Dictionary

(setq dictionary-server "localhost")

(defun definition-at-point nil (interactive)
       (if (use-region-p)
           (dictionary-new-search
            (cons (buffer-substring-no-properties (mark) (point))
                  dictionary-default-dictionary))
         (dictionary-lookup-definition)))
(autoload 'dictionary-new-search "dictionary")

;;;; EPUB reader (claude, nov.el)

(defun epub--xml (path)
  (with-temp-buffer (insert-file-contents path) (libxml-parse-xml-region (point-min) (point-max))))

(defun epub-open (&optional file) (interactive "fEPUB: ")
       (require 'shr)
       (let* ((file (or file buffer-file-name))
              (dir (make-temp-file "epub-" t))
              (_ (call-process "unzip" nil nil nil "-qq" "-od" dir file))
              (opf-path (dom-attr
                         (car (dom-by-tag
                               (epub--xml (expand-file-name "META-INF/container.xml" dir))
                               'rootfile))
                         'full-path))
              (opf-dir (file-name-directory
                        (expand-file-name opf-path dir)))
              (opf (epub--xml (expand-file-name opf-path dir)))
              (manifest
               (mapcar (lambda (i)
                         (cons (dom-attr i 'id)
                               (expand-file-name (url-unhex-string (dom-attr i 'href)) opf-dir)))
                       (dom-by-tag opf 'item))))
         (let ((inhibit-read-only t)) (erase-buffer)
              (dolist (f (delq nil (mapcar
                                    (lambda (r)
                                      (cdr (assoc (dom-attr r 'idref) manifest)))
                                    (dom-by-tag opf 'itemref))))
                (when (string-match-p "\\.x?html?\\'" f)
                  (shr-insert-document
                   (with-temp-buffer
                     (insert-file-contents f)
                     (while (re-search-forward
                             "\\(src\\|href\\)=\"\\([^\"]+\\)\"" nil t)
                       (let ((v (match-string 2)))
                         (unless (string-match-p "^[a-z]+://" v)
                           (replace-match
                            (format "%s=\"file://%s\""
                                    (match-string 1)
                                    (expand-file-name (url-unhex-string v) (file-name-directory f)))
                            t t))))
                     (libxml-parse-html-region
                      (point-min) (point-max)))))))
         (goto-char (point-min)) (eww-mode) (viper-mode) (set-buffer-modified-p nil)
         (setq buffer-file-name file
               default-directory (file-name-directory file))
         (setq-local revert-buffer-function #'ignore
                     write-contents-functions '(ignore))
         (add-hook 'after-change-functions
                   (lambda (&rest _) (set-buffer-modified-p nil)) nil t)
         (add-hook 'kill-buffer-hook
                   (lambda () (delete-directory dir t)) nil t)))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . epub-open))

;;; Apps

;;;; ERC (IRC)

(setq erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-join-buffer 'buffer
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 18
      erc-header-line-format nil
      erc-prompt-for-password nil
      erc-use-auth-source-for-nickserv-password t
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE" "353" "366")
      erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#emacs-social" "##rust" "#uxn"
         "#zig" "#janet" "#clojure" "#racket" "#ocaml")))

(defun my-erc-tls () (interactive)
       (erc-tls :server "irc.libera.chat" :port 6697 :nick "brongulus"))

(with-eval-after-load 'erc
  (dolist (mod '(keep-place log nicks services xdcc)) (push mod erc-modules))
  (with-eval-after-load 'erc-track
    (define-key erc-track-minor-mode-map "\C-j" #'erc-track-switch-buffer))
  (erc-fill-mode 1) (erc-timestamp-mode -1) (erc-update-modules))

;;;; Gnus (news/RSS)

(setq gnus-directory "~/.emacs.d/gnus" gnus-startup-file "~/.emacs.d/.newsrc"
      gnus-use-dribble-file nil gnus-always-read-dribble-file nil
      gnus-interactive-exit nil gnus-widen-article-window t
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-use-adaptive-scoring '(word line) gnus-summary-expunge-below 0
      gnus-group-uncollapsed-levels 2 gnus-inhibit-startup-message t
      gnus-select-method '(nntp "news.gwene.org")
      gnus-sum-thread-tree-indent " " gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-root "" gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-single-leaf     "╰─►"
      gnus-user-date-format-alist '(((gnus-seconds-today) . " %H:%M") (t . "%b %d"))
      gnus-topic-line-format "%(%{%n - %A%}%) %v\n"
      gnus-group-line-format "%S%4y: %(%-40,40c%)\n"
      gnus-summary-line-format
      (concat " %0{%U%R%}" "%1{%&user-date;%}" "%3{ %}" " "
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

;;;; MPC (music)

(with-eval-after-load 'mpc
  (setq mpc-browser-tags '(Directory)
        mpc-mpd-music-directory "~/Downloads/music"
        mpc-songs-format "%-5{Time} %25{Title} %20{Album} %20{Artist}")
  (advice-add 'mpc :before (lambda (&rest _args) (tab-bar-new-tab)))
  (advice-add 'mpc :after (lambda (&rest _args) (call-interactively 'window-layout-transpose)))
  (advice-add 'mpc-quit :after (lambda (&rest _args) (tab-bar-close-tab)))
  (add-hook 'mpc-status-mode-hook (lambda nil (setq-local mode-line-format nil)))
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

;;;; Info and Man

(setq Info-default-directory-list '("~/.emacs.d/info")
      Info-use-header-line nil
      woman-manpath '("/usr/share/man")
      woman-cache-filename (expand-file-name "~/.emacs.d/woman-cache.el"))

;;; External packages

;;;; Eldoc-box (vendored since I can't live without this)

(with-eval-after-load 'eglot (load "~/.emacs.d/eldoc-box" :noerr :no-message))
(setq eldoc-box-clear-with-C-g t)

(defun my/eldoc-get-help () (interactive)
       (if (derived-mode-p 'emacs-lisp-mode) (describe-symbol (symbol-at-point))
         (if (and (display-graphic-p) (symbolp 'eldoc-box-help-at-point))
             (eldoc-box-help-at-point)
           (eldoc-doc-buffer t))))

(with-eval-after-load 'eldoc
  (with-eval-after-load 'eldoc-box
    (setq eldoc-box-max-pixel-width 800
          eldoc-box-max-pixel-height 700
          eldoc-box-only-multi-line t)))

;;;; Mark-multiple clone — experiment to see how far opus 4.6 can go

(load "~/.emacs.d/mini-mark-multiple" :noerr :no-message)
(dolist (b '(("M-p" . mmm/mark-previous-like-this) ("M-n" . mmm/mark-next-like-this)
             ("M-'" . mmm/mark-all-like-this) ("M-r" . mmm/mark-all-in-defun)))
  (keymap-global-set (car b) (cdr b)))

;;;; Corfu clone — experiment to see how far opus 4.6 can go

(load "~/.emacs.d/ac-lite" :noerr :no-message)
(add-hook 'prog-mode-hook #'ac-mode)

;;; init.el ends here
