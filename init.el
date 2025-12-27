;; init.el --- NANO Emacs (minimal version)  -*- lexical-binding: t -*-
;; --- Speed benchmarking ---------------------------------------------------
;; (load "~/.emacs.d/lisp/benchmarking.el" :noerr :no-message)
;; (setq init-start-time (current-time))
(setq inhibit-startup-screen t
      ;; toggle-debug-on-error t
      custom-file (make-temp-file "emacs-custom"))
;; (profiler-start 'cpu)

;; --- Typography stack -----------------------------------------------------
(defvar my-font-configs
  '((input :family "Input Mono Narrow" :weight light :bold-weight regular)
    (ioskeley :family "Ioskeley Mono" :weight regular :bold-weight bold)
    (commit :family "CommitMono Nerd Font Mono" :weight regular :bold-weight bold)
    (victor :family "Victor Mono" :weight regular :bold-weight demi-bold)))
(let ((config (alist-get 'input my-font-configs)))
  (set-face-attribute 'default nil :family (plist-get config :family)
                      :weight (plist-get config :weight)
                      :height (if (eq system-name 'android) 160 150))
  (set-face-attribute 'bold nil :weight (plist-get config :bold-weight))
  (set-face-attribute 'bold-italic nil :weight (plist-get config :bold-weight))
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
  
  (if (not (string= (plist-get config :family) "Input Mono Narrow"))
      (dolist (face '(fixed-pitch-serif variable-pitch variable-pitch-text))
        (set-face-attribute face nil :family (face-attribute 'default :family)))
    (dolist (face '(variable-pitch variable-pitch-text))
      (set-face-attribute face nil :family "Input Sans Narrow"))
    (set-face-attribute 'fixed-pitch-serif nil :family "Input Serif Condensed")))
(setq-default line-spacing 5) ; 7
(set-face-attribute 'nobreak-space nil :underline nil)
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?→))
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
;; (setq default-input-method 'english-dvorak)
;; (define-key (current-global-map) (kbd "<f8>") #'toggle-input-method)

;; --- Activate / Deactivate modes ------------------------------------------
(blink-cursor-mode -1) (kill-ring-deindent-mode 1)
(run-with-idle-timer 0.1 nil #'fido-vertical-mode)
(global-subword-mode 1) (global-eldoc-mode -1)
(defun my-lazy-load-modes ()
  (pixel-scroll-precision-mode 1) ;(winner-mode 1)
  (delete-selection-mode 1) (global-auto-revert-mode 1) (which-key-mode 1)
  (minibuffer-depth-indicate-mode) (savehist-mode 1) (which-function-mode 1)
  (save-place-mode 1) (global-goto-address-mode) (tooltip-mode -1)
  (unless (display-graphic-p) (xterm-mouse-mode)))
(run-with-idle-timer 0.3 nil #'my-lazy-load-modes)

;; --- Minimal theme --------------------------------------
(setq modus-themes-common-palette-overrides
      '((fringe bg-main)
        (bg-line-number-inactive bg-main)
        (bg-line-number-active bg-main))
      modus-vivendi-palette-overrides
      '((bg-main "#212121")))
;; (load-theme 'modus-operandi-deuteranopia)
;; (setq custom-theme-directory "~/.emacs.d/themes/"
;;       custom-safe-themes t)
;; (load-theme 'stillpoint)
(set-face-attribute 'fringe nil :background (face-background 'default))
(load "~/.emacs.d/lisp/nano-theme" :noerr :no-message)

;; --- Header & mode lines --------------------------------------------------
(defvar tab-bar--tab-keymaps (make-vector 20 nil)
  "Pre-allocated keymaps for tabs.")
(dotimes (i 20)
  (let ((map (make-sparse-keymap))
        (idx i))
    (define-key map [mode-line mouse-1]
                `(lambda () (interactive) (tab-bar-select-tab ,(1+ idx))))
    (aset tab-bar--tab-keymaps i map)))

(setq-default flymake-mode-line-counter-format
              '("" flymake-mode-line-error-counter
                flymake-mode-line-warning-counter
                flymake-mode-line-note-counter " ")
              flymake-mode-line-format
              '(" " flymake-mode-line-exception flymake-mode-line-counters)
              global-mode-string nil)
(setq-default mode-line-end-spaces
              '((:eval (when (and (featurep 'org-clock)
                                  (org-clock-is-active))
                         org-mode-line-string))
                (:eval (when (or (eq major-mode 'compilation-mode)
                                 (eq major-mode 'comint-mode))
                         compilation-mode-line-errors))
                (:eval (when (bound-and-true-p flymake-mode)
                         flymake-mode-line-format))
                " "))
(setq-default mode-line-format
              '("%e"
                (:eval
                 (when (mode-line-window-selected-p)
                   (let* ((tabs (tab-bar-tabs))
                          (count (length tabs)))
                     (when (> count 1)
                       (let ((active (tab-bar--current-tab-index)))
                         (propertize
                          (concat " "
                                  (mapconcat
                                   (lambda (i)
                                     (propertize (if (= i active) "⦿" "○") 'mouse-face 'mode-line-highlight
                                                 'local-map (aref tab-bar--tab-keymaps i)))
                                   (number-sequence 0 (1- count)) " ")
                                  " ")
                          'face 'bold))))))
                (:eval (when (and (buffer-narrowed-p) (not (derived-mode-p 'Info-mode)))
                         (propertize "(N)")))
                (:eval (let ((prefix (cond ((buffer-modified-p) "** ")
                                           (buffer-read-only "RO ")
                                           (t "   "))))
                         (propertize (format "%s%s" prefix
                                             (replace-regexp-in-string "\\*" "" (buffer-name)))
                                     'face (if (buffer-modified-p) 'bold-italic 'bold)
                                     'help-echo (buffer-file-name))))
                (:eval (propertize (string-trim-left
                                    (format-mode-line vc-mode))
                                   'face '(:weight light :slant italic)))
                (:eval (unless display-line-numbers
                         (propertize "   L%l" 'face 'shadow)))
                (:eval (let ((prefix (cond
                                      ((or defining-kbd-macro executing-kbd-macro) "▶▶")
                                      ((region-active-p)
                                       (concat "%p " (format "{%d}"
                                                             (count-lines (region-beginning)
                                                                          (region-end)))))
                                      ((eq major-mode 'nov-mode)
                                       (format "[%d/%d]" ;(/ (window-start) 0.01 (point-max))
                                               (1+ nov-documents-index)
                                               (length nov-documents)))
                                      ((eq major-mode 'doc-view-mode)
                                       (format "[%d/%d]" (doc-view-current-page)
                                               (doc-view-last-page-number)))
                                      ((or meow-mode buffer-read-only
                                           (eq major-mode 'eww-mode))
                                       "%p")
                                      ((buffer-modified-p) "**")
                                      (t                   "--"))))
                         (propertize (concat "   " prefix " ") 'face 'shadow)))
                mode-line-format-right-align
                (when (and (bound-and-true-p eglot--managed-mode) (eglot-managed-p))
                  eglot-mode-line-progress)
                ;; (:eval (when (derived-mode-p 'text-mode)
                ;;          (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
                ;;                 (end (if (use-region-p) (region-end) (point-max)))
                ;;                 (word-count (count-words beg end)))
                ;;            (propertize (format " %d Words" word-count)
                ;;                        'face 'font-lock-comment-face))))
                (:eval (propertize
                        (concat " "
                                (if (derived-mode-p 'prog-mode)
                                    (format-mode-line (when which-function-mode which-func-current))
                                  (format-time-string "%a %H:%M"))
                                " ")
                        'face (if (or (display-graphic-p) (mode-line-window-selected-p))
                                  'mode-line-active
                                'mode-line-inactive)))
                (:eval (when (mode-line-window-selected-p)
                         mode-line-end-spaces))))

(add-hook 'post-command-hook #'(lambda nil (when (region-active-p) (force-mode-line-update))))

(defvar default-mode-line-format mode-line-format)
(defun toggle-mode-line nil (interactive)
       (if mode-line-format
           (setq-local mode-line-format nil)
         (setq-local mode-line-format default-mode-line-format)))
(define-key (current-global-map) (kbd "C-x t m") #'toggle-mode-line)

;; --- Minibuffer completion ------------------------------------------------
(setq tab-always-indent 'complete
      tab-first-completion 'word-or-paren
      completions-detailed t
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 9
      icomplete-separator " . "
      icomplete-with-completion-tables t
      icomplete-in-buffer nil; t
      icomplete-max-delay-chars 0
      icomplete-scroll t
      resize-mini-windows 'grow-only)
(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-j") #'icomplete-fido-exit)
  (define-key icomplete-fido-mode-map (kbd "TAB") #'icomplete-forward-completions)
  (define-key icomplete-fido-mode-map (kbd "<backtab>") #'icomplete-backward-completions)
  (define-key icomplete-fido-mode-map (kbd "<escape>") #'minibuffer-keyboard-quit))

(add-hook 'minibuffer-setup-hook
          (lambda nil (setq-local truncate-lines t line-spacing nil)))

(defun file-capf ()
  "File completion at point function. src: eshelyaron."
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (when bounds
      (list (car bounds) (cdr bounds) #'completion-file-name-table
            :annotation-function (lambda (_) " File")
            :exclusive 'no))))
(add-hook 'completion-at-point-functions #'file-capf)

;; --- Minimal key bindings -------------------------------------------------
(defun nano-quit (&optional interactive)
  "A sensible `keyboard-quit'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ((derived-mode-p 'completion-list-mode)
           (delete-completion-window))
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))
(define-key (current-global-map) [remap keyboard-quit] #'nano-quit)

(defun my-goto-doc nil (interactive)
       (cond ((derived-mode-p 'emacs-lisp-mode)
              (describe-symbol (symbol-at-point)))
             ((locate-library "eldoc-box")
              (eldoc-box-help-at-point))
             (t (eldoc-doc-buffer t))))

(defun my-scroll-other-down nil (interactive)
       (let ((mode (with-current-buffer (window-buffer (other-window-for-scrolling))
                     major-mode)))
         (with-selected-window (other-window-for-scrolling)
           (cond ((eq mode 'Info-mode) (Info-scroll-up))
                 ((eq mode 'nov-mode) (nov-scroll-up 5))
                 ((eq mode 'doc-view-mode) (doc-view-scroll-up-or-next-page 5))
                 (t (scroll-up-command 5))))))
(defun my-scroll-other-up nil (interactive)
       (let ((mode (with-current-buffer (window-buffer (other-window-for-scrolling))
                     major-mode)))
         (with-selected-window (other-window-for-scrolling)
           (cond ((eq mode 'Info-mode) (Info-scroll-down))
                 ((eq mode 'nov-mode) (nov-scroll-down 5))
                 ((eq mode 'doc-view-mode) (doc-view-scroll-down-or-previous-page 5))
                 (t (scroll-down-command 5))))))

(define-key (current-global-map) (kbd "s-t") nil)
(dolist (bind '(("C-x C-m" . execute-extended-command)
                ("C-x x b" . ibuffer) ("C-x x e" . eval-last-sexp)
                ("C-x x c" . save-buffers-kill-emacs)
                ("C-x x f" . find-file) ("C-x x s" . save-buffer)
                ("C-x x z" . restart-emacs) ("C-z" . delete-backward-char)
                ("C-o" . other-window) ("C-x /" . project-find-regexp)
                ("C-x ;" . comment-line) ("C-h ." . my-goto-doc)
                ("C-h '" . describe-face) ("C-," . my-scroll-other-down)
                ("C-." . my-scroll-other-up) ("C-<tab>" . tab-next)
                ("C-S-<tab>" . tab-previous) ("C-x C-b" . ibuffer)
                ("M-s r" . replace-regexp) ("C-x k" . kill-current-buffer)
                ("C-x f" . recentf-open) ("C-g" . keyboard-quit)))
  (define-key (current-global-map) (kbd (car bind)) (cdr bind)))
(define-key (current-global-map) (kbd "C-x m") esc-map)
(define-key (current-global-map) (kbd "C-<wheel-up>") nil)
(define-key (current-global-map) (kbd "C-<wheel-down>") nil)
(define-key window-prefix-map (kbd "m") #'maximize-window)
(define-key window-prefix-map (kbd "u") #'winner-undo)
(define-key window-prefix-map (kbd "r") #'winner-redo)

;; --- Sane settings --------------------------------------------------------
(set-default-coding-systems 'utf-8)
(setq-default tab-width 4
              completion-styles
              '(basic substring initials flex) ;partial-completion
              completion-cycle-threshold t
              ;; cursor-type 'bar
              enable-recursive-minibuffers t
              imenu-flatten t
              display-line-numbers-width 4
              display-line-numbers-widen t
              initial-scratch-message nil
              indent-tabs-mode nil
              mouse-wheel-tilt-scroll t
              mouse-wheel-flip-direction t
              mouse-wheel-scroll-amount-horizontal 4
              pop-up-windows nil
              ring-bell-function 'ignore
              select-enable-clipboard t
              show-paren-context-when-offscreen t
              show-paren-when-point-inside-paren t
              use-short-answers t
              use-dialog-box nil
              uniquify-buffer-name-style 'forward)

(setq completion-category-defaults nil
      completion-category-overrides
      '((project-file (styles basic partial-completion substring initials flex))
        (file (styles basic partial-completion substring initials flex))))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(when (featurep 'recentf)
  (add-hook 'kill-emacs-hook #'recentf-cleanup))
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook #'hs-minor-mode)
(dolist (mode-hook '(prog-mode-hook conf-mode-hook yaml-ts-mode-hook))
  (add-hook mode-hook #'display-line-numbers-mode))
;;   (add-hook mode-hook #'hl-line-mode))
(add-to-list 'auto-mode-alist '("\\.log\\'" . (lambda () (display-line-numbers-mode))))
;; (add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

(put 'narrow-to-region 'disabled nil)

(advice-add #'server-force-delete :around #'silent-command)
(run-with-idle-timer 1 nil
                     #'(lambda nil
                         (unless server-mode (server-force-delete) (server-mode))))

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backup/" t))
      backup-directory-alist `(("." . "~/.emacs.d/backup/"))
      lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t))
      ;; ^^ https://emacs.stackexchange.com/a/81518/28970
      auto-revert-verbose nil
      ;; auto-revert-avoid-polling t
      blink-cursor-delay 0.8
      comint-prompt-read-only t
      comint-buffer-maximum-size 2048
      compilation-ask-about-save nil
      completion-ignore-case t
      completion-auto-help 'lazy;nil
      confirm-kill-emacs 'yes-or-no-p
      confirm-nonexistent-file-or-buffer nil
      delete-pair-blink-delay t
      delete-pair-push-mark t
      delete-by-moving-to-trash t
      diff-default-read-only t
      dired-clean-confirm-killing-deleted-buffers nil
      dired-create-destination-dirs 'ask
      dired-deletion-confirmer 'y-or-n-p
      dired-dwim-target t
      dired-omit-verbose nil
      dired-use-ls-dired (not (eq system-type 'darwin))
      dired-kill-when-opening-new-dired-buffer t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      electric-pair-preserve-balance 'electric-pair-inhibit-predicate
      electric-pair-delete-adjacent-pairs t
      ;; electric-pair-open-newline-between-pairs nil
      electric-pair-skip-whitespace nil
      eldoc-echo-area-prefer-doc-buffer t
      eldoc-idle-delay 0.2
      eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-display-truncation-message nil
      find-function-C-source-directory "~/repos/emacs/src"
      ffap-machine-p-known 'reject
      flymake-suppress-zero-counters t
      flymake-no-changes-timeout 2
      flymake-show-diagnostics-at-end-of-line 'short
      flymake-warning-bitmap '(large-circle compilation-warning)
      flymake-error-bitmap '(large-circle compilation-error)
      flymake-note-bitmap '(large-circle compilation-info)
      help-window-select t
      hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil
      kill-buffer-delete-auto-save-files t
      kill-do-not-save-duplicates t
      kill-ring-max 1000
      pixel-scroll-precision-interpolate-page t
      recentf-max-saved-items 200
      recentf-auto-cleanup 'never
      save-abbrevs nil
      save-interprogram-paste-before-kill t
      savehist-additional-variables '(register-alist kill-ring)
      maximum-scroll-margin 0.5
      scroll-conservatively 101
      scroll-preserve-screen-position t
      set-mark-command-repeat-pop t
      shell-command-prompt-show-cwd t
      shell-kill-buffer-on-exit t
      shell-file-name (car (process-lines "which" "fish"))
      tab-bar-show nil
      tramp-mode nil ;; FIXME
      vc-allow-rewriting-published-history 'ask
      vc-display-status 'no-backend
      vc-follow-symlinks t
      widget-image-enable nil
      which-func-unknown ""
      xref-search-program (if (executable-find "rg") 'ripgrep 'grep)
      xref-auto-jump-to-first-xref nil
      xref-show-definitions-function 'xref-show-definitions-buffer-at-bottom
      xref-show-xrefs-function 'xref-show-definitions-completing-read)

(run-with-idle-timer
 0.9 nil (lambda nil
           (file-to-register "~/Downloads/videos/" ?v)
           (file-to-register "~/.emacs.d/init.el" ?i)
           (file-to-register "~/dotfiles/flake.nix" ?n)
           (file-to-register "~/Dropbox/org/log.org" ?l)
           (file-to-register "~/Dropbox/org/jap_log.org" ?j)
           (file-to-register "/Volumes/PortableSSD/" ?p)))

(when (executable-find "rg")
  (setq grep-command "rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)"
        grep-command-position 27))

(setq isearch-wrap-pause 'no
      isearch-lazy-count t
      isearch-allow-scroll 'unlimited
      isearch-regexp-lax-whitespace t
      search-whitespace-regexp ".*?"
      sentence-end-double-space nil)

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "M-o") #'isearch-occur)
  (define-key isearch-mode-map (kbd "M-<") #'isearch-beginning-of-buffer)
  (define-key isearch-mode-map (kbd "M->") #'isearch-end-of-buffer)
  (define-key isearch-mode-map (kbd "TAB") #'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<backtab>") #'isearch-repeat-backward))

(with-eval-after-load 'completion-preview
  (setq completion-preview-message-format nil)
  (define-key completion-preview-active-mode-map (kbd "M-n") #'completion-preview-next-candidate)
  (define-key completion-preview-active-mode-map (kbd "M-p") #'completion-preview-prev-candidate))

;; (with-eval-after-load 'dabbrev
;;   (advice-add #'dabbrev-capf :before #'dabbrev--reset-global-variables)
;;   (add-hook 'completion-at-point-functions #'dabbrev-capf 100))

(with-eval-after-load 'dired
  (when (eq system-type 'darwin)
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil
          dired-listing-switches
          "-l --almost-all --human-readable --group-directories-first"))
  (set-face-attribute 'dired-directory nil :inherit 'warning)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "SPC") ctl-x-map)
  (define-key dired-mode-map (kbd "j") #'next-line)
  (define-key dired-mode-map (kbd "k") #'previous-line)
  (define-key dired-mode-map (kbd "\\") #'dired-up-directory)
  (define-key dired-mode-map (kbd "I") #'dired-kill-subdir)
  (define-key dired-mode-map (kbd "q") #'kill-current-buffer)
  (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-o") #'other-window))

(add-hook 'ediff-before-setup-hook #'tab-bar-new-tab)
(add-hook 'ediff-quit-hook
          (lambda nil
            (tab-bar-close-tab)
            (kill-buffer ediff-registry-buffer)))
(with-eval-after-load 'ediff
  (advice-add 'ediff-quit :around (lambda (&rest args)
                                    (ediff-really-quit args)))
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-diff-options "-w"))

(define-key occur-mode-map (kbd "TAB") #'occur-mode-display-occurrence)

;; install-info --dir-file=./dir --info-file=
(push "~/.emacs.d/info" Info-default-directory-list)
(setq Info-use-header-line nil)
(add-hook 'Info-mode-hook #'(lambda nil (setq-local left-margin-width 5)))

(defun silent-command (fn &rest args)
  "Used to suppress output of FN."
  (let ((inhibit-message t)
        (message-log-max nil)
        (save-silently t))
    (apply fn args)))

(add-to-list 'write-file-functions
             '(lambda () (when (derived-mode-p 'emacs-lisp-mode) (check-parens)) nil))

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  (mapc #'disable-theme custom-enabled-themes))
;; terminal stuff
(unless (display-graphic-p)
  (setq interprogram-cut-function
        (lambda (text)
          (when (use-region-p)
            (let* ((clipboard-commands
                    '(("darwin" . "pbcopy")
                      ("gnu/linux" . "xclip -selection clipboard")
                      ("windows-nt" . "clip")))
                   (copy-cmd (or (cdr (assoc (symbol-name system-type) clipboard-commands))
                                 nil)))
              (when copy-cmd
                (call-process-region
                 (region-beginning) (region-end) copy-cmd)
                (deactivate-mark))))))
  (menu-bar-mode -1))

;; --- Window Management ----------------------------------------------------
(dolist (pops '(("\\*eshell-pop\\*" . -2 ) ;; <-- prima donna
                ("^\\*term.*\\*$" . -1) ("^\\*compilation.*\\*$" . -1)
                ("vc-git :.\*" . 0) ("\\*vc.\*-log\\*" . 0) ("\\*eldoc\\*" . 0) ("\\*Help\\*" . 0)
                ("\\*Warnings\\*" . 1) ("\\*log-edit-files\\*" . 1)
                ("\\*Occur.*\\*$" . 1) ("\\*grep.*\\*$" . 1) ("CAPTURE-.*" . 1)
                ("\\*Org Select\\*" . 1) ("\\*xref\\*" . 1) ;("^\\*Dictionary\\*" . 1)))
                ("\\*Flymake diagnostics.*\\*$" . 1)))
  (add-to-list 'display-buffer-alist
               `(,(car pops)
                 display-buffer-in-side-window
                 (body-function . select-window)
                 (side . bottom)
                 (slot . ,(cdr pops))
                 (window-height . 0.33)
                 ,(when (display-graphic-p)
                    `(window-parameters . ((header-line-format . "")
                                           ,(unless (string= (car pops)
                                                             "^\\*compilation.*\\*$")
                                              '(mode-line-format . ""))))))))
(add-to-list 'display-buffer-alist ; eldoc\\|Help\\|
             '("\\*\\(Dictionary\\)\\*" display-buffer-in-side-window
               (body-function . select-window)
               (window-parameters . ((split-window . #'ignore)))
               (side . right) (slot . 1) (window-width . 82)))
(with-eval-after-load 'eldoc
  (when (fboundp 'eldoc-doc-buffer)
    (advice-add 'eldoc-doc-buffer :after
                (lambda (&rest _)
                  (with-current-buffer (get-buffer "*eldoc*")
                    (visual-line-mode 1))))))
(add-hook 'help-mode-hook #'visual-line-mode)

(defvar side-face-cookie nil)
(defun toggle-side-normal-window ()
  "Toggle the current window between a side window and a normal window."
  (interactive)
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (side (window-parameter window 'window-side)))
    (delete-window window)
    (if side
        (let ((display-buffer-overriding-action '((display-buffer-pop-up-window))))
          (pop-to-buffer buffer)
          (setq side-face-cookie
                (face-remap-add-relative 'header-line :overline (face-background 'default))))
      (progn
        (display-buffer buffer)
        (face-remap-remove-relative side-face-cookie)))))

(define-key (current-global-map) (kbd "<f10>") #'toggle-side-normal-window)
(when (display-graphic-p)
  (dolist (modes '(occur-hook vc-git-log-edit-mode-hook
                              compilation-mode-hook term-mode-hook eshell-mode-hook
                              help-mode-hook grep-mode-hook special-mode-hook))
    (add-hook modes (lambda () (setq-local header-line-format "")))))

(defun my-smart-window-selection-advice (orig-fun &rest args)
  "Use 'pos strategy on window-delete from same buffer split, else 'mru."
  (let ((delete-window-choose-selected 
         (let* ((current-buffer (current-buffer))
                (current-window (selected-window))
                (same-buffer-windows (delq current-window
                                           (get-buffer-window-list current-buffer nil t))))
           (if same-buffer-windows 'pos 'mru))))
    (apply orig-fun args)))

(advice-add 'delete-window :around #'my-smart-window-selection-advice)

(with-eval-after-load 'flymake
  (define-key flymake-project-diagnostics-mode-map (kbd "q") #'quit-window))
(with-eval-after-load 'comint-mode
  (define-key comint-mode-map "q" #'kill-buffer-and-window))
(with-eval-after-load 'compile
  (push 'go-test compilation-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-test
                 . (".*?\\([[:alnum:]_./-]+\\.go\\):\\([0-9]+\\)\\(?:\\(?::\\([0-9]+\\)\\)?\\| \\+0x[0-9a-f]+\\)"
                    1 2 3 nil 1)))
  (setq compile-command (or (car-safe compile-history) ""))
  (define-key compilation-minor-mode-map "q" #'kill-buffer-and-window))

(setq switch-to-buffer-obey-display-actions t)
(setq display-buffer-base-action
      '((display-buffer-pop-up-window)))

(define-key (current-global-map) (kbd "M-j") #'window-toggle-side-windows)
(define-key occur-mode-map (kbd "C-o") #'other-window)

;; --- Shell/term/compile ---------------------------------------------------
(define-advice term-handle-exit (:after (&rest _args) term-kill-on-exit)
  (kill-buffer))

(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))

(with-eval-after-load 'compile
  (setq compilation-scroll-output t)
  (define-key compilation-mode-map (kbd "i") (lambda nil (interactive)
                                               (comint-mode)
                                               (setq-local buffer-read-only nil))))

(add-hook 'compilation-finish-functions
          (lambda (buffer status)
            (when (eq major-mode 'comint-mode)
              (setq-local buffer-read-only t)
              (compilation-minor-mode))))

(add-hook 'compilation-filter-hook (lambda nil
                                     (goto-address-mode -1)
                                     (unless (eq major-mode 'grep-mode)
                                       (ansi-color-compilation-filter)
                                       (ansi-osc-compilation-filter))))

;; --- Programming ----------------------------------------------------------
(define-key (current-global-map) (kbd "C-x c c") #'compile)
(define-key (current-global-map) (kbd "C-x c r") #'recompile)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|HACK\\|TODO\\|WIP\\|BUG\\|DONE\\)"
                1 font-lock-warning-face t)
               (";" . 'shadow)))))

(with-eval-after-load 'treesit
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
               ;; (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
               ;;             "split_parser" "tree-sitter-markdown/src") ;; 31
               ;; (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
               ;;                    "split_parser" "tree-sitter-markdown-inline/src") ;; 31
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

  (when (string< emacs-version "31")
    (add-hook 'prog-mode-hook #'my/setup-install-grammars))
  (setq go-ts-mode-indent-offset 4))

;; (define-derived-mode zig-mode c-mode "zig-mode")  ;; Until zig-ts-mode is core
(nconc auto-mode-alist
       `(("\\.zig\\.zon\\'"    . js-json-mode)
         ("\\.nix\\'"          . conf-mode)
         ("\\.fish\\'"         . conf-mode)
         ("\\.rs\\'"           . rust-ts-mode)
         ("\\.go\\'"           . go-ts-mode)
         ("\\go\\.mod\\'"      . go-mod-ts-mode)
         ("\\.ts\\'"           . typescript-ts-mode)
         ("\\.lua\\'"          . lua-ts-mode)
         ("\\.ya?ml\\'"        . yaml-ts-mode)
         ("\\.json\\'"         . js-json-mode)
         ("\\Dockerfile\\'"    . dockerfile-ts-mode)
         ("\\.dockerignore\\'" . dockerfile-ts-mode)
         ("\\.bin\\'"          . hexl-mode)
         ("\\.tpl\\'"          . php-ts-mode)
         ("\\.info\\'"         . Info-mode)))
;; ,(when (string> emacs-version "31")
;;  '("\\.md\\'" . markdown-ts-mode))))

(dolist (mode '(rust-ts-mode-hook go-ts-mode-hook python-mode-hook zig-mode-hook c++-mode-hook))
  (add-hook mode #'eglot-ensure))
;; (add-hook 'go-ts-mode-hook #'whitespace-mode)
(add-hook 'rust-ts-mode-hook
          (lambda nil (add-to-list 'process-environment "CARGO_TERM_COLOR=always" :append)))

(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda ()
                         (define-key c-mode-map (kbd "C-c C-c") #'compile)
                         (c-toggle-comment-style -1)))
(add-hook 'c-ts-mode-hook
          (lambda nil
            (setq-local c-ts-mode-indent-style 'gnu)
            (setq-local c-ts-mode-indent-offset 4)
            (c-ts-mode-toggle-comment-style -1)))

(with-eval-after-load 'project
  (setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)
  (setq project-vc-extra-root-markers '("Cargo.toml" "build.zig")); "go.mod")) ; use go.work instead
  (setq project-vc-ignores '("**/vendor/**")))

(define-key (current-global-map) (kbd "C-x =") #'eglot-code-actions)
(with-eval-after-load 'eglot
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil
        eglot-events-buffer-config '(:size 0 :format short)
        eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-inlay-hints-mode nil)

  (setq python-flymake-command '("ruff" "check" "--output-format=concise" 
                                 "--stdin-filename" "stdin" "-"))
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))
  (push '(zig-mode . ("zls")) eglot-server-programs)

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (eq major-mode 'python-mode)
                (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
              (when (eq major-mode 'go-ts-mode)
                (setq eldoc-documentation-functions
                      (remove #'eglot-signature-eldoc-function eldoc-documentation-functions)))))

  (defun my-eglot-organize-imports () (interactive)
         (ignore-errors (eglot-code-actions nil nil "source.organizeImports" t)))
  (defun my-eglot-setup ()
    (interactive)
    (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
    (add-hook 'before-save-hook 'eglot-format-buffer nil t))
  (add-hook 'eglot-managed-mode-hook #'my-eglot-setup))

(load "~/.emacs.d/lisp/snippets" :noerr :no-message)
(dolist (fn '(foxy-start-server-with-timer foxy-cycle-files foxy-run-all-tests))
  (autoload fn "~/.emacs.d/lisp/foxy.el"))
(with-eval-after-load 'foxy
  (setq-default foxy-compile-command (concat foxy-compile-command
                                             "-I" (getenv "HOME") "/comp/include ")))
(define-key (current-global-map) (kbd"C-x l") #'foxy-start-server-with-timer)
(define-key (current-global-map) (kbd"C-x ]") #'foxy-cycle-files)
(define-key (current-global-map) (kbd"C-x [") #'(lambda nil (interactive) (foxy-cycle-files -1)))
(define-key (current-global-map) (kbd"C-x '") #'foxy-run-all-tests)

;; --- Misc functions -------------------------------------------------------
(setq-default fill-column 140)
(setq dictionary-server "localhost")
(with-eval-after-load 'dictionary
  (set-face-attribute 'dictionary-word-definition-face nil :family (face-attribute 'default :family)))

(defun toggle-zen-buffer ()
  "Toggle center alignment of the buffer. Inspired by: jamesdyer."
  (interactive)
  (let* ((special-modes (or (eq major-mode 'org-mode) (eq major-mode 'markdown-mode)))
         (sm-half (ceiling (window-screen-lines) 2))
         (margin (if (or (equal (window-margins) '(0 . 0))
                         (null (car (window-margins))))
                     (/ (- (window-total-width) (if special-modes 150 fill-column)) 2) 0)))
    (visual-line-mode 1)
    (when (>= margin 0)
      (set-window-margins nil margin margin)
      ;; persist for the buffer
      (setq-local zen-buffer-enabled (> margin 0))
      (setq-local zen-buffer-margin margin)
      (when special-modes
        (text-scale-set (if (eq text-scale-mode-amount 0) 2 0))
        (setq-local line-spacing (if (eq line-spacing 7) 0.7 7))))
    (setq-local scroll-margin (if (or (zerop scroll-margin) (> margin 0)) sm-half 0)))) ;99999
(define-key (current-global-map) (kbd "C-x 9") #'toggle-zen-buffer)

(defun zen-buffer-apply-margins ()
  "Apply zen margins if enabled for this buffer."
  (when (and (bound-and-true-p zen-buffer-enabled)
             (bound-and-true-p zen-buffer-margin))
    (set-window-margins nil zen-buffer-margin zen-buffer-margin)))
(add-hook 'buffer-list-update-hook #'zen-buffer-apply-margins)

(defun match-pair nil
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 t t)
    (cond ((looking-at "\\s\(\\|\{") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)\\|\}") (forward-char 1) (backward-list 1))
          (t (backward-up-list 1 t t)))))

(defvar insert-pair-map ;; src: oantolin
  (let ((map (make-sparse-keymap)))
    (define-key map [t] #'insert-pair)
    map))

(defun mark-inner () ; src: Signal-Syllabub3072
  "Mark interior of the current list or string."
  (interactive)
  (condition-case nil
      (if (nth 3 (syntax-ppss)) ; string or list
          (let ((start (nth 8 (syntax-ppss))))
            (goto-char start)
            (set-mark (point))
            (forward-sexp))
        (backward-up-list) (set-mark (point))
        (down-list) (up-list) (backward-down-list))
    (error (message "No inner list or string found."))))

(defun my-select-fwd-line (arg)
  "Select ARG lines from current line and move cursor. src: Kaushal Modi."
  (interactive "p")
  (or arg (setq arg 1))
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun my-chord (initial-key final-key fn)
  (interactive) ;; src: wasamasa
  (let* ((timeout 0.4)
         (event (read-event nil nil timeout)))
    (if event ;; timeout met
        (if (and (characterp event) (= event final-key))
            (funcall fn)
          (insert initial-key)
          (push event unread-command-events))
      (insert initial-key))))

(defun my-mark-word nil
  (interactive)
  (if (use-region-p)
      (call-interactively 'mark-word)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (goto-char (car bounds))
        (push-mark (cdr bounds) nil t)))))

(defun my/quote-as-word (orig-fun &rest args)
  "Temporarily treat quotes as word constituents."
  (let ((old-syntax (char-syntax ?\")))
    (unwind-protect
        (progn
          (modify-syntax-entry ?\" "w")
          (apply orig-fun args))
      (modify-syntax-entry ?\" (string old-syntax)))))

(dolist (func '(kill-word backward-kill-word backward-word forward-word 
                          mark-word transpose-words capitalize-word
                          upcase-word downcase-word))
  (advice-add func :around #'my/quote-as-word))

(defun dired-vc-current (&optional dir-path) (interactive)
       (when (and dir-path (file-directory-p dir-path))
         (let ((current-buffer (current-buffer)))
           (dired-vc-left dir-path) (kill-buffer current-buffer))))

(defun dired-vc-left (&optional dir-path) (interactive)
       (let ((dir (dired-noselect (or dir-path (vc-root-dir) default-directory))))
         (display-buffer-in-side-window
          dir `((side . left) (slot . 0) (window-width . 0.2)
                (window-parameters . ((no-delete-other-windows . t)))))
         (with-current-buffer dir
           (use-local-map (copy-keymap (current-local-map)))
           (select-window (get-buffer-window dir))
           (define-key (current-local-map) (kbd "\\")
                       (lambda nil (interactive)
                         (dired-vc-current (file-name-parent-directory default-directory))))
           (define-key (current-local-map) (kbd "q") #'kill-buffer-and-window)
           (define-key (current-local-map) (kbd "`") #'window-toggle-side-windows)
           (define-key (current-local-map) (kbd "RET")
                       (lambda nil (interactive)
                         (let ((file (dired-get-file-for-visit)))
                           (if (file-directory-p file)
                               (dired-vc-current file)
                             (with-selected-window (or (window-in-direction 'right)
                                                       (split-window-right))
                               (find-file file))
                             (windmove-right))))))))
(define-key (current-global-map) (kbd "C-x d") #'dired-vc-left)

;; --- Mini Meow ------------------------------------------------------------
(autoload 'viper-ex "viper")
(define-key special-mode-map (kbd "j") #'next-line)
(define-key special-mode-map (kbd "k") #'previous-line)
(define-key special-mode-map (kbd "q") #'kill-buffer-and-window)

(defvar meow-mode-map (make-sparse-keymap) "")

(define-minor-mode meow-mode ""
  :init-value nil
  :lighter " meow"
  :keymap meow-mode-map)

(define-global-minor-mode global-meow-mode meow-mode
  (lambda ()
    (when (and (not (minibufferp)) (not noninteractive)
               (derived-mode-p 'fundamental-mode 'messages-buffer-mode
                               'prog-mode 'conf-mode 'outline-mode 'text-mode))
      (meow-mode 1))))
(global-meow-mode 1)
(defun meow-insert nil (interactive) (meow-mode -1))
(defun meow--set-cursor-type (type)
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((shape (or (car-safe type) type))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal (concat "\e[" param " q")))))
(add-hook 'meow-mode-hook
          (lambda nil (if (or meow-mode (derived-mode-p 'special-mode))
                          (progn (meow--set-cursor-type 'box) (blink-cursor-mode -1))
                        (progn (meow--set-cursor-type '(bar . 3)) (blink-cursor-mode 1)))))

(defun ctrl-meta-prefix-command ()
  "Read next key and execute it with C-M- prefix"
  (interactive)
  (let* ((key (read-key "C-M-"))
         (cmd (key-binding (vector (list 'control 'meta key)))))
    (if cmd
        (call-interactively cmd)
      (message "C-M-%c is not bound" key))))

(defun definition-at-point nil
  (interactive)
  (if (use-region-p)
      (dictionary-new-search
       (cons (buffer-substring-no-properties (mark) (point)) dictionary-default-dictionary))
    (dictionary-lookup-definition)))

(defun my/project-find-regexp-in-buffer ()
  "Find regexp in project, showing results in buffer."
  (interactive)
  (let ((xref-show-xrefs-function 'xref--show-xref-buffer))
    (call-interactively 'project-find-regexp)))

(define-key (current-global-map) (kbd "j") (lambda nil (interactive) (my-chord ?j ?k 'meow-mode)))
(define-key (current-global-map) (kbd "C-j") (lambda nil (interactive) (meow-mode t)))
(define-key (current-global-map) [escape] (lambda nil (interactive) (meow-mode t)))
(define-key meow-mode-map (kbd "g") (make-sparse-keymap))
(define-key meow-mode-map (kbd "m") (make-sparse-keymap))
(define-key meow-mode-map (kbd "z") (make-sparse-keymap))
(define-key meow-mode-map (kbd "H") help-map)
(define-key meow-mode-map (kbd "SPC") ctl-x-map)
(define-key meow-mode-map (kbd "SPC g") 'ctrl-meta-prefix-command)
(define-key meow-mode-map (kbd "ms") insert-pair-map)
(dolist (num '(0 1 2 3 4 5 6 7 8 9))
  (define-key meow-mode-map (int-to-string num) #'digit-argument))
(dolist (pair '(("\\" . dired-jump) ("gl" . move-end-of-line) ("ge" . move-end-of-line)
                ("gh" . back-to-indentation) ("gj" . end-of-buffer) ("gk" . beginning-of-buffer)
                ("q" . quit-window) ("=" . mark-inner) ("-" . negative-argument)
                ("e" . forward-word) ("b" . backward-word)
                ("v" . set-mark-command) ("h" . backward-char) ("j" . next-line)
                ("k" . previous-line) ("l" . forward-char) ("i" . meow-insert)
                ("y" . kill-ring-save) ("%" . match-pair) ("o" . other-window)
                ("D" . pixel-scroll-interpolate-down) ("U" . pixel-scroll-interpolate-up)
                ("gT" . tab-bar-switch-to-prev-tab) ("gt" . tab-bar-switch-to-next-tab)
                ("x" . my-select-fwd-line) ("X" . exchange-point-and-mark) ("O" . occur)
                ("w" . my-mark-word) ("," . my-scroll-other-down) ("s" . isearch-forward-regexp)
                ("." . my-scroll-other-up) ("/" . comment-line) ("gf" . ffap)
                ("gS" . scratch-buffer) ("*" . isearch-forward-symbol-at-point)
                ("ga" . (lambda nil (interactive) (org-agenda nil "n"))) ("gc" . org-capture)
                ("`" . window-toggle-side-windows) ("zz" . pop-to-mark-command)
                ("gi" . eglot-find-implementation) ("gs" . imenu) ("#" . definition-at-point)
                ("{" . flymake-goto-prev-error) ("}" . flymake-goto-next-error)
                ("g/" . xref-find-definitions-other-window) ("gd" . xref-find-definitions)
                ("gb" . xref-go-back) ("K" . my-goto-doc) (":" . viper-ex) ("gr" . xref-find-references)
                ("gx" . flymake-show-buffer-diagnostics) ("gX" . flymake-show-project-diagnostics)
                ("&" . align-regexp) ("C" . string-rectangle) ("mi" . mark-inner) ("p" . yank)
                ("P" . yank-pop) ("+" . eglot-rename) ("mm" . file-to-register)
                ("ml" . down-list) ("mu" . up-list) ("mb" . backward-list) ("mf" . forward-list)
                ("mj" . forward-sexp) ("mk" . backward-sexp) (";" . keyboard-quit)
                ("'" . register-to-point) ("md" . delete-pair) ("+" . eglot-code-actions)
                ("F" . my/project-find-regexp-in-buffer) ("Z" . undo-redo) ("u" . undo-only)
                ("R" . replace-regexp) ("zf" . hs-toggle-hiding) ("zc" . hs-hide-all) ("zs" . hs-show-all)
                ("<" . indent-rigidly-left-to-tab-stop) (">" . indent-rigidly-right-to-tab-stop)
                ("a" . (lambda nil (interactive) (meow-insert) (forward-char 1)))
                ("c" . (lambda nil (interactive) (meow-insert)
                         (if (use-region-p) (call-interactively 'kill-region) (delete-char 1))))
                ("A" . (lambda nil (interactive) (call-interactively 'move-end-of-line)
                         (meow-insert) (call-interactively 'newline)))
                ("I" . (lambda nil (interactive) (back-to-indentation) (meow-insert)
                         (call-interactively 'newline) (previous-line)
                         (call-interactively 'indent-for-tab-command)))
                ("d" . (lambda nil (interactive)
                         (if (use-region-p) (call-interactively 'kill-region) (delete-char 1))))
                ("f". (lambda nil (interactive) ;; (forward-char 1)
                        (call-interactively 'set-mark-command)
                        (let ((start-point (point))
                              (found-pos (search-forward (char-to-string (read-char nil t)) nil t)))
                          (if found-pos
                              (backward-char 1)
                            (goto-char start-point) (backward-char 1) (deactivate-mark)))))
                ("|" . (lambda nil (interactive)
                         (let ((current-prefix-arg '(4)))
                           (call-interactively 'shell-command-on-region))))
                ("r" . (lambda nil (interactive)
                         (delete-char 1) (insert-char (read-char nil t)) (backward-char 1)))
                ("J" . (lambda nil (interactive) (delete-indentation t)))))
  (define-key meow-mode-map (kbd (car pair)) (cdr pair)))

(when (eq system-type 'darwin)
  (select-frame-set-input-focus (selected-frame)))

;; --- VC -------------------------------------------------------------------
(with-eval-after-load 'smerge-mode
  (define-key ctl-x-map (kbd ",") smerge-basic-map)
  (repeat-mode 1)
  (setq diff-refine 'navigation)
  (map-keymap (lambda (_key cmd)
                (when (symbolp cmd) (put cmd 'repeat-map 'smerge-basic-map)))
              smerge-basic-map))

(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "q") #'kill-current-buffer))
(with-eval-after-load 'diff
  (define-key diff-mode-shared-map (kbd "q") #'kill-current-buffer))
(add-hook 'vc-before-checkin-hook #'tab-bar-new-tab)
(define-key vc-prefix-map (kbd "e") #'vc-ediff)
(define-key vc-prefix-map (kbd "f")
            (lambda () (interactive) (vc-git--pushpull "push" nil '("--force-with-lease"))))
(define-key vc-prefix-map (kbd "z") #'vc-git-stash)
(define-key vc-prefix-map (kbd "A") #'vc-git-stash-apply)
(define-key vc-prefix-map (kbd "S") #'vc-git-stash-show)
(define-key vc-prefix-map (kbd "Z") #'vc-git-stash-pop)

(define-advice log-edit-show-files (:after (&rest _args) show-diff)
  (setq-local other-window-scroll-default 'get-mru-window)
  (log-edit-show-diff)
  (switch-to-buffer "*vc-log*"))

(dolist (fn '(log-edit-done log-edit-kill-buffer))
  (eval `(define-advice ,fn (:after (&rest _args) buffer-cleanup)
           (ignore-errors
             (progn
               (kill-buffer "*log-edit-files*")
               (kill-buffer "*vc-diff*")
               (kill-buffer "*vc*")))
           (tab-bar-close-tab))))

(defun vc-ediff-quit nil
  (interactive)
  (let* ((revision-buf
          (if (string-match-p "\\*vc-\\|\\*ediff-revision" (buffer-name ediff-buffer-A))
              ediff-buffer-B
            ediff-buffer-A))
         (file-buf (if (eq revision-buf ediff-buffer-B) ediff-buffer-A ediff-buffer-B)))
    (ediff-really-quit nil)
    (kill-buffer revision-buf)
    (when (buffer-live-p file-buf)
      (switch-to-buffer file-buf))))

(define-advice ediff-vc-internal (:around (orig-fun &rest args) custom-quit)
  (apply orig-fun args)
  (switch-to-buffer "*Ediff Control Panel*")
  (define-key ediff-mode-map (kbd "q") #'vc-ediff-quit))

(setq vc-annotate-background-mode t)
(with-eval-after-load 'vc-annotate
  (defun vc-annotate-readable (&rest _)
    (dolist (anno-face (seq-filter
                        (lambda (face)
                          (string-prefix-p "vc-annotate-face-" (symbol-name face)))
                        (face-list)))
      (face-remap-add-relative anno-face :foreground "black")))

  (if vc-annotate-background-mode
      (advice-add 'vc-annotate-lines :after #'vc-annotate-readable))
  (define-key vc-annotate-mode-map
              "q" (lambda () (interactive)
                    (kill-current-buffer)
                    (tab-bar-close-tab)))
  (add-to-list 'display-buffer-alist
               '("^\\*Annotate.*\\*$"
                 (display-buffer-reuse-mode-window display-buffer-in-tab))))

;; --- Eshell ---------------------------------------------------------------
;; Eshell refs:
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(define-key (current-global-map) (kbd "C-\\")
            (lambda nil (interactive)
              (defvar eshell-buffer-name)
              (let ((eshell-buffer-name "*eshell-pop*"))
                (silent-command 'eshell))))
(setq eshell-aliases-file "~/.config/alias"
      eshell-scroll-to-bottom-on-input 'all
      eshell-hist-ignoredups 'erase
      eshell-history-size 20000
      eshell-save-history-on-exit t
      eshell-glob-case-insensitive t)

(defun my-eshell-only-aliases () ; create aliases that shouldn't be exported to common file
  (push '("source" ". $1") eshell-command-aliases-list)
  (push '("mkcd" "mkdir -p $1 && cd $1") eshell-command-aliases-list)
  (push '("k" "kubecolor $*") eshell-command-aliases-list)
  (push '("z" "eshell/z") eshell-command-aliases-list)
  (push '("ky" "kubecolor -oyaml $*") eshell-command-aliases-list)
  (push '("clear" "clear t") eshell-command-aliases-list)
  (push '("d" "dired-other-window $1") eshell-command-aliases-list)
  (push '("dired" "dired $1") eshell-command-aliases-list)
  (push '("ee" "find-file-other-window $1") eshell-command-aliases-list)
  (push '("ff" "find-file $1") eshell-command-aliases-list)
  (push '("e" "find-file $1") eshell-command-aliases-list)
  (push '("jq" "jq -M $*") eshell-command-aliases-list)
  (push '("rg" "rg --color=never --no-line-number $*") eshell-command-aliases-list)
  (push '("gd" "vc-diff") eshell-command-aliases-list)
  (push '("glog" "vc-print-root-log") eshell-command-aliases-list)
  (push '("groot" "cd ${git rev-parse --show-toplevel}") eshell-command-aliases-list)
  (push '("gpr" "git fetch origin pull/$1/head:$2; git checkout $2") eshell-command-aliases-list)
  (push '("nix-update-mac" "cd ~/dotfiles && HOSTNAME=${hostname -s} nix build .#darwinConfigurations.${hostname -s}.system --impure && cd -") eshell-command-aliases-list)
  (push '("darwin-rebuild-mac" "cd ~/dotfiles && HOSTNAME=${hostname -s} sudo ./result/sw/bin/darwin-rebuild switch --flake . --impure && cd -") eshell-command-aliases-list)
  (push '("gk" "export KUBECONFIG=${gardenctl kubectl-env zsh | awk -F\"'\" '/export KUBECONFIG/ {print \$2}'} && test -n \"$TMUX\" && (shell-command \"tmux set-option -p @kubeconfig \\\"$KUBECONFIG\\\"  && tmux refresh-client -S\")") eshell-command-aliases-list))

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

(with-eval-after-load 'em-term
  (setenv "PAGER" "cat")
  (dolist (cmd '("fzf" "yazi" "mpv" "emacsclient" "bat" "gh"))
    (add-to-list 'eshell-visual-commands cmd))
  (setq eshell-visual-options '(("git" "--help" "--paginate" "--patch")))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show"))))

(with-eval-after-load 'em-ls
  (set-face-attribute 'eshell-ls-directory nil :inherit font-lock-keyword-face))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook #'completion-preview-mode)
  (add-hook 'eshell-mode-hook
            (lambda nil
              (when (not (or (getenv "GCTL_SESSION_ID") (getenv "TERM_SESSION_ID")))
                (setenv "GCTL_SESSION_ID" (string-trim
                                           (shell-command-to-string "uuidgen"))))
              (setenv "GOPATH" (concat (getenv "HOME") "/go"))
              (eshell/addpath (concat (getenv "GOPATH") "/bin"))
              (eshell/addpath (concat (getenv "HOME") "/.krew/bin"))
              (add-to-list 'process-environment "KUBECTX_IGNORE_FZF=1" :append)))
  (push 'file-capf completion-at-point-functions)
  ;; src: doom
  (setq eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'my/eshell-default-prompt-fn)

  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'error)))

  (defun pwd-shorten-dirs (pwd)
    "Shorten all directory names in PWD except the last two."
    (let* ((dirs (split-string pwd "/"))
           (shortened-dirs
            (append
             (mapcar (lambda (dir)
                       (cond
                        ((string-empty-p dir) "")
                        ((string-prefix-p "." dir) (substring dir 0 2))
                        (t (substring dir 0 1))))
                     (butlast dirs))
             (last dirs))))
      (string-join shortened-dirs "/")))

  (defun get-kubectl-output (cmd)
    (when-let* ((kubeconfig (getenv "KUBECONFIG"))
                ((not (string-empty-p (string-trim kubeconfig))))
                (result (eshell-command-result 
                         (concat "kubectl --kubeconfig=" kubeconfig " " cmd))))
      (string-trim result)))

  (defun get-k8s-context-and-namespace ()
    (when-let* ((context (get-kubectl-output "config current-context"))
                ((not (string-empty-p context)))
                ((not (string-match "error" context))))
      (let* ((ns-cmd (format "config view -o 'jsonpath={.contexts[?(@.name==\"%s\")].context.namespace}'"
                             context))
             (namespace (or (get-kubectl-output ns-cmd) "default")))
        (propertize (format "(%s|%s) " context namespace)
                    'face 'error))))

  (defun my/eshell-default-prompt-fn ()
    (concat
     (get-k8s-context-and-namespace)
     ;; pwd git last-status
     (let ((pwd (eshell/pwd)))
       (propertize (if (equal pwd "~")
                       pwd
                     (pwd-shorten-dirs (abbreviate-file-name pwd)))
                   'face '(:inherit font-lock-keyword-face :weight bold)))
     (propertize (my/eshell--git-prompt)
                 'face '(:inherit font-lock-constant-face :slant italic))
     (if (zerop eshell-last-command-status)
         (propertize " λ" 'face 'success)
       (propertize (format " [%s] λ" eshell-last-command-status) 'face 'warning))
     " "))

  (defun my/eshell--git-prompt ()
    (let* ((git-dir (locate-dominating-file default-directory ".git"))
           (rebase-in-progress-p
            (and git-dir (or (file-exists-p (expand-file-name ".git/rebase-merge" git-dir))
                             (file-exists-p (expand-file-name ".git/rebase-apply" git-dir)))
                 (not (string-empty-p (my/eshell--git-output '("rev-parse" "--verify" "REBASE_HEAD") 128)))))
           (merge-in-progress-p
            (not (string-empty-p (my/eshell--git-output '("rev-parse" "--verify" "MERGE_HEAD") 128))))
           (git-branch
            (my/eshell--git-output '("symbolic-ref" "-q" "--short" "HEAD") -1)))
      (cond
       (rebase-in-progress-p " (REBASE-i)")
       (merge-in-progress-p " (MERGE-i)")
       (t git-branch))))

  (defun my/eshell--git-output (command err-status)
    (let* ((result (with-temp-buffer 
                     (list (or (apply #'call-process "git" nil t nil command) err-status)
                           (string-trim (buffer-string)))))
           (status (car result))
           (output (cadr result)))
      (if (equal status 0)
          (format " (%s)" output)
        "")))

  (defun eshell-venv ()
    "Activate Python virtual environment in eshell"
    (interactive)
    (let ((venv-path (expand-file-name ".venv")))
      (when (file-directory-p venv-path)
        (setenv "VIRTUAL_ENV" venv-path)
        (eshell-set-path (concat venv-path "/bin:" (getenv "PATH"))))))
  
  (defun eshell-insert-history () ; src: howard abrams
    "Displays the eshell history to select and insert back into your eshell."
    (interactive)
    (insert (completing-read "Eshell history: "
                             (delete-dups
                              (ring-elements eshell-history-ring)))))

  (defun eshell/z (&optional regexp) ; src: karthink
    "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
    (let ((eshell-dirs (delete-dups
                        (mapcar 'abbreviate-file-name
                                (ring-elements eshell-last-dir-ring)))))
      (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                   (completing-read "cd: " eshell-dirs)))))
  
  (defun my-eshell-narrow-to-prompt ()
    "Narrow buffer to prompt at point. src: ambrevar."
    (interactive)
    (narrow-to-region
     (save-excursion
       (forward-line) (call-interactively #'eshell-previous-prompt)
       (beginning-of-line) (point))
     (save-excursion
       (forward-line) (call-interactively #'eshell-next-prompt)
       (re-search-backward eshell-prompt-regexp nil t)
       (when (and (require 'eshell-prompt-extras nil 'noerror)
                  (eq eshell-prompt-function #'epe-theme-multiline-with-status))
         (previous-line))
       (point)))))

(defun direnv-update () ; src: claude and purcell/envrc
  (require 'json)
  (if-let* ((dir (locate-dominating-file
                  default-directory (lambda (d) (file-exists-p (expand-file-name ".envrc" d))))))
      (let* ((default-directory dir)
             (tmp (make-temp-file "direnv"))
             (env (unwind-protect
                      (with-temp-buffer
                        (when (and (zerop (call-process "direnv" nil (list t tmp) nil "export" "json"))
                                   (> (buffer-size) 0))
                          (goto-char 1)
                          (let ((json-key-type 'string)) (json-read-object))))
                    (delete-file tmp))))
        (when env
          (let* ((merged (append (mapcar (lambda (p) (if (cdr p) (format "%s=%s" (car p) (cdr p)) (car p))) env)
                                 (default-value 'process-environment)))
                 (path (getenv-internal "PATH" merged)))
            (setq-local process-environment merged exec-path (parse-colon-path path))
            (if (fboundp 'eshell-set-path) (eshell-set-path path) (setq-local eshell-path-env path)))))
    (kill-local-variable 'process-environment)
    (kill-local-variable 'exec-path)
    (when (derived-mode-p 'eshell-mode)
      (if (fboundp 'eshell-set-path) (eshell-set-path (butlast exec-path))
        (kill-local-variable 'eshell-path-env)))))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (setq-local global-hl-line-mode nil)
              (setenv "TERM" "xterm-256color")
              (add-hook 'eshell-directory-change-hook #'direnv-update nil t)
              (direnv-update)
              (define-key eshell-hist-mode-map (kbd "<up>") nil t)
              (define-key eshell-hist-mode-map (kbd "<down>") nil t)
              (define-key eshell-hist-mode-map (kbd "C-p")
                          #'eshell-previous-matching-input-from-input)
              (define-key eshell-hist-mode-map (kbd "C-n")
                          #'eshell-next-matching-input-from-input)
              (define-key eshell-mode-map (kbd "C-x n d") #'my-eshell-narrow-to-prompt)
              (define-key eshell-mode-map (kbd "C-u") (lambda nil (interactive) (kill-line 0)))
              (define-key eshell-mode-map (kbd "C-w") #'backward-kill-word)
              (define-key eshell-hist-mode-map (kbd "C-r") #'eshell-insert-history)))

(setq doc-view-resolution 600
      doc-view-continuous t
      doc-view-mupdf-use-svg t
      large-file-warning-threshold (* 50 (expt 2 20)))
(with-eval-after-load 'doc-view ;; requires `'gs', `mupdf-tools'
  (define-key doc-view-mode-map (kbd "SPC") ctl-x-map)
  (define-key doc-view-mode-map (kbd "j") #'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "k") #'doc-view-scroll-down-or-previous-page))

(with-eval-after-load 'org
  (load "~/.emacs.d/lisp/org-conf" :noerr :no-message)
  (add-hook 'org-capture-mode-hook 'meow-insert))

(with-eval-after-load 'gnus
  (load "~/.emacs.d/lisp/gnus-conf" nil :no-message)
  (with-eval-after-load 'gnus-art
    (define-key gnus-article-mode-map (kbd "SPC") ctl-x-map)
    (define-key gnus-article-mode-map (kbd "j") #'next-line)
    (define-key gnus-article-mode-map (kbd "k") #'previous-line))
  (with-eval-after-load 'gnus-group
    (define-key gnus-group-mode-map (kbd "SPC") ctl-x-map)
    (define-key gnus-group-mode-map (kbd "j") #'next-line)
    (define-key gnus-group-mode-map (kbd "k") #'previous-line))
  (with-eval-after-load 'gnus-topic
    (define-key gnus-topic-mode-map (kbd "SPC") ctl-x-map))
  (with-eval-after-load 'gnus-sum
    (define-key gnus-summary-mode-map (kbd "SPC") ctl-x-map)
    (define-key gnus-summary-mode-map (kbd "j") #'next-line)
    (define-key gnus-summary-mode-map (kbd "k") #'previous-line)))

;; erc
;; (use-package erc
;;   ;; auth: machine irc.libera.chat login "USER" password PASSWORD
;;   :ensure nil
;;   :commands my/irc
;;   :hook (erc-join . hl-line-mode)
;;   ;; :hook (erc-join . (lambda nil
;;   ;;                     (setq-local erc-fill-column (min (- (window-width) 3) 85))))
;;   ;; :hook (erc-kill-server . (lambda nil ;; (erc-status-sidebar-kill)
;;   ;;                            (tab-bar-close-tab)))
;;   :init
;;   (setq erc-autojoin-channels-alist '(("libera.chat" "#emacs")); "##rust")))
;;         erc-default-server "irc.libera.chat"
;;         erc-nick "brongulus"
;;         erc-nickserv-get-password nil
;;         erc-use-auth-source-for-nickserv-password t
;;         ;; (erc-fill-column (min (- (window-width) 3) 85))
;;         ;; (erc-status-side-bar-width 12)
;;         erc-autojoin-timing 'ident
;;         erc-fill-function 'erc-fill-static
;;         erc-fill-static-center 14
;;         erc-format-nick-function 'erc-format-@nick
;;         erc-header-line-face-method t
;;         erc-track-position-in-mode-line t
;;         erc-track-showcount t
;;         erc-track-shorten-function nil
;;         erc-track-exclude-server-buffer t
;;         erc-sasl-user "brongulus"
;;         erc-sasl-auth-source-function #'erc-auth-source-search
;;         erc-join-buffer 'bury ; window
;;         erc-kill-server-buffer-on-quit t
;;         erc-kill-buffer-on-part t
;;         erc-hide-list '("JOIN" "PART" "QUIT" "353") ;; 353 hide names
;;         erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK")
;;         erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
;;                                   "324" "329" "332" "333" "353" "477"))
;;   :config
;;   (defun my/irc nil
;;     "Setup ERC and connect if not already."
;;     (interactive)
;;     ;; (if (get-buffer "Libera.Chat") ;; ERC already active?
;;     ;;     (pop-to-buffer "Libera.Chat")
;;     ;;   (progn
;;     ;;     (tab-bar-new-tab)
;;     (erc-tls :server "irc.libera.chat" :port 6667 :nick "brongulus" :password nil))
;;   ;; (erc-track-switch-buffer 1))
;;   ;; (erc-status-sidebar-open))))
;;   (erc-services-mode 1)
;;   (erc-autojoin-mode)
;;   (erc-track-mode t)
;;   (erc-timestamp-mode t)
;;   (set-face-attribute 'erc-timestamp-face nil :foreground
;;                       (face-foreground 'font-lock-comment-face))
;;   (dolist (mod '(keep-place sasl log nickbar nicks services xdcc))
;;     (push mod erc-modules))
;;   (erc-update-modules))

;; newsticker
(setq newsticker-retrieval-interval 0
      newsticker-url-list
      '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
        ("Gluer" "https://gluer.org/atom")
        ("DDV" "https://drewdevault.com/blog/index.xml")
        ("Nawaz" "https://blog.nawaz.org/feeds/all.atom.xml")
        ("Arch" "https://archlinux.org/feeds/news/")
        ("Andrewk" "https://vimeo.com/andrewrk/videos/rss")
        ("ikechan"
         "https://www.youtube.com/feeds/videos.xml?channel_id=UCpGJxlhKXfdOKkBhuDH6ujA")
        ("kotatsugame"
         "https://www.youtube.com/feeds/videos.xml?channel_id=UCL8EOznhSyreT9O0-KFxgZQ")
        ("kaname"
         "https://www.youtube.com/feeds/videos.xml?channel_id=UC2_krAagEXVPftDXZCDiVZA")
        ("joshua"
         "https://www.youtube.com/feeds/videos.xml?channel_id=UCqnP1HkcAnueBjyKCdaoNHg")
        ("HLTV" "https://www.hltv.org/rss/news")
        ("PSA" "https://psa.wf/feed/")))

(defun my/close-newsticker ()
  "Kill all tree-view related buffers."
  (tab-bar-close-tab)
  (dolist (buf '("*Newsticker List*" "*Newsticker Item*" "*Newsticker Tree*"))
    (kill-buffer buf)))

(with-eval-after-load 'newst-reader
  (advice-add 'newsticker-show-news :around
              (lambda (orig-fun &rest args) (tab-bar-new-tab) (apply orig-fun args))))

(with-eval-after-load 'newst-treeview
  (dolist (map (list newsticker-treeview-mode-map newsticker-treeview-list-mode-map))
    (define-key map (kbd "SPC") ctl-x-map)
    (define-key map (kbd ",") #'newsticker-treeview-next-page))
  (advice-add 'newsticker-treeview-quit :after 'my/close-newsticker))

;; mini-ontop
(defvar mini-ontop--stack nil) ;; inspired by hkjels/mini-ontop
(defun mini-ontop--should-activate-p ()
  "Return non-nil if mini-ontop should activate for the current command."
  (or (eq this-command 'execute-extended-command)
      (eq this-command 'execute-extended-command-for-buffer)
      (string-prefix-p "describe-" (symbol-name this-command))
      (string-prefix-p "help-" (symbol-name this-command))))
(defun mini-ontop--save ()
  (when (mini-ontop--should-activate-p)
    (let (saved)
      (dolist (w (window-list))
        (with-selected-window w
          (when (and (not (minibufferp)) (<= (minibuffer-depth) 1)
                     (< (- (line-number-at-pos (window-end w t))
                           (line-number-at-pos (point)))
                        15))
            (push (list w (window-buffer w) (point)) saved)
            (forward-line -15))))
      (push saved mini-ontop--stack))))
(defun mini-ontop--restore ()
  (dolist (e (pop mini-ontop--stack))
    (when (and (window-live-p (car e)) (buffer-live-p (cadr e)))
      (set-window-point (car e) (caddr e)))))

(add-hook 'minibuffer-setup-hook #'mini-ontop--save)
(add-hook 'minibuffer-exit-hook #'mini-ontop--restore)

;; eww
(setq browse-url-browser-function 'eww-browse-url
      browse-url-new-window-flag t
      eww-default-download-directory "~/Downloads/eww/")
(setq browse-url-handlers nil)
(dolist (url '("github\\.com" "github\\.tools" "youtube\\.com" "youtu\\.be"))
  (push (cons url 'browse-url-default-browser) browse-url-handlers))
(add-hook 'html-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-v") 
                           (lambda () (interactive)
                             (eww (concat "file://" buffer-file-name))))))
(setq-default shr-max-width 110 shr-width 110)
(with-eval-after-load 'shr
  (setq shr-max-width 110 shr-width 110)
  (defun my-url-expand-file-name-fixed (orig-fun file &optional base)
    "Preserve spaces when expanding file URLs."
    (funcall orig-fun (url-encode-url file) base))
  (advice-add 'url-expand-file-name :around #'my-url-expand-file-name-fixed))
(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "SPC") ctl-x-map)
  (define-key eww-mode-map (kbd "#") #'definition-at-point)
  (setq eww-header-line-format nil)
  (setq eww-auto-rename-buffer 'title))

;; --- External -------------------------------------------------------------
(run-with-idle-timer
 0.2 nil (lambda nil
           (load "~/.emacs.d/lisp/dev-conf" nil :no-message)
           (if (locate-library "corfu")
               (global-corfu-mode)
             (add-hook 'prog-mode-hook #'completion-preview-mode))))

;; --- 31 stuff -------------------------------------------------------------
(when (string> emacs-version "31")
  (setq treesit-auto-install-grammar 'always
        treesit-font-lock-level 4)
  ;; hs-show-indicators t)
  (setq kill-region-dwim 'emacs-word)
  (with-eval-after-load 'dired (setq dired-hide-details-hide-absolute-location t))
  (with-eval-after-load 'eglot (setq eglot-code-action-indicator "+"))
  ;; (with-eval-after-load 'icomplete (setq icomplete-vertical-in-buffer-adjust-list t))
  (setq flymake-show-diagnostics-at-end-of-line 'short));fancy))

;; --- Speed benchmarking ---------------------------------------------------
;; (let ((init-time (float-time (time-subtract (current-time) init-start-time)))
;;       (total-time (string-to-number (emacs-init-time "%f"))))
;;   (message (concat
;;             (propertize "Startup time: " 'face 'bold)
;;             (format "%.2fs " init-time)
;;             (propertize (format "(+ %.2fs system time)"
;;                                 (- total-time init-time))))))
