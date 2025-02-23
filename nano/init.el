;; init.el --- NANO Emacs (minimal version)  -*- lexical-binding: t -*-

;; Copyright (c) 2025  Nicolas P. Rougier
;; Released under the GNU General Public License 3.0
;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/nano-emacs

;; This is NANO Emacs in 256 lines, without any dependency
;; Usage (command line):  emacs -Q -l nano.el -[light|dark]

;; --- Speed benchmarking ---------------------------------------------------
(load "~/.emacs.d/lisp/benchmarking" :noerr :no-message)

;; Place at the very start of init.el
;; (require 'profiler)
;; (profiler-start 'cpu)

(setq init-start-time (current-time))
(setq inhibit-startup-screen t)

;; --- Typography stack -----------------------------------------------------
;; (set-face-attribute 'default nil :height 160 :weight 'light :family "Input Mono Narrow")
;; (set-face-attribute 'bold nil :weight 'regular)
;; (set-face-attribute 'bold-italic nil :weight 'regular)
(set-face-attribute 'default nil :height 140 :weight 'regular :family "VictorMono Nerd Font Mono")
(set-face-attribute 'bold nil :weight 'semi-bold)
(set-face-attribute 'bold-italic nil :weight 'semi-bold)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?→))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;; --- Frame / windows layout & behavior ------------------------------------
(setq default-frame-alist
      '((left-fringe . 0) (right-fringe . 0) (internal-border-width . 20)
        (bottom-divider-width . 0) (right-divider-width . 0) (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)
(setq-default pop-up-windows nil)
(setq-default mode-line-format "")

;; --- Activate / Deactivate modes ------------------------------------------
(blink-cursor-mode -1) (global-hl-line-mode 1)
(icomplete-vertical-mode 1) (pixel-scroll-precision-mode 1)

;; --- Minimal NANO (not a real) theme --------------------------------------
(defvar nano-current-theme 'dark "Current nano variant being used.")
(defface nano-default '((t)) ".")   (defface nano-default-i '((t)) ".")
(defface nano-highlight '((t)) ".") (defface nano-highlight-i '((t)) ".")
(defface nano-subtle '((t)) ".")    (defface nano-subtle-i '((t)) ".")
(defface nano-faded '((t)) ".")     (defface nano-faded-i '((t)) ".")
(defface nano-salient '((t)) ".")   (defface nano-salient-i '((t)) ".")
(defface nano-popout '((t)) ".")    (defface nano-popout-i '((t)) ".")
(defface nano-strong '((t)) ".")    (defface nano-strong-i '((t)) ".")
(defface nano-critical '((t)) ".")  (defface nano-critical-i '((t)) ".")
(defface nano-string '((t)) ".")    (defface nano-string-i '((t)) ".")

(defun nano-set-face (name &optional foreground background weight)
  "Set NAME and NAME-i faces with given FOREGROUND, BACKGROUND and WEIGHT."
  (apply #'set-face-attribute `(,name nil
                                      ,@(when foreground `(:foreground ,foreground))
                                      ,@(when background `(:background ,background))
                                      ,@(when weight `(:weight ,weight))))
  (apply #'set-face-attribute `(,(intern (concat (symbol-name name) "-i")) nil
                                :foreground ,(face-background 'nano-default)
                                ,@(when foreground `(:background ,foreground))
                                :weight regular)))

(defun nano-link-face (sources faces &optional attributes)
  "Make FACES to inherit from SOURCES faces and unspecify ATTRIBUTES."
  (let ((attributes (or attributes
                        '(:foreground :background :family :weight
                                      :height :slant :overline :underline :box))))
    (dolist (face (seq-filter #'facep faces))
      (dolist (attribute attributes)
        (set-face-attribute face nil attribute 'unspecified))
      (set-face-attribute face nil :inherit sources))))

(defun nano-install-theme ()
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'nano-default)
                      :background (face-background 'nano-default))
  (dolist (item '((nano-default .  (variable-pitch variable-pitch-text
                                                   fixed-pitch fixed-pitch-serif))
                  (nano-highlight . (hl-line highlight))
                  (nano-subtle .    (match region
                                           lazy-highlight widget-field))
                  (nano-faded .     (shadow
                                     font-lock-comment-face
                                     font-lock-doc-face
                                     icomplete-section
                                     completions-annotations))
                  (nano-popout .    (warning help-key-binding))
                  (nano-string .   (font-lock-string-face))
                  (nano-salient .   (success link
                                             help-argument-name
                                             custom-visibility
                                             font-lock-type-face
                                             font-lock-keyword-face
                                             font-lock-builtin-face
                                             completions-common-part))
                  (nano-strong .    (font-lock-function-name-face
                                     font-lock-variable-name-face
                                     icomplete-first-match
                                     minibuffer-prompt))
                  (nano-critical .  (error
                                     completions-first-difference))
                  (nano-default-i . (custom-button-mouse
                                     isearch))
                  (nano-critical-i . (isearch-fail))
                  ((nano-subtle nano-strong) . (custom-button
                                                icomplete-selected-match))
                  ((nano-faded-i nano-strong) . (show-paren-match))))
    (nano-link-face (car item) (cdr item)))

  (set-face-attribute 'font-lock-string-face nil :slant 'italic :weight 'semi-bold)
  (set-face-attribute 'link nil :underline t)
  (set-face-attribute 'vertical-border nil :inherit 'nano-faded)

  (when (eq system-type 'darwin)
    (modify-all-frames-parameters `((ns-appearance . ,nano-current-theme))))
  
  (with-eval-after-load 'ansi-color
    (let* ((color-themes ;; ansi-colors
            '((black . ((dark . "#30343d") (light . "#EEEEEE")))
              (red . ((dark . "#c47779") (light . "#c56655")))
              (green . ((dark . "#a7bf87") (light . "#5f8700")))
              (yellow . ((dark . "#d9c18c") (light . "#bb9200")))
              (blue . ((dark . "#81a2be") (light . "#6079db")))
              (magenta . ((dark . "#b294bb") (light . "#7646c1")))
              (cyan . ((dark . "#7b2bd") (light . "#6594bd")))
              (white . ((dark . "#cccccc") (light . "#1a1a1a")))))
           (theme-variant (if (eq nano-current-theme 'light) 'light 'dark)))
      (dolist (color-def color-themes)
        (let* ((color-name (car color-def))
               (color-value (alist-get theme-variant (cdr color-def))))
          (set-face-attribute
           (intern (format "ansi-color-%s" color-name))
           nil
           :foreground color-value
           :background color-value)))))
  
  ;; Mode & header lines
  (set-face-attribute 'header-line nil
                      :background 'unspecified
                      :underline nil
                      :box `(:line-width 1 :color ,(face-background 'nano-default))
                      :inherit 'nano-subtle)
  (set-face-attribute 'mode-line nil
                      :background (face-background 'default)
                      :underline (face-foreground 'nano-faded)
                      :height 40 :overline nil :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'default)
                      :underline (face-foreground 'nano-faded)
                      :height 40 :overline nil :box nil))

(defun nano-light (&rest args)
  "NANO light theme (based on material colors)."
  (interactive)
  (nano-set-face 'nano-default "#37474F" "#F7F7F7") ;; Blue Grey / L800
  (nano-set-face 'nano-strong "#000000" nil 'regular) ;; Black
  (nano-set-face 'nano-highlight nil "#EEEEEE") ;; Very Light Grey
  (nano-set-face 'nano-subtle nil "#C9D0D9") ;; Blue Grey / L50
  (nano-set-face 'nano-faded "#90A4AE") ;; Blue Grey / L300
  (nano-set-face 'nano-salient "#673AB7") ;; Deep Purple / L500
  (nano-set-face 'nano-popout "#FFAB91") ;; Deep Orange / L200
  (nano-set-face 'nano-critical "#FF6F00") ;; Amber / L900
  (nano-set-face 'nano-string "grey50")
  (setq nano-current-theme 'light)
  (nano-install-theme))

(defun nano-dark (&rest args)
  "NANO dark theme (based on nord colors)."
  (interactive)
  (nano-set-face 'nano-default "#ECEFF4" "#282C33") ;; Snow Storm 3
  (nano-set-face 'nano-strong "#ECEFF4" nil 'regular) ;; Polar Night 0
  (nano-set-face 'nano-highlight nil "#21242b")  ;; Polar Night 1
  (nano-set-face 'nano-subtle nil "#434C5E") ;; Polar Night 2
  (nano-set-face 'nano-faded "#677691") ;; 
  (nano-set-face 'nano-salient "#81A1C1")  ;; Frost 2
  (nano-set-face 'nano-popout "#D08770") ;; Aurora 1
  (nano-set-face 'nano-critical "#EBCB8B") ;; Aurora 2
  (nano-set-face 'nano-string "grey70")
  (setq nano-current-theme 'dark)
  (nano-install-theme))

;; --- Command line theme chooser -------------------------------------------
(add-to-list 'command-switch-alist '("-dark"  . nano-dark))
(add-to-list 'command-switch-alist '("-light" . nano-light))
(if (member "-dark" command-line-args) (nano-dark) (nano-light))

;; --- Header & mode lines --------------------------------------------------
(setq nano-header-line
      '(:eval
        (let ((prefix (cond ((buffer-modified-p)  '("**"  . nano-critical-i))
                            (view-mode            '("[N]" . nano-string-i))
                            (buffer-read-only     '("RO"  . nano-default-i))
                            (t                    '("%p"  . nano-faded-i))))
              (box-face '(:line-width 4 :style flat-button))
              (coords (concat
                       (truncate-string-to-width
                        (format-mode-line
                         (when which-function-mode
                           which-func-current))
                        20 nil nil t)
                       (format-mode-line " %c:%l ")))
              (tabs (let* ((tabs (length (tab-bar-tabs)))
                           (active-tab (tab-bar--current-tab-index)))
                      (if (<= tabs 1)
                          ""
                        (let ((result '()))
                          (dotimes (i tabs)
                            (if (= i active-tab)
                                (push (format "[%d]" (1+ i)) result)
                              (push (format "%d" (1+ i)) result)))
                          (concat " " (mapconcat 'identity (reverse result) " ") " "))))))
          (list
           (propertize (concat " " (car prefix) " ")
                       'face `(,(cdr prefix) :box ,box-face))
           (propertize (format-mode-line " %b") 'face `(nano-strong :box ,box-face))
           (propertize (format-mode-line vc-mode)
                       'face `(:foreground "dark cyan" :box ,box-face))
           (propertize " " 'face `(:box ,box-face)
                       'display `(space :align-to (- right ,(+ (length coords) (length tabs)))))
           (propertize coords 'face `(nano-faded :box ,box-face))
           (propertize tabs 'face `(nano-faded-i :box ,box-face))))))
(setq-default header-line-format nano-header-line)

;; --- Minibuffer completion ------------------------------------------------
(setq tab-always-indent 'complete
      tab-first-completion 'word-or-paren
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 9
      icomplete-separator " . "
      icomplete-with-completion-tables t
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t
      resize-mini-windows 'grow-only)
(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "TAB") #'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "<backtab>") #'icomplete-backward-completions)
  (define-key icomplete-minibuffer-map (kbd "RET") #'icomplete-fido-ret)
  (define-key icomplete-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit)
  (define-key icomplete-minibuffer-map (kbd "DEL") #'icomplete-fido-backward-updir))

(defun file-capf ()
  "File completion at point function. src: eshelyaron."
  (pcase (bounds-of-thing-at-point 'filename)
    (`(,beg . ,end)
     (list beg end #'completion-file-name-table
           :annotation-function (lambda (_) " File")
           :exclusive 'no))))
(add-hook 'completion-at-point-functions #'file-capf)

;; --- Minimal key bindings -------------------------------------------------
(defun nano-quit ()
  "Quit minibuffer from anywhere (code from Protesilaos Stavrou)."
  (interactive)
  (cond ((region-active-p) (keyboard-quit))
        ((derived-mode-p 'completion-list-mode) (delete-completion-window))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        (t (keyboard-quit))))

(defun my-goto-doc nil (interactive)
       (if (derived-mode-p 'emacs-lisp-mode)
           (describe-symbol (symbol-at-point))
         (eldoc-doc-buffer t)))

(defun my-scroll-other-down nil (interactive)
       (with-selected-window (other-window-for-scrolling)
         (scroll-up-command 5)))
(defun my-scroll-other-up nil (interactive)
       (with-selected-window (other-window-for-scrolling)
         (scroll-down-command 5)))

(define-key (current-global-map) (kbd "C-x C-m") #'execute-extended-command)
(define-key (current-global-map) (kbd "C-x m") #'execute-extended-command)
(define-key (current-global-map) (kbd "C-x x b") #'ibuffer)
(define-key (current-global-map) (kbd "C-x x c") #'save-buffers-kill-emacs)
(define-key (current-global-map) (kbd "C-x x e") #'eval-last-sexp)
(define-key (current-global-map) (kbd "C-x x f") #'find-file)
(define-key (current-global-map) (kbd "C-x x s") #'save-buffer)
(define-key (current-global-map) (kbd "C-x x z") #'restart-emacs)
(define-key (current-global-map) (kbd "C-o") #'other-window)
(define-key (current-global-map) (kbd "C-x ;") #'comment-line)
(define-key (current-global-map) (kbd "C-h .") #'my-goto-doc)
(define-key (current-global-map) (kbd "C-h '") #'describe-face)
(define-key (current-global-map) (kbd "C-,") #'my-scroll-other-down)
(define-key (current-global-map) (kbd "C-.") #'my-scroll-other-up)
(define-key (current-global-map) (kbd "C-x v e") #'vc-ediff)
(define-key (current-global-map) (kbd "C-x v f")
            (lambda () (interactive) (vc-git--pushpull "push" nil '("--force-with-lease"))))
(define-key (current-global-map) (kbd "C-<tab>") #'tab-next)
(define-key (current-global-map) (kbd "C-S-<tab>") #'tab-previous)
(define-key (current-global-map) (kbd "C-x C-b") #'ibuffer)
(define-key (current-global-map) (kbd "M-s r") #'replace-regexp)
(define-key (current-global-map) (kbd "C-x k") #'kill-current-buffer)
(define-key (current-global-map) (kbd "C-x f") #'recentf-open)
(define-key (current-global-map) (kbd "C-g") #'nano-quit)
(define-key (current-global-map) (kbd "C-z")  #'restart-emacs)
(define-key (current-global-map) (kbd "C-<wheel-up>") nil) ;; No text resize via mouse scroll
(define-key (current-global-map) (kbd "C-<wheel-down>") nil) ;; No text resize via mouse scroll

;; --- Sane settings --------------------------------------------------------
(set-default-coding-systems 'utf-8)
(setq-default tab-width 4
              completion-styles
              '(basic partial-completion substring flex emacs22)
              completion-cycle-threshold t
              cursor-type 'bar
              line-spacing 3
              imenu-flatten t
              initial-scratch-message nil
              abbrev-mode t
              indent-tabs-mode nil
              mouse-wheel-tilt-scroll t
              mouse-wheel-flip-direction t
              ring-bell-function 'ignore
              select-enable-clipboard t
              use-short-answers t
              uniquify-buffer-name-style 'forward)

;;(add-hook 'after-init-hook #'repeat-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(save-place-mode 1) (global-subword-mode 1) (winner-mode 1)
(savehist-mode 1) (which-key-mode 1) (delete-selection-mode 1)
(global-auto-revert-mode 1) (which-function-mode 1)
(require 'server)
(unless (server-running-p) (server-start))

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backup/" t))
      backup-directory-alist `(("." . "~/.emacs.d/backup/"))
      lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t))
      ;; ^^ https://emacs.stackexchange.com/a/81518/28970
      comint-prompt-read-only t
      compilation-ask-about-save nil
      completion-ignore-case t
      completion-auto-help 'lazy;nil
      confirm-kill-emacs 'yes-or-no-p
      dired-dwim-target t
      dired-omit-verbose nil
      dired-use-ls-dired nil
      dired-kill-when-opening-new-dired-buffer t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      eldoc-echo-area-prefer-doc-buffer t
      eldoc-idle-delay 0.3
      eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-display-truncation-message nil
      flymake-no-changes-timeout 2
      flymake-show-diagnostics-at-end-of-line 'short
      help-window-select t
      recentf-max-menu-items 25
      recentf-max-saved-items 200
      recentf-auto-cleanup 'never
      save-abbrevs nil
      save-interprogram-paste-before-kill t
      shell-command-prompt-show-cwd t
      shell-kill-buffer-on-exit t
      shell-file-name (car (process-lines "which" "fish"))
      tab-bar-show nil
      vc-display-status 'no-backend
      vc-follow-symlinks t
      which-func-unknown ""
      xref-search-program (if (executable-find "rg") 'ripgrep 'grep)
      xref-auto-jump-to-first-xref nil ; 'move
      xref-show-definitions-function 'xref-show-definitions-buffer-at-bottom
      xref-show-xrefs-function 'xref-show-definitions-buffer-at-bottom)

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
  (define-key isearch-mode-map (kbd "TAB") #'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<backtab>") #'isearch-repeat-backward))

(with-eval-after-load 'completion-preview
  (define-key completion-preview-active-mode-map (kbd "M-n") #'completion-preview-next-candidate)
  (define-key completion-preview-active-mode-map (kbd "M-p") #'completion-preview-prev-candidate))

(with-eval-after-load 'dired
  (set-face-attribute 'dired-directory nil :slant 'italic)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "\\") #'dired-up-directory)
  (define-key dired-mode-map (kbd "q") #'kill-current-buffer)
  (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-o") #'other-window))

(with-eval-after-load 'eww
  (setq eww-header-line-format nil)
  (setq eww-auto-rename-buffer 'title))

(add-hook 'ediff-before-setup-hook #'tab-bar-new-tab)
(add-hook 'ediff-quit-hook
          (lambda nil
            (tab-bar-close-tab)
            (kill-buffer ediff-registry-buffer)))
(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-diff-options "-w"))

;; install-info --dir-file=./dir --info-file=
(push "~/.emacs.d/info" Info-default-directory-list)

(dolist (pops '(("^\\*term.*\\*$" . -1) ("\\*eshell-pop\\*" . -1)
                ("^\\*compilation.*\\*$" . -1)
                ("vc-git :.\*" . 0) ("\\*vc.\*-log\\*" . 0)
                ("\\*eldoc\\*" . 0) ("\\*Help\\*" . 0)
                ("\\*Warnings\\*" . 1)
                ("\\*log-edit-files\\*" . 1)
                ("\\*Occur.*\\*$" . 1)
                ("\\*grep.*\\*$" . 1)))
  (add-to-list 'display-buffer-alist
               `(,(car pops)
                 display-buffer-in-side-window
                 (body-function . select-window)
                 (side . bottom)
                 (slot . ,(cdr pops))
                 (window-height . 0.33))))

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
          (setq-local header-line-format nano-header-line))
      (progn
        (display-buffer buffer)
        (setq-local header-line-format nil)))))

(define-key (current-global-map) (kbd "<f10>") #'toggle-side-normal-window)

(dolist (modes '(help-mode-hook vc-git-log-edit-mode-hook
                                compilation-mode-hook term-mode-hook eshell-mode-hook
                                occur-hook grep-mode-hook special-mode-hook))
  (add-hook modes (lambda () (setq-local header-line-format nil))))

(with-eval-after-load 'help-mode
  (define-key help-mode-map "q" #'kill-buffer-and-window))

(setopt switch-to-buffer-obey-display-actions t)
(define-key (current-global-map) (kbd "M-j") #'window-toggle-side-windows)

(define-key occur-mode-map (kbd "q") #'kill-buffer-and-window)
(define-key occur-mode-map (kbd "C-o") #'other-window)
(define-key occur-mode-map (kbd "TAB") #'occur-mode-display-occurrence)

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

;; --- Shell/term/compile ---------------------------------------------------
(define-advice term-handle-exit (:after (&rest _args) term-kill-on-exit)
  (kill-buffer))

(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))

(add-hook 'compilation-mode-hook
          (lambda nil
            "Enable comint mode to allow for providing program input."
            (comint-mode)
            (setq-local buffer-read-only nil)))

(add-hook 'compilation-finish-functions
          (lambda (buffer status)
            "Reset comint mode so that we get the compilation-mode goodness."
            (setq-local buffer-read-only t)
            (compilation-minor-mode)))

(add-hook 'term-mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil)
            (term-set-escape-char ?\C-x)
            (define-key term-raw-map "\C-o" 'other-window)
            (define-key term-raw-map "\M-y" 'yank-pop)
            (define-key term-raw-map "\C-y" 'yank)
            (define-key term-raw-map "\M-w" 'kill-ring-save)
            (define-key term-raw-map "\M-j" 'window-toggle-side-windows)))

;; Eshell refs:
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(define-key (current-global-map) (kbd "C-\\")
            (lambda nil (interactive)
              (defvar eshell-buffer-name)
              (let ((eshell-buffer-name "*eshell-pop*"))
                (eshell))))
(setq eshell-aliases-file "~/.config/alias"
      eshell-scroll-to-bottom-on-input 'all
      eshell-hist-ignoredups t
      eshell-history-size 20000
      eshell-save-history-on-exit t
      eshell-glob-case-insensitive t)

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
                                      (concat (match-string 2) " $1"))
                                eshell-command-aliases-list)))
                (forward-line 1))
              eshell-command-aliases-list)))))
(advice-add 'eshell-read-aliases-list :override #'my-eshell-read-aliases-list)

(defun eshell-insert-history () ; src: howard abrams
  "Displays the eshell history to select and insert back into your eshell."
  (interactive)
  (insert (completing-read "Eshell history: "
                           (delete-dups
                            (ring-elements eshell-history-ring)))))

(defun my-eshell-narrow-to-prompt ()
  "Narrow buffer to prompt at point. src: ambrevar."
  (interactive)
  (narrow-to-region
   (save-excursion
     (forward-line)
     (call-interactively #'eshell-previous-prompt)
     (beginning-of-line)
     (point))
   (save-excursion
     (forward-line)
     (call-interactively #'eshell-next-prompt)
     (re-search-backward eshell-prompt-regexp nil t)
     (when (and (require 'eshell-prompt-extras nil 'noerror)
                (eq eshell-prompt-function #'epe-theme-multiline-with-status))
       (previous-line))
     (point))))

(with-eval-after-load 'em-term
  (dolist (cmd '("fzf" "yazi" "mpv" "emacsclient" "bat"))
    (add-to-list 'eshell-visual-commands cmd))
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate" "--patch"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook #'completion-preview-mode)
  (push 'file-capf completion-at-point-functions))
;;   (add-to-list 'eshell-modules-list 'eshell-rebind)
;;   (add-to-list 'eshell-modules-list 'eshell-smart))
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (goto-address-mode)
              (setq-local global-hl-line-mode nil)
              (setenv "TERM" "xterm-256color")
              (keymap-unset eshell-hist-mode-map "<up>" t)
              (keymap-unset eshell-hist-mode-map "<down>" t)
              (define-key eshell-mode-map (kbd "C-x n d") #'my-eshell-narrow-to-prompt)
              (define-key eshell-hist-mode-map (kbd "C-r") #'eshell-insert-history)))

(unless (display-graphic-p)
  (define-key (current-global-map) (kbd "M-w")
              (lambda () (interactive)
                (when (use-region-p)
                  (let* ((clipboard-commands
                          '(("darwin" . "pbcopy")
                            ("gnu/linux" . "xclip -selection clipboard")
                            ("windows-nt" . "clip")))
                         (copy-cmd (or (cdr (assoc (symbol-name system-type)
                                                   clipboard-commands))
                                       nil)))
                    (when copy-cmd
                      (call-process-region
                       (region-beginning) (region-end) copy-cmd)
                      (deactivate-mark))))))
  (xterm-mouse-mode))

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
(add-hook 'compilation-filter-hook #'ansi-osc-compilation-filter)

;; --- Programming ----------------------------------------------------------
(define-key prog-mode-map (kbd "C-c C-c") #'compile)
(define-key prog-mode-map (kbd "C-c C-r") #'recompile)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|HACK\\|TODO\\|WIP\\|BUG\\|DONE\\)"
                1 font-lock-warning-face t)
               (";" . 'font-lock-comment-face)))))

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

  (add-hook 'prog-mode-hook #'my/setup-install-grammars)
  (setq go-ts-mode-indent-offset 4))

(nconc auto-mode-alist
       '(("\\.zig\\'" . c-mode) ;; Until zig-ts-mode is core
         ("\\.zig\\.zon\\'" . js-json-mode)
         ("\\.nix\\'" . js-json-mode)
         ("\\.fish\\'" . conf-mode)
         ("\\.rs\\'" . rust-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\go\\.mod\\'"  . go-mod-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.lua\\'" . lua-ts-mode)
         ("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerignore\\'" . dockerfile-ts-mode)
         ("\\.bin\\'" . hexl-mode)
         ("\\.info\\'" . Info-mode)))

(dolist (mode '(rust-ts-mode-hook go-ts-mode-hook python-mode-hook))
  (add-hook mode #'eglot-ensure))
(add-hook 'rust-ts-mode-hook
          (lambda nil (add-to-list 'process-environment "CARGO_TERM_COLOR=always" :append)))

(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook 'c-ts-mode-hook
          (lambda nil
            (setq-local c-ts-mode-indent-style 'gnu)
            (setq-local c-ts-mode-indent-offset 4)
            (c-ts-mode-toggle-comment-style -1)))

(with-eval-after-load 'project
  (setq project-vc-extra-root-markers '("go.mod" "Cargo.toml"))
  (setq project-vc-ignores '("**/vendor/**")))

(with-eval-after-load 'eglot
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-config 0
        eglot-autoshutdown t
        eglot-inlay-hints-mode nil)

  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server")))
  
  (defun my-eglot-organize-imports ()
    (interactive)
    (ignore-errors
      (eglot-code-actions nil nil "source.organizeImports" t)))
  
  (defun my-eglot-setup ()
    (interactive)
    (when (not (eq major-mode 'sql-mode))
      (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
      (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

  (add-hook 'eglot-managed-mode-hook #'my-eglot-setup))

(load "~/.emacs.d/lisp/snippets" :noerr :no-message)

;; --- Misc functions -------------------------------------------------------
(setq-default fill-column 100)
(defun toggle-centered-buffer ()
  "Toggle center alignment of the buffer. Source: jamesdyer."
  (interactive)
  (let* ((current-margins (window-margins))
         (margin (if (or (equal current-margins '(0 . 0))
                         (null (car (window-margins))))
                     (/ (- (window-total-width) fill-column) 2)
                   0)))
    (visual-line-mode 1)
    (set-window-margins nil margin margin)))
(define-key (current-global-map) (kbd "<f9>") #'toggle-centered-buffer)

(defun match-pair nil
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 t t)
    (cond ((looking-at "\\s\(\\|\{") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)\\|\}") (forward-char 1) (backward-list 1))
          (t (backward-up-list 1 t t)))))

(defun my/select-fwd-line (arg)
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

;; --- Mini Meow ------------------------------------------------------------
(define-global-minor-mode global-view-mode view-mode
  (lambda () ; src: xenodium
    (when (and (not (minibufferp)) (not noninteractive) ; 'special-mode
               (derived-mode-p 'messages-buffer-mode 'prog-mode 'conf-mode 'outline-mode))
      (view-mode 1))))
(global-view-mode 1)
(defun meow--set-cursor-type (type)
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((shape (or (car-safe type) type))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal (concat "\e[" param " q")))))
(defun view-mode-edit-command (fn)
  (interactive)
  (View-exit) (ignore-errors (call-interactively fn)) (view-mode 1))
(add-hook 'view-mode-hook
          (lambda nil (meow--set-cursor-type (if view-mode 'box 'bar))))
(define-key (current-global-map) (kbd "j") (lambda nil (interactive) (my-chord ?j ?k 'view-mode)))
(with-eval-after-load 'view
  ;; normal binds
  (dolist (pair '(("\\" . dired-jump) ("A" . move-beginning-of-line) ("E" . move-end-of-line)
                  ("a" . back-to-indentation) ("e" . forward-word) ("b" . backward-word)
                  ("v" . set-mark-command) ("h" . backward-char) ("j" . next-line)
                  ("k" . previous-line) ("l" . forward-char) ("i" . View-exit)
                  ("y" . kill-ring-save) ("%" . match-pair) ("o" . other-window)
                  ("D" . View-scroll-half-page-forward) ("U" . View-scroll-half-page-backward)
                  ("[" . tab-bar-switch-to-prev-tab) ("]" . tab-bar-switch-to-next-tab)
                  ("x" . my/select-fwd-line) ("X" . exchange-point-and-mark) ("O" . occur)
                  ("w" . mark-word) ("," . my-scroll-other-down) ("M-h" . mark-paragraph)
                  ("M-j" . window-toggle-side-windows) ("C-M-h" . mark-defun)
                  ("." . my-scroll-other-up) (";" . keyboard-quit) ("F" . ffap)
                  ("*" . isearch-forward-symbol-at-point) ("`" . window-toggle-side-windows)
                  ("Z" . pop-to-mark-command) ("I" . eglot-find-implementation)
                  ("K" . my-goto-doc) ("G" . xref-find-definitions) ("B" . xref-go-back)
                  ("!" . flymake-show-buffer-diagnostics) ("=" . mark-sexp)
                  ("?" . xref-find-references)))
    (define-key view-mode-map (kbd (car pair)) (cdr pair)))
  ;; buffer modifying binds
  (dolist (pair '(("d" . kill-region) ("p" . yank) ("P" . yank-pop) ("r" . eglot-rename)
                  ("DEL" . backward-delete-char-untabify) ("&" . align-regexp)
                  ("z" . undo-redo) ("u" . undo-only) ("R" . replace-regexp)
                  ("C-x ;" . comment-line) ("C-x C-;" . comment-line)
                  ("J" . delete-indentation) ("C-k" . kill-line) ("C-/" . undo-only)
                  ("f" . hs-toggle-hiding) ("c" . hs-hide-all) ("g" . hs-show-all)
                  ("C" . string-rectangle) ("TAB" . indent-for-tab-command)))
    (define-key view-mode-map (kbd (car pair))
                #'(lambda nil (interactive)
                    (view-mode-edit-command (cdr pair)))))
  (define-key view-mode-map (kbd "H") help-map)
  (define-key view-mode-map (kbd "SPC") ctl-x-map))

;; --- OSX Specific ---------------------------------------------------------
(when (eq system-type 'darwin)
  (select-frame-set-input-focus (selected-frame))
  (setq mac-option-modifier 'meta
        ns-function-modifier 'super
        mac-right-option-modifier 'alt
        mac-command-modifier 'hyper))

;; --- Minibuffer setup -----------------------------------------------------
(defun nano-minibuffer--setup ()
  ;;(set-window-margins nil 3 0)
  (setq truncate-lines t)
  (setq-local line-spacing nil))
(add-hook 'minibuffer-setup-hook #'nano-minibuffer--setup)

;; --- Speed benchmarking ---------------------------------------------------
(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
            (propertize "Startup time: " 'face 'bold)
            (format "%.2fs " init-time)
            (propertize (format "(+ %.2fs system time)"
                                (- total-time init-time))
                        'face 'shadow))))

;; (profiler-report)
;; (profiler-stop)
