;; init.el --- NANO Emacs (minimal version)  -*- lexical-binding: t -*-
;; Originally themed by: Nicolas P. Rougier <nicolas.rougier@inria.fr>

;; --- Speed benchmarking ---------------------------------------------------
;; (load "~/.emacs.d/lisp/benchmarking.el" :noerr :no-message)
;; (setq init-start-time (current-time))
(setq inhibit-startup-screen t
      custom-file (make-temp-file "emacs-custom"))

;; --- Typography stack -----------------------------------------------------
(set-face-attribute 'default nil :height (if is-android 160 140) :family "VictorMono Nerd Font Mono")
(set-face-attribute 'bold nil :weight 'bold)
(set-face-attribute 'bold-italic nil :weight 'bold)
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?→))
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;; --- Frame / windows layout & behavior ------------------------------------
(setq default-frame-alist
      '((left-fringe . 8) (right-fringe . 8) (internal-border-width . 20)
        (bottom-divider-width . 0) (right-divider-width . 0) (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)
(setq-default pop-up-windows nil)

;; --- Activate / Deactivate modes ------------------------------------------
(blink-cursor-mode -1) (kill-ring-deindent-mode 1)
(fido-vertical-mode 1) (global-subword-mode 1)
(defun my-lazy-load-modes () (pixel-scroll-precision-mode 1) (winner-mode 1)
       (delete-selection-mode 1) (global-auto-revert-mode 1) (minibuffer-depth-indicate-mode)
       (which-key-mode 1) (savehist-mode 1) (which-function-mode 1)
       (save-place-mode 1) (global-goto-address-mode) (tooltip-mode -1)
       (unless (display-graphic-p) (xterm-mouse-mode)))
(run-with-idle-timer 0.5 nil #'my-lazy-load-modes)

;; --- Minimal theme --------------------------------------
(defvar nano-current-theme 'burn "Current nano variant being used.")
(defvar nano-monochrome t "Should the font-locking have colours.")
(setq kitty-send-command "kitty @ --to=\"unix:/tmp/$(ls /tmp | grep mykitty)\" ")
(setq nano-bg-theme-map
      '(("#f7f7f7" . light) ("#fbf8ef" . amber) ("#282c33" . dark) ("#212121" . burn)))
(unless (eq system-type 'android)
  (let ((color (shell-command-to-string
                (concat kitty-send-command "get-colors | grep ^background | awk '{printf $2}'"))))
    (setq nano-current-theme (cdr (assoc color nano-bg-theme-map)))))
(defface nano-default '((t)) ".")   (defface nano-default-i '((t)) ".")
(defface nano-highlight '((t)) ".") (defface nano-highlight-i '((t)) ".")
(defface nano-subtle '((t)) ".")    (defface nano-subtle-i '((t)) ".")
(defface nano-faded '((t)) ".")     (defface nano-faded-i '((t)) ".")
(defface nano-salient '((t)) ".")   (defface nano-salient-i '((t)) ".")
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
  (set-face-attribute 'default nil :foreground (face-foreground 'nano-default)
                      :background (face-background 'nano-default))
  (dolist (item '((nano-default      . (minibuffer-prompt fixed-pitch-serif fixed-pitch
                                                          variable-pitch variable-pitch-text))
                  (nano-highlight    . (hl-line highlight custom-button-mouse lazy-highlight))
                  (nano-subtle       . (match region isearch widget-field
                                              custom-button icomplete-selected-match))
                  (nano-faded        . (shadow vertical-border font-lock-comment-face
                                               font-lock-doc-face icomplete-section
                                               completions-annotations))
                  (nano-string       . (font-lock-string-face font-lock-constant-face))
                  (nano-salient      . (link help-argument-name custom-visibility
                                             font-lock-type-face font-lock-keyword-face
                                             font-lock-builtin-face font-lock-variable-name-face
                                             font-lock-function-name-face))
                  (nano-critical     . (error xref-file-header warning help-key-binding))
                  (nano-critical-i   . (isearch-fail completions-common-part))
                  (nano-faded-i      . (show-paren-match))))
    (nano-link-face (car item) (cdr item)))

  (set-face-attribute 'fringe nil :background (face-background 'default))
  (set-face-attribute 'font-lock-string-face nil :slant 'italic :weight 'semi-bold)
  (set-face-attribute 'font-lock-doc-face nil :slant 'italic)
  (set-face-attribute 'font-lock-builtin-face nil :slant 'italic)
  (set-face-attribute 'link nil :underline t)
  (set-face-attribute 'region nil :extend nil)
  (set-face-attribute 'cursor nil :background "#00c2ff")
  (set-face-attribute 'line-number-current-line nil :foreground (face-foreground 'default)
                      :background (face-background 'nano-highlight) :weight 'bold)
  (with-eval-after-load 'make-mode
    (set-face-attribute 'makefile-targets nil :inherit 'font-lock-keyword-face))

  (when (eq system-type 'darwin)
    (modify-all-frames-parameters `((ns-appearance . ,nano-current-theme))))

  (let* ((color-themes ;; ansi-colors
          '((black   . ((dark . "#30343d") (light . "#EEEEEE")))
            (red     . ((dark . "#c47779") (light . "#c56655")))
            (green   . ((dark . "#a7bf87") (light . "#5f8700")))
            (yellow  . ((dark . "#d9c18c") (light . "#bb9200")))
            (blue    . ((dark . "#80ace3") (light . "#0184bc")))
            (magenta . ((dark . "#ab7bca") (light . "#7646c1")))
            (cyan    . ((dark . "#7db2bd") (light . "#6594bd")))
            (white   . ((dark . "#cccccc") (light . "#1a1a1a")))))
         (theme-variant (if (eq nano-current-theme 'light) 'light 'dark)))
    (dolist (color-def color-themes)
      (let* ((color-name (car color-def))
             (color-value (alist-get theme-variant (cdr color-def))))
        (with-eval-after-load 'ansi-color
          (set-face-attribute (intern (format "ansi-color-%s" color-name)) nil
                              :foreground color-value :background color-value)
          (set-face-attribute (intern (format "ansi-color-bright-%s" color-name)) nil
                              :foreground color-value :background color-value))))
    (set-face-attribute 'success nil :foreground
                        (alist-get theme-variant (alist-get 'green color-themes)))
    (with-eval-after-load 'diff-hl
      (set-face-attribute 'diff-hl-insert nil :background (face-background 'default)
                          :foreground (alist-get theme-variant (alist-get 'green color-themes)))
      (set-face-attribute 'diff-hl-change nil :background (face-background 'default)
                          :foreground (alist-get theme-variant (alist-get 'yellow color-themes)))
      (set-face-attribute 'diff-hl-delete nil :background (face-background 'default)
                          :foreground (alist-get theme-variant (alist-get 'red color-themes))))
    (unless nano-monochrome
      (let ((face-color-map
             '((font-lock-builtin-face . blue) (font-lock-function-name-face . blue)
               (font-lock-variable-name-face . blue)
               (font-lock-constant-face . yellow) (font-lock-type-face . cyan)
               (font-lock-keyword-face . magenta) (font-lock-property-name-face . magenta)
               (font-lock-preprocessor-face . orange) (font-lock-string-face . green))))
        (dolist (fc face-color-map)
          (set-face-attribute (car fc) nil :foreground
                              (alist-get theme-variant (alist-get (cdr fc) color-themes)))))))

  (with-eval-after-load 'eglot (set-face-attribute 'eglot-mode-line nil :inherit 'nano-faded))
  (with-eval-after-load 'whitespace
    (setq whitespace-style '(face tabs spaces tab-mark trailing)); indentation::tab space-after-tab::tab))
    (setq whitespace-indentation-regexp
          `(,(format "^\t*\\(\\( \\{%d\\}\\)+\\)" tab-width) . "^ *\\(\t+\\)."))
    (setq tabify-regexp "^\t* [ \t]+"
          whitespace-display-mappings
          '((space-mark     ?\       [?·]       [?.])
            (newline-mark   ?\n      [?↵ ?\n] [?$ ?\n])
            (tab-mark       ?\t      [?│ ?\t] [?\\ ?\t])))
    (dolist (face '(whitespace-tab whitespace-space))
      (set-face-attribute face nil :background 'unspecified :foreground (face-foreground 'nano-faded)))
    (set-face-attribute 'whitespace-trailing nil :background 'unspecified :foreground (face-foreground 'nano-critical))
    (set-face-attribute 'whitespace-line nil :background 'unspecified :foreground 'unspecified))

  (with-eval-after-load 'outline
    (dolist (face '(outline-1 outline-2 outline-3 outline-4 outline-5
                              outline-6 outline-7 outline-8))
      (set-face-attribute face nil :height 1.2 :inherit 'bold)))
  (with-eval-after-load 'org
    (dolist (face '(org-block org-block-begin-line org-block-end-line))
      (set-face-attribute face nil :background (face-background 'nano-highlight) :extend t :inherit 'default))
    (set-face-attribute 'org-drawer nil :foreground (face-foreground 'nano-faded))
    (set-face-attribute 'org-footnote nil :foreground (face-foreground 'nano-faded) :underline t)
    (set-face-attribute 'org-date nil :foreground (face-foreground 'link))
    (set-face-attribute 'org-table nil :foreground (face-foreground 'nano-default))
    (set-face-attribute 'org-verbatim nil :inherit 'org-latex-and-related)
    (set-face-attribute 'org-code nil :inherit 'org-latex-and-related))
  (with-eval-after-load 'sh-script
    (set-face-attribute 'sh-quoted-exec nil :foreground (face-foreground 'nano-salient) :italic t))

  ;; Mode & header lines
  (set-face-attribute 'header-line nil
                      :background 'unspecified
                      :underline nil
                      :overline (face-foreground 'nano-faded))
  (set-face-attribute 'mode-line nil
                      :foreground (face-foreground 'default)
                      :background 'unspecified
                      :box '(:line-width 1 :style flat-button)
                      :overline (face-foreground 'nano-faded))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (face-foreground 'nano-faded)
                      :background 'unspecified
                      :box '(:line-width 1 :style flat-button)
                      :inverse-video (not (display-graphic-p))
                      :overline (face-foreground 'nano-faded))
  (unless (display-graphic-p)
    (set-face-attribute 'mode-line-active nil
                        :foreground (face-background 'default)
                        :background (face-foreground 'nano-salient))))

(defun nano-light (&rest args)
  "NANO light theme (was based on material colors)."
  (interactive)
  (nano-set-face 'nano-default "#37474F" "#F7F7F7")
  (nano-set-face 'nano-highlight nil "#d0d0d0")
  (nano-set-face 'nano-subtle "#F7F7F7" "#393939")
  (nano-set-face 'nano-faded "#949494")
  (nano-set-face 'nano-salient "#37474F" nil 'bold)
  (nano-set-face 'nano-critical "#eb9250" nil 'bold)
  (nano-set-face 'nano-string "#767676")
  (setq nano-current-theme 'light)
  (nano-install-theme))

(defun nano-dark (&rest args)
  "NANO dark theme (was based on nord colors)."
  (interactive)
  (nano-set-face 'nano-default "#ECEFF4" "#282C33")
  (nano-set-face 'nano-highlight nil "#21242b")
  (nano-set-face 'nano-subtle nil "#434C5E")
  (nano-set-face 'nano-faded "#6A717C")
  (nano-set-face 'nano-salient "#FFFFFF" nil 'bold)
  (nano-set-face 'nano-critical "#f3a171" nil 'bold)
  (nano-set-face 'nano-string "#aaaaaa")
  (setq nano-current-theme 'dark)
  (nano-install-theme))

(defun nano-amber (&rest args)
  "Change background of light theme to plan9"
  (interactive)
  (nano-light)
  (set-face-attribute 'nano-default nil :foreground "#352f19" :background "#fbf8ef")
  (set-face-attribute 'nano-highlight nil :background "#E9E4E2")
  (let ((nano-current-theme 'light)) (nano-install-theme))
  (setq nano-current-theme 'amber))

(defun nano-burn (&rest args)
  "Darken background of dark theme"
  (interactive)
  (nano-dark)
  (set-face-attribute 'nano-default nil :foreground "#e3dac4" :background "#212121")
  (set-face-attribute 'nano-faded nil :foreground "#666666")
  (set-face-attribute 'nano-subtle nil :foreground "#212121" :background "#e3dac4")
  (set-face-attribute 'nano-string nil :foreground "#e9e2d1")
  (set-face-attribute 'nano-salient nil :foreground "#eee6d9")
  (set-face-attribute 'nano-highlight nil :background "#393939")
  (let ((nano-current-theme 'dark)) (nano-install-theme))
  (setq nano-current-theme 'burn))

(defun nano-toggle-theme nil
  (interactive)
  (cond ((eq nano-current-theme 'burn) (nano-light))
        ((eq nano-current-theme 'light) (nano-amber))
        ((eq nano-current-theme 'amber) (nano-dark))
        ((eq nano-current-theme 'dark) (nano-burn)))
  (if (or (eq nano-current-theme 'light) (eq nano-current-theme 'amber))
      (shell-command-to-string (concat kitty-send-command "set-colors --all --configured ~/.config/kitty/theme-light.conf"))
    (shell-command-to-string (concat kitty-send-command "set-colors --all --configured ~/.config/kitty/theme.conf")))
  (let ((bg-color (car (rassoc nano-current-theme nano-bg-theme-map))))
    (shell-command-to-string
     (concat kitty-send-command "set-colors background=" bg-color " selection-foreground=" bg-color))))

(defun nano-monochrome nil
  (interactive)
  (setq nano-monochrome (not nano-monochrome)) (nano-install-theme))

(define-key (current-global-map) (kbd "<f6>") #'nano-toggle-theme)
(define-key (current-global-map) (kbd "<f7>") #'nano-monochrome)
;; Set current theme based on terminal
(funcall (intern (concat "nano-" (symbol-name nano-current-theme))))

;; --- Header & mode lines --------------------------------------------------
(setq-default flymake-mode-line-counter-format
              '("" flymake-mode-line-error-counter
                flymake-mode-line-warning-counter
                flymake-mode-line-note-counter " ")
              flymake-mode-line-format
              '(" " flymake-mode-line-exception flymake-mode-line-counters)
              global-mode-string nil)
(setq-default mode-line-end-spaces '((:eval (when (or (eq major-mode 'compilation-mode)
                                                      (eq major-mode 'comint-mode))
                                              compilation-mode-line-errors))
                                     (:eval (when (bound-and-true-p flymake-mode)
                                              flymake-mode-line-format))
                                     " "))
(setq-default mode-line-format
              '("%e"
                (:eval (when (mode-line-window-selected-p)
                         (let ((tabs (let* ((tabs (length (tab-bar-tabs)))
                                            (active-tab (tab-bar--current-tab-index)))
                                       (if (<= tabs 1)
                                           ""
                                         (let ((result '()))
                                           (dotimes (i tabs)
                                             (if (= i active-tab)
                                                 (push (format "[%d]" (1+ i)) result)
                                               (push (format "%d" (1+ i)) result)))
                                           (concat " " (mapconcat 'identity (reverse result) " ")
                                                   " "))))))
                           (propertize tabs 'face 'bold))))
                (:eval (when (and (buffer-narrowed-p)
                                  (not (derived-mode-p 'Info-mode)))
                         (propertize " (N)"))); 'face font-lock-constant-face)))
                (:eval (propertize " %b" 'face (if (buffer-modified-p) 'bold-italic 'bold)
                                   'help-echo (buffer-file-name)))
                (:eval (propertize (string-trim-left
                                    (format-mode-line vc-mode))
                                   'face '(:weight light :slant italic)))
                (:eval (let ((prefix (cond
                                      ((or defining-kbd-macro executing-kbd-macro) "▶▶")
                                      ((region-active-p)
                                       (concat "%p " (format "{%d}" (count-lines (region-beginning) (region-end)))))
                                      ((eq major-mode 'doc-view-mode)
                                       (format "[%d/%d]" (doc-view-current-page) (doc-view-last-page-number)))
                                      ((or meow-mode (eq major-mode 'eww-mode)) "%p")
                                      ((buffer-modified-p)       "**")
                                      (buffer-read-only          "RO")
                                      (t                         "--"))))
                         (propertize (concat "   " prefix " "))))
                mode-line-format-right-align
                (when (and (bound-and-true-p eglot--managed-mode) (eglot-managed-p)) eglot-mode-line-progress)
                (:eval (unless (eq major-mode 'dired-mode)
                         (propertize (concat " " (format-mode-line
                                                  (when which-function-mode which-func-current)))
                                     'face (if (or (display-graphic-p) (mode-line-window-selected-p))
                                               'mode-line-active
                                             'mode-line-inactive))))
                (:eval (when (mode-line-window-selected-p)
                         mode-line-end-spaces))))

(add-hook 'post-command-hook #'(lambda nil (when (region-active-p) (force-mode-line-update))))

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
  (define-key icomplete-minibuffer-map (kbd "C-j") #'icomplete-fido-exit)
  (define-key icomplete-fido-mode-map (kbd "TAB") #'icomplete-forward-completions)
  (define-key icomplete-fido-mode-map (kbd "<backtab>") #'icomplete-backward-completions)
  (define-key icomplete-fido-mode-map (kbd "<escape>") #'minibuffer-keyboard-quit))

(add-hook 'minibuffer-setup-hook
          (lambda nil (setq-local truncate-lines t
                                  line-spacing nil)))

(defun file-capf ()
  "File completion at point function. src: eshelyaron."
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (when bounds
      (list (car bounds) (cdr bounds) #'completion-file-name-table
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
         (if (and (display-graphic-p)
                  (require 'eldoc-box nil t))
             (eldoc-box-help-at-point)
           (eldoc-doc-buffer t))))

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

(define-key (current-global-map) (kbd "C-x C-m") #'execute-extended-command)
(define-key (current-global-map) (kbd "C-x m") esc-map)
(define-key (current-global-map) (kbd "C-x x b") #'ibuffer)
(define-key (current-global-map) (kbd "C-x x c") #'save-buffers-kill-emacs)
(define-key (current-global-map) (kbd "C-x x e") #'eval-last-sexp)
(define-key (current-global-map) (kbd "C-x x f") #'find-file)
(define-key (current-global-map) (kbd "C-x x s") #'save-buffer)
(define-key (current-global-map) (kbd "C-x x z") #'restart-emacs)
(define-key (current-global-map) (kbd "C-o") #'other-window)
(define-key (current-global-map) (kbd "C-x /") #'project-find-regexp)
(define-key (current-global-map) (kbd "C-x ;") #'comment-line)
(define-key (current-global-map) (kbd "C-h .") #'my-goto-doc)
(define-key (current-global-map) (kbd "C-h '") #'describe-face)
(define-key (current-global-map) (kbd "C-,") #'my-scroll-other-down)
(define-key (current-global-map) (kbd "C-.") #'my-scroll-other-up)
(define-key (current-global-map) (kbd "C-<tab>") #'tab-next)
(define-key (current-global-map) (kbd "C-S-<tab>") #'tab-previous)
(define-key (current-global-map) (kbd "C-x C-b") #'ibuffer)
(define-key (current-global-map) (kbd "M-s r") #'replace-regexp)
(define-key (current-global-map) (kbd "C-x k") #'kill-current-buffer)
(define-key (current-global-map) (kbd "C-x f") #'recentf-open)
(define-key (current-global-map) (kbd "C-g") #'nano-quit)
(define-key (current-global-map) (kbd "C-z")  #'restart-emacs)
(define-key (current-global-map) (kbd "C-<wheel-up>") nil)
(define-key (current-global-map) (kbd "C-<wheel-down>") nil)
(define-key window-prefix-map (kbd "m") #'maximize-window)
(define-key window-prefix-map (kbd "u") #'winner-undo)
(define-key window-prefix-map (kbd "r") #'winner-redo)

;; --- Sane settings --------------------------------------------------------
(set-default-coding-systems 'utf-8)
(setq-default tab-width 4
              completion-styles
              '(basic partial-completion substring flex emacs22)
              completion-cycle-threshold t
              ;; cursor-type 'bar
              enable-recursive-minibuffers t
              line-spacing 3
              imenu-flatten t
              display-line-numbers-width 4
              display-line-numbers-widen t
              initial-scratch-message nil
              indent-tabs-mode nil
              mouse-wheel-tilt-scroll t
              mouse-wheel-flip-direction t
              mouse-wheel-scroll-amount-horizontal 4
              ring-bell-function 'ignore
              select-enable-clipboard t
              show-paren-context-when-offscreen t
              show-paren-when-point-inside-paren t
              use-short-answers t
              use-dialog-box nil
              uniquify-buffer-name-style 'forward)

(when (featurep 'recentf)
  (add-hook 'kill-emacs-hook #'recentf-cleanup))
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'prog-mode-hook (electric-pair-mode t))
(unless (require 'corfu nil t)
  (add-hook 'prog-mode-hook #'completion-preview-mode))
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(dolist (mode-hook '(prog-mode-hook conf-mode-hook yaml-ts-mode-hook))
  (add-hook mode-hook #'display-line-numbers-mode))
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
      comint-prompt-read-only t
      comint-buffer-maximum-size 2048
      compilation-ask-about-save nil
      completion-ignore-case t
      completion-auto-help 'lazy;nil
      confirm-kill-emacs 'yes-or-no-p
      confirm-nonexistent-file-or-buffer nil
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
      ffap-machine-p-known 'reject
      flymake-suppress-zero-counters t
      flymake-no-changes-timeout 2
      flymake-show-diagnostics-at-end-of-line 'short
      flymake-margin-indicators-string
      '((error "»" compilation-error)
        (warning "»" compilation-warning)
        (note "»" compilation-info))
      help-window-select t
      hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil
      kill-buffer-delete-auto-save-files t
      pixel-scroll-precision-interpolate-page t
      recentf-max-saved-items 200
      recentf-auto-cleanup 'never
      save-abbrevs nil
      save-interprogram-paste-before-kill t
      scroll-margin 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      set-mark-command-repeat-pop t
      shell-command-prompt-show-cwd t
      shell-kill-buffer-on-exit t
      shell-file-name (car (process-lines "which" "fish"))
      tab-bar-show nil
      vc-allow-rewriting-published-history 'ask
      vc-display-status 'no-backend
      vc-follow-symlinks t
      which-func-unknown ""
      xref-search-program (if (executable-find "rg") 'ripgrep 'grep)
      xref-auto-jump-to-first-xref nil
      xref-show-definitions-function 'xref-show-definitions-buffer-at-bottom
      xref-show-xrefs-function 'xref-show-definitions-completing-read)

(defun meain/electric-pair-conservative-inhibit (char)
  (or
   ;; I find it more often preferable not to pair when the
   ;; same char is next.
   (eq char (char-after))
   ;; Don't pair up when we insert the second of "" or of ((.
   (and (eq char (char-before))
        (eq char (char-before (1- (point)))))
   ;; I also find it often preferable not to pair next to a word.
   (eq (char-syntax (following-char)) ?w)
   ;; Don't pair at the end of a word, unless parens.
   (and
    (eq (char-syntax (char-before (1- (point)))) ?w)
    (eq (preceding-char) char)
    (not (eq (char-syntax (preceding-char)) ?\()))))
(setq electric-pair-inhibit-predicate 'meain/electric-pair-conservative-inhibit)

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

(with-eval-after-load 'replace
  (setq list-matching-lines-default-context-lines 2)
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
  (advice-add 'occur-context-lines :around #'clean-occur-context-line))

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

(with-eval-after-load 'eww
  (setq eww-header-line-format nil)
  (setq eww-auto-rename-buffer 'title))

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
                ("vc-git :.\*" . 0) ("\\*vc.\*-log\\*" . 0) ("\\*eldoc\\*" . 0)
                ("\\*Help\\*" . 0) ("\\*Warnings\\*" . 1) ("\\*log-edit-files\\*" . 1)
                ("\\*Occur.*\\*$" . 1) ("\\*grep.*\\*$" . 1) ("CAPTURE-.*" . 1)
                ("\\*Org Select\\*" . 1) ("\\*xref\\*" . 1)))
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

(with-eval-after-load 'comint-mode
  (define-key comint-mode-map "q" #'kill-buffer-and-window))
(with-eval-after-load 'compile
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

(add-hook 'term-mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil)
            (term-set-escape-char ?\C-x)
            (define-key term-raw-map "\C-o" 'other-window)
            (define-key term-raw-map "\M-y" 'yank-pop)
            (define-key term-raw-map "\C-y" 'yank)
            (define-key term-raw-map "\M-w" 'kill-ring-save)
            (define-key term-raw-map "\M-j" 'window-toggle-side-windows)))

(add-hook 'compilation-filter-hook (lambda nil
                                     (unless (eq major-mode 'grep-mode)
                                       (ansi-color-compilation-filter)
                                       (ansi-osc-compilation-filter))))

;; --- Programming ----------------------------------------------------------
(define-key (current-global-map) (kbd "C-x c c") #'compile)
(define-key (current-global-map) (kbd "C-x c r") #'recompile)
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

(define-derived-mode zig-mode c-mode "zig-mode")  ;; Until zig-ts-mode is core
(nconc auto-mode-alist
       `(("\\.zig\\'"          . zig-mode)
         ("\\.zig\\.zon\\'"    . js-json-mode)
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
(add-hook 'go-ts-mode-hook #'whitespace-mode)
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
  (setq project-vc-extra-root-markers '("go.mod" "Cargo.toml" "build.zig"))
  (setq project-vc-ignores '("**/vendor/**")))

(with-eval-after-load 'eglot
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-config '(:size 0 :format short)
        eglot-sync-connect 0
        eglot-autoshutdown t
        eglot-inlay-hints-mode nil)

  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))
  (push '(zig-mode . ("zls")) eglot-server-programs)

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
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
;; (defun whitespace-tabify nil
;;   (interactive)
;;   (let ((modified (buffer-modified-p)))
;;     (call-interactively 'tabify t)
;;     (whitespace-mode 1)
;;     (call-interactively 'untabify t)
;;     (set-buffer-modified-p modified)))
(setq-default fill-column 120)
(defvar old--mode-line-format nil)
(defun toggle-zen-buffer ()
  "Toggle center alignment of the buffer. Source: jamesdyer."
  (interactive)
  (let* ((special-modes (or (eq major-mode 'org-mode) (eq major-mode 'markdown-mode)))
         (margin (if (or (equal (window-margins) '(0 . 0))
                         (null (car (window-margins))))
                     (/ (- (window-total-width) (if special-modes 160 fill-column)) 2) 0)))
    (visual-line-mode 1)
    (set-window-margins nil margin margin)
    (when special-modes
      (text-scale-set (if (eq text-scale-mode-amount 0) 2 0))
      (setq-local line-spacing (if (eq line-spacing 3) 0.5 3)))))
(define-key (current-global-map) (kbd "<f9>") #'toggle-zen-buffer)

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
          (lambda nil (meow--set-cursor-type
                       (if (or meow-mode (derived-mode-p 'special-mode)) 'box 'bar))))
(define-key (current-global-map) (kbd "j") (lambda nil (interactive) (my-chord ?j ?k 'meow-mode)))
(define-key meow-mode-map (kbd "g") (make-sparse-keymap))
(define-key meow-mode-map (kbd "m") (make-sparse-keymap))
(define-key meow-mode-map (kbd "z") (make-sparse-keymap))
(define-key meow-mode-map (kbd "H") help-map)
(define-key meow-mode-map (kbd "SPC") ctl-x-map)
(define-key meow-mode-map (kbd "ms") insert-pair-map)
(dolist (num '(0 1 2 3 4 5 6 7 8 9))
  (define-key meow-mode-map (int-to-string num) #'digit-argument))
(dolist (pair '(("\\" . dired-jump) ("gl" . move-end-of-line) ("ge" . move-end-of-line)
                ("gh" . back-to-indentation) ("gj" . end-of-buffer) ("gk" . beginning-of-buffer)
                ("q" . quit-window) ("=" . mark-sexp) ("-" . negative-argument)
                ("/" . isearch-forward-regexp) ("e" . forward-word) ("b" . backward-word)
                ("v" . set-mark-command) ("h" . backward-char) ("j" . next-line)
                ("k" . previous-line) ("l" . forward-char) ("i" . meow-insert)
                ("y" . kill-ring-save) ("%" . match-pair) ("o" . other-window)
                ("D" . pixel-scroll-interpolate-down) ("U" . pixel-scroll-interpolate-up)
                ("gT" . tab-bar-switch-to-prev-tab) ("gt" . tab-bar-switch-to-next-tab)
                ("x" . my-select-fwd-line) ("X" . exchange-point-and-mark) ("O" . occur)
                ("w" . my-mark-word) ("," . my-scroll-other-down) ("s" . isearch-forward-regexp)
                ("." . my-scroll-other-up) (";" . keyboard-quit) ("gf" . ffap)
                ("gS" . scratch-buffer) ("*" . isearch-forward-symbol-at-point)
                ("ga" . (lambda nil (interactive) (org-agenda nil "n"))) ("gc" . org-capture)
                ("`" . window-toggle-side-windows) ("zz" . pop-to-mark-command)
                ("gi" . eglot-find-implementation) ("gs" . imenu) ("(" . down-list)
                (")" . up-list) ("[" . backward-list) ("]" . forward-list)
                ("{" . flymake-goto-prev-error) ("}" . flymake-goto-next-error)
                ("g/" . xref-find-definitions-other-window) ("gd" . xref-find-definitions)
                ("gb" . xref-go-back) ("K" . my-goto-doc) (":" . goto-line)
                ("gx" . flymake-show-buffer-diagnostics) ("gr" . xref-find-references)
                ("&" . align-regexp) ("C" . string-rectangle) ("p" . yank)
                ("P" . yank-pop) ("+" . eglot-rename) ("mm" . point-to-register)
                ("'" . register-to-point) ("md" . delete-pair) ("+" . eglot-code-actions)
                ("Z" . undo-redo) ("u" . undo-only) ("R" . replace-regexp)
                ("zf" . hs-toggle-hiding) ("zc" . hs-hide-all) ("zs" . hs-show-all)
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
                ("F" . (lambda nil (interactive)
                         (let ((xref-show-xrefs-function 'xref--show-xref-buffer))
                           (call-interactively 'project-find-regexp))))
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
  ;; fixing vc-annotate : vc-annotate-background-mode doesn't play
  ;; well with white fg, so we tweak the faces to have black fg
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
  ;; vc-annotate messes up the window-arrangement, give it a dedicated tab
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
      eshell-hist-ignoredups t
      eshell-history-size 20000
      eshell-save-history-on-exit t
      eshell-glob-case-insensitive t)

(defun my-eshell-only-aliases () ; create aliases that shouldn't be exported to common file
  (push '("source" ". $1") eshell-command-aliases-list)
  (push '("mkcd" "mkdir -p $1 && cd $1") eshell-command-aliases-list)
  (push '("k" "kubecolor $*") eshell-command-aliases-list)
  (push '("ky" "kubecolor -oyaml $*") eshell-command-aliases-list)
  (push '("clear" "clear t") eshell-command-aliases-list)
  (push '("d" "dired-other-window $1") eshell-command-aliases-list)
  (push '("dired" "dired $1") eshell-command-aliases-list)
  (push '("ee" "find-file-other-window $1") eshell-command-aliases-list)
  (push '("ff" "find-file $1") eshell-command-aliases-list)
  (push '("e" "find-file $1") eshell-command-aliases-list)
  (push '("gd" "vc-diff") eshell-command-aliases-list)
  (push '("glog" "vc-print-root-log") eshell-command-aliases-list)
  (push '("groot" "cd ${git rev-parse --show-toplevel}") eshell-command-aliases-list)
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
       (point)))))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (setq-local global-hl-line-mode nil)
              (setenv "TERM" "xterm-256color")
              (define-key eshell-hist-mode-map (kbd "<up>") nil t)
              (define-key eshell-hist-mode-map (kbd "<down>") nil t)
              (define-key eshell-hist-mode-map (kbd "C-p")
                          #'eshell-previous-matching-input-from-input)
              (define-key eshell-hist-mode-map (kbd "C-n")
                          #'eshell-next-matching-input-from-input)
              (define-key eshell-mode-map (kbd "C-x n d") #'my-eshell-narrow-to-prompt)
              (define-key eshell-mode-map (kbd "C-u") (lambda nil (interactive) (kill-line 0)))
              (define-key eshell-mode-map (kbd "C-w") #'backward-kill-word)
              (define-key eshell-mode-map (kbd "C-x n d") #'my-eshell-narrow-to-prompt)
              (define-key eshell-hist-mode-map (kbd "C-r") #'eshell-insert-history)))

(setq doc-view-resolution 600
      doc-view-continuous t
      doc-view-mupdf-use-svg t
      large-file-warning-threshold (* 50 (expt 2 20)))
(with-eval-after-load 'doc-view ;; requires `'gs', `mupdf-tools'
  (define-key doc-view-mode-map (kbd "j") #'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "k") #'doc-view-scroll-down-or-previous-page))

(with-eval-after-load 'org
  (load "~/.emacs.d/lisp/org-conf" :noerr :no-message)
  (add-hook 'org-capture-mode-hook 'meow-insert))

(with-eval-after-load 'gnus
  (load "~/.emacs.d/lisp/gnus-conf" nil :no-message)
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

;; --- External -------------------------------------------------------------
(run-with-idle-timer
 0.7 nil (lambda nil (load "~/.emacs.d/lisp/dev-conf" nil :no-message)
           (when (require 'corfu nil t) (global-corfu-mode))))

;; --- 31 stuff -------------------------------------------------------------
(when (string> emacs-version "31")
  (setq treesit-auto-install-grammar 'always)
  (setq kill-region-dwim 'emacs-word)
  (with-eval-after-load 'dired (setq dired-hide-details-hide-absolute-location t))
  (with-eval-after-load 'eglot (setq eglot-code-action-indicator ""))
  ;; (with-eval-after-load 'icomplete (setq icomplete-vertical-in-buffer-adjust-list t))
  (setq flymake-show-diagnostics-at-end-of-line 'fancy))

;; --- Speed benchmarking ---------------------------------------------------
;; (let ((init-time (float-time (time-subtract (current-time) init-start-time)))
;;       (total-time (string-to-number (emacs-init-time "%f"))))
;;   (message (concat
;;             (propertize "Startup time: " 'face 'bold)
;;             (format "%.2fs " init-time)
;;             (propertize (format "(+ %.2fs system time)"
;;                                 (- total-time init-time))))))
