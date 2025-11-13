;;;; nano-theme -*- lexical-binding: t -*-
(defvar nano-current-theme 'burn "Current nano variant being used.")
(defvar nano-monochrome t "Should the font-locking have colours.")
(setq kitty-send-command "kitty @ --to=\"unix:/tmp/$(ls /tmp | grep mykitty)\" ")
(setq nano-bg-theme-map
      '(("#f7f7f7" . light) ("#F5E4C1" . amber) ("#222323" . dark) ("#121213" . burn)))
;; FIXME: `shell-command-to-string' causes startup slowdown
;; (run-with-idle-timer
;;  0.5 nil
;;  (lambda nil
;;    (unless (or (eq system-type 'android) (string= "" (shell-command-to-string "pgrep kitty")))
;;      (let ((color (shell-command-to-string
;;                    (concat kitty-send-command
;;                            "get-colors | grep ^background | awk '{printf $2}'"))))
;;        (setq nano-current-theme (cdr (assoc color nano-bg-theme-map)))
;;        (funcall (intern (concat "nano-" (symbol-name nano-current-theme))))))))
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
        (set-face-attribute face nil attribute 'unspecified)
        (when (face-attribute sources attribute)
          (set-face-attribute face nil attribute (face-attribute sources attribute)))))))

(defun nano-install-theme ()
  (set-face-attribute 'cursor nil :background "#00c2ff")
  (set-face-attribute 'default nil :foreground (face-foreground 'nano-default)
                      :background (face-background 'nano-default))
  (dolist (item '((nano-highlight    . (hl-line highlight custom-button-mouse lazy-highlight))
                  (nano-subtle       . (match region isearch widget-field custom-button
                                              completions-common-part icomplete-selected-match))
                  (nano-faded        . (shadow font-lock-comment-face icomplete-section
                                               completions-annotations line-number))
                  (nano-string       . (font-lock-string-face font-lock-doc-face icomplete-first-match))
                  (nano-salient      . (link help-argument-name custom-visibility
                                             minibuffer-prompt font-lock-type-face
                                             font-lock-variable-name-face
                                             font-lock-function-name-face))
                  (nano-critical     . (error warning help-key-binding))
                  (nano-critical-i   . (secondary-selection isearch-fail))
                  (nano-faded-i      . (show-paren-match))))
    (nano-link-face (car item) (cdr item)))

  (set-face-attribute 'fringe nil :background (face-background 'default))
  (set-face-attribute 'vertical-border nil :inherit nil
                      :background (face-background 'default)
                      :foreground (face-foreground 'shadow))
  ;; (set-face-attribute 'font-lock-doc-face nil :background (face-background 'highlight))
  (with-eval-after-load 'xref
    (set-face-attribute 'xref-match nil :underline t :inherit nil)
    (set-face-attribute 'xref-file-header nil :background (face-background 'nano-highlight)))
  (dolist (face '(font-lock-string-face font-lock-doc-face font-lock-builtin-face))
    (set-face-attribute face nil :slant 'italic))
  (set-face-attribute 'font-lock-function-call-face nil :slant 'italic :weight 'regular)
  (set-face-attribute 'font-lock-variable-use-face nil :weight 'regular
                      :foreground (face-foreground 'default))
  (dolist (face '(font-lock-builtin-face font-lock-constant-face font-lock-keyword-face
                                         font-lock-property-use-face font-lock-property-name-face))
    (set-face-attribute face nil :foreground (face-foreground 'default) :inherit nil))
  (set-face-attribute 'link nil :underline t)
  (set-face-attribute 'completions-common-part nil :underline t)
  (set-face-attribute 'region nil :extend nil)
  (set-face-attribute 'line-number-current-line nil :foreground (face-foreground 'default)
		              :weight (face-attribute 'bold :weight) :background 'unspecified)
  (with-eval-after-load 'make-mode
    (set-face-attribute 'makefile-targets nil :inherit 'font-lock-keyword-face))

  (when (eq system-type 'darwin)
    (modify-all-frames-parameters `((ns-appearance . ,nano-current-theme))))

  (let* ((color-themes ;; ansi-colors
          '((black   . ((dark . "#30343d") (light . "#EEEEEE")))
            (red     . ((dark . "#c47779") (light . "#c56655")))
            (green   . ((dark . "#99c476") (light . "#427b58")))
            (yellow  . ((dark . "#dab067") (light . "#b57614"))) ; dark FFBF00
            (blue    . ((dark . "#6eaadb") (light . "#04508c")))
            (magenta . ((dark . "#c9b1ca") (light . "#7646c1")))
            (cyan    . ((dark . "#6fcfd2") (light . "#076678")))
            (white   . ((dark . "#cccccc") (light . "#1a1a1a")))))
         (theme-variant (if (or (eq nano-current-theme 'light) (eq nano-current-theme 'amber))
                            'light 'dark)))
    (dolist (color-def color-themes)
      (let* ((color-name (car color-def))
             (color-value (alist-get theme-variant (cdr color-def))))
        (with-eval-after-load 'ansi-color
          (set-face-attribute (intern (format "ansi-color-%s" color-name)) nil
                              :foreground color-value :background color-value)
          (set-face-attribute (intern (format "ansi-color-bright-%s" color-name)) nil
                              :foreground color-value :background color-value))))
    (with-eval-after-load 'icomplete
      (set-face-attribute 'icomplete-first-match nil :foreground
                          (alist-get theme-variant (alist-get 'blue color-themes))))
    (with-eval-after-load 'ido
      (set-face-attribute 'ido-subdir nil :foreground (face-foreground 'warning))
      (set-face-attribute 'ido-first-match nil :foreground
                          (alist-get theme-variant (alist-get 'blue color-themes)))
      (set-face-attribute 'ido-only-match nil :foreground
                          (alist-get theme-variant (alist-get 'green color-themes))))
    (set-face-attribute 'success nil :foreground
                        (alist-get theme-variant (alist-get 'green color-themes)))
    (with-eval-after-load 'compile
      (set-face-attribute 'compilation-warning nil
                          :foreground (alist-get theme-variant (alist-get 'yellow color-themes))))
    (with-eval-after-load 'diff-hl
      (set-face-attribute 'diff-hl-insert nil :background (face-background 'default)
                          :foreground (alist-get theme-variant (alist-get 'green color-themes)))
      (set-face-attribute 'diff-hl-change nil :background (face-background 'default)
                          :foreground (alist-get theme-variant (alist-get 'yellow color-themes)))
      (set-face-attribute 'diff-hl-delete nil :background (face-background 'default)
                          :foreground (alist-get theme-variant (alist-get 'red color-themes))))
    (with-eval-after-load 'howm
      (set-face-attribute 'action-lock-face nil :underline
                          (alist-get theme-variant (alist-get 'blue color-themes)))
      (set-face-attribute 'howm-mode-keyword-face nil :background
                          (alist-get theme-variant (alist-get 'blue color-themes))
                          :foreground (face-background 'default))
      (set-face-attribute 'howm-mode-title-face nil :foreground
                          (alist-get theme-variant (alist-get 'blue color-themes)))
      (set-face-attribute 'howm-mode-ref-face nil :foreground
                          (alist-get theme-variant (alist-get 'cyan color-themes)))
      (set-face-attribute 'howm-reminder-todo-face nil :foreground
                          (alist-get theme-variant (alist-get 'yellow color-themes)))
      (set-face-attribute 'howm-reminder-today-face nil :background
                          (face-foreground 'warning))
      (set-face-attribute 'howm-reminder-defer-face nil :foreground
                          (alist-get theme-variant (alist-get 'magenta color-themes)))
      (set-face-attribute 'howm-reminder-schedule-face nil :foreground
                          (alist-get theme-variant (alist-get 'green color-themes)))
      (set-face-attribute 'howm-reminder-deadline-face nil :foreground
                          (alist-get theme-variant (alist-get 'red color-themes)))
      (set-face-attribute 'howm-reminder-late-deadline-face nil :background
                          (alist-get theme-variant (alist-get 'red color-themes)))
      (set-face-attribute 'howm-reminder-normal-face nil :foreground
                          (alist-get theme-variant (alist-get 'blue color-themes)))
      (set-face-attribute 'howm-view-name-face nil :background
                          (alist-get theme-variant (alist-get 'blue color-themes))
                          :foreground (face-background 'default))
      (set-face-attribute 'howm-view-hilit-face nil :underline
                          (alist-get theme-variant (alist-get 'blue color-themes))
                          :foreground 'unspecified)
      (set-face-attribute 'howm-view-empty-face nil :background 'unspecified)
      )
    
    ;; (font-lock-variable-name-face . blue) (font-lock-keyword-face . magenta)
    ;; (font-lock-type-face . cyan) (font-lock-property-name-face . magenta)
    (unless nano-monochrome
      (let ((face-color-map
             '((font-lock-builtin-face . blue) (font-lock-function-name-face . blue)
               (font-lock-constant-face . yellow) (font-lock-number-face . yellow)
               (font-lock-preprocessor-face . magenta) (font-lock-doc-face . cyan)
               (font-lock-string-face . green))))
        (dolist (fc face-color-map)
          (set-face-attribute (car fc) nil :foreground
                              (alist-get theme-variant (alist-get (cdr fc) color-themes)))))
      (set-face-attribute 'font-lock-builtin-face nil :slant 'unspecified)
      (set-face-attribute 'font-lock-function-call-face nil :slant 'unspecified)
      (set-face-attribute 'font-lock-function-name-face nil :weight (face-attribute 'bold :weight))))

  (with-eval-after-load 'dired
    (set-face-attribute 'dired-marked nil :foreground (face-foreground 'font-lock-string-face))
    (set-face-attribute 'dired-header nil :foreground (face-foreground 'font-lock-function-name-face)))
  
  (with-eval-after-load 'eglot
    (set-face-attribute 'eglot-mode-line nil :inherit 'nano-faded)
    (set-face-attribute 'eglot-highlight-symbol-face nil :underline t))
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

  (with-eval-after-load 'markdown-mode
    (dolist (face '(markdown-pre-face)); markdown-code-face))
      (set-face-attribute face nil :background (face-background 'nano-highlight) :extend t)))
  (with-eval-after-load 'org
    (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4
                                org-level-5 org-level-6 org-level-7 org-level-8))
      (set-face-attribute face nil :height 1.1 :inherit 'bold))
    (dolist (face '(org-block org-block-begin-line org-block-end-line))
      (set-face-attribute face nil :background (face-background 'nano-highlight) :extend t :inherit 'default))
    (set-face-attribute 'org-document-title nil :foreground (face-foreground 'nano-salient))
    (set-face-attribute 'org-todo nil :foreground (face-foreground 'org-scheduled-previously))
    (set-face-attribute 'org-done nil :foreground (face-foreground 'font-lock-comment-face))
    (set-face-attribute 'org-mode-line-clock nil :weight (face-attribute 'bold :weight)
                        :foreground (face-foreground 'warning)
                        :background (face-background 'highlight))
    (set-face-attribute 'org-drawer nil :foreground (face-foreground 'shadow))
    (set-face-attribute 'org-footnote nil :foreground (face-foreground 'shadow) :underline t)
    (set-face-attribute 'org-date nil :foreground (face-foreground 'link))
    (set-face-attribute 'org-table nil :foreground (face-foreground 'nano-default))
    (set-face-attribute 'org-ellipsis nil :foreground (face-foreground 'nano-default) :underline nil)
    (set-face-attribute 'org-verbatim nil :inherit 'org-latex-and-related)
    (set-face-attribute 'org-code nil :inherit 'org-latex-and-related))
  (with-eval-after-load 'org-agenda
    (set-face-attribute 'org-agenda-structure nil :height 1.2 :foreground (face-foreground 'default))
    (set-face-attribute 'org-agenda-done nil :foreground (face-foreground 'default)))

  (with-eval-after-load 'sh-script
    (set-face-attribute 'sh-heredoc nil :foreground (face-foreground 'font-lock-constant-face))
    (set-face-attribute 'sh-quoted-exec nil :foreground (face-foreground 'nano-salient) :italic t))
  (with-eval-after-load 'shr
    (set-face-attribute 'shr-text nil :height (face-attribute 'default :height))
    (set-face-attribute 'shr-code nil :weight (face-attribute 'bold :weight)))

  ;; Mode & header lines
  (set-face-attribute 'header-line nil :background 'unspecified :underline nil
                      :overline (face-foreground 'shadow))
  (set-face-attribute 'mode-line nil
                      :foreground (face-foreground 'default)
                      :background 'unspecified
                      :box '(:line-width 1 :style flat-button)
                      :overline (face-foreground 'shadow))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (face-foreground 'shadow)
                      :background 'unspecified
                      :box '(:line-width 1 :style flat-button)
                      :inverse-video (not (display-graphic-p))
                      :overline (face-foreground 'shadow))
  (unless (display-graphic-p)
    (set-face-attribute 'mode-line-active nil
                        :foreground (face-background 'default)
                        :background (face-foreground 'nano-salient))))

(defun nano-light (&rest args)
  "NANO light theme (was based on material colors)."
  (interactive)
  (nano-set-face 'nano-default "#37474F" "#F7F7F7")
  (nano-set-face 'nano-highlight nil "#d0d0d0")
  (nano-set-face 'nano-subtle "#37474F" "#BAD7FB")
  (nano-set-face 'nano-faded "#949494")
  (nano-set-face 'nano-salient "#1b2229" nil (face-attribute 'bold :weight))
  (nano-set-face 'nano-critical "#eb9250" nil (face-attribute 'bold :weight))
  (nano-set-face 'nano-string "#767676")
  (setq nano-current-theme 'light)
  (nano-install-theme))

(defun nano-dark (&rest args)
  "NANO dark theme (was based on nord colors)."
  (interactive)
  (nano-set-face 'nano-default "#d0d0d0" "#222323")
  (nano-set-face 'nano-highlight nil "#2b2b2b")
  (nano-set-face 'nano-subtle "#e8e8e8" "#005f87")
  (nano-set-face 'nano-faded "#707070")
  (nano-set-face 'nano-salient "#ffffff" nil (face-attribute 'bold :weight))
  (nano-set-face 'nano-critical "#b77e64" nil (face-attribute 'bold :weight))
  (nano-set-face 'nano-string "#bcd7d3")
  (setq nano-current-theme 'dark)
  (nano-install-theme))

(defun nano-amber (&rest args)
  "Change background of light theme to plan9"
  (interactive)
  (nano-light)
  (set-face-attribute 'nano-default nil :foreground "#352f19" :background "#F5E4C1")
  (set-face-attribute 'nano-highlight nil :background "wheat3")
  (set-face-attribute 'nano-subtle nil :foreground "#F7F7F7" :background "#005f87")
  (set-face-attribute 'nano-faded nil :foreground "#a89984")
  (let ((nano-current-theme 'light)) (nano-install-theme))
  (setq nano-current-theme 'amber))

(defun nano-burn (&rest args)
  "You know what it is, Black 'n Yellow"
  (interactive)
  (nano-dark)
  (set-face-attribute 'nano-default nil :foreground "#ddc898" :background "#121213")
  (set-face-attribute 'nano-faded nil :foreground "#7a766e")
  (set-face-attribute 'nano-subtle nil :foreground "#121213" :background "#BAD7FB")
  (set-face-attribute 'nano-string nil :foreground "#af9661")
  (set-face-attribute 'nano-salient nil :foreground "#d4af5a" :weight 'demi-bold)
  (set-face-attribute 'nano-highlight nil :background "#393939")
  (let ((nano-current-theme 'dark)) (nano-install-theme))
  (set-face-attribute 'font-lock-variable-name-face nil :weight 'regular)
  (set-face-attribute 'font-lock-variable-use-face nil :weight 'regular)
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

(define-key (current-global-map) (kbd "C-x 6") #'nano-toggle-theme)
(define-key (current-global-map) (kbd "C-x 7") #'nano-monochrome)
;; Set current theme based on terminal
(funcall (intern (concat "nano-" (symbol-name nano-current-theme))))
