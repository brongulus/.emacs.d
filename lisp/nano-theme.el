;;;; nano-theme -*- lexical-binding: t -*-
(defvar nano-current-theme 'burn "Current nano variant being used.")
(defvar nano-monochrome t "Should the font-locking have colours.")
(setq kitty-send-command "kitty @ --to=\"unix:/tmp/$(ls /tmp | grep mykitty)\" ")
(setq nano-bg-theme-map
      '(("#f7f7f7" . light) ("#fbf8ef" . amber) ("#1b1b1b" . dark) ("#212121" . burn)))
(unless (or (eq system-type 'android) (string= "" (shell-command-to-string "pgrep kitty")))
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
  (dolist (item '((nano-highlight    . (hl-line highlight custom-button-mouse lazy-highlight
                                                icomplete-selected-match completions-common-part))
                  (nano-subtle       . (match region isearch widget-field custom-button))
                  (nano-faded        . (shadow vertical-border font-lock-comment-face
                                               font-lock-doc-face icomplete-section
                                               completions-annotations))
                  (nano-string       . (font-lock-string-face font-lock-constant-face))
                  (nano-salient      . (link help-argument-name custom-visibility
                                             minibuffer-prompt font-lock-builtin-face
                                             font-lock-type-face font-lock-keyword-face
                                             font-lock-variable-name-face
                                             font-lock-function-name-face
                                             font-lock-property-name-face))
                  (nano-critical     . (error xref-file-header warning help-key-binding))
                  (nano-critical-i   . (isearch-fail))
                  (nano-faded-i      . (show-paren-match))))
    (nano-link-face (car item) (cdr item)))

  (set-face-attribute 'fringe nil :background (face-background 'default))
  (set-face-attribute 'font-lock-string-face nil :slant 'italic :weight 'semi-bold)
  (set-face-attribute 'font-lock-doc-face nil :slant 'italic)
  (set-face-attribute 'font-lock-builtin-face nil :slant 'italic)
  (set-face-attribute 'link nil :underline t)
  (set-face-attribute 'completions-common-part nil :underline t)
  (set-face-attribute 'region nil :extend nil)
  (set-face-attribute 'cursor nil :background "#00c2ff") ; FIXME
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

  (with-eval-after-load 'outline
    (dolist (face '(outline-1 outline-2 outline-3 outline-4 outline-5 outline-6 outline-7 outline-8))
      (set-face-attribute face nil :height 1.2 :inherit 'bold)))
  (with-eval-after-load 'markdown-mode
    (dolist (face '(markdown-pre-face)); markdown-code-face))
      (set-face-attribute face nil :background (face-background 'nano-highlight) :extend t)))
  (with-eval-after-load 'org
    (dolist (face '(org-block org-block-begin-line org-block-end-line))
      (set-face-attribute face nil :background (face-background 'nano-highlight) :extend t :inherit 'default))
    (set-face-attribute 'org-drawer nil :foreground (face-foreground 'nano-faded))
    (set-face-attribute 'org-footnote nil :foreground (face-foreground 'nano-faded) :underline t)
    (set-face-attribute 'org-date nil :foreground (face-foreground 'link))
    (set-face-attribute 'org-table nil :foreground (face-foreground 'nano-default))
    (set-face-attribute 'org-ellipsis nil :foreground (face-foreground 'nano-default) :underline nil)
    (set-face-attribute 'org-verbatim nil :inherit 'org-latex-and-related)
    (set-face-attribute 'org-code nil :inherit 'org-latex-and-related))
  (with-eval-after-load 'sh-script
    (set-face-attribute 'sh-quoted-exec nil :foreground (face-foreground 'nano-salient) :italic t))
  (with-eval-after-load 'shr
    (set-face-attribute 'shr-code nil :weight 'bold))

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
  (nano-set-face 'nano-default "#e8e8e8" "#1b1b1b")
  (nano-set-face 'nano-highlight nil "#2b2b2b")
  (nano-set-face 'nano-subtle "#CCCCCC" "#464646")
  (nano-set-face 'nano-faded "#707070")
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
  (set-face-attribute 'nano-string nil :foreground "#ebdbb2")
  (set-face-attribute 'nano-salient nil :foreground "#f9f5d7")
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
