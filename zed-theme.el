;;; zed-theme.el -- Zed theme for Emacs.  -*- lexical-binding: t; -*-

;; Author: All credits go to atom/zed theme
;; Version: 1.0

;;; Commentary:
;; This theme is based on the theme created by the atom devs.
;; Source: <https://github.com/atom/atom/tree/master/packages/one-dark-ui>

;;; Code:

(deftheme zed
  "Zed skin.")

(defcustom load-theme-light nil
  "Use dark or light theme."
  :type 'boolean
  :group 'zed)

(setq zed-icons-p t);(require 'nerd-icons nil t))

(let ((selection-color (if load-theme-light
                           "#C9D0D9"
                         "#3d4b5c"))
      (highlight-color (if load-theme-light
                           "#FFD863"
                         "#3d4b5c"))
      (secondary-color (if load-theme-light
                           "#e0e3ed" ;"#FBE9AD"
                         "#38404e"))
      (inactive-color (if load-theme-light
                          "#5e636e"
                        "#848993"))
      (border-color (if load-theme-light
                        "grey"
                      "#474856"))
      (subtle-color (if load-theme-light
                        "#EEEEEE"
                      "#30343d"))
      (modeline-color (if load-theme-light
                          "#dcdcdd"
                        "#3c414c"))
      (background-color (if load-theme-light
                            "#f7f7f7"
                          "#282c33"))
      (foreground-color (if load-theme-light
                            "#1a1a1a"
                          "#cccccc"))
      (green-color (if load-theme-light
                       "#5f8700"
                     "#a7bf87"))
      (blue-color (if load-theme-light
                      "#6079db"
                    "#81a2be"))
      (cyan-color (if load-theme-light
                      "#6594bd"
                    "#7db2bd"))
      (red-color (if load-theme-light
                     "#c56655"
                   "#c47779"))
      (orange-color "#f5871f")
      (magenta-color (if load-theme-light
                         "#875faf"
                       "#b294bb"))
      (yellow-color (if load-theme-light
                        "#bb9200"
                      "#d9c18c"))
      (light-yellow-color (if load-theme-light
                              "#eab700"
                            "#d9c18c"))
      (_x-border-color "#a5a5a5"))
  (custom-theme-set-faces
   'zed
   ;; Basics
   `(default ((t (:background ,background-color :foreground ,foreground-color))))
   `(cursor ((t (:background "#00c2ff"))))
   `(border-glyph ((t (nil))))
   `(info-node ((t (:italic t :bold t))))
   `(info-xref ((t (:inherit link))))
   `(left-margin ((t (nil))))
   `(right-margin ((t (nil))))
   `(pointer ((t (nil))))

   ;; Frame
   `(child-frame-border ((t (:background ,border-color :foreground ,border-color))))
   `(fringe ((t (:background ,background-color))))
   `(header-line ((t (:background ,background-color))))
   `(mode-line ((t (:background ,modeline-color :foreground ,foreground-color
                                ,@(when (display-graphic-p)
                                    (list :underline
                                          (list :color border-color :position -2)
                                          :overline border-color))
                                :box (:line-width 4 :style flat-button)))))
   `(mode-line-highlight ((t (:box (:line-width 2 :color ,inactive-color)))))
   `(mode-line-inactive ((t (:inherit mode-line :foreground ,inactive-color))))

   ;; Parens
   `(show-paren-match ((t (:background ,selection-color))))
   `(show-paren-mismatch ((t (:foreground ,foreground-color :background ,red-color))))

   ;; Highlighting
   `(hl-line ((t (:background ,@(if load-theme-light
                                    (list "#dcdcdd")
                                  (list "#2D323B"))))))
   `(highline-face ((t (:background ,subtle-color))))
   `(highlight ((t (:background ,highlight-color))))
   `(highlight-symbol-face ((t (:background ,secondary-color))))
   `(isearch ((t (:background ,orange-color :foreground ,background-color))))
   `(lazy-highlight ((t (:background ,light-yellow-color :foreground ,background-color))))
   `(primary-selection ((t (:background ,selection-color))))
   `(region ((t (:background ,selection-color :extend nil))))
   `(secondary-selection ((t (:background ,secondary-color :extend nil))))
   `(match ((t (:inherit highlight))))
   `(rectangle-preview ((t (:inherit region))))
   `(shadow ((t (:foreground "grey50"))))

   ;; Font-lock
   `(font-lock-builtin-face ((t (:foreground ,blue-color :inherit italic))))
   `(font-lock-comment-face ((t (:foreground ,inactive-color))))
   `(font-lock-constant-face ((t (:foreground ,yellow-color))))
   `(font-lock-number-face ((t (:inherit font-lock-constant-face))))
   `(font-lock-doc-string-face ((t (:foreground "#1A93AE" :background "#F4F9FE"))))
   `(font-lock-function-name-face ((t (:foreground ,blue-color))))
   `(font-lock-keyword-face ((t (:foreground ,magenta-color))))
   `(font-lock-preprocessor-face ((t (:foreground ,magenta-color))))
   `(font-lock-property-name-face ((t (:foreground ,yellow-color))))
   `(font-lock-reference-face ((t (:foreground "#4E279A" :background "#F3F2FF"))))
   `(font-lock-string-face ((t (:foreground ,green-color))))
   `(font-lock-type-face ((t (:foreground ,cyan-color))))
   `(font-lock-variable-name-face ((t (:foreground ,foreground-color))))
   `(font-lock-warning-face ((t (:foreground ,red-color :weight bold))))

   ;; Diff Mode
   `(diff-header ((t (:inherit secondary-selection))))
   `(diff-file-header ((t (:bold t :inherit diff-header))))

   ;; Ediff
   `(ediff-odd-diff-A ((t (:inherit secondary-selection :extend t))))
   `(ediff-odd-diff-B ((t (:inherit secondary-selection :extend t))))
   `(ediff-odd-diff-C ((t (:inherit secondary-selection :extend t))))
   `(ediff-even-diff-A ((t (:inherit secondary-selection :extend t))))
   `(ediff-even-diff-B ((t (:inherit secondary-selection :extend t))))
   `(ediff-even-diff-C ((t (:inherit secondary-selection :extend t))))
   
   ;; Magit (Not using)
   `(magit-diff-file-header ((t (:bold t :inherit diff-header))))
   `(magit-diff-hunk-header ((t (:inherit diff-header))))
   `(magit-diff-add ((t (:inherit diff-added :foreground "grey20"))))
   `(magit-diff-del ((t (:inherit diff-removed :foreground "grey20"))))
   `(magit-diff-none ((t (:inherit diff-context :foreground "grey20"))))
   `(magit-item-highlight ((t (:background nil :foreground ,foreground-color))))

   ;; Custom
   `(ansi-color-bright-green ((t (:foreground ,green-color :background ,green-color))))
   `(ansi-color-bright-red ((t (:foreground ,red-color :background ,red-color))))
   `(ansi-color-bright-yellow ((t (:foreground ,yellow-color :background ,yellow-color))))
   `(ansi-color-bright-magenta ((t (:foreground ,magenta-color :background ,magenta-color))))
   `(ansi-color-bright-blue ((t (:foreground ,blue-color :background ,blue-color))))
   `(ansi-color-bright-cyan ((t (:foreground ,cyan-color :background ,cyan-color))))
   `(ansi-color-green ((t (:foreground ,green-color :background ,green-color))))
   `(ansi-color-red ((t (:foreground ,red-color :background ,red-color))))
   `(ansi-color-yellow ((t (:foreground ,yellow-color :background ,yellow-color))))
   `(ansi-color-magenta ((t (:foreground ,magenta-color :background ,magenta-color))))
   `(ansi-color-blue ((t (:foreground ,blue-color :background ,blue-color))))
   `(ansi-color-cyan ((t (:foreground ,cyan-color :background ,cyan-color))))
   
   `(avy-lead-face ((t (:foreground ,orange-color :weight bold
                                    :height 0.85 :box (:line-width -1)))))
   `(avy-lead-face-0 ((t (:inherit avy-lead-face))))
   `(avy-lead-face-1 ((t (:inherit avy-lead-face))))
   `(avy-lead-face-2 ((t (:inherit avy-lead-face))))
   
   `(compilation-info ((t (:foreground ,inactive-color))))
   `(compilation-warning ((t (:foreground ,inactive-color))))
   `(compilation-error ((t (:foreground ,inactive-color))))
   `(success ((t (:foreground ,green-color))))
   `(warning ((t (:foreground ,yellow-color))))
   `(error ((t (:foreground ,red-color))))
   `(flymake-note ((t (:underline ,`(:style line :position -1 :color ,green-color)))))
   `(flymake-warning ((t (:underline ,`(:style line :position -1 :color ,yellow-color)))))
   `(flymake-error ((t (:underline ,`(:style line :position -1 :color ,red-color)))))
   `(diff-hl-insert ((t (:foreground ,green-color))))
   `(diff-hl-change ((t (:foreground ,light-yellow-color))))
   `(diff-hl-delete ((t (:foreground ,red-color))))
   
   `(completions-highlight ((t (:inherit highlight :extend t))))
   `(corfu-default ((t (:background ,modeline-color))))
   `(corfu-current ((t (:background ,selection-color :foreground ,foreground-color))))
   `(dired-directory ((t (:foreground ,yellow-color))))
   `(dired-header ((t (:foreground ,green-color :height 1.2))))
   `(dired-marked ((t (:foreground ,magenta-color))))
   `(fill-column-indicator ((t (:inherit font-lock-comment-face :height 1.1))))
   `(fixed-pitch ((t (:inherit default))))
   `(fixed-pitch-serif ((t (:inherit default))))
   `(info-menu-star ((t (:foreground ,red-color))))
   `(icomplete-selected-match ((t (:inherit highlight :extend t))))
   `(tty-menu-selected-face ((t (:inherit highlight))))
   `(tty-menu-enabled-face ((t (:background ,modeline-color))))
   `(tty-menu-disabled-face ((t (:background ,modeline-color :foreground ,inactive-color))))
   `(link ((t (:foreground ,cyan-color))))
   `(link-visited ((t (:foreground ,blue-color))))
   `(line-number ((t (:inherit font-lock-comment-face))))
   `(line-number-current-line ((t (:inherit hl-line :foreground ,foreground-color))))
   `(orderless-match-face-0 ((t (:foreground ,blue-color :weight bold))))
   `(orderless-match-face-1 ((t (:foreground ,yellow-color :weight bold))))
   `(orderless-match-face-2 ((t (:foreground ,green-color :weight bold))))
   `(orderless-match-face-3 ((t (:foreground ,red-color :weight bold))))
   `(popper-echo-area ((t (:inherit mode-line))))
   `(pulse-highlight-start-face ((t (:background ,light-yellow-color))))
   `(pulse-highlight-face ((t (:background ,light-yellow-color :extend t))))
   `(vertical-border ((t (:foreground ,border-color))))
   `(whitespace-space ((t (:weight light :inherit font-lock-comment-face))))
   `(whitespace-line ((t (:inherit font-lock-comment-face))))
   `(whitespace-newline ((t (:inherit font-lock-comment-face))))
   `(whitespace-trailing ((t (:inherit font-lock-warning-face))))

   `(erc-notice-face ((t (:inherit font-lock-comment-face :weight bold))))
   `(erc-current-nick-face ((t (:foreground ,yellow-color :weight bold))))
   `(erc-my-nick-prefix-face ((t (:foreground ,yellow-color :weight bold))))
   `(erc-button ((t (:inherit link))))
   `(erc-input-face ((t (:inherit default))))
   `(erc-nick-default-face ((t (:foreground ,blue-color :weight bold))))
   `(erc-error-face ((t (:foreground ,red-color))))
   `(erc-timestamp-face ((t (:inherit font-lock-comment-face :weight bold))))
   `(erc-header-line ((t (:background ,modeline-color :foreground ,green-color))))

   `(variable-pitch-text ((t (:inherit variable-pitch))))
   `(shr-code ((t (:inherit default :slant italic))))
   `(gnus-group-mail-1 ((t (:foreground ,green-color :weight bold))))
   `(gnus-group-mail-2 ((t (:foreground ,green-color :weight bold))))
   `(gnus-group-mail-3 ((t (:foreground ,green-color :weight bold))))
   `(gnus-group-mail-low ((t (:foreground ,green-color :weight bold))))
   `(gnus-group-mail-1-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-2-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-3-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-low-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-1 ((t (:foreground ,green-color :weight bold))))
   `(gnus-group-news-2 ((t (:foreground ,green-color :weight bold))))
   `(gnus-group-news-3 ((t (:foreground ,green-color :weight bold))))
   `(gnus-group-news-low ((t (:foreground ,green-color :weight bold))))
   `(gnus-group-news-1-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-2-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-3-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-low-empty ((t (:foreground "#5e636e"))))
   `(gnus-header ((t (:inherit default))))
   `(gnus-header-content ((t (:foreground ,inactive-color))))
   `(gnus-header-from ((t (:foreground ,inactive-color))))
   `(gnus-header-subject ((t (:foreground ,inactive-color))))
   `(gnus-header-name ((t (:foreground ,green-color :weight bold))))
   `(gnus-header-newsgroups ((t (:foreground ,green-color :weight bold))))
   `(gnus-summary-selected ((t (:background ,modeline-color))))
   `(gnus-summary-normal-ticked ((t (:foreground ,yellow-color))))
   `(gnus-summary-normal-unread ((t (:foreground ,blue-color :weight bold))))
   `(gnus-summary-normal-read ((t (:foreground ,inactive-color))))
   `(gnus-summary-normal-ancient ((t (:foreground ,inactive-color))))
   
   `(solaire-default-face ((t (:inherit default :background ,subtle-color))))
   `(solaire-fringe-face ((t (:inherit fringe :background ,subtle-color))))
   `(solaire-header-line-face ((t (:inherit header-line :background ,subtle-color))))

   `(magit-section-highlight ((t (:inherit hl-line))))
   `(Man-overstrike ((t (:inherit font-lock-constant-face :bold t))))
   `(Man-underline ((t (:inherit font-lock-keyword-face :bold t))))
   
   `(meow-beacon-indicator
     ((t (:foreground ,inactive-color :background ,green-color))))
   `(meow-normal-indicator
     ((t (:foreground ,inactive-color))))
   `(meow-motion-indicator
     ((t (:foreground ,inactive-color))))
   `(meow-keypad-indicator
     ((t (:foreground ,inactive-color))))
   `(meow-insert-indicator
     ((t (:foreground ,inactive-color :background ,yellow-color))))
   `(minibuffer-prompt ((t (:foreground ,yellow-color))))
   `(widget-field ((t (:background ,subtle-color :extend t))))
   
   `(org-block ((t (:inherit default :background ,subtle-color :extend t :height 1.0))))
   `(org-block-begin-line ((t (:inherit org-meta-line :background ,subtle-color :extend t))))
   `(org-block-end-line ((t (:inherit org-meta-line :background ,subtle-color :extend t))))
   `(org-document-title ((t (:height 1.1))))
   `(org-document-info ((t (:height 1.1))))
   `(org-drawer ((t (:inherit shadow))))
   `(org-ellipsis ((t (:inherit shadow))))
   `(org-level-1 ((t (:height 1.1 :weight bold))))
   `(org-level-2 ((t (:height 1.1 :weight bold))))
   `(org-level-3 ((t (:height 1.1 :weight bold))))
   `(org-level-4 ((t (:height 1.1 :weight bold))))
   `(org-level-5 ((t (:height 1.1 :weight bold))))
   `(org-level-6 ((t (:height 1.1 :weight bold))))
   `(org-level-7 ((t (:height 1.1 :weight bold))))
   `(org-level-8 ((t (:height 1.1 :weight bold))))
   `(org-agenda-structure ((t (:foreground ,magenta-color :height 1.1))))
   `(org-agenda-structure-filter ((t (:inherit org-agenda-structure))))
   `(org-agenda-date ((t (:foreground ,blue-color))))
   `(org-agenda-date-today ((t (:foreground ,cyan-color))))
   `(org-time-grid ((t (:inherit font-lock-comment-face))))
   `(org-agenda-current-time ((t (:foreground ,foreground-color))))
   `(org-imminent-deadline ((t (:foreground ,red-color :weight bold))))
   `(org-todo ((t (:foreground ,inactive-color :box 1))))
   `(org-scheduled ((t (:foreground ,yellow-color))))
   `(org-scheduled-today ((t (:foreground ,green-color :weight bold))))
   `(org-agenda-done ((t (:foreground ,inactive-color))))
   `(org-done ((t (:foreground , inactive-color))))
   `(org-date ((t (:inherit link))))
   `(org-tag ((t (:inherit org-inline-src-block :extend nil))))
   `(org-latex-and-related ((t (:inherit org-inline-src-block :extend nil))))
   `(org-verbatim ((t (:inherit org-inline-src-block :extend nil))))
   `(org-habit-clear-face ((t (:foreground ,inactive-color))))
   `(org-habit-clear-future-face ((t (:foreground ,inactive-color))))
   `(org-habit-alert-face ((t (:foreground ,yellow-color))))
   `(org-habit-alert-future-face ((t (:foreground ,yellow-color))))
   `(org-habit-overdue-face ((t (:foreground ,red-color))))
   `(org-habit-overdue-future-face ((t (:foreground ,red-color))))
   `(org-habit-ready-face ((t (:foreground ,green-color))))
   `(org-habit-ready-future-face ((t (:foreground ,green-color))))
   
   `(tab-bar ((t (:background ,subtle-color :box ,border-color :height 160))))
   `(tab-bar-tab ((t (:inherit default :height 160))))
   `(tab-bar-tab-inactive
     ((t (:background ,subtle-color :foreground ,inactive-color :weight regular :height 160
                      ,@(when (display-graphic-p)
                          (list :underline
                                (list :color border-color :position -1)))))))

   `(tab-line ((t (:inherit tab-bar))))
   `(tab-line-tab ((t (:inherit tab-bar-tab))))
   `(tab-line-tab-inactive ((t (:inherit tab-bar-tab-inactive))))
   `(tab-line-tab-special ((t (:italic nil))))
   `(tab-line-tab-current ((t (:inherit tab-line-tab))))
   `(tab-line-highlight ((t (:inherit tab-line-tab))))

   `(eldoc-box-border ((t (:inherit child-frame-border))))
   `(eldoc-box-body ((t (:background ,subtle-color))))
   `(deadgrep-filename-face ((t (:inherit bold :inverse-video t))))
   `(denote-faces-date ((t (:inherit font-lock-comment-face))))
   `(denote-faces-link ((t (:inherit link :background ,subtle-color))))
   ;; `(vertico-posframe ((t (:foreground ,foreground-color :background "#1a1a1a")))
   `(vertico-posframe-border ((t (:inherit child-frame-border))))
   `(why-this-face ((t (:inherit font-lock-comment-face))))
   `(which-key-key-face ((t (:inherit default))))
   `(which-key-command-description-face ((t (:inherit font-lock-comment-face :slant italic))))
   `(which-func ((t (:foreground ,inactive-color))))))

;; Mode-line
(with-eval-after-load 'macrursors
  (setq macrursors-mode-line ; src: karthink
        '((:eval
           (when (or defining-kbd-macro executing-kbd-macro)
             (let ((sep (propertize " " 'face '(:background "#00x2ff" :foreground "black")))
                   (vsep (propertize " " 'face '(:inherit variable-pitch))))
               ;; "●"
               (propertize
                (concat
                 sep "REC" vsep
                 (number-to-string kmacro-counter) vsep "▶" vsep
                 (when macrursors-mode
                   (if macrursors--overlays
                       (format (concat "[%d/%d]" vsep)
                               (1+ (cl-count-if (lambda (p) (< p (point))) macrursors--overlays
                                                :key #'overlay-start))
                               (1+ (length macrursors--overlays)))
                     (concat "[1/1]" vsep))))
                'face '(:background "#00c2ff" :foreground "black"))))))))

(setq global-mode-string nil
      project-mode-line nil
      project-mode-line-face 'font-lock-comment-face
      ;; clean eglot
      eglot-menu-string (char-to-string #x2699)
      which-func-non-auto-modes '(erc-mode)
      which-func-unknown ""
      which-func-update-delay 1.0
      ;; which-func-modes '(text-mode prog-mode)
      which-func-format
      `(:propertize which-func-current face which-func)
      
      ;; dont show which-func information in misc-info
      mode-line-misc-info (delete
                           '(which-function-mode
                             (which-func-mode
                              (which-func--use-mode-line
                               (#1="" which-func-format " "))))
                           mode-line-misc-info)
      
      ;; all the stuff that will be right-aligned
      mode-line-end-spaces
      `("%n " mode-line-misc-info
        ;; project-mode-line-format
        ;; (:eval (propertize (format " %s" (upcase (if (stringp mode-name)
        ;;                                                mode-name
        ;;                                              (car mode-name))))
        ;;                      'help-echo "Mouse-1: Show major-mode-menu"
        ;;                      'local-map mode-line-major-mode-keymap))
        (:eval (when (bound-and-true-p flymake-mode)
                 (let ((flymake-mode-line-counter-format
                        (if zed-icons-p
                            (with-eval-after-load 'nerd-icons
                              `(""
                                ,(when (flymake--mode-line-counter :error)
                                   (concat " " (nerd-icons-codicon
                                                "nf-cod-error" :face 'compilation-error
                                                :v-adjust 0.1)
                                           " "))
                                flymake-mode-line-error-counter
                                ,(when (flymake--mode-line-counter :warning)
                                   (concat " " (nerd-icons-codicon
                                                "nf-cod-warning" :face 'compilation-warning
                                                :v-adjust 0.1)
                                           ""))
                                flymake-mode-line-warning-counter
                                ,(when (flymake--mode-line-counter :note)
                                   (concat " " (nerd-icons-codicon
                                                "nf-cod-info" :face 'compilation-info
                                                :v-adjust 0.1)
                                           ""))
                                flymake-mode-line-note-counter ""))
                          '(" " flymake-mode-line-error-counter
                            flymake-mode-line-warning-counter
                            flymake-mode-line-note-counter ""))))
                   (flymake--mode-line-counters))))
        (:eval (when (bound-and-true-p vc-mode)
                 (propertize vc-mode 'face
                             '(:inherit success))))
        " "))

(defun my/ml-padding ()
  "Adding padding to the modeline so that spme elements can be right aligned."
  (let ((r-length (length (format-mode-line mode-line-end-spaces))))
    (propertize " "
                'display `(space :align-to (- right ,r-length)))))

(setq-default
 mode-line-format
 '((:eval (when (mode-line-window-selected-p)
            (let* ((state (meow--current-state))
                   (indicator-face
                    (if (bound-and-true-p meow-global-mode)
                        (alist-get state meow-indicator-face-alist)
                      'font-lock-comment-face))
                   (buffer-state (cond (defining-kbd-macro " >> ")
                                       (buffer-read-only " RO ")
                                       ((buffer-modified-p) " ** ")
                                       ((file-remote-p default-directory)
                                        (concat " " (file-remote-p
                                                     default-directory 'host)
                                                " "))
                                       (t " -- "))))
              (propertize buffer-state
                          'face `(,indicator-face
                                  :box (:style flat-button) :weight bold)))))
   (:eval (when (bound-and-true-p macrursors-mode)
            macrursors-mode-line))
   "%e"
   (:eval (propertize " %p " 'face 'which-func))
   (:eval (propertize " %b " 'face (if (and (buffer-modified-p)
                                            (or (derived-mode-p 'prog-mode)
                                                (derived-mode-p 'text-mode)))
                                       '(:inherit font-lock-warning-face :weight bold)
                                     '(:weight bold))
                      'help-echo "Mouse-1: Show major-mode-menu"
                      'local-map mode-line-major-mode-keymap))
   (which-function-mode (which-func-mode
                         ("" which-func-format " ")))
   mode-line-format-right-align
   (:eval (when (mode-line-window-selected-p)
            mode-line-end-spaces))))

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|HACK\\|TODO\\|BUG\\|DONE\\)"
                1 font-lock-warning-face t)
               (";" . 'font-lock-comment-face)))))

;; Tabs
(with-eval-after-load 'tab-line
  (setq tab-line-close-button
        (propertize (concat " " (make-string 1 #x00D7) " ") 'close-tab t)
        tab-line-left-button zed-tab-back-button
        tab-line-right-button zed-tab-forward-button))

(with-eval-after-load 'tab-bar
  (defun zed-tab-name (tab _i)
    "A cleaner tab name emulating atom one."
    (let* ((selected-p (eq (car tab) 'current-tab))
           (name (alist-get 'name tab))
           (name (concat " " name " "))
           (face (if selected-p
                     'tab-bar-tab
                   'tab-bar-tab-inactive)))
      (concat
       (when (display-graphic-p)
         (propertize " " 'face `(:background ,(face-foreground 'vertical-border nil t))
                     'display '(space :width (1))))
       ;; apply face to buffer name
       (apply 'propertize
              (concat
               " "
               (propertize (string-replace "%" "%%" name) ;; (bug#57848)
                           'follow-link 'ignore))
              `(tab ,tab
                    ,@(if selected-p '(selected t))
                    face ,face
                    display (raise 0.2)))
       ;; show dot if selected buffer modified else close button
       (when (and selected-p (buffer-modified-p)
                  (or (derived-mode-p 'prog-mode)
                      (derived-mode-p 'text-mode)))
         (propertize (concat "" (make-string 1 #x23fA) "")
                     'face `(:inherit tab-bar-tab
                                      :foreground ,(face-background 'cursor nil t))
                     'display '(raise 0.2)))
       (apply 'propertize
              (if (and selected-p tab-bar-close-button-show
                       (not (buffer-modified-p)))
                  tab-bar-close-button
                " ")
              `(tab ,tab
                    ,@(if selected-p '(selected t))
                    face ,face
                    display (raise 0.2)))

       (when (display-graphic-p)
         (propertize " " 'face `(:background ,(face-foreground 'vertical-border nil t))
                     'display '(space :width (1)))))))

  (defun tab-bar-format-menu-bar ()
    "Produce the Menu button for the tab bar that shows the menu bar."
    `((menu-bar menu-item ,(propertize " ≡ " 'display '((raise 0.1)))
                tab-bar-menu-bar :help "Menu Bar")))

  (defun tab-bar-format-history ()
    "Produce back and forward buttons for the tab bar."
    (when tab-bar-history-mode
      `((sep-history-back menu-item ,(tab-bar-separator) ignore)
        (history-back
         menu-item ,zed-tab-back-button tab-bar-history-back
         :help "Click to go back in tab history")
        (sep-history-forward menu-item ,(tab-bar-separator) ignore)
        (history-forward
         menu-item ,zed-tab-forward-button tab-bar-history-forward
         :help "Click to go forward in tab history"))))
  
  (setq tab-bar-tab-name-format-function #'zed-tab-name
        tab-bar-format
        '(tab-bar-format-history
          tab-bar-format-tabs
          tab-bar-separator
          tab-bar-format-align-right
          tab-bar-format-menu-bar))
  
  (add-hook 'tab-bar-mode-hook #'tab-bar-history-mode)
  
  (setq tab-bar-separator nil
        tab-bar-tab-name-truncated-max 16
        tab-bar-tab-name-function #'tab-bar-tab-name-truncated
        tab-bar-tab-name-ellipsis "…"
        tab-bar-close-button-show 'selected
        tab-bar-close-last-tab-choice 'tab-bar-mode-disable
        tab-bar-close-button
        (propertize (concat (make-string 1 #x00D7) " ") 'close-tab t)
        zed-tab-forward-button
        (propertize "⏵" 'display '(raise 0.2))
        zed-tab-back-button
        (propertize "⏴" 'display '(raise 0.2))))

;;;###autoload
(defun zed-toggle-theme ()
  "Toggle between dark and light mode."
  (interactive)
  (if load-theme-light
      (setq load-theme-light nil)
    (setq load-theme-light t))
  (load-theme 'zed :no-confirm))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zed)
;;; zed-theme.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
