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

(defface zed-sans-font
  '((t (:font "Fira Sans")))
  "Font used by sans faces.")

(setq zed-icons-p t);(require 'nerd-icons nil t))

(setq zed-adjust 0.2)

(setq zed-tab-forward-button
      (propertize "⏵" 'display `(raise 0.3))
      zed-tab-back-button
      (propertize "⏴ " 'display `(raise 0.3)))


(let ((selection-color (if load-theme-light
                           "#C9D0D9"
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
      (orange-color (if load-theme-light
                        "#eb9350"
                      "#f3a171"))
      (magenta-color (if load-theme-light
                         "#7646c1"
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
   ;; `(mode-line ((t (:background ,modeline-color :foreground ,foreground-color
   ;;                              ,@(when (display-graphic-p)
   ;;                                  (list :underline
   ;;                                        (list :color border-color :position -2)
   ;;                                        :overline border-color))
   ;;                              :extend t
   ;;                              :box (:line-width 4 :style flat-button)))))
   ;; `(mode-line-highlight ((t (:box (:line-width 2 :color ,inactive-color)))))
   ;; `(mode-line-inactive ((t (:inherit mode-line :foreground ,inactive-color))))

   ;; Parens
   `(show-paren-match ((t (:background ,selection-color))))
   `(show-paren-mismatch ((t (:foreground ,foreground-color :background ,red-color))))

   ;; Highlighting
   `(hl-line ((t (:background ,@(if load-theme-light
                                    (list "#f0f0f0")
                                  (list "#21242b"))))))
   `(highline-face ((t (:background ,subtle-color))))
   `(highlight ((t (:background ,selection-color))))
   `(highlight-symbol-face ((t (:background ,secondary-color))))
   `(isearch ((t (:background ,green-color :foreground ,background-color))))
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
   `(font-lock-preprocessor-face ((t (:foreground ,orange-color))))
   `(font-lock-property-name-face ((t (:foreground ,@(if load-theme-light
                                                         (list magenta-color)
                                                       (list yellow-color))))))
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

   ;; howm
   `(action-lock-face ((t (:inherit button))))
   `(howm-mode-keyword-face ((t (:foreground ,blue-color :underline t))))
   `(howm-mode-title-face ((t (:weight bold :foreground ,yellow-color))))
   `(howm-mode-ref-face ((t (:inherit howm-mode-keyword-face))))
   `(howm-reminder-today-face ((t (:foreground ,orange-color))))
   `(howm-reminder-normal-face ((t (:foreground ,red-color))))
   `(howm-view-empty-face ((t ())))
   `(howm-view-hilit-face ((t (:inherit match))))
   `(howm-view-name-face ((t (:inherit font-lock-comment-face))))
   
   ;; Magit
   `(magit-section-highlight ((t (:inherit hl-line))))
   `(magit-section-heading ((t (:foreground ,yellow-color :extend t))))
   `(magit-diff-file-header ((t (:bold t :inherit diff-header))))
   `(magit-diff-hunk-header ((t (:inherit diff-header))))
   `(magit-diff-add ((t (:inherit diff-added :foreground "grey20"))))
   `(magit-diff-del ((t (:inherit diff-removed :foreground "grey20"))))
   `(magit-diff-none ((t (:inherit diff-context :foreground "grey20"))))
   `(magit-item-highlight ((t (:background nil :foreground ,foreground-color))))
   `(magit-log-author ((t (:foreground ,orange-color))))
   `(forge-pullreq-open ((t (:foreground ,green-color))))

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
   
   `(compilation-info ((t (:foreground ,green-color))))
   `(compilation-warning ((t (:foreground ,yellow-color))))
   `(compilation-error ((t (:foreground ,red-color))))
   `(success ((t (:foreground ,green-color))))
   `(warning ((t (:foreground ,yellow-color))))
   `(error ((t (:foreground ,red-color))))
   `(flymake-note ((t (:underline ,`(:style line :position -1 :color ,green-color)))))
   `(flymake-warning ((t (:underline ,`(:style line :position -1 :color ,yellow-color)))))
   `(flymake-error ((t (:underline ,`(:style line :position -1 :color ,red-color)))))
   `(diff-hl-insert ((t (:foreground ,green-color))))
   `(diff-hl-change ((t (:foreground ,light-yellow-color))))
   `(diff-hl-delete ((t (:foreground ,red-color))))
   `(diff-hl-dired-insert ((t (:background ,green-color :foreground ,green-color))))
   `(diff-hl-dired-change ((t (:background ,light-yellow-color :foreground ,light-yellow-color))))
   `(diff-hl-dired-delete ((t (:background ,red-color :foreground ,red-color))))
   
   `(completions-highlight ((t (:inherit highlight :extend t))))
   ;; `(corfu-default ((t (:background ,modeline-color :foreground ,foreground-color))))
   ;; `(corfu-border ((t (:inherit corfu-default))))
   `(corfu-current ((t (:background ,selection-color :foreground ,foreground-color))))
   `(dired-directory ((t (:foreground ,yellow-color))))
   `(dired-header ((t (:foreground ,green-color :height 1.2))))
   `(dired-marked ((t (:foreground ,magenta-color))))
   `(fill-column-indicator ((t (:inherit font-lock-comment-face :height 1.1))))
   `(fixed-pitch ((t (:inherit default))))
   `(fixed-pitch-serif ((t (:inherit default))))
   `(help-key-binding ((t (:background ,subtle-color))))
   `(info-menu-star ((t (:foreground ,red-color))))
   `(info-title-4 ((t (:weight medium :height 1.1 :inherit variable-pitch))))
   `(icomplete-selected-match ((t (:inherit highlight :extend t))))
   `(tty-menu-selected-face ((t (:inherit highlight))))
   `(tty-menu-enabled-face ((t (:background ,modeline-color))))
   `(tty-menu-disabled-face ((t (:background ,modeline-color :foreground ,inactive-color))))
   `(link ((t (:foreground ,cyan-color))))
   `(link-visited ((t (:foreground ,blue-color))))
   `(line-number ((t (:inherit font-lock-comment-face))))
   `(line-number-current-line ((t (:inherit hl-line :foreground ,foreground-color))))
   `(orderless-match-face-0 ((t (:foreground ,magenta-color :weight bold))))
   `(orderless-match-face-1 ((t (:foreground ,green-color :weight bold))))
   `(orderless-match-face-2 ((t (:foreground ,yellow-color :weight bold))))
   `(orderless-match-face-3 ((t (:foreground ,red-color :weight bold))))
   `(popper-echo-area ((t (:inherit mode-line))))
   `(pulse-highlight-start-face ((t (:background ,light-yellow-color))))
   `(pulse-highlight-face ((t (:background ,light-yellow-color :extend t))))
   `(vertical-border ((t (:foreground ,border-color))))
   `(whitespace-tab ((t (:inherit font-lock-comment-face))))
   `(whitespace-space ((t (:weight light :foreground ,background-color))))
   `(whitespace-line ((t (:background ,background-color))))
   `(whitespace-indentation ((t (:foreground ,inactive-color))))
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

   `(variable-pitch-text ((t (:inherit variable-pitch :height 1.1))))
   `(shr-code ((t (:inherit variable-pitch-text :background ,subtle-color))))
   `(shr-h1 ((t (:inherit variable-pitch-text :height 1.2 :weight bold))))
   `(shr-h2 ((t (:inherit variable-pitch-text :height 1.1 :weight bold))))
   `(shr-h3 ((t (:inherit variable-pitch-text :height 1.1 :weight bold))))
   `(shr-h4 ((t (:inherit variable-pitch-text :height 1.1 :weight bold))))
   `(shr-h5 ((t (:inherit variable-pitch-text :height 1.1 :weight bold))))
   `(shr-h6 ((t (:inherit variable-pitch-text :height 1.1 :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,cyan-color))))
   `(gnus-cite-2 ((t (:foreground ,yellow-color))))
   `(gnus-cite-3 ((t (:foreground ,green-color))))
   `(gnus-cite-4 ((t (:foreground ,orange-color))))
   `(gnus-cite-5 ((t (:foreground ,red-color))))
   `(gnus-cite-6 ((t (:foreground ,magenta-color))))
   `(gnus-cite-7 ((t (:foreground ,blue-color))))
   `(gnus-group-mail-1 ((t (:foreground ,blue-color :weight bold))))
   `(gnus-group-mail-2 ((t (:foreground ,blue-color :weight bold))))
   `(gnus-group-mail-3 ((t (:foreground ,blue-color :weight bold))))
   `(gnus-group-mail-low ((t (:foreground ,blue-color :weight bold))))
   `(gnus-group-mail-1-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-2-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-3-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-low-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-1 ((t (:foreground ,blue-color :weight bold))))
   `(gnus-group-news-2 ((t (:foreground ,blue-color :weight bold))))
   `(gnus-group-news-3 ((t (:foreground ,blue-color :weight bold))))
   `(gnus-group-news-low ((t (:foreground ,blue-color :weight bold))))
   `(gnus-group-news-1-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-2-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-3-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-low-empty ((t (:foreground "#5e636e"))))
   `(gnus-header ((t (:height 1.05 :inherit default))))
   `(gnus-header-content ((t (:height 1.05 :foreground ,inactive-color))))
   `(gnus-header-from ((t (:height 1.05 :foreground ,foreground-color :weight bold))))
   `(gnus-header-subject ((t (:height 1.05 :foreground ,foreground-color :weight bold))))
   `(gnus-header-name ((t (:height 1.05 :foreground ,yellow-color :weight bold))))
   `(gnus-header-newsgroups ((t (:height 1.05 :foreground ,green-color :weight bold))))
   `(gnus-summary-selected ((t (:background ,modeline-color))))
   `(gnus-summary-normal-ticked ((t (:foreground ,yellow-color))))
   `(gnus-summary-normal-unread ((t (:foreground ,green-color :weight bold))))
   `(gnus-summary-normal-read ((t (:foreground ,inactive-color))))
   `(gnus-summary-normal-ancient ((t (:foreground ,inactive-color))))
   
   `(solaire-default-face ((t (:inherit default :background ,subtle-color))))
   `(solaire-fringe-face ((t (:inherit fringe :background ,subtle-color))))
   `(solaire-header-line-face ((t (:inherit header-line :background ,subtle-color))))

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
     ((t (:foreground ,inactive-color))));"black" :background "#00c2ff"))))
   `(minibuffer-prompt ((t (:foreground ,@(if load-theme-light
                                              (list magenta-color)
                                            (list yellow-color))))))
   `(widget-field ((t (:background ,subtle-color :extend t))))
   
   `(org-block ((t (:inherit default :background ,subtle-color :extend t :height 1.0))))
   `(org-block-begin-line ((t (:inherit org-meta-line :background ,subtle-color :extend t))))
   `(org-block-end-line ((t (:inherit org-meta-line :background ,subtle-color :extend t))))
   `(org-document-title ((t (:height 1.1))))
   `(org-document-info ((t (:height 1.1))))
   `(org-drawer ((t (:inherit shadow))))
   `(org-ellipsis ((t (:inherit shadow))))
   `(org-quote ((t (:inherit org-block :slant italic))))
   `(org-level-1 ((t (:inherit default :height 1.2 :weight regular))))
   `(org-level-2 ((t (:inherit default :height 1.2 :weight regular))))
   `(org-level-3 ((t (:inherit default :height 1.2 :weight regular))))
   `(org-level-4 ((t (:inherit default :height 1.2 :weight regular))))
   `(org-level-5 ((t (:inherit default :height 1.2 :weight regular))))
   `(org-level-6 ((t (:inherit default :height 1.2 :weight regular))))
   `(org-level-7 ((t (:inherit default :height 1.2 :weight regular))))
   `(org-level-8 ((t (:inherit default :height 1.2 :weight regular))))
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
   `(org-habit-alert-future-face ((t (:foreground ,red-color))))
   `(org-habit-overdue-face ((t (:foreground ,red-color))))
   `(org-habit-overdue-future-face ((t (:foreground ,red-color))))
   `(org-habit-ready-face ((t (:foreground ,green-color))))
   `(org-habit-ready-future-face ((t (:foreground ,green-color))))

   `(markdown-code-face ((t (:background ,subtle-color :extend t :weight semi-bold
                                         :font ,(face-attribute 'default :family)))))
   `(markdown-inline-code-face ((t (:background ,subtle-color))))
   `(markdown-header-face-1 ((t (:height 1.1 :weight bold :inherit markdown-header-face))))
   `(markdown-header-face-2 ((t (:height 1.1 :weight bold :inherit markdown-header-face))))
   `(markdown-header-face-3 ((t (:height 1.1 :weight bold :inherit markdown-header-face))))
   `(markdown-header-face-4 ((t (:height 1.1 :weight bold :inherit markdown-header-face))))
   `(markdown-header-face-5 ((t (:height 1.1 :weight bold :inherit markdown-header-face))))
   `(markdown-header-face-6 ((t (:height 1.1 :weight bold :inherit markdown-header-face))))
   `(markdown-header-face-7 ((t (:height 1.1 :weight bold :inherit markdown-header-face))))
   `(markdown-header-face-8 ((t (:height 1.1 :weight bold :inherit markdown-header-face))))
   
   `(tab-bar ((t (:box ,border-color :height 120
                       :background
                       ,@(if (display-graphic-p)
                             (list subtle-color)
                           (list modeline-color))))))
   `(tab-bar-tab ((t (:inherit default :weight regular :height 150))))
   `(tab-bar-tab-inactive
     ((t (:foreground ,inactive-color :weight regular :height 150
                      ,@(if (display-graphic-p)
                            (list :background subtle-color
                                  :underline
                                  (list :color border-color :position -1))
                          (list :background modeline-color))))))

   `(tab-line ((t (:inherit tab-bar))))
   `(tab-line-tab ((t (:inherit tab-bar-tab))))
   `(tab-line-tab-inactive ((t (:inherit tab-bar-tab-inactive))))
   `(tab-line-tab-special ((t (:italic nil))))
   `(tab-line-tab-current ((t (:inherit tab-line-tab))))
   `(tab-line-highlight ((t (:inherit tab-line-tab))))

   `(eglot-mode-line ((t (:foreground ,yellow-color))))
   `(eldoc-box-border ((t (:inherit child-frame-border))))
   `(eldoc-box-body ((t (:inherit zed-sans-font :weight regular :background ,subtle-color))))
   `(eglot-highlight-symbol-face ((t (:background ,secondary-color))))
   `(deadgrep-filename-face ((t (:inherit tab-bar))))
   `(denote-faces-date ((t (:inherit font-lock-comment-face))))
   `(denote-faces-link ((t (:inherit link :background ,subtle-color))))
   `(sh-heredoc ((t (:foreground ,yellow-color))))
   `(sh-quoted-exec ((t (:foreground ,cyan-color))))
   `(completions-annotations ((t (:foreground ,blue-color))))
   `(completions-common-part ((t (:foreground ,blue-color))))
   `(vertico-current ((t (:foreground ,blue-color :inverse-video t :extend t))))
   ;; `(vertico-posframe ((t (:foreground ,foreground-color :background "#1a1a1a")))
   `(vertico-posframe-border ((t (:inherit child-frame-border))))
   `(why-this-face ((t (:inherit font-lock-comment-face))))
   `(which-key-key-face ((t (:inherit default))))
   `(which-key-command-description-face ((t (:inherit font-lock-comment-face :slant italic))))
   `(which-func ((t (:foreground ,inactive-color))))
   `(xref-file-header ((t (:inherit tab-bar))))))

;; Mode-line
;; (with-eval-after-load 'macrursors
;;   (setq macrursors-mode-line ; src: karthink
;;         '((:eval
;;            (when (or defining-kbd-macro executing-kbd-macro)
;;              (let ((sep (propertize " " 'face '(:background "#00x2ff" :foreground "black")))
;;                    (vsep (propertize " " 'face '(:inherit variable-pitch))))
;;                ;; "●"
;;                (propertize
;;                 (concat
;;                  sep "REC" vsep
;;                  (number-to-string kmacro-counter) vsep "▶" vsep
;;                  (when macrursors-mode
;;                    (if macrursors--overlays
;;                        (format (concat "[%d/%d]" vsep)
;;                                (1+ (cl-count-if (lambda (p) (< p (point))) macrursors--overlays
;;                                                 :key #'overlay-start))
;;                                (1+ (length macrursors--overlays)))
;;                      (concat "[1/1]" vsep))))
;;                 'face '(:background "#00c2ff" :foreground "black"))))))))

;; (setq global-mode-string nil
;;       project-mode-line nil
;;       ;; project-mode-line-face 'font-lock-comment-face
;;       ;; clean eglot
;;       eglot-menu-string (char-to-string #x2714)
;;       which-func-non-auto-modes '(erc-mode)
;;       which-func-unknown ""
;;       which-func-update-delay 1.0
;;       ;; which-func-modes '(text-mode prog-mode)
;;       which-func-format
;;       `(:propertize which-func-current face which-func)

;;       ;; dont show which-func information in misc-info
;;       mode-line-misc-info (delete
;;                            '(which-function-mode
;;                              (which-func-mode
;;                               (which-func--use-mode-line
;;                                (#1="" which-func-format " "))))
;;                            mode-line-misc-info)

;;       ;; all the stuff that will be right-aligned
;;       mode-line-end-spaces
;;       `("%n "
;;         mode-line-misc-info
;;         ;; project-mode-line-format
;;         ;; (:eval (propertize (format " %s" (upcase (if (stringp mode-name)
;;         ;;                                                mode-name
;;         ;;                                              (car mode-name))))
;;         ;;                      'help-echo "Mouse-1: Show major-mode-menu"
;;         ;;                      'local-map mode-line-major-mode-keymap))
;;         (:eval (when (bound-and-true-p flymake-mode)
;;                  (let ((flymake-mode-line-counter-format
;;                         (if zed-icons-p
;;                             (with-eval-after-load 'nerd-icons
;;                               `(""
;;                                 ,(when (flymake--mode-line-counter :error)
;;                                    (concat " " (nerd-icons-codicon
;;                                                 "nf-cod-error"
;;                                                 :face 'compilation-error
;;                                                 :v-adjust 0.1)
;;                                            " "))
;;                                 flymake-mode-line-error-counter
;;                                 ,(when (flymake--mode-line-counter :warning)
;;                                    (concat " " (nerd-icons-codicon
;;                                                 "nf-cod-warning"
;;                                                 :face 'compilation-warning
;;                                                 :v-adjust 0.1)
;;                                            ""))
;;                                 flymake-mode-line-warning-counter
;;                                 ,(when (flymake--mode-line-counter :note)
;;                                    (concat " " (nerd-icons-codicon
;;                                                 "nf-cod-info"
;;                                                 :face 'compilation-info
;;                                                 :v-adjust 0.1)
;;                                            ""))
;;                                 flymake-mode-line-note-counter ""))
;;                           '(" " flymake-mode-line-error-counter
;;                             flymake-mode-line-warning-counter
;;                             flymake-mode-line-note-counter ""))))
;;                    (flymake--mode-line-counters))))
;;         (:eval (when (mode-line-window-selected-p)
;;                  (meow-indicator)))
;;         ;; (:eval (when (bound-and-true-p vc-mode)
;;         ;;          (propertize vc-mode 'face
;;         ;;                      '(:inherit success))))
;;         " "))

;; (defun my/ml-padding ()
;;   "Adding padding to the modeline so that spme elements can be right aligned."
;;   (let ((r-length (length (format-mode-line mode-line-end-spaces))))
;;     (propertize " "
;;                 'display `(space :align-to (- right ,r-length)))))

;; (setq-default
;;  mode-line-format
;;  '(;; (:eval (when (mode-line-window-selected-p)
;;    ;;          (let* ((state meow--current-state)
;;    ;;                 (indicator-face
;;    ;;                  (if (bound-and-true-p meow-global-mode)
;;    ;;                      (alist-get state meow-indicator-face-alist)
;;    ;;                    'font-lock-comment-face))
;;    ;;                 (buffer-state (cond (defining-kbd-macro " >> ")
;;    ;;                                     (buffer-read-only " RO ")
;;    ;;                                     ((buffer-modified-p) " ** ")
;;    ;;                                     ((file-remote-p default-directory)
;;    ;;                                      (concat " " (file-remote-p
;;    ;;                                                   default-directory 'host)
;;    ;;                                              " "))
;;    ;;                                     (t " -- "))))
;;    ;;            (propertize buffer-state
;;    ;;                        'face `(,indicator-face
;;    ;;                                :box (:style flat-button) :weight bold)))))
;;    (:eval (when (bound-and-true-p macrursors-mode)
;;             macrursors-mode-line))
;;    "%e"
;;    (:eval (propertize " %p " 'face 'which-func))
;;    (:eval (propertize " %b " 'face (if (and (buffer-modified-p)
;;                                             (or (derived-mode-p 'prog-mode)
;;                                                 (derived-mode-p 'text-mode)))
;;                                        '(:inherit font-lock-warning-face :weight bold)
;;                                      '(:weight bold))
;;                       'help-echo "Mouse-1: Show major-mode-menu"
;;                       'local-map mode-line-major-mode-keymap))
;;    (which-function-mode (which-func-mode
;;                          ("" which-func-format " ")))
;;    mode-line-format-right-align
;;    (:eval (when (mode-line-window-selected-p)
;;             mode-line-end-spaces))))

;; Trying a no-mode-line view
(setq mode-line-format (list ""))
(setq-default mode-line-format (list ""))
(unless (display-graphic-p)
  (setq mode-line-format (make-string (window-width) ?─ t))
  (setq-default mode-line-format (make-string (window-width) ?─ t)))
(set-face-attribute 'mode-line nil
                    :box nil
                    :inherit nil
                    :foreground (if load-theme-light
                                    "grey"
                                  "#474856")
                    :background (if (display-graphic-p)
                                    (if load-theme-light
                                        "grey"
                                      "#474856")
                                  (face-background 'default))
                    :height 0.1)
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :inherit nil
                    :foreground (if load-theme-light
                                    "grey"
                                  "#474856")
                    :background (if (display-graphic-p)
                                    (if load-theme-light
                                        "grey"
                                      "#474856")
                                  (face-background 'default))
                    :height 0.1)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|HACK\\|TODO\\|WIP\\|BUG\\|DONE\\)"
                1 font-lock-warning-face t)
               (";" . 'font-lock-comment-face)))))

;; Tabs
(with-eval-after-load 'tab-line
  (setq tab-line-close-button
        (propertize (concat " " (make-string 1 #x00D7) " ") 'close-tab t)
        tab-line-left-button zed-tab-back-button
        tab-line-right-button zed-tab-forward-button))


(with-eval-after-load 'tab-bar
  (setq tab-bar-separator nil
        ;; FIXME auto-width doesnt work with zed-tab-name
        tab-bar-auto-width t
        tab-bar-auto-width-max '((120) 16)
        tab-bar-tab-name-truncated-max 16
        tab-bar-tab-name-function #'tab-bar-tab-name-truncated
        tab-bar-tab-name-ellipsis "…"
        tab-bar-close-button-show 'selected
        tab-bar-close-last-tab-choice 'tab-bar-mode-disable
        tab-bar-close-button
        (propertize (concat (make-string 1 #x00D7) " ") 'close-tab t))

  (defun zed-tab-name (tab _i)
    "A cleaner tab name emulating atom one."
    (let* ((selected-p (eq (car tab) 'current-tab))
           (name (alist-get 'name tab))
           (name (concat " " name " "))
           (face (if selected-p
                     'tab-bar-tab
                   'tab-bar-tab-inactive))
           (vsep (when (display-graphic-p)
                   (propertize " " 'face '(:inherit child-frame-border)
                               'display '(space :width (1))))))
      (concat
       vsep
       (apply 'propertize
              " "
              `(tab ,tab
                    ,@(if selected-p '(selected t))
                    face ,face
                    display (raise ,zed-adjust)))
       ;; flymake/compile error
       (when (and selected-p
                  (eq major-mode 'compilation-mode))
         (let* ((error (unless (zerop compilation-num-errors-found)
                         (propertize (concat (int-to-string compilation-num-errors-found) " ")
                                     'face 'compilation-error)))
                (warning (unless (zerop compilation-num-warnings-found)
                           (propertize (concat (int-to-string compilation-num-warnings-found) " ")
                                       'face 'compilation-warning)))
                (info (unless (zerop compilation-num-infos-found)
                        (propertize (concat (int-to-string compilation-num-infos-found) " ")
                                    'face 'compilation-info)))
                (compilation-str (concat " " error warning info)))
           (add-face-text-property 0 (length compilation-str)
                                   '(:inherit tab-bar-tab :box nil
                                              :height 120 :weight regular)
                                   t
                                   compilation-str)
           (propertize compilation-str 'display '(raise 0.20))))
       (when (and selected-p
                  (bound-and-true-p flymake-mode))
         (let* ((flymake-mode-line-counter-format
                 '("" flymake-mode-line-error-counter
                   flymake-mode-line-warning-counter
                   flymake-mode-line-note-counter ""))
                (flymake-str (format-mode-line flymake-mode-line-counters)))
           (add-face-text-property 0 (length flymake-str)
                                   '(:inherit tab-bar-tab :box nil
                                              :height 120 :weight regular)
                                   t
                                   flymake-str)
           (propertize flymake-str 'display `(raise 0.20))))
       ;; in case the buffer is narrowed
       (when (and selected-p (buffer-narrowed-p)
                  (not (or (derived-mode-p 'Info-mode)
                           (derived-mode-p 'nov-mode))))
         (propertize " [N]"
                     'face `(:inherit tab-bar-tab :inherit zed-sans-font
                                      :foreground ,(face-background 'cursor nil t))
                     'display `(raise ,zed-adjust)))

       ;; apply face to buffer name
       (apply 'propertize
              (propertize (string-replace "%" "%%" name) ;; (bug#57848)
                          'follow-link 'ignore)
              `(tab ,tab
                    ,@(if selected-p '(selected t))
                    face (:inherit ,face :inherit zed-sans-font)
                    display (raise ,zed-adjust)))
       ;; show dot if selected buffer modified else close button
       (when (and selected-p (buffer-modified-p)
                  (or (derived-mode-p 'prog-mode)
                      (derived-mode-p 'text-mode)))
         (propertize (make-string 1 #x23fA)
                     'face `(:inherit tab-bar-tab
                                      :foreground ,(face-background 'cursor nil t))
                     'display `(raise ,zed-adjust)))
       (apply 'propertize
              (if (and selected-p tab-bar-close-button-show
                       (not (buffer-modified-p)))
                  tab-bar-close-button
                " ")
              `(tab ,tab
                    ,@(if selected-p '(selected t))
                    face ,face
                    display (raise 0.15)))
       vsep)))
  
  (defun zed-bar-format-menu-bar ()
    "Produce the Menu button for the tab bar that shows the menu bar."
    `((menu-bar menu-item ,(propertize " ≡ " 'display `((raise ,zed-adjust)))
                tab-bar-menu-bar :help "Menu Bar")))

  (defun zed-bar-format-history ()
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

  (defun zed-bar-lsp nil
    (when (or (derived-mode-p 'prog-mode)
              (eq major-mode 'org-mode))
      `((eglot-menu menu-item
                    ,(propertize (string-replace "%" "%%"
                                                 (or (gethash (selected-window)
                                                              which-func-table)
                                                     ""))
                                 'face '(:inherit font-lock-comment-face
                                                  :inherit zed-sans-font :height 130)
                                 'display `((raise 0.2)))
                    (lambda (event)
                      (interactive "e")
                      (when (and (bound-and-true-p eglot--managed-mode)
                                 (eglot-managed-p)
                                 (project-current))
                        (popup-menu eglot-menu)))
                    :help "Show LSP menu"))))

  (defun zed-bar-vc nil
    (when (bound-and-true-p vc-mode)
      `((vc-menu menu-item
                 ,(propertize (replace-regexp-in-string "^.." " " vc-mode)
                              'face '(:inherit success :inherit zed-sans-font :height 130)
                              'display `((raise 0.2)))
                 (lambda (event)
                   (interactive "e")
                   (popup-menu vc-menu-map))
                 :help "Show vc-menu"))))

  (defun zed-bar-progress nil
    `((major-mode
       menu-item
       ,(propertize
         (cond
          ((eq major-mode 'pdf-view-mode)
           (format " %d/%d"
                   (pdf-view-current-page)
                   (pdf-cache-number-of-pages)))
          ((eq major-mode 'nov-mode)
           (format "%d%% %d/%d"
                   (/ (window-start) 0.01 (point-max))
                   (1+ nov-documents-index)
                   (length nov-documents)))
          ((memq major-mode '(gnus-summary-mode gnus-article-mode))
           (format "%s %d%%"
                   (let ((unread-count (cdar gnus-topic-unreads)))
                     (if (or (not unread-count) (eq unread-count 0))
                         ""
                       (propertize (format "Unread: %d" unread-count)
                                   'face '(:inherit font-lock-comment-face))))
                   (/ (window-start) 0.01 (point-max))))
          ((when (or defining-kbd-macro executing-kbd-macro)
             (concat
              " REC "
              (number-to-string kmacro-counter)  " ▶ "
              (when (and (featurep 'macrursors) macrursors-mode)
                (if macrursors--overlays
                    (format (concat "[%d/%d]" " ")
                            (1+ (cl-count-if (lambda (p) (< p (point))) macrursors--overlays
                                             :key #'overlay-start))
                            (1+ (length macrursors--overlays)))
                  (concat "[1/1]" " "))))))
          (t (format " %d%%"
                     (/ (window-start) 0.01 (point-max)))))
         'display `((raise ,zed-adjust)))
       (lambda (event) ;; src: mouse-menu-bar-map
         (interactive "e")
         (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
         (let* ((local-menu (and (current-local-map)
                                 (lookup-key (current-local-map) [menu-bar])))
                (local-title-or-map (and local-menu (cadr local-menu))))
           (or (null local-menu)
               (stringp local-title-or-map)
               (setq local-menu
                     (cons 'keymap
                           (cons (concat (format-mode-line mode-name)
                                         " Mode Menu")
                                 (cdr local-menu)))))
           (if local-menu
               (popup-menu local-menu)
             (message "No menu found for %s" major-mode))))
       :help "Show major mode menu")))
  
  (setq tab-bar-tab-name-format-function #'zed-tab-name
        tab-bar-format
        '(tab-bar-format-tabs
          tab-bar-separator
          tab-bar-format-align-right
          zed-bar-lsp
          zed-bar-vc
          zed-bar-progress
          zed-bar-format-menu-bar))
  
  (if (display-graphic-p)
      (push 'zed-bar-format-history tab-bar-format)
    (setq tab-bar-separator ""
          tab-bar-close-button-show nil
          tab-bar-auto-width nil)))

;;;###autoload
(defun zed-toggle-theme ()
  "Toggle between dark and light mode."
  (interactive)
  (if load-theme-light
      (progn
        (unless (display-graphic-p)
          (call-process-shell-command
           "kitty @ --to=\"unix:/tmp/$(ls /tmp | grep mykitty)\" set-colors --all --configured ~/.config/kitty/theme.conf"
           nil 0))
        (when (eq system-type 'darwin)
          (modify-all-frames-parameters '((ns-appearance . dark))))
        (setq load-theme-light nil))
    (progn
      (unless (display-graphic-p)
        (call-process-shell-command
         "kitty @ --to=\"unix:/tmp/$(ls /tmp | grep mykitty)\" set-colors --all --configured ~/.config/kitty/theme-light.conf"
         nil 0))
      (when (eq system-type 'darwin)
        (modify-all-frames-parameters '((ns-appearance . light))))
      (setq load-theme-light t)))
  (load-theme 'zed :no-confirm)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'dired-mode)
        (dolist (face '(default fringe))
          (face-remap-add-relative
					 face :background (face-background 'help-key-binding)))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zed)
;;; zed-theme.el ends here

;;; Brutalist
;; type: underline, keyword: bold

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
