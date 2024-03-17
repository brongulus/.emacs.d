;;; languid-theme.el --- Languid Theme -*- lexical-binding: t; -*-

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Maintainer: Étienne Deparis <etienne@depar.is>
;; Author: film42
;; Version: 1.7.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/racula/emacs

;;; Commentary:

;; This is a personal touch on the theme with bits from palenight.

;; A dark color theme available for a number of editors.
;; This theme tries as much as possible to follow the consensual
;; specification (see URL `https://spec.languidtheme.com/').

;;; Code:
(deftheme languid)


;;;; Configuration options:

(defgroup languid nil
  "Languid theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom languid-enlarge-headings nil
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'languid)

(defcustom languid-height-title-1 1.0
  "Font size 100%."
  :type 'number
  :group 'languid)

(defcustom languid-height-title-2 1.0;1
  "Font size 110%."
  :type 'number
  :group 'languid)

(defcustom languid-height-title-3 1.0;3
  "Font size 130%."
  :type 'number
  :group 'languid)

(defcustom languid-height-doc-title 1.0;44
  "Font size 144%."
  :type 'number
  :group 'languid)

(defcustom languid-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'languid)

(defcustom load-theme-light nil
  "Use dark or light theme"
  :type 'boolean
  :group 'languid)

(defvar languid-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Languid theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))

There is a lot of discussion behind the 256 colors theme (see URL
`https://github.com/languid/emacs/pull/57').  Please take time to
read it before opening a new issue about your will.")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(if load-theme-light
    (let ((selection-color (if (featurep 'ns) "ns_selection_color" "#C9D0D9"))
          (highlight-color "#FFD863");; "#EEE00A")
          (secondary-color "#FBE9AD")
          (active-color "#EEEEEE")
          (passive-color "#AAAAAA")
          (subtle-color "white smoke")
          (error-color "#F93232")
          (border-color "#a5a5a5"))
      (custom-theme-set-faces
       'languid
       ;; Basics
       '(default ((t (:background "grey99" :foreground "black"))))
       '(blue ((t (:foreground "blue"))))
       '(bold ((t (:bold t))))
       '(bold-italic ((t (:italic t :bold t))))
       '(border-glyph ((t (nil))))
       '(green ((t (:foreground "green"))))
       '(info-node ((t (:italic t :bold t))))
       '(info-xref ((t (:bold t))))
       '(italic ((t (:italic t))))
       '(left-margin ((t (nil))))
       '(pointer ((t (nil))))
       '(red ((t (:foreground "red"))))
       '(right-margin ((t (nil))))
       '(underline ((t (:underline t))))
       '(yellow ((t (:foreground "yellow"))))

       ;; Frame
       '(fringe ((t (:background "grey99"))))
       '(mode-line ((t (:background "#e8e8e8" :foreground "black" :box (:line-width 4 :style flat-button)))))
       '(mode-line-highlight ((t (:box (:line-width 2 :color "#9599B0")))))
       '(mode-line-inactive ((t (:background "#e8e8e8" :foreground "grey20" :box (:line-width 4 :style flat-button)))))

       ;; Parens
       `(show-paren-match ((t (:background ,passive-color))))
       `(show-paren-mismatch ((t (:foreground "#F9F2CE" :background ,error-color))))

       ;; Highlighting
       `(hl-line ((t (:background ,active-color))))
       `(highline-face ((t (:background ,active-color))))
       `(highlight ((t (:background ,highlight-color))))
       `(highlight-symbol-face ((t (:background ,secondary-color))))
       `(isearch ((t (:background ,highlight-color))))
       `(lazy-highlight ((t (:background ,secondary-color))))
       `(primary-selection ((t (:background ,selection-color))))
       `(region ((t (:background ,selection-color :extend nil))))
       `(secondary-selection ((t (:background ,secondary-color :extend nil))))
       `(shadow ((t (:foreground "grey50" :background ,subtle-color))))
       `(text-cursor ((t (:background "black" :foreground ,passive-color))))
       `(zmacs-region ((t (:background ,selection-color))))

       ;; More
       '(mumamo-background-chunk-submode ((t (:background "#EAEBE6"))))

       ;; Font-lock
       '(font-lock-builtin-face ((t (:foreground "#626FC9"))))
       '(font-lock-comment-face ((t (:foreground "#7F7F7F"))))
       '(font-lock-constant-face ((t (:foreground "#7653C1" :background "#F3F2FF"))))
       '(font-lock-doc-string-face ((t (:foreground "#1A93AE" :background "#F4F9FE"))))
       '(font-lock-function-name-face ((t (:foreground "#4E279A"))))
       '(font-lock-keyword-face ((t (:foreground "#6700B9"))))
       '(font-lock-preprocessor-face ((t (:foreground "#434343"))))
       '(font-lock-reference-face ((t (:foreground "#4E279A" :background "#F3F2FF"))))
       '(font-lock-string-face ((t (:foreground "#BC670F" :background "#FDFBF5"))))
       '(font-lock-type-face ((t (:foreground "#699D36"))))
       '(font-lock-variable-name-face ((t (:foreground "#7B8C4D"))))
       '(font-lock-warning-face ((t (:foreground "#F93232"))))

       ;; Diff Mode
       '(diff-file-header ((t (:bold t :inherit diff-header))))
       '(diff-header ((t (:background "#DDDDFF" :foreground "grey20"))))
       '(diff-added ((t (:background "#DDFFDD"))))
       '(diff-removed ((t (:background "#FFDDDD"))))
       '(diff-changed ((t (:background "#FFFFDD"))))
       '(diff-refine-change ((t (:background "#DDDDFF"))))

       ;; Magit
       '(magit-diff-file-header ((t (:bold t :inherit diff-header))))
       '(magit-diff-hunk-header ((t (:inherit diff-header))))
       '(magit-diff-add ((t (:inherit diff-added :foreground "grey20"))))
       '(magit-diff-del ((t (:inherit diff-removed :foreground "grey20"))))
       '(magit-diff-none ((t (:inherit diff-context :foreground "grey20"))))
       '(magit-item-highlight ((t (:background nil :foreground "black"))))

       ;; Custom
       '(ansi-color-bright-green ((t (:bold t :foreground "green2"))))
       '(avy-lead-face ((t (:inherit secondary-selection :weight bold))))
       '(avy-lead-face-0 ((t (:inherit secondary-selection :weight bold))))
       '(avy-lead-face-1 ((t (:inherit secondary-selection :weight bold))))
       '(avy-lead-face-2 ((t (:inherit secondary-selection :weight bold))))
       '(completions-highlight ((t (:inherit highlight :extend t))))
       '(diff-hl-insert ((t (:foreground "lime green"))))
       '(diff-hl-delete ((t (:foreground "orange red"))))
       '(diff-hl-change ((t (:foreground "deep sky blue"))))
       '(icomplete-selected-match ((t (:inherit highlight :extend t))))
       '(line-number ((t (:foreground "grey50"))))
       '(line-number-current-line ((t (:inherit hl-line :foreground "black"))))
       '(vertical-border ((t (:foreground "grey"))))
       `(solaire-default-face ((t (:inherit default :background ,subtle-color))))
       '(meow-beacon-indicator
         ((t (:background "black" :foreground "medium spring green" :inverse-video t))))
       `(meow-normal-indicator
         ((t (:background "black" :foreground ,selection-color :inverse-video t))))
       `(meow-motion-indicator
         ((t (:background "black" :foreground ,selection-color :inverse-video t))))
       `(meow-keypad-indicator
         ((t (:background "black" :foreground ,selection-color :inverse-video t))))
       `(meow-insert-indicator
         ((t (:background "black" :foreground ,highlight-color :inverse-video t))))
       `(tab-bar ((t (:background "#E8E8E8"))))
       `(tab-bar-tab ((t (:background "grey99" :foreground "black"))))
       `(tab-bar-tab-inactive
         ((t (:background "#E8E8E8" :foreground "grey20" :box ,`(:line-width (1 . -1) :color "#E8E8E8")))))
       ))
  ;;; dark
  (let ((languid-bg      "#1b222d"); "#1b222d" "#181818")  ;; 1B212D
        (languid-fg      "#f8f8f2"); "#ffffff" "brightwhite")
        (languid-current "#282a36"); "#303030" "brightblack")
        (languid-comment "#6272a4"); "#5f5faf" "blue")
        (languid-cyan    "#82aaff"); "#87d7ff" "brightcyan")
        (languid-green   "#8fc8bb"); "#5fff87" "green")
        (languid-orange  "#ffd484"); "#ffd484" "brightred")
        (languid-pink    "#ff79c6"); "#ff87d7" "magenta")
        (languid-purple  "#A986C5"); "#af87ff" "brightmagenta")
        (languid-red     "#FB7385"); "#ff8787" "red")
        (languid-yellow  "#ffd484"); "#ffd484" "brightyellow")
        (languid-hl      "#74829b"); "#98a8c5" "grey")
        ;; Other colors
        (links           "#80cbc4"); "#87d7ff" "brightblue")
        (bg-alt          "#1E2738"); "#1e1e1e" "brightblack")
        (bg2             "#2b2a3e"); "#121212" "brightblack")
        (bg3             "#39465e"); "#262626" "brightblack")
        (bg4             "#1E2633"); "#444444" "brightblack")
        (fg2             "#e2e2dc"); "#e4e4e4" "brightwhite")
        (fg3             "#ccccc7"); "#c6c6c6" "white")
        (fg4             "#b6b6b2"); "#b2b2b2" "white")
        ;; (dark-red        "#db4b4b" "#870000" "red") ; 40% darker
        ;; (dark-green      "#41a6b5" "#00af00" "green") ; 40% darker
        (light-blue      "#98a8c5"); "#98a8c5" "brightblue") ; string
        (other-blue      "#0189cc")); "#0087ff" "brightblue"))
    (custom-theme-set-faces
     'languid
     `(default ((t (:background ,languid-bg :foreground ,languid-fg))))
     `(default-italic ((t (:slant italic))))
     `(error ((t (:foreground ,languid-red))))
     `(ffap ((t (:foreground ,fg4))))
     `(fringe ((t (:background ,languid-bg :foreground ,fg4))))
     `(header-line ((t (:background ,languid-bg))))
     `(highlight ((t (:foreground ,fg2 :background ,bg3))))
     `(hl-line ((t (:inherit secondary-selection :extend t))))
     `(info-quoted-name ((t (:foreground ,languid-orange))))
     `(info-title-1 ((t (:height 1.3 :inherit variable-pitch))))
     `(info-title-2 ((t (:height 1.2 :inherit variable-pitch))))
     `(info-title-3 ((t (:height 1.1 :inherit variable-pitch))))
     `(info-title-4 ((t (:height 1.0 :inherit variable-pitch))))
     `(info-glossary-word ((t (:box nil :italic t :foreground ,languid-fg))))
     `(info-reference-item ((t (:background ,languid-bg))))
     `(info-isolated-quote ((t (:background ,languid-bg))))
     `(info-isolated-backquote ((t (:background ,languid-bg))))
     `(info-file ((t (:foreground ,languid-yellow))))
     `(info-menu ((t (:foreground ,languid-yellow))))
     `(info-macro-ref-item ((t (:foreground ,languid-yellow))))
     `(info-double-quoted-name ((t (:foreground ,languid-yellow))))
     `(info-string ((t (:foreground ,languid-yellow))))
     `(info-menu-star ((t (:foreground ,languid-red))))
     `(info-indented-text ((t (:foreground ,other-blue))))
     `(info-user-option-ref-item ((t (:foreground ,languid-red))))
     `(info-special-form-ref-item ((t (:foreground ,languid-green))))
     `(fixed-pitch ((t (:inherit default))))
     `(lazy-highlight ((t (:foreground ,fg2 :background ,bg2))))
     `(link ((t (:foreground ,links :underline t))))
     `(linum ((t (:slant italic :foreground ,bg4 :background ,languid-bg))))
     `(line-number ((t (:foreground ,languid-comment :background,languid-bg))))
     `(line-number-current-line ((t (:inherit hl-line :foreground ,languid-yellow))))
     `(match ((t (:background ,bg3))))
     `(menu ((t (:background ,languid-current :inverse-video nil :foreground ,languid-fg))))
     `(minibuffer-prompt ((t (:weight bold :foreground ,languid-yellow))))
     `(read-multiple-choice-face ((t (:inherit completions-first-difference))))
     `(region ((t (:inherit match :extend nil))))
     `(secondary-selection ((t (:background "#283243" :extend nil))))
     `(shadow ((t (:foreground ,languid-comment))))
     `(success ((t (:foreground ,languid-green))))
     `(tooltip ((t (:foreground ,languid-fg :background ,bg-alt))))
     `(trailing-whitespace ((t (:background ,languid-orange))))
     `(vertical-border ((t (:foreground ,bg3))))
     `(warning ((t (:foreground ,languid-orange))))
     `(font-lock-builtin-face ((t (:foreground ,languid-green :slant italic))))
     `(font-lock-comment-face ((t (:inherit shadow :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:inherit shadow))))
     `(font-lock-constant-face ((t (:foreground ,languid-purple))))
     `(font-lock-doc-face ((t (:foreground ,languid-comment))))
     `(font-lock-function-name-face ((t (:foreground ,languid-green :weight bold))))
     `(font-lock-keyword-face ((t (:foreground ,languid-yellow :weight bold))))
     `(font-lock-negation-char-face ((t (:foreground ,languid-cyan))))
     `(font-lock-preprocessor-face ((t (:foreground ,languid-orange))))
     `(font-lock-reference-face ((t (:inherit font-lock-constant-face) )))
     `(font-lock-regexp-grouping-backslash ((t (:foreground ,languid-cyan))))
     `(font-lock-regexp-grouping-construct ((t (:foreground ,languid-purple))))
     `(font-lock-string-face ((t (:foreground ,light-blue))))
     `(font-lock-type-face ((t (:foreground ,languid-yellow :slant italic))))
     `(font-lock-variable-name-face ((t (:foreground ,languid-purple :weight bold))))
     `(font-lock-warning-face ((t (:inherit warning :background ,bg2))))
     `(ansi-color-black ((t (:foreground ,languid-bg :background ,languid-bg))))
     `(ansi-color-bright-black ((t (:foreground "black" :background "black"))))
     `(ansi-color-blue ((t (:foreground ,languid-purple :background ,languid-purple))))
     `(ansi-color-bright-blue ((t (:foreground ,languid-purple :background ,languid-purple :weight bold))))
     `(ansi-color-cyan ((t (:foreground ,languid-cyan :background ,languid-cyan))))
     `(ansi-color-bright-cyan ((t (:foreground ,languid-cyan :background ,languid-cyan :weight bold))))
     `(ansi-color-green ((t (:foreground ,languid-green :background ,languid-green))))
     `(ansi-color-bright-green ((t (:foreground ,languid-green :background ,languid-green :weight bold))))
     `(ansi-color-magenta ((t (:foreground ,languid-pink :background ,languid-pink))))
     `(ansi-color-bright-magenta ((t (:foreground ,languid-pink :background ,languid-pink :weight bold))))
     `(ansi-color-red ((t (:foreground ,languid-red :background ,languid-red))))
     `(ansi-color-bright-red ((t (:foreground ,languid-red :background ,languid-red :weight bold))))
     `(ansi-color-white ((t (:foreground ,languid-fg :background ,languid-fg))))
     `(ansi-color-bright-white ((t (:foreground "white" :background "white"))))
     `(ansi-color-yellow ((t (:foreground ,languid-yellow :background ,languid-yellow))))
     `(ansi-color-bright-yellow ((t (:foreground ,languid-yellow :background ,languid-yellow :weight bold))))
     `(avy-lead-face   ((t (:background ,languid-yellow :foreground "black" :weight bold :height 0.95))))
     `(avy-lead-face-0 ((t (:background ,languid-yellow :foreground "black" :weight bold :height 0.95))))
     `(avy-lead-face-1 ((t (:background ,languid-yellow :foreground "black" :weight bold :height 0.95))))
     `(avy-lead-face-2 ((t (:background ,languid-yellow :foreground "black" :weight bold :height 0.95))))
     `(completions-annotations ((t (:inherit font-lock-comment-face))))
     `(completions-common-part ((t (:foreground ,languid-yellow))))
     `(completions-highlight ((t (:inherit highlight :extend t))))
     `(completions-first-difference ((t (:foreground ,languid-cyan :weight bold))))
     `(corfu-default ((t (:inherit tooltip :background ,bg-alt))))
     `(corfu-current ((t (:background ,bg3 :foreground ,languid-fg))))
     `(diff-hl-change ((t (:foreground ,languid-cyan))))
     `(diff-hl-delete ((t (:foreground ,languid-red))))
     `(diff-hl-insert ((t (:foreground "medium spring green"))))
     `(dired-directory ((t (:foreground ,languid-green :weight normal))))
     `(dired-flagged ((t (:foreground ,languid-pink))))
     `(dired-header ((t (:foreground ,languid-yellow :weight bold))))
     `(dired-ignored ((t (:inherit shadow))))
     `(dired-mark ((t (:foreground ,languid-fg :weight bold))))
     `(dired-marked ((t (:foreground ,languid-orange :weight bold))))
     `(dired-perm-write ((t (:foreground ,fg3 :underline t))))
     `(dired-symlink ((t (:foreground ,languid-yellow :weight normal :slant italic))))
     `(dired-broken-symlink ((t (:foreground ,languid-red :weight bold :slant italic))))
     `(dired-warning ((t (:foreground ,languid-orange :underline t))))
     `(eldoc-box-border ((t (:background ,languid-comment))))
     `(elfeed-search-date-face ((t (:foreground ,languid-comment))))
     `(elfeed-search-title-face ((t (:foreground ,languid-fg))))
     `(elfeed-search-unread-title-face ((t (:foreground ,languid-pink :weight bold))))
     `(elfeed-search-feed-face ((t (:foreground ,languid-fg :weight bold))))
     `(elfeed-search-tag-face ((t (:foreground ,languid-green))))
     `(elfeed-search-last-update-face ((t (:weight bold))))
     `(elfeed-search-unread-count-face ((t (:foreground ,languid-pink))))
     `(elfeed-search-filter-face ((t (:foreground ,languid-green :weight bold))))
     `(elfeed-log-error-level-face ((t (:foreground ,languid-red))))
     `(elfeed-log-warn-level-face ((t (:foreground ,languid-orange))))
     `(elfeed-log-info-level-face ((t (:foreground ,languid-cyan))))
     `(elfeed-log-debug-level-face ((t (:foreground ,languid-comment))))
     `(enh-ruby-heredoc-delimiter-face ((t (:foreground ,languid-yellow))))
     `(enh-ruby-op-face ((t (:foreground ,languid-pink))))
     `(enh-ruby-regexp-delimiter-face ((t (:foreground ,languid-yellow))))
     `(enh-ruby-string-delimiter-face ((t (:foreground ,languid-yellow))))
     `(eww-valid-certificate ((t (:foreground ,languid-green))))
     `(flymake-error ((t (:underline ,`(:style line :color ,languid-red :position -1)))))
     `(flymake-warning ((t (:underline ,`(:style line :color ,languid-yellow :position -1)))))
     `(flymake-note ((t (:underline ,`(:style line :color ,languid-green :position -1)))))
     `(flyspell-duplicate ((t (:underline `(:style wave :color ,languid-orange)))))
     `(flyspell-incorrect ((t (:underline `(:style wave :color ,languid-red)))))
     `(font-latex-bold-face ((t (:foreground ,languid-purple))))
     `(font-latex-italic-face ((t (:foreground ,languid-pink :slant italic))))
     `(font-latex-match-reference-keywords ((t (:foreground ,languid-cyan))))
     `(font-latex-match-variable-keywords ((t (:foreground ,languid-fg))))
     `(font-latex-string-face ((t (:foreground ,languid-yellow))))
     `(go-test--ok-face ((t (:inherit success))))
     `(go-test--error-face ((t (:inherit error))))
     `(go-test--warning-face ((t (:inherit warning))))
     `(go-test--pointer-face ((t (:foreground ,languid-pink))))
     `(go-test--standard-face ((t (:foreground ,languid-cyan))))
     `(gnus-group-mail-1 ((t (:foreground ,languid-pink :weight bold))))
     `(gnus-group-mail-1-empty ((t (:inherit gnus-group-mail-1 :weight normal))))
     `(gnus-group-mail-2 ((t (:foreground ,languid-cyan :weight bold))))
     `(gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-2 :weight normal))))
     `(gnus-group-mail-3 ((t (:foreground ,languid-comment :weight bold))))
     `(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :weight normal))))
     `(gnus-group-mail-low ((t (:foreground ,languid-current :weight bold))))
     `(gnus-group-mail-low-empty ((t (:inherit gnus-group-mail-low :weight normal))))
     `(gnus-group-news-1 ((t (:foreground ,languid-pink :weight bold))))
     `(gnus-group-news-1-empty ((t (:inherit gnus-group-news-1 :weight normal))))
     `(gnus-group-news-2 ((t (:foreground ,languid-cyan :weight bold))))
     `(gnus-group-news-2-empty ((t (:inherit gnus-group-news-2 :weight normal))))
     `(gnus-group-news-3 ((t (:foreground ,languid-comment :weight bold))))
     `(gnus-group-news-3-empty ((t (:inherit gnus-group-news-3 :weight normal))))
     `(gnus-group-news-4 ((t (:inherit gnus-group-news-low))))
     `(gnus-group-news-4-empty ((t (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-5 ((t (:inherit gnus-group-news-low))))
     `(gnus-group-news-5-empty ((t (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-6 ((t (:inherit gnus-group-news-low))))
     `(gnus-group-news-6-empty ((t (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-low ((t (:foreground ,languid-current :weight bold))))
     `(gnus-group-news-low-empty ((t (:inherit gnus-group-news-low :weight normal))))
     `(gnus-header-content ((t (:foreground ,languid-cyan))))
     `(gnus-header-from ((t (:foreground ,languid-fg))))
     `(gnus-header-name ((t (:foreground ,languid-yellow))))
     `(gnus-header-subject ((t (:foreground ,languid-red :weight bold))))
     `(gnus-summary-markup-face ((t (:foreground ,languid-cyan))))
     `(gnus-summary-high-unread ((t (:foreground ,languid-pink :weight bold))))
     `(gnus-summary-high-read ((t (:inherit gnus-summary-high-unread :weight normal))))
     `(gnus-summary-high-ancient ((t (:inherit gnus-summary-high-read))))
     `(gnus-summary-high-ticked ((t (:inherit gnus-summary-high-read :underline t))))
     `(gnus-summary-normal-unread ((t (:foreground ,other-blue :weight bold))))
     `(gnus-summary-normal-read ((t (:foreground ,languid-comment :weight normal))))
     `(gnus-summary-normal-ancient ((t (:inherit gnus-summary-normal-read :weight normal))))
     `(gnus-summary-normal-ticked ((t (:foreground ,languid-pink :weight bold))))
     `(gnus-summary-low-unread ((t (:foreground ,languid-comment :weight bold))))
     `(gnus-summary-low-read ((t (:inherit gnus-summary-low-unread :weight normal))))
     `(gnus-summary-low-ancient ((t (:inherit gnus-summary-low-read))))
     `(gnus-summary-low-ticked ((t (:inherit gnus-summary-low-read :underline t))))
     `(gnus-summary-selected ((t (:inverse-video t))))
     `(haskell-operator-face ((t (:foreground ,languid-pink))))
     `(haskell-constructor-face ((t (:foreground ,languid-purple))))
     `(highlight-indentation-face ((t (:background ,bg2))))
     ;;; howm
     `(action-lock-face ((t (:inherit help-key-binding))))
     `(howm-mode-keyword-face ((t (:background ,languid-cyan :foreground ,languid-bg))))
     `(howm-mode-ref-face ((t (:inherit org-link))))
     `(howm-reminder-today-face ((t (:background ,languid-orange :foreground ,languid-bg))))
     `(howm-view-empty-face ((t (:background ,languid-bg :foreground ,languid-bg))))
     `(howm-view-hilit-face ((t (:inherit match))))
     `(howm-view-name-face ((t (:background ,languid-cyan :foreground ,languid-bg))))
     `(icompletep-determined ((t (:foreground ,languid-orange))))
     `(ido-first-match ((t (:weight bold :foreground ,languid-pink))))
     `(icomplete-selected-match ((t (:inherit highlight :extend t))))
     `(ido-only-match ((t (:foreground ,languid-orange))))
     `(ido-subdir ((t (:foreground ,languid-yellow))))
     `(ido-virtual ((t (:foreground ,languid-cyan))))
     `(ido-incomplete-regexp ((t (:inherit font-lock-warning-face))))
     `(ido-indicator ((t (:foreground ,languid-fg :background ,languid-pink))))
     `(isearch ((t (:inherit match :weight bold))))
     `(isearch-fail ((t (:foreground ,languid-bg :background ,languid-orange))))
     `(magit-branch-local ((t (:foreground ,languid-cyan))))
     `(magit-branch-remote ((t (:foreground ,languid-green))))
     `(magit-tag ((t (:foreground ,languid-orange))))
     `(magit-section-heading ((t (:foreground ,languid-yellow :weight bold))))
     `(magit-section-highlight ((t (:background ,bg3 :extend t))))
     `(magit-diff-context-highlight ((t (:background ,bg3 :foreground ,fg3 :extend t))))
     `(magit-diff-revision-summary ((t (:foreground ,languid-orange :background ,languid-bg :weight bold))))
     `(magit-diff-revision-summary-highlight ((t (:foreground ,languid-orange :background ,bg3 :weight bold :extend t))))
     `(magit-diff-added ((t (:background "#335533" :foreground "#ddffdd" :extend t))))
     `(magit-diff-added-highlight ((t (:background "#336633" :foreground "#cceecc" :extend t))))
     `(magit-diff-removed ((t (:background "#553333" :foreground "#ffdddd" :extend t))))
     `(magit-diff-removed-highlight ((t (:background "#663333" :foreground "#eecccc" :extend t))))
     `(magit-diff-file-heading ((t (:foreground ,languid-fg))))
     `(magit-diff-file-heading-highlight ((t (:inherit magit-section-highlight))))
     `(magit-diffstat-added ((t (:foreground ,languid-green))))
     `(magit-diffstat-removed ((t (:foreground ,languid-red))))
     `(magit-hash ((t (:foreground ,fg2))))
     `(magit-hunk-heading ((t (:background ,bg3))))
     `(magit-hunk-heading-highlight ((t (:background ,bg3))))
     `(magit-item-highlight ((t (:background ,bg3))))
     `(magit-log-author ((t (:foreground ,fg3))))
     `(magit-process-ng ((t (:foreground ,languid-orange :weight bold))))
     `(magit-process-ok ((t (:foreground ,languid-green :weight bold))))
     `(markdown-blockquote-face ((t (:foreground ,languid-yellow :slant italic))))
     `(markdown-footnote-face ((t (:foreground ,other-blue))))
     `(markdown-header-face ((t (:weight normal))))
     `(markdown-header-face-1 ((t (:inherit bold :foreground ,languid-pink :height ,languid-height-title-1))))
     `(markdown-header-face-2 ((t (:inherit bold :foreground ,languid-purple :height ,languid-height-title-2))))
     `(markdown-header-face-3 ((t (:foreground ,languid-green :height ,languid-height-title-3))))
     `(markdown-header-face-4 ((t (:foreground ,languid-yellow))))
     `(markdown-header-face-5 ((t (:foreground ,languid-cyan))))
     `(markdown-header-face-6 ((t (:foreground ,languid-orange))))
     `(markdown-header-face-7 ((t (:foreground ,other-blue))))
     `(markdown-header-face-8 ((t (:foreground ,languid-fg))))
     `(markdown-inline-code-face ((t (:foreground ,languid-green))))
     `(markdown-plain-url-face ((t (:inherit link))))
     `(markdown-pre-face ((t (:foreground ,languid-orange))))
     `(markdown-table-face ((t (:foreground ,languid-purple))))
     `(markdown-list-face ((t (:foreground ,languid-cyan))))
     `(markdown-language-keyword-face ((t (:foreground ,languid-comment))))
     `(meow-normal-indicator ((t (:foreground ,languid-comment :inverse-video t))))
     `(meow-motion-indicator ((t (:foreground ,languid-comment :inverse-video t))))
     `(meow-keypad-indicator ((t (:foreground ,languid-comment :inverse-video t))))
     `(meow-beacon-indicator ((t (:foreground ,languid-green :inverse-video t))))
     `(meow-insert-indicator ((t (:foreground ,languid-yellow :inverse-video t))))
     `(message-header-to ((t (:foreground ,languid-fg :weight bold))))
     `(message-header-cc ((t (:foreground ,languid-fg :bold bold))))
     `(message-header-subject ((t (:foreground ,languid-orange))))
     `(message-header-newsgroups ((t (:foreground ,languid-purple))))
     `(message-header-other ((t (:foreground ,languid-purple))))
     `(message-header-name ((t (:foreground ,languid-green))))
     `(message-header-xheader ((t (:foreground ,languid-cyan))))
     `(message-separator ((t (:foreground ,languid-cyan :slant italic))))
     `(message-cited-text ((t (:foreground ,languid-purple))))
     `(message-cited-text-1 ((t (:foreground ,languid-purple))))
     `(message-cited-text-2 ((t (:foreground ,languid-orange))))
     `(message-cited-text-3 ((t (:foreground ,languid-comment))))
     `(message-cited-text-4 ((t (:foreground ,fg2))))
     `(message-mml ((t (:foreground ,languid-green :weight normal))))
     `(mode-line ((t (:background "#141922" :box (:line-width 4 :style flat-button) :foreground ,languid-fg)))) ;:overline ,languid-comment
     `(mode-line-inactive ((t (:background "#141922" :box (:line-width 4 :style flat-button) :foreground ,languid-comment)))) ;:overline ,bg3
     `(mini-modeline-mode-line ((t (:inherit mode-line :height 0.1 :box nil))))
     `(pulse-highlight-start-face ((t (:background ,languid-yellow))))
     `(pulse-highlight-face ((t (:background ,languid-yellow :extend t))))
     `(org-agenda-date ((t (:foreground ,languid-cyan :underline nil))))
     `(org-agenda-dimmed-todo-face ((t (:foreground ,languid-comment))))
     `(org-agenda-done ((t (:foreground ,languid-green))))
     `(org-agenda-structure ((t (:foreground ,languid-purple))))
     `(org-block ((t (:foreground ,languid-yellow :background ,bg-alt :extend t))))
     `(org-block-begin-line ((t (:foreground ,languid-comment :background ,bg-alt :extend t))))
     `(org-block-end-line ((t (:foreground ,languid-comment :background ,bg-alt :extend t))))
     `(org-code ((t (:foreground ,languid-green))))
     `(org-column ((t (:background ,bg4))))
     `(org-column-title ((t (:inherit org-column :weight bold :underline t))))
     `(org-date ((t (:foreground ,languid-cyan :underline t))))
     `(org-document-info ((t (:foreground ,other-blue))))
     `(org-document-info-keyword ((t (:foreground ,languid-comment))))
     `(org-document-title ((t (:weight bold :foreground ,languid-orange :height ,languid-height-doc-title))))
     `(org-done ((t (:foreground ,languid-green))))
     `(org-ellipsis ((t (:foreground ,languid-comment))))
     `(org-footnote ((t (:foreground ,other-blue))))
     `(org-formula ((t (:foreground ,languid-pink))))
     `(org-headline-done ((t (:foreground ,languid-comment :weight normal :strike-through t))))
     `(org-hide ((t (:foreground ,languid-bg :background ,languid-bg))))
     `(org-level-1 ((t (:inherit bold :foreground ,languid-red :height ,languid-height-title-1))))
     `(org-level-2 ((t (:inherit bold :foreground ,languid-purple :height ,languid-height-title-2))))
     `(org-level-3 ((t (:weight normal :foreground ,languid-pink :weight normal :foreground ,languid-yellow))))
     `(org-level-5 ((t (:weight normal :foreground ,languid-cyan))))
     `(org-level-6 ((t (:weight normal :foreground ,languid-orange))))
     `(org-level-7 ((t (:weight normal :foreground ,other-blue))))
     `(org-level-8 ((t (:weight normal :foreground ,languid-fg))))
     `(org-link ((t (:foreground ,languid-cyan :underline t))))
     `(org-priority ((t (:foreground ,languid-cyan))))
     `(org-quote ((t (:foreground ,languid-yellow :slant italic))))
     `(org-scheduled ((t (:foreground ,languid-green))))
     `(org-scheduled-previously ((t (:foreground ,languid-yellow))))
     `(org-scheduled-today ((t (:foreground ,languid-green))))
     `(org-sexp-date ((t (:foreground ,fg4))))
     `(org-special-keyword ((t (:foreground ,languid-yellow))))
     `(org-table ((t (:foreground ,languid-purple))))
     `(org-tag ((t (:foreground ,languid-pink :weight bold :background ,bg2))))
     `(org-todo ((t (:foreground ,languid-orange :weight bold :background ,bg2))))
     `(org-upcoming-deadline ((t (:foreground ,languid-yellow))))
     `(org-verbatim ((t (:inherit org-quote))))
     `(org-warning ((t (:weight bold :foreground ,languid-pink))))
     `(outline-1 ((t (:foreground ,languid-pink))))
     `(outline-2 ((t (:foreground ,languid-purple))))
     `(outline-3 ((t (:foreground ,languid-green))))
     `(outline-4 ((t (:foreground ,languid-yellow))))
     `(outline-5 ((t (:foreground ,languid-cyan))))
     `(outline-6 ((t (:foreground ,languid-orange))))
     `(pabbrev-suggestions-face ((t (:foreground ,languid-comment :underline t))))
     `(pabbrev-single-suggestion-face ((t (:foreground ,languid-comment :slant italic))))
     `(pabbrev-suggestions-label-face ((t (:underline t :weight bold))))
     `(persp-selected-face ((t (:weight bold :foreground ,languid-pink))))
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,languid-fg))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,languid-cyan))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,languid-purple))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,languid-pink))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,languid-orange))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,languid-green))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,languid-yellow))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,other-blue))))
     `(rainbow-delimiters-unmatched-face ((t (:foreground ,languid-orange))))
     `(rst-level-1 ((t (:foreground ,languid-pink :weight bold))))
     `(rst-level-2 ((t (:foreground ,languid-purple :weight bold))))
     `(rst-level-3 ((t (:foreground ,languid-green))))
     `(rst-level-4 ((t (:foreground ,languid-yellow))))
     `(rst-level-5 ((t (:foreground ,languid-cyan))))
     `(rst-level-6 ((t (:foreground ,languid-orange))))
     `(rst-level-7 ((t (:foreground ,other-blue))))
     `(rst-level-8 ((t (:foreground ,languid-fg))))
     `(show-paren-match-face ((t (:background unspecified :foreground ,languid-cyan :weight bold))))
     `(show-paren-match ((t (:background unspecified :foreground ,languid-cyan :weight bold))))
     `(show-paren-match-expression ((t (:inherit match))))
     `(show-paren-mismatch ((t (:inherit font-lock-warning-face))))
     `(slime-repl-inputed-output-face ((t (:foreground ,languid-purple))))
     `(solaire-default-face ((t (:inherit default :background "#171d26"))))
     `(solaire-fringe-face ((t (:inherit fringe :background "#171d26"))))
     ;; `(solaire-line-number-face ((t (:inherit line-number))))
     ;; `(solaire-hl-line-face ((t (:inherit hl-line))))
     ;; `(solaire-org-hide-face ((t (:inherit org-hide org-indent))))
     ;; `(solaire-mode-line-face ((t (:inherit mode-line))))
     ;; `(solaire-mode-line-inactive-face((t (:inherit mode-line-inactive))))
     `(solaire-header-line-face ((t (:inherit header-line :background "#171d26"))))
     `(spam ((t (:inherit gnus-summary-normal-read :foreground ,languid-orange :strike-through t :slant oblique))))
     `(tab-bar ((t (:foreground ,languid-yellow :background "#141922"))))
     `(tab-bar-tab ((t (:foreground ,languid-yellow :background ,languid-bg))))
     `(tab-bar-tab-inactive ((t (:foreground ,languid-comment :background "#141922" :box ,``(:line-width `(1 . -1) :color "black")))))
     `(term ((t (:foreground ,languid-fg :background ,languid-bg))))
     `(term-color-black ((t (:foreground ,languid-bg :background ,languid-comment))))
     `(term-color-blue ((t (:foreground ,languid-purple :background ,languid-purple))))
     `(term-color-cyan ((t (:foreground ,languid-cyan :background ,languid-cyan))))
     `(term-color-green ((t (:foreground ,languid-green :background ,languid-green))))
     `(term-color-magenta ((t (:foreground ,languid-pink :background ,languid-pink))))
     `(term-color-red ((t (:foreground ,languid-red :background ,languid-red))))
     `(term-color-white ((t (:foreground ,languid-fg :background ,languid-fg))))
     `(term-color-yellow ((t (:foreground ,languid-yellow :background ,languid-yellow))))
     `(tree-sitter-hl-face:attribute ((t (:inherit font-lock-constant-face))))
     `(tree-sitter-hl-face:comment ((t (:inherit font-lock-comment-face))))
     `(tree-sitter-hl-face:constant ((t (:inherit font-lock-constant-face))))
     `(tree-sitter-hl-face:constant.builtin ((t (:inherit font-lock-builtin-face))))
     `(tree-sitter-hl-face:constructor ((t (:inherit font-lock-constant-face))))
     `(tree-sitter-hl-face:escape ((t (:foreground ,languid-pink))))
     `(tree-sitter-hl-face:function ((t (:inherit font-lock-function-name-face))))
     `(tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-builtin-face))))
     `(tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face :weight normal))))
     `(tree-sitter-hl-face:function.macro ((t (:inherit font-lock-preprocessor-face))))
     `(tree-sitter-hl-face:function.special ((t (:inherit font-lock-preprocessor-face))))
     `(tree-sitter-hl-face:keyword ((t (:inherit font-lock-keyword-face))))
     `(tree-sitter-hl-face:punctuation ((t (:foreground ,languid-pink))))
     `(tree-sitter-hl-face:punctuation.bracket ((t (:foreground ,languid-fg))))
     `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,languid-fg))))
     `(tree-sitter-hl-face:punctuation.special ((t (:foreground ,languid-pink))))
     `(tree-sitter-hl-face:string ((t (:inherit font-lock-string-face))))
     `(tree-sitter-hl-face:string.special ((t (:foreground ,languid-red))))
     `(tree-sitter-hl-face:tag ((t (:inherit font-lock-keyword-face))))
     `(tree-sitter-hl-face:type ((t (:inherit font-lock-type-face))))
     `(tree-sitter-hl-face:type.parameter ((t (:foreground ,languid-pink))))
     `(tree-sitter-hl-face:variable ((t (:inherit font-lock-variable-name-face))))
     `(tree-sitter-hl-face:variable.parameter ((t (:inherit tree-sitter-hl-face:variable :weight normal))))
     `(web-mode-builtin-face ((t (:inherit font-lock-builtin-face))))
     `(web-mode-comment-face ((t (:inherit font-lock-comment-face))))
     `(web-mode-constant-face ((t (:inherit font-lock-constant-face))))
     `(web-mode-css-property-name-face ((t (:inherit font-lock-constant-face))))
     `(web-mode-doctype-face ((t (:inherit font-lock-comment-face))))
     `(web-mode-function-name-face ((t (:inherit font-lock-function-name-face))))
     `(web-mode-html-attr-name-face ((t (:foreground ,languid-purple))))
     `(web-mode-html-attr-value-face ((t (:foreground ,languid-green))))
     `(web-mode-html-tag-face ((t (:foreground ,languid-pink :weight bold))))
     `(web-mode-keyword-face ((t (:foreground ,languid-pink))))
     `(web-mode-string-face ((t (:foreground ,languid-yellow))))
     `(web-mode-type-face ((t (:inherit font-lock-type-face))))
     `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
     `(which-func ((t (:inherit font-lock-function-name-face))))
     `(which-key-key-face ((t (:inherit font-lock-builtin-face))))
     `(which-key-command-description-face ((t (:inherit default))))
     `(which-key-separator-face ((t (:inherit font-lock-comment-delimiter-face))))
     `(which-key-local-map-description-face ((t (:foreground ,languid-green))))
     `(whitespace-big-indent ((t (:background ,languid-red :foreground ,languid-red))))
     `(whitespace-empty ((t (:background ,languid-orange :foreground ,languid-red))))
     `(whitespace-hspace ((t (:background ,bg3 :foreground ,languid-comment))))
     `(whitespace-indentation ((t (:background ,languid-orange :foreground ,languid-red))))
     `(whitespace-line ((t (:background ,languid-bg :foreground ,languid-pink))))
     `(whitespace-newline ((t (:foreground ,languid-comment))))
     `(whitespace-space ((t (:background ,languid-bg :foreground ,languid-comment))))
     `(whitespace-space-after-tab ((t (:background ,languid-orange :foreground ,languid-red))))
     `(whitespace-space-before-tab ((t (:background ,languid-orange :foreground ,languid-red))))
     `(whitespace-tab ((t (:background ,bg2 :foreground ,languid-comment))))
     `(whitespace-trailing ((t (:inherit trailing-whitespace))))
     
     )))

;; mode-line
(setq global-mode-string nil
              mode-line-end-spaces
              `("%n " mode-line-misc-info
                (:eval (when (bound-and-true-p flymake-mode)
                         (flymake--mode-line-counters)))
                " Ln %l"))
(defun my/ml-padding ()
  "Adding padding to the modeline so that spme elements can be right aligned."
  (let ((r-length (length (format-mode-line mode-line-end-spaces))))
    (propertize " "
                'display `(space :align-to (- right ,r-length)))))

(setq-default mode-line-format
              '((:eval (when (and (bound-and-true-p meow-global-mode)
                                  (mode-line-window-selected-p))
                         (let* ((state (meow--current-state))
                                (indicator-face (alist-get state meow-indicator-face-alist))
                                (buffer-state (cond (buffer-read-only
                                                     " RO ")
                                                    ((buffer-modified-p)
                                                     " ** ")
                                                    ((file-remote-p default-directory)
                                                     (concat " "
                                                             (file-remote-p
                                                              default-directory 'host)
                                                             " "))
                                                    (t
                                                     " %p "))))
                           (propertize buffer-state
                                       'face `(,indicator-face
                                               :inverse-video t
                                               :box (:style flat-button))))))
                "%e "
                (:eval (nerd-icons-icon-for-buffer))
                (:eval (propertize " %b " 'help-echo (buffer-file-name)))
                "("
                (:eval mode-name)
                (:eval vc-mode) ;; vc-display-status 'no-backend
                ")"
                (:eval (if (string> emacs-version "29.2")
                           mode-line-format-right-align
                         (my/ml-padding)))
                (:eval (when (mode-line-window-selected-p)
                         mode-line-end-spaces))))

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|HACK\\|TODO\\|BUG\\|DONE\\)"
                1 font-lock-warning-face t)))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'languid)

;; tabs
(with-eval-after-load 'tab-bar
  (defun clean-tab-name (tab i)
    (let* ((name (alist-get 'name tab))
           (total-width (frame-width))
           (tabs (delq (tab-bar--current-tab) (tab-bar-tabs)))
           (tab-bar-width (apply '+ (mapcar (lambda (tab) (length (alist-get 'name tab))) tabs)))
           (padding (/ (- total-width tab-bar-width) (length tabs)))
           (padstr (if (> padding 0)
                       (make-string (/ padding 2) ?\s)
                     "")))
      (if load-theme-light
          (if (eq (car tab) 'current-tab)
              (propertize (concat "│" padstr name padstr)
                          'face `(:background "grey99" :foreground "black" :slant italic :box ,`(:line-width (2 . 2) :color "grey99")))
            (propertize (concat padstr name padstr)
                        'face `(:background "#E8E8E8" :foreground "grey20" :box ,`(:line-width (2 . 2) :color "#E8E8E8"))))
        (if (eq (car tab) 'current-tab)
            (propertize (concat "│" padstr name padstr)
                        'face `(:background "#1b222d" :slant italic :foreground "#ffd484"
                                            :box ,`(:line-width 2 :style flat-button)))
          (propertize (concat padstr name padstr)
                      'face `(:background "#141922" :foreground "#6272a4"
                                          :box ,`(:line-width (-4 . 0) :color "white")))))))

  (advice-add 'tab-bar-tab-name-format-default :override #'clean-tab-name)

  (setq tab-bar-close-button-show nil
        tab-bar-auto-width nil
        tab-bar-new-button-show nil
        tab-bar-show 1
        tab-bar-separator ""))
;; tab-bar-tab-name-function 'tab-bar-tab-name-truncated
;; tab-bar-tab-name-truncated-max 12))

;;;###autoload
(defun languid-toggle-theme ()
  "Toggle between dark and light mode."
  (interactive)
  (if load-theme-light
      (setq load-theme-light nil)
    (setq load-theme-light t))
  (load-theme 'languid :no-confirm))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'languid)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; languid-theme.el ends here

