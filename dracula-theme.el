;;; dracula-theme.el --- Dracula Theme -*- lexical-binding: t; -*-

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Maintainer: Étienne Deparis <etienne@depar.is>
;; Author: film42
;; Version: 1.7.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/dracula/emacs

;;; Commentary:

;; This is a personal touch on the theme with bits from palenight.

;; A dark color theme available for a number of editors.
;; This theme tries as much as possible to follow the consensual
;; specification (see URL `https://spec.draculatheme.com/').

;;; Code:
(deftheme dracula)


;;;; Configuration options:

(defgroup dracula nil
  "Dracula theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom dracula-enlarge-headings nil
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'dracula)

(defcustom dracula-height-title-1 1.0
  "Font size 100%."
  :type 'number
  :group 'dracula)

(defcustom dracula-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'dracula)

(defcustom dracula-height-title-3 1.3
  "Font size 130%."
  :type 'number
  :group 'dracula)

(defcustom dracula-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'dracula)

(defcustom dracula-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'dracula)

;; (defun dired-vc-left()
;;   (interactive)
;;   (let ((dir (if (eq (vc-root-dir) nil)
;;                  (dired-noselect default-directory)
;;                (dired-noselect (vc-root-dir)))))
;;     (display-buffer-in-side-window
;;      dir `((side . left)
;;            (slot . 0)
;;            (window-width . 0.2)
;;            (window-parameters . ((mode-line-format . (" %b"))))))
;;     (windmove-left)))

(defvar dracula-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Dracula theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))

There is a lot of discussion behind the 256 colors theme (see URL
`https://github.com/dracula/emacs/pull/57').  Please take time to
read it before opening a new issue about your will.")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]

(let ((dracula-bg      "#1b222d"); "#1b222d" "#181818")  ;; 1B212D
      (dracula-fg      "#f8f8f2"); "#ffffff" "brightwhite")
      (dracula-current "#282a36"); "#303030" "brightblack")
      (dracula-comment "#6272a4"); "#5f5faf" "blue")
      (dracula-cyan    "#82aaff"); "#87d7ff" "brightcyan")
      (dracula-green   "#8fc8bb"); "#5fff87" "green")
      (dracula-orange  "#ffd484"); "#ffd484" "brightred")
      (dracula-pink    "#ff79c6"); "#ff87d7" "magenta")
      (dracula-purple  "#A986C5"); "#af87ff" "brightmagenta")
      (dracula-red     "#FB7385"); "#ff8787" "red")
      (dracula-yellow  "#ffd484"); "#ffd484" "brightyellow")
      (dracula-hl      "#98a8c5"); "#98a8c5" "grey")
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
   'dracula
   `(default ((t (:background ,dracula-bg :foreground ,dracula-fg))))
   `(default-italic ((t (:slant italic))))
   `(error ((t (:foreground ,dracula-red))))
   `(ffap ((t (:foreground ,fg4))))
   `(fringe ((t (:background ,dracula-bg :foreground ,fg4))))
   `(header-line ((t (:background ,dracula-bg))))
   `(highlight ((t (:foreground ,fg2 :background ,bg3))))
   `(hl-line ((t (:background ,bg3 :extend t))))
   `(info-quoted-name ((t (:foreground ,dracula-orange))))
   `(info-title-1 ((t (:height 1.3 :inherit variable-pitch))))
   `(info-title-2 ((t (:height 1.2 :inherit variable-pitch))))
   `(info-title-3 ((t (:height 1.1 :inherit variable-pitch))))
   `(info-title-4 ((t (:height 1.0 :inherit variable-pitch))))
   `(info-glossary-word ((t (:box nil :italic t :foreground ,dracula-fg))))
   `(info-reference-item ((t (:background ,dracula-bg))))
   `(info-isolated-quote ((t (:background ,dracula-bg))))
   `(info-isolated-backquote ((t (:background ,dracula-bg))))
   `(info-file ((t (:foreground ,dracula-yellow))))
   `(info-menu ((t (:foreground ,dracula-yellow))))
   `(info-macro-ref-item ((t (:foreground ,dracula-yellow))))
   `(info-double-quoted-name ((t (:foreground ,dracula-yellow))))
   `(info-string ((t (:foreground ,dracula-yellow))))
   `(info-menu-star ((t (:foreground ,dracula-red))))
   `(info-indented-text ((t (:foreground ,other-blue))))
   `(info-user-option-ref-item ((t (:foreground ,dracula-red))))
   `(info-special-form-ref-item ((t (:foreground ,dracula-green))))
   `(fixed-pitch ((t (:inherit default))))
   `(lazy-highlight ((t (:foreground ,fg2 :background ,bg2))))
   `(link ((t (:foreground ,links :underline t))))
   `(linum ((t (:slant italic :foreground ,bg4 :background ,dracula-bg))))
   `(line-number ((t (:foreground ,dracula-comment :background ,dracula-bg))))
   `(line-number-current-line ((t (:foreground ,dracula-yellow :background ,dracula-bg))))
   `(match ((t (:background ,dracula-hl :foreground ,dracula-bg))))
   `(menu ((t (:background ,dracula-current :inverse-video nil :foreground ,dracula-fg))))
   `(minibuffer-prompt ((t (:weight bold :foreground ,dracula-yellow))))
   `(read-multiple-choice-face ((t (:inherit completions-first-difference))))
   `(region ((t (:inherit match :extend nil))))
   `(secondary-selection ((t (:background "SkyBlue4" :extend nil))))
   `(shadow ((t (:foreground ,dracula-comment))))
   `(success ((t (:foreground ,dracula-green))))
   `(tooltip ((t (:foreground ,dracula-fg :background ,bg-alt))))
   `(trailing-whitespace ((t (:background ,dracula-orange))))
   `(vertical-border ((t (:foreground ,bg3))))
   `(warning ((t (:foreground ,dracula-orange))))
   `(font-lock-builtin-face ((t (:foreground ,dracula-green :slant italic))))
   `(font-lock-comment-face ((t (:inherit shadow :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit shadow))))
   `(font-lock-constant-face ((t (:foreground ,dracula-purple))))
   `(font-lock-doc-face ((t (:foreground ,dracula-comment))))
   `(font-lock-function-name-face ((t (:foreground ,dracula-green :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,dracula-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,dracula-cyan))))
   `(font-lock-preprocessor-face ((t (:foreground ,dracula-orange))))
   `(font-lock-reference-face ((t (:inherit font-lock-constant-face) )))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,dracula-cyan))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,dracula-purple))))
   `(font-lock-string-face ((t (:foreground ,light-blue))))
   `(font-lock-type-face ((t (:foreground ,dracula-yellow :slant italic))))
   `(font-lock-variable-name-face ((t (:foreground ,dracula-purple :weight bold))))
   `(font-lock-warning-face ((t (:inherit warning :background ,bg2))))
   `(ansi-color-black ((t (:foreground ,dracula-bg :background ,dracula-bg))))
   `(ansi-color-bright-black ((t (:foreground "black" :background "black"))))
   `(ansi-color-blue ((t (:foreground ,dracula-purple :background ,dracula-purple))))
   `(ansi-color-bright-blue ((t (:foreground ,dracula-purple :background ,dracula-purple :weight bold))))
   `(ansi-color-cyan ((t (:foreground ,dracula-cyan :background ,dracula-cyan))))
   `(ansi-color-bright-cyan ((t (:foreground ,dracula-cyan :background ,dracula-cyan :weight bold))))
   `(ansi-color-green ((t (:foreground ,dracula-green :background ,dracula-green))))
   `(ansi-color-bright-green ((t (:foreground ,dracula-green :background ,dracula-green :weight bold))))
   `(ansi-color-magenta ((t (:foreground ,dracula-pink :background ,dracula-pink))))
   `(ansi-color-bright-magenta ((t (:foreground ,dracula-pink :background ,dracula-pink :weight bold))))
   `(ansi-color-red ((t (:foreground ,dracula-red :background ,dracula-red))))
   `(ansi-color-bright-red ((t (:foreground ,dracula-red :background ,dracula-red :weight bold))))
   `(ansi-color-white ((t (:foreground ,dracula-fg :background ,dracula-fg))))
   `(ansi-color-bright-white ((t (:foreground "white" :background "white"))))
   `(ansi-color-yellow ((t (:foreground ,dracula-yellow :background ,dracula-yellow))))
   `(ansi-color-bright-yellow ((t (:foreground ,dracula-yellow :background ,dracula-yellow :weight bold))))
   `(avy-lead-face ((t (:inherit secondary-selection :weight bold))))
   `(avy-lead-face-0 ((t (:inherit secondary-selection :weight bold))))
   `(avy-lead-face-1 ((t (:inherit secondary-selection :weight bold))))
   `(avy-lead-face-2 ((t (:inherit secondary-selection :weight bold))))
   `(completions-annotations ((t (:inherit font-lock-comment-face))))
   `(completions-common-part ((t (:foreground ,dracula-yellow))))
   `(completions-first-difference ((t (:foreground ,dracula-cyan :weight bold))))
   `(corfu-default ((t (:inherit tooltip :background ,bg-alt))))
   `(corfu-current ((t (:background ,bg3 :foreground ,dracula-fg))))
   `(diff-hl-change ((t (:foreground ,dracula-cyan))))
   `(diff-hl-delete ((t (:foreground ,dracula-red))))
   `(diff-hl-insert ((t (:foreground ,dracula-green))))
   `(dired-directory ((t (:foreground ,dracula-cyan :weight normal))))
   `(dired-flagged ((t (:foreground ,dracula-pink))))
   `(dired-header ((t (:foreground ,fg3 :background ,dracula-bg))))
   `(dired-ignored ((t (:inherit shadow))))
   `(dired-mark ((t (:foreground ,dracula-fg :weight bold))))
   `(dired-marked ((t (:foreground ,dracula-orange :weight bold))))
   `(dired-perm-write ((t (:foreground ,fg3 :underline t))))
   `(dired-symlink ((t (:foreground ,dracula-yellow :weight normal :slant italic))))
   `(dired-broken-symlink ((t (:foreground ,dracula-red :weight bold :slant italic))))
   `(dired-warning ((t (:foreground ,dracula-orange :underline t))))
   `(eldoc-box-border ((t (:background ,dracula-comment))))
   `(elfeed-search-date-face ((t (:foreground ,dracula-comment))))
   `(elfeed-search-title-face ((t (:foreground ,dracula-fg))))
   `(elfeed-search-unread-title-face ((t (:foreground ,dracula-pink :weight bold))))
   `(elfeed-search-feed-face ((t (:foreground ,dracula-fg :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,dracula-green))))
   `(elfeed-search-last-update-face ((t (:weight bold))))
   `(elfeed-search-unread-count-face ((t (:foreground ,dracula-pink))))
   `(elfeed-search-filter-face ((t (:foreground ,dracula-green :weight bold))))
   `(elfeed-log-error-level-face ((t (:foreground ,dracula-red))))
   `(elfeed-log-warn-level-face ((t (:foreground ,dracula-orange))))
   `(elfeed-log-info-level-face ((t (:foreground ,dracula-cyan))))
   `(elfeed-log-debug-level-face ((t (:foreground ,dracula-comment))))
   `(enh-ruby-heredoc-delimiter-face ((t (:foreground ,dracula-yellow))))
   `(enh-ruby-op-face ((t (:foreground ,dracula-pink))))
   `(enh-ruby-regexp-delimiter-face ((t (:foreground ,dracula-yellow))))
   `(enh-ruby-string-delimiter-face ((t (:foreground ,dracula-yellow))))
   `(eww-valid-certificate ((t (:foreground ,dracula-green))))
   `(flymake-error ((t (:underline `(:style line :color ,dracula-red :position -1)))))
   `(flymake-warning ((t (:underline `(:style line :color ,dracula-yellow :position -1)))))
   `(flymake-note ((t (:underline `(:style line :color ,dracula-green :position -1)))))
   `(flyspell-duplicate ((t (:underline `(:style wave :color ,dracula-orange)))))
   `(flyspell-incorrect ((t (:underline `(:style wave :color ,dracula-red)))))
   `(font-latex-bold-face ((t (:foreground ,dracula-purple))))
   `(font-latex-italic-face ((t (:foreground ,dracula-pink :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,dracula-cyan))))
   `(font-latex-match-variable-keywords ((t (:foreground ,dracula-fg))))
   `(font-latex-string-face ((t (:foreground ,dracula-yellow))))
   `(go-test--ok-face ((t (:inherit success))))
   `(go-test--error-face ((t (:inherit error))))
   `(go-test--warning-face ((t (:inherit warning))))
   `(go-test--pointer-face ((t (:foreground ,dracula-pink))))
   `(go-test--standard-face ((t (:foreground ,dracula-cyan))))
   `(gnus-group-mail-1 ((t (:foreground ,dracula-pink :weight bold))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-mail-1 :weight normal))))
   `(gnus-group-mail-2 ((t (:foreground ,dracula-cyan :weight bold))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-2 :weight normal))))
   `(gnus-group-mail-3 ((t (:foreground ,dracula-comment :weight bold))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :weight normal))))
   `(gnus-group-mail-low ((t (:foreground ,dracula-current :weight bold))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-mail-low :weight normal))))
   `(gnus-group-news-1 ((t (:foreground ,dracula-pink :weight bold))))
   `(gnus-group-news-1-empty ((t (:inherit gnus-group-news-1 :weight normal))))
   `(gnus-group-news-2 ((t (:foreground ,dracula-cyan :weight bold))))
   `(gnus-group-news-2-empty ((t (:inherit gnus-group-news-2 :weight normal))))
   `(gnus-group-news-3 ((t (:foreground ,dracula-comment :weight bold))))
   `(gnus-group-news-3-empty ((t (:inherit gnus-group-news-3 :weight normal))))
   `(gnus-group-news-4 ((t (:inherit gnus-group-news-low))))
   `(gnus-group-news-4-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-5 ((t (:inherit gnus-group-news-low))))
   `(gnus-group-news-5-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-6 ((t (:inherit gnus-group-news-low))))
   `(gnus-group-news-6-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-low ((t (:foreground ,dracula-current :weight bold))))
   `(gnus-group-news-low-empty ((t (:inherit gnus-group-news-low :weight normal))))
   `(gnus-header-content ((t (:foreground ,dracula-cyan))))
   `(gnus-header-from ((t (:foreground ,dracula-fg))))
   `(gnus-header-name ((t (:foreground ,dracula-yellow))))
   `(gnus-header-subject ((t (:foreground ,dracula-red :weight bold))))
   `(gnus-summary-markup-face ((t (:foreground ,dracula-cyan))))
   `(gnus-summary-high-unread ((t (:foreground ,dracula-pink :weight bold))))
   `(gnus-summary-high-read ((t (:inherit gnus-summary-high-unread :weight normal))))
   `(gnus-summary-high-ancient ((t (:inherit gnus-summary-high-read))))
   `(gnus-summary-high-ticked ((t (:inherit gnus-summary-high-read :underline t))))
   `(gnus-summary-normal-unread ((t (:foreground ,other-blue :weight bold))))
   `(gnus-summary-normal-read ((t (:foreground ,dracula-comment :weight normal))))
   `(gnus-summary-normal-ancient ((t (:inherit gnus-summary-normal-read :weight normal))))
   `(gnus-summary-normal-ticked ((t (:foreground ,dracula-pink :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,dracula-comment :weight bold))))
   `(gnus-summary-low-read ((t (:inherit gnus-summary-low-unread :weight normal))))
   `(gnus-summary-low-ancient ((t (:inherit gnus-summary-low-read))))
   `(gnus-summary-low-ticked ((t (:inherit gnus-summary-low-read :underline t))))
   `(gnus-summary-selected ((t (:inverse-video t))))
   `(haskell-operator-face ((t (:foreground ,dracula-pink))))
   `(haskell-constructor-face ((t (:foreground ,dracula-purple))))
   `(highlight-indentation-face ((t (:background ,bg2))))
   `(icompletep-determined ((t (:foreground ,dracula-orange))))
   `(ido-first-match ((t (:weight 'bold :foreground dracula-pink))))
   `(ido-only-match ((t (:foreground ,dracula-orange))))
   `(ido-subdir ((t (:foreground ,dracula-yellow))))
   `(ido-virtual ((t (:foreground ,dracula-cyan))))
   `(ido-incomplete-regexp ((t (:inherit font-lock-warning-face))))
   `(ido-indicator ((t (:foreground ,dracula-fg :background ,dracula-pink))))
   `(isearch ((t (:inherit match :weight bold))))
   `(isearch-fail ((t (:foreground ,dracula-bg :background ,dracula-orange))))
   `(magit-branch-local ((t (:foreground ,dracula-cyan))))
   `(magit-branch-remote ((t (:foreground ,dracula-green))))
   `(magit-tag ((t (:foreground ,dracula-orange))))
   `(magit-section-heading ((t (:foreground ,dracula-yellow :weight bold))))
   `(magit-section-highlight ((t (:background ,bg3 :extend t))))
   `(magit-diff-context-highlight ((t (:background ,bg3 :foreground ,fg3 :extend t))))
   `(magit-diff-revision-summary ((t (:foreground ,dracula-orange :background ,dracula-bg :weight bold))))
   `(magit-diff-revision-summary-highlight ((t (:foreground ,dracula-orange :background ,bg3 :weight bold :extend t))))
   `(magit-diff-added ((t (:background "#335533" :foreground "#ddffdd" :extend t))))
   `(magit-diff-added-highlight ((t (:background "#336633" :foreground "#cceecc" :extend t))))
   `(magit-diff-removed ((t (:background "#553333" :foreground "#ffdddd" :extend t))))
   `(magit-diff-removed-highlight ((t (:background "#663333" :foreground "#eecccc" :extend t))))
   `(magit-diff-file-heading ((t (:foreground ,dracula-fg))))
   `(magit-diff-file-heading-highlight ((t (:inherit magit-section-highlight))))
   `(magit-diffstat-added ((t (:foreground ,dracula-green))))
   `(magit-diffstat-removed ((t (:foreground ,dracula-red))))
   `(magit-hash ((t (:foreground ,fg2))))
   `(magit-hunk-heading ((t (:background ,bg3))))
   `(magit-hunk-heading-highlight ((t (:background ,bg3))))
   `(magit-item-highlight ((t (:background ,bg3))))
   `(magit-log-author ((t (:foreground ,fg3))))
   `(magit-process-ng ((t (:foreground ,dracula-orange :weight bold))))
   `(magit-process-ok ((t (:foreground ,dracula-green :weight bold))))
   `(markdown-blockquote-face ((t (:foreground ,dracula-yellow :slant italic))))
   `(markdown-footnote-face ((t (:foreground ,other-blue))))
   `(markdown-header-face ((t (:weight normal))))
   `(markdown-header-face-1 ((t (:inherit bold :foreground ,dracula-pink :height ,dracula-height-title-1))))
   `(markdown-header-face-2 ((t (:inherit bold :foreground ,dracula-purple :height ,dracula-height-title-2))))
   `(markdown-header-face-3 ((t (:foreground ,dracula-green :height ,dracula-height-title-3))))
   `(markdown-header-face-4 ((t (:foreground ,dracula-yellow))))
   `(markdown-header-face-5 ((t (:foreground ,dracula-cyan))))
   `(markdown-header-face-6 ((t (:foreground ,dracula-orange))))
   `(markdown-header-face-7 ((t (:foreground ,other-blue))))
   `(markdown-header-face-8 ((t (:foreground ,dracula-fg))))
   `(markdown-inline-code-face ((t (:foreground ,dracula-green))))
   `(markdown-plain-url-face ((t (:inherit link))))
   `(markdown-pre-face ((t (:foreground ,dracula-orange))))
   `(markdown-table-face ((t (:foreground ,dracula-purple))))
   `(markdown-list-face ((t (:foreground ,dracula-cyan))))
   `(markdown-language-keyword-face ((t (:foreground ,dracula-comment))))
   `(meow-normal-indicator ((t (:foreground ,dracula-comment :inverse-video t))))
   `(meow-motion-indicator ((t (:foreground ,dracula-comment :inverse-video t))))
   `(meow-keypad-indicator ((t (:foreground ,dracula-comment :inverse-video t))))
   `(meow-beacon-indicator ((t (:foreground ,dracula-green :inverse-video t))))
   `(meow-insert-indicator ((t (:foreground ,dracula-yellow :inverse-video t))))
   `(message-header-to ((t (:foreground ,dracula-fg :weight bold))))
   `(message-header-cc ((t (:foreground ,dracula-fg :bold bold))))
   `(message-header-subject ((t (:foreground ,dracula-orange))))
   `(message-header-newsgroups ((t (:foreground ,dracula-purple))))
   `(message-header-other ((t (:foreground ,dracula-purple))))
   `(message-header-name ((t (:foreground ,dracula-green))))
   `(message-header-xheader ((t (:foreground ,dracula-cyan))))
   `(message-separator ((t (:foreground ,dracula-cyan :slant italic))))
   `(message-cited-text ((t (:foreground ,dracula-purple))))
   `(message-cited-text-1 ((t (:foreground ,dracula-purple))))
   `(message-cited-text-2 ((t (:foreground ,dracula-orange))))
   `(message-cited-text-3 ((t (:foreground ,dracula-comment))))
   `(message-cited-text-4 ((t (:foreground ,fg2))))
   `(message-mml ((t (:foreground ,dracula-green :weight normal))))
   `(mode-line ((t (:background "#13181f" :box (:line-width 4 :style flat-button) :foreground ,dracula-fg)))) ;:overline ,dracula-comment
   `(mode-line-inactive ((t (:background "#13181f" :box (:line-width 4 :style flat-button) :foreground ,dracula-comment)))) ;:overline ,bg3
   `(mini-modeline-mode-line ((t (:inherit mode-line :height 0.1 :box nil))))
   `(org-agenda-date ((t (:foreground ,dracula-cyan :underline nil))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,dracula-comment))))
   `(org-agenda-done ((t (:foreground ,dracula-green))))
   `(org-agenda-structure ((t (:foreground ,dracula-purple))))
   `(org-block ((t (:foreground ,dracula-yellow :background ,bg-alt :extend t))))
   `(org-block-begin-line ((t (:foreground ,dracula-comment :background ,bg-alt :extend t))))
   `(org-block-end-line ((t (:foreground ,dracula-comment :background ,bg-alt :extend t))))
   `(org-code ((t (:foreground ,dracula-green))))
   `(org-column ((t (:background ,bg4))))
   `(org-column-title ((t (:inherit org-column :weight bold :underline t))))
   `(org-date ((t (:foreground ,dracula-cyan :underline t))))
   `(org-document-info ((t (:foreground ,other-blue))))
   `(org-document-info-keyword ((t (:foreground ,dracula-comment))))
   `(org-document-title ((t (:weight bold :foreground ,dracula-orange :height ,dracula-height-doc-title))))
   `(org-done ((t (:foreground ,dracula-green))))
   `(org-ellipsis ((t (:foreground ,dracula-comment))))
   `(org-footnote ((t (:foreground ,other-blue))))
   `(org-formula ((t (:foreground ,dracula-pink))))
   `(org-headline-done ((t (:foreground ,dracula-comment :weight normal :strike-through t))))
   `(org-hide ((t (:foreground ,dracula-bg :background ,dracula-bg))))
   `(org-level-1 ((t (:inherit bold :foreground ,dracula-red :height ,dracula-height-title-1))))
   `(org-level-2 ((t (:inherit bold :foreground ,dracula-purple :height ,dracula-height-title-2))))
   `(org-level-3 ((t (:weight normal :foreground ,dracula-pink :weight normal :foreground ,dracula-yellow))))
   `(org-level-5 ((t (:weight normal :foreground ,dracula-cyan))))
   `(org-level-6 ((t (:weight normal :foreground ,dracula-orange))))
   `(org-level-7 ((t (:weight normal :foreground ,other-blue))))
   `(org-level-8 ((t (:weight normal :foreground ,dracula-fg))))
   `(org-link ((t (:foreground ,dracula-cyan :underline t))))
   `(org-priority ((t (:foreground ,dracula-cyan))))
   `(org-quote ((t (:foreground ,dracula-yellow :slant italic))))
   `(org-scheduled ((t (:foreground ,dracula-green))))
   `(org-scheduled-previously ((t (:foreground ,dracula-yellow))))
   `(org-scheduled-today ((t (:foreground ,dracula-green))))
   `(org-sexp-date ((t (:foreground ,fg4))))
   `(org-special-keyword ((t (:foreground ,dracula-yellow))))
   `(org-table ((t (:foreground ,dracula-purple))))
   `(org-tag ((t (:foreground ,dracula-pink :weight bold :background ,bg2))))
   `(org-todo ((t (:foreground ,dracula-orange :weight bold :background ,bg2))))
   `(org-upcoming-deadline ((t (:foreground ,dracula-yellow))))
   `(org-verbatim ((t (:inherit org-quote))))
   `(org-warning ((t (:weight bold :foreground ,dracula-pink))))
   `(outline-1 ((t (:foreground ,dracula-pink))))
   `(outline-2 ((t (:foreground ,dracula-purple))))
   `(outline-3 ((t (:foreground ,dracula-green))))
   `(outline-4 ((t (:foreground ,dracula-yellow))))
   `(outline-5 ((t (:foreground ,dracula-cyan))))
   `(outline-6 ((t (:foreground ,dracula-orange))))
   `(pabbrev-suggestions-face ((t (:foreground ,dracula-comment :underline t))))
   `(pabbrev-single-suggestion-face ((t (:foreground ,dracula-comment :slant italic))))
   `(pabbrev-suggestions-label-face ((t (:underline t :weight bold))))
   `(persp-selected-face ((t (:weight bold :foreground ,dracula-pink))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dracula-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dracula-cyan))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dracula-purple))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dracula-pink))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dracula-orange))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dracula-green))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dracula-yellow))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,other-blue))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,dracula-orange))))
   `(rst-level-1 ((t (:foreground ,dracula-pink :weight bold))))
   `(rst-level-2 ((t (:foreground ,dracula-purple :weight bold))))
   `(rst-level-3 ((t (:foreground ,dracula-green))))
   `(rst-level-4 ((t (:foreground ,dracula-yellow))))
   `(rst-level-5 ((t (:foreground ,dracula-cyan))))
   `(rst-level-6 ((t (:foreground ,dracula-orange))))
   `(rst-level-7 ((t (:foreground ,other-blue))))
   `(rst-level-8 ((t (:foreground ,dracula-fg))))
   `(show-paren-match-face ((t (:background unspecified :foreground ,dracula-cyan :weight bold))))
   `(show-paren-match ((t (:background unspecified :foreground ,dracula-cyan :weight bold))))
   `(show-paren-match-expression ((t (:inherit match))))
   `(show-paren-mismatch ((t (:inherit font-lock-warning-face))))
   `(slime-repl-inputed-output-face ((t (:foreground ,dracula-purple))))
   `(spam ((t (:inherit gnus-summary-normal-read :foreground ,dracula-orange :strike-through t :slant oblique))))
   `(tab-bar ((t (:foreground ,dracula-yellow :background "#13181f"))))
   `(tab-bar-tab ((t (:foreground ,dracula-yellow :background ,dracula-bg))))
   `(tab-bar-tab-inactive ((t (:foreground ,dracula-comment :background "#13181f" :box ,``(:line-width `(1 . -1) :color "black")))))
   `(term ((t (:foreground ,dracula-fg :background ,dracula-bg))))
   `(term-color-black ((t (:foreground ,dracula-bg :background ,dracula-comment))))
   `(term-color-blue ((t (:foreground ,dracula-purple :background ,dracula-purple))))
   `(term-color-cyan ((t (:foreground ,dracula-cyan :background ,dracula-cyan))))
   `(term-color-green ((t (:foreground ,dracula-green :background ,dracula-green))))
   `(term-color-magenta ((t (:foreground ,dracula-pink :background ,dracula-pink))))
   `(term-color-red ((t (:foreground ,dracula-red :background ,dracula-red))))
   `(term-color-white ((t (:foreground ,dracula-fg :background ,dracula-fg))))
   `(term-color-yellow ((t (:foreground ,dracula-yellow :background ,dracula-yellow))))
   `(tree-sitter-hl-face:attribute ((t (:inherit font-lock-constant-face))))
   `(tree-sitter-hl-face:comment ((t (:inherit font-lock-comment-face))))
   `(tree-sitter-hl-face:constant ((t (:inherit font-lock-constant-face))))
   `(tree-sitter-hl-face:constant.builtin ((t (:inherit font-lock-builtin-face))))
   `(tree-sitter-hl-face:constructor ((t (:inherit font-lock-constant-face))))
   `(tree-sitter-hl-face:escape ((t (:foreground ,dracula-pink))))
   `(tree-sitter-hl-face:function ((t (:inherit font-lock-function-name-face))))
   `(tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-builtin-face))))
   `(tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face :weight normal))))
   `(tree-sitter-hl-face:function.macro ((t (:inherit font-lock-preprocessor-face))))
   `(tree-sitter-hl-face:function.special ((t (:inherit font-lock-preprocessor-face))))
   `(tree-sitter-hl-face:keyword ((t (:inherit font-lock-keyword-face))))
   `(tree-sitter-hl-face:punctuation ((t (:foreground ,dracula-pink))))
   `(tree-sitter-hl-face:punctuation.bracket ((t (:foreground ,dracula-fg))))
   `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,dracula-fg))))
   `(tree-sitter-hl-face:punctuation.special ((t (:foreground ,dracula-pink))))
   `(tree-sitter-hl-face:string ((t (:inherit font-lock-string-face))))
   `(tree-sitter-hl-face:string.special ((t (:foreground ,dracula-red))))
   `(tree-sitter-hl-face:tag ((t (:inherit font-lock-keyword-face))))
   `(tree-sitter-hl-face:type ((t (:inherit font-lock-type-face))))
   `(tree-sitter-hl-face:type.parameter ((t (:foreground ,dracula-pink))))
   `(tree-sitter-hl-face:variable ((t (:inherit font-lock-variable-name-face))))
   `(tree-sitter-hl-face:variable.parameter ((t (:inherit tree-sitter-hl-face:variable :weight normal))))
   `(web-mode-builtin-face ((t (:inherit font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-css-property-name-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-doctype-face ((t (:inherit font-lock-comment-face))))
   `(web-mode-function-name-face ((t (:inherit font-lock-function-name-face))))
   `(web-mode-html-attr-name-face ((t (:foreground ,dracula-purple))))
   `(web-mode-html-attr-value-face ((t (:foreground ,dracula-green))))
   `(web-mode-html-tag-face ((t (:foreground ,dracula-pink :weight bold))))
   `(web-mode-keyword-face ((t (:foreground ,dracula-pink))))
   `(web-mode-string-face ((t (:foreground ,dracula-yellow))))
   `(web-mode-type-face ((t (:inherit font-lock-type-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(which-func ((t (:inherit font-lock-function-name-face))))
   `(which-key-key-face ((t (:inherit font-lock-builtin-face))))
   `(which-key-command-description-face ((t (:inherit default))))
   `(which-key-separator-face ((t (:inherit font-lock-comment-delimiter-face))))
   `(which-key-local-map-description-face ((t (:foreground ,dracula-green))))
   `(whitespace-big-indent ((t (:background ,dracula-red :foreground ,dracula-red))))
   `(whitespace-empty ((t (:background ,dracula-orange :foreground ,dracula-red))))
   `(whitespace-hspace ((t (:background ,bg3 :foreground ,dracula-comment))))
   `(whitespace-indentation ((t (:background ,dracula-orange :foreground ,dracula-red))))
   `(whitespace-line ((t (:background ,dracula-bg :foreground ,dracula-pink))))
   `(whitespace-newline ((t (:foreground ,dracula-comment))))
   `(whitespace-space ((t (:background ,dracula-bg :foreground ,dracula-comment))))
   `(whitespace-space-after-tab ((t (:background ,dracula-orange :foreground ,dracula-red))))
   `(whitespace-space-before-tab ((t (:background ,dracula-orange :foreground ,dracula-red))))
   `(whitespace-tab ((t (:background ,bg2 :foreground ,dracula-comment))))
   `(whitespace-trailing ((t (:inherit trailing-whitespace))))
                 
                 ))

  ;; (apply #'custom-theme-set-faces
  ;;        'dracula
  ;;        (let ((expand-with-func
  ;;               (lambda (func spec)
  ;;                 (let (reduced-color-list)
  ;;                   (dolist (col colors reduced-color-list)
  ;;                     (push (list (car col) (funcall func col))
  ;;                           reduced-color-list))
  ;;                   (eval `(let ,reduced-color-list
  ;;                            (backquote ,spec))))))
  ;;              whole-theme)
  ;;          (pcase-dolist (`(,face . ,spec) faces)
  ;;            (push `(,face
  ;;                    ((((min-colors 16777216)) ; fully graphical envs
  ;;                      ,(funcall expand-with-func 'cadr spec))
  ;;                     (((min-colors 256))      ; terminal withs 256 colors
  ;;                      ,(if dracula-use-24-bit-colors-on-256-colors-terms
  ;;                           (funcall expand-with-func 'cadr spec)
  ;;                         (funcall expand-with-func 'caddr spec)))
  ;;                     (t                       ; should be only tty-like envs
  ;;                      ,(funcall expand-with-func 'cadddr spec))))
  ;;                  whole-theme))
  ;;          whole-theme))

  ;; (apply #'custom-theme-set-variables
  ;;        'dracula
  ;;        (let ((get-func
  ;;               (pcase (display-color-cells)
  ;;                 ((pred (<= 16777216)) 'car) ; fully graphical envs
  ;;                 ((pred (<= 256)) 'cadr)     ; terminal withs 256 colors
  ;;                 (_ 'caddr))))               ; should be only tty-like envs
  ;;          `((ansi-color-names-vector
  ;;             [,(funcall get-func (alist-get 'dracula-bg colors))
  ;;              ,(funcall get-func (alist-get 'dracula-red colors))
  ;;              ,(funcall get-func (alist-get 'dracula-green colors))
  ;;              ,(funcall get-func (alist-get 'dracula-yellow colors))
  ;;              ,(funcall get-func (alist-get 'dracula-comment colors))
  ;;              ,(funcall get-func (alist-get 'dracula-purple colors))
  ;;              ,(funcall get-func (alist-get 'dracula-cyan colors))
  ;;              ,(funcall get-func (alist-get 'dracula-fg colors))])))))

;; mode-line
(setq-default flymake-mode-line-counter-format
              '("" flymake-mode-line-error-counter
                flymake-mode-line-warning-counter
                flymake-mode-line-note-counter "")
              flymake-mode-line-format
                '("" flymake-mode-line-exception
                  flymake-mode-line-counters))
(setq-default global-mode-string nil
              mode-line-end-spaces
              '("" mode-line-misc-info
                flymake-mode-line-format " Ln %l"))
(defun my/ml-padding ()
  "Adding padding to the modeline so that spme elements can be right aligned."
    (let ((r-length (length (format-mode-line mode-line-end-spaces))))
      (propertize " "
                  'display `(space :align-to (- right ,r-length)))))

(setq-default mode-line-format
              '(;; (:eval (when (mode-line-window-selected-p)
                ;;          (meow-indicator)))
                (:eval (when (and (bound-and-true-p meow-global-mode)
                                  (mode-line-window-selected-p))
                         (let* ((state (meow--current-state))
                               (indicator-face (alist-get state meow-indicator-face-alist))
                               (buffer-state (cond (buffer-read-only
                                                    " RO ")
                                                   ((buffer-modified-p)
                                                    " ** ")
                                                   (t
                                                    " RW "))))
                           (propertize buffer-state
                                       'face `(,indicator-face
                                               :inverse-video t
                                               :box (:style flat-button))))))
                "%e"
                (:eval (file-remote-p default-directory 'host))
                " "
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
            (hi-lock-face-phrase-buffer "todo:" 'hi-green)
            (hi-lock-face-phrase-buffer "fixme:" 'hi-salmon)
            ;; (setq-local display-line-numbers 'relative)
            (font-lock-add-keywords
             nil
             '((";" . 'shadow)))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula)

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
      (if (eq (car tab) 'current-tab)
          (propertize (concat "│" padstr name padstr)
                      'face `(:background "#1b222d" :slant italic :foreground "#ffd484"
                                          :box ,`(:line-width (0 . ,(/ (line-pixel-height) 8))
                                                              :color "#1b222d")))
        (propertize (concat padstr name padstr)
                    'face `(:background "#13181f" :foreground "#6272a4"
                                        :box ,`(:line-width (2 . -1) :color "black"))))))

  (advice-add 'tab-bar-tab-name-format-default :override #'clean-tab-name)

  (setq tab-bar-close-button-show nil
        tab-bar-auto-width nil
        tab-bar-new-button-show nil
        tab-bar-show 1
        tab-bar-separator ""))
        ;; tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        ;; tab-bar-tab-name-truncated-max 12))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dracula-theme.el ends here

