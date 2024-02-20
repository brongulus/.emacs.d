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

(defcustom dracula-tab-min-width 10
  "Minimum width of a tab in characters."
  :type 'integer
  :group 'tab-bar)

(defcustom dracula-tab-max-width 30
  "Maximum width of a tab in characters."
  :type 'integer
  :group 'tab-bar)

(defcustom dracula-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'dracula)

(defun dracula-tab-line-name-buffer (buffer &rest _buffers)
  (with-current-buffer buffer
            (let* ((window-width (window-width (get-buffer-window)))
               (tab-amount (length (tab-bar-tabs)))
               (window-max-tab-width (if (>= (* (+ dracula-tab-max-width 3) tab-amount) window-width)
                                         (/ window-width tab-amount)
                                       dracula-tab-max-width))
               (tab-width (- (cond ((> window-max-tab-width dracula-tab-max-width)
                                    dracula-tab-max-width)
                                   ((< window-max-tab-width dracula-tab-min-width)
                                    dracula-tab-min-width)
                                   (t window-max-tab-width))
                             3)) ;; compensation for ' x ' button
               (buffer-name (string-trim (buffer-name)))
               (name-width (length buffer-name)))
          (if (>= name-width tab-width)
              (concat  " " (truncate-string-to-width buffer-name (- tab-width 2)) "…")
            (let* ((padding (make-string (+ (/ (- tab-width name-width) 2) 1) ?\s))
                   (buffer-name (concat padding buffer-name)))
              (concat buffer-name (make-string (- tab-width (length buffer-name)) ?\s)))))))

(setq tab-line-close-button-show nil
      tab-line-new-button-show nil
      tab-line-separator ""
      tab-line-tab-name-function #'dracula-tab-line-name-buffer)
      
(defun dired-vc-left()
  (interactive)
  (let ((dir (if (eq (vc-root-dir) nil)
                 (dired-noselect default-directory)
               (dired-noselect (vc-root-dir)))))
    (display-buffer-in-side-window
     dir `((side . left)
           (slot . 0)
           (window-width . 0.2)
           (window-parameters . ((mode-line-format . (" %b"))))))
    (windmove-left)))

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

(let ((colors '(;; Upstream theme color
                (dracula-bg      "#1b222d" "#1b222d" "#181818")  ;; 1B212D
                (dracula-fg      "#f8f8f2" "#ffffff" "brightwhite")
                (dracula-current "#282a36" "#303030" "brightblack")
                (dracula-comment "#6272a4" "#5f5faf" "blue")
                (dracula-cyan    "#82aaff" "#87d7ff" "brightcyan")
                (dracula-green   "#8fc8bb" "#5fff87" "green")
                (dracula-orange  "#ffd484" "#ffd484" "brightred")
                (dracula-pink    "#ff79c6" "#ff87d7" "magenta")
                (dracula-purple  "#A986C5" "#af87ff" "brightmagenta")
                (dracula-red     "#FB7385" "#ff8787" "red")
                (dracula-yellow  "#ffd484" "#ffd484" "brightyellow")
                (dracula-hl      "#98a8c5" "#98a8c5" "grey")
                ;; Other colors
                (links           "#80cbc4" "#87d7ff" "brightblue")
                (bg-alt          "#1E2738" "#1e1e1e" "brightblack")
                (bg2             "#2b2a3e" "#121212" "brightblack")
                (bg3             "#39465e" "#262626" "brightblack")
                (bg4             "#1E2633" "#444444" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                ;; (dark-red        "#db4b4b" "#870000" "red") ; 40% darker
                ;; (dark-green      "#41a6b5" "#00af00" "green") ; 40% darker
                (light-blue      "#98a8c5" "#98a8c5" "brightblue") ; string
                (other-blue      "#0189cc" "#0087ff" "brightblue")))
      (box-width (/ (line-pixel-height) 2))
      (faces '(;; default / basic faces
               ;; (cursor :background ,other-blue)
               (default :background ,dracula-bg :foreground ,dracula-fg)
               (default-italic :slant italic)
               (error :foreground ,dracula-red)
               (ffap :foreground ,fg4)
               (fringe :background ,dracula-bg :foreground ,fg4)
               (header-line :background ,dracula-bg)
               (highlight :foreground ,fg2 :background ,bg3)
               (hl-line :background ,bg3 :extend t)
               (info-quoted-name :foreground ,dracula-orange)
               ;; info+
               (info-title-1 :height 1.3 :inherit variable-pitch)
               (info-title-2 :height 1.2 :inherit variable-pitch)
               (info-title-3 :height 1.1 :inherit variable-pitch)
               (info-title-4 :height 1.0 :inherit variable-pitch)
               (info-glossary-word :box nil :italic t :foreground ,dracula-fg)
               (info-reference-item :background ,dracula-bg)
               (info-isolated-quote :background ,dracula-bg)
               (info-isolated-backquote :background ,dracula-bg)
               (info-file :foreground ,dracula-yellow)
               (info-menu :foreground ,dracula-yellow)
               (info-macro-ref-item :foreground ,dracula-yellow)
               (info-double-quoted-name :foreground ,dracula-yellow)
               (info-string :foreground ,dracula-yellow)
               (info-menu-star :foreground ,dracula-red)
               (info-indented-text :foreground ,other-blue)
               (info-user-option-ref-item :foreground ,dracula-red)
               (info-special-form-ref-item :foreground ,dracula-green)
               (fixed-pitch :inherit default)

               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,links :underline t)
               (linum :slant italic :foreground ,bg4 :background ,dracula-bg)
               (line-number :foreground ,dracula-comment :background ,dracula-bg)
               (line-number-current-line :foreground ,dracula-yellow :background ,dracula-bg)
               (match :background ,dracula-hl :foreground ,dracula-bg)
               (menu :background ,dracula-current :inverse-video nil
                     ,@(if dracula-alternate-mode-line-and-minibuffer
                           (list :foreground fg3)
                         (list :foreground dracula-fg)))
               (minibuffer-prompt
                ,@(if dracula-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-fg)
                    (list :weight 'bold :foreground dracula-yellow)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (shadow :foreground ,dracula-comment)
               (success :foreground ,dracula-green)
               (tooltip :foreground ,dracula-fg :background ,bg-alt)
               (trailing-whitespace :background ,dracula-orange)
               (vertical-border :foreground ,bg2)
               (warning :foreground ,dracula-orange)
               ;; syntax / font-lock
               (font-lock-builtin-face :foreground ,dracula-cyan :slant italic)
               (font-lock-comment-face :inherit shadow :slant italic)
               (font-lock-comment-delimiter-face :inherit shadow)
               (font-lock-constant-face :foreground ,dracula-purple)
               (font-lock-doc-face :foreground ,dracula-comment)
               (font-lock-function-name-face :foreground ,dracula-green :weight bold)
               (font-lock-keyword-face :foreground ,dracula-red :weight bold)
               (font-lock-negation-char-face :foreground ,dracula-cyan)
               (font-lock-preprocessor-face :foreground ,dracula-orange)
               (font-lock-reference-face :inherit font-lock-constant-face) ;; obsolete
               (font-lock-regexp-grouping-backslash :foreground ,dracula-cyan)
               (font-lock-regexp-grouping-construct :foreground ,dracula-purple)
               (font-lock-string-face :foreground ,light-blue)
               (font-lock-type-face :foreground ,dracula-yellow)
               (font-lock-variable-name-face :foreground ,dracula-fg :weight bold)
               (font-lock-warning-face :inherit warning :background ,bg2)
               ;; ansi-color
               (ansi-color-black :foreground ,dracula-bg :background ,dracula-bg)
               (ansi-color-bright-black :foreground "black" :background "black")
               (ansi-color-blue :foreground ,dracula-purple :background ,dracula-purple)
               (ansi-color-bright-blue :foreground ,dracula-purple
                                       :background ,dracula-purple
                                       :weight bold)
               (ansi-color-cyan :foreground ,dracula-cyan :background ,dracula-cyan)
               (ansi-color-bright-cyan :foreground ,dracula-cyan
                                       :background ,dracula-cyan
                                       :weight bold)
               (ansi-color-green :foreground ,dracula-green :background ,dracula-green)
               (ansi-color-bright-green :foreground ,dracula-green
                                        :background ,dracula-green
                                        :weight bold)
               (ansi-color-magenta :foreground ,dracula-pink :background ,dracula-pink)
               (ansi-color-bright-magenta :foreground ,dracula-pink
                                          :background ,dracula-pink
                                          :weight bold)
               (ansi-color-red :foreground ,dracula-red :background ,dracula-red)
               (ansi-color-bright-red :foreground ,dracula-red
                                      :background ,dracula-red
                                      :weight bold)
               (ansi-color-white :foreground ,dracula-fg :background ,dracula-fg)
               (ansi-color-bright-white :foreground "white" :background "white")
               (ansi-color-yellow :foreground ,dracula-yellow :background ,dracula-yellow)
               (ansi-color-bright-yellow :foreground ,dracula-yellow
                                         :background ,dracula-yellow
                                         :weight bold)
               ;; completions
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,dracula-yellow)
               (completions-first-difference :foreground ,dracula-cyan :weight bold)
               ;; corfu
               (corfu-default :inherit tooltip :background ,bg-alt)
               (corfu-current :background ,bg3 :foreground ,dracula-fg)
               ;; diff-hl
               (diff-hl-change :foreground ,dracula-orange :background ,dracula-orange)
               (diff-hl-delete :foreground ,dracula-red :background ,dracula-red)
               (diff-hl-insert :foreground ,dracula-green :background ,dracula-green)
               ;; dired
               (dired-directory :foreground ,dracula-green :weight normal)
               (dired-flagged :foreground ,dracula-pink)
               (dired-header :foreground ,fg3 :background ,dracula-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,dracula-fg :weight bold)
               (dired-marked :foreground ,dracula-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,dracula-yellow :weight normal :slant italic)
               (dired-broken-symlink :foreground ,dracula-red :weight bold :slant italic)
               (dired-warning :foreground ,dracula-orange :underline t)
               ;; eldoc-box
               (eldoc-box-border :background ,dracula-comment)
               ;; elfeed
               (elfeed-search-date-face :foreground ,dracula-comment)
               (elfeed-search-title-face :foreground ,dracula-fg)
               (elfeed-search-unread-title-face :foreground ,dracula-pink :weight bold)
               (elfeed-search-feed-face :foreground ,dracula-fg :weight bold)
               (elfeed-search-tag-face :foreground ,dracula-green)
               (elfeed-search-last-update-face :weight bold)
               (elfeed-search-unread-count-face :foreground ,dracula-pink)
               (elfeed-search-filter-face :foreground ,dracula-green :weight bold)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,dracula-red)
               (elfeed-log-warn-level-face :foreground ,dracula-orange)
               (elfeed-log-info-level-face :foreground ,dracula-cyan)
               (elfeed-log-debug-level-face :foreground ,dracula-comment)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,dracula-yellow)
               (enh-ruby-op-face :foreground ,dracula-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,dracula-yellow)
               (enh-ruby-string-delimiter-face :foreground ,dracula-yellow)
               ;; eww
               (eww-valid-certificate :foreground ,dracula-green)
               ;; flymake
               (flymake-error :underline (:style line :color ,dracula-red :position -1))
                              ;; :background ,bg2)
               (flymake-warning :underline (:style line :color ,dracula-yellow :position -1))
                                ;; :background ,bg2)
               (flymake-note :underline (:style line :color ,dracula-green :position -1))
                                ;; :background ,bg2)

               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,dracula-orange))
               (flyspell-incorrect :underline (:style wave :color ,dracula-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,dracula-purple)
               (font-latex-italic-face :foreground ,dracula-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,dracula-cyan)
               (font-latex-match-variable-keywords :foreground ,dracula-fg)
               (font-latex-string-face :foreground ,dracula-yellow)
               ;; go-test
               (go-test--ok-face :inherit success)
               (go-test--error-face :inherit error)
               (go-test--warning-face :inherit warning)
               (go-test--pointer-face :foreground ,dracula-pink)
               (go-test--standard-face :foreground ,dracula-cyan)
               ;; gnus
               (gnus-group-mail-1 :foreground ,dracula-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,dracula-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,dracula-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,dracula-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,dracula-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,dracula-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,dracula-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,dracula-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,dracula-cyan)
               (gnus-header-from :foreground ,dracula-fg)
               (gnus-header-name :foreground ,dracula-yellow)
               (gnus-header-subject :foreground ,dracula-red :weight bold)
               (gnus-summary-markup-face :foreground ,dracula-cyan)
               (gnus-summary-high-unread :foreground ,dracula-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,dracula-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight normal)
               (gnus-summary-normal-ticked :foreground ,dracula-pink :weight bold)
               (gnus-summary-low-unread :foreground ,dracula-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,dracula-pink)
               (haskell-constructor-face :foreground ,dracula-purple)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icomplete
               (icompletep-determined :foreground ,dracula-orange)
               ;; ido
               (ido-first-match
                ,@(if dracula-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground dracula-green)
                    (list :weight 'bold :foreground dracula-pink)))
               (ido-only-match :foreground ,dracula-orange)
               (ido-subdir :foreground ,dracula-yellow)
               (ido-virtual :foreground ,dracula-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,dracula-fg :background ,dracula-pink)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,dracula-bg :background ,dracula-orange)
               ;; magit
               (magit-branch-local :foreground ,dracula-cyan)
               (magit-branch-remote :foreground ,dracula-green)
               (magit-tag :foreground ,dracula-orange)
               (magit-section-heading :foreground ,dracula-yellow :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,dracula-orange
                                            :background ,dracula-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,dracula-orange
                                                      :background ,bg3
                                                      :weight bold
                                                      :extend t)
               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,dracula-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,dracula-green)
               (magit-diffstat-removed :foreground ,dracula-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,dracula-orange :weight bold)
               (magit-process-ok :foreground ,dracula-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,dracula-yellow
                                         :slant italic)
               (markdown-code-face :foreground ,dracula-orange)
               (markdown-footnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,dracula-pink
                ,@(when dracula-enlarge-headings
                    (list :height dracula-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,dracula-purple
                ,@(when dracula-enlarge-headings
                    (list :height dracula-height-title-2)))
               (markdown-header-face-3
                :foreground ,dracula-green
                ,@(when dracula-enlarge-headings
                    (list :height dracula-height-title-3)))
               (markdown-header-face-4 :foreground ,dracula-yellow)
               (markdown-header-face-5 :foreground ,dracula-cyan)
               (markdown-header-face-6 :foreground ,dracula-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,dracula-fg)
               (markdown-inline-code-face :foreground ,dracula-green)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,dracula-orange)
               (markdown-table-face :foreground ,dracula-purple)
               (markdown-list-face :foreground ,dracula-cyan)
               (markdown-language-keyword-face :foreground ,dracula-comment)
               ;; meow
               (meow-beacon-indicator :foreground ,dracula-green :inverse-video t)
               (meow-insert-indicator :foreground ,dracula-yellow :inverse-video t)
               ;; message
               (message-header-to :foreground ,dracula-fg :weight bold)
               (message-header-cc :foreground ,dracula-fg :bold bold)
               (message-header-subject :foreground ,dracula-orange)
               (message-header-newsgroups :foreground ,dracula-purple)
               (message-header-other :foreground ,dracula-purple)
               (message-header-name :foreground ,dracula-green)
               (message-header-xheader :foreground ,dracula-cyan)
               (message-separator :foreground ,dracula-cyan :slant italic)
               (message-cited-text :foreground ,dracula-purple)
               (message-cited-text-1 :foreground ,dracula-purple)
               (message-cited-text-2 :foreground ,dracula-orange)
               (message-cited-text-3 :foreground ,dracula-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,dracula-green :weight normal)
               ;; mode-line
               (mode-line :background ,dracula-bg ;; :inherit variable-pitch
                          :overline ,dracula-comment :inverse-video nil
                          ,@(if dracula-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground dracula-comment)))
               (mode-line-inactive ;; :inherit variable-pitch
                :background ,dracula-bg :inverse-video nil
                ,@(if dracula-alternate-mode-line-and-minibuffer
                      (list :foreground dracula-comment :box dracula-bg)
                    (list :foreground bg3 :overline bg3)))
               (mini-modeline-mode-line :inherit mode-line :height 0.1 :box nil)
               ;; org
               (org-agenda-date :foreground ,dracula-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,dracula-comment)
               (org-agenda-done :foreground ,dracula-green)
               (org-agenda-structure :foreground ,dracula-purple)
               (org-block :foreground ,dracula-yellow :background ,bg-alt :extend t)
               (org-block-begin-line :foreground ,dracula-comment :background ,bg-alt :extend t)
               (org-block-end-line :foreground ,dracula-comment :background ,bg-alt :extend t)
               (org-code :foreground ,dracula-green)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,dracula-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,dracula-comment)
               (org-document-title :weight bold :foreground ,dracula-orange
                                   ,@(when dracula-enlarge-headings
                                       (list :height dracula-height-doc-title)))
               (org-done :foreground ,dracula-green)
               (org-ellipsis :foreground ,dracula-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,dracula-pink)
               (org-headline-done :foreground ,dracula-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,dracula-bg :background ,dracula-bg)
               (org-level-1 :inherit bold :foreground ,dracula-red
                            ,@(when dracula-enlarge-headings
                                (list :height dracula-height-title-1)))
               (org-level-2 :inherit bold :foreground ,dracula-purple
                            ,@(when dracula-enlarge-headings
                                (list :height dracula-height-title-2)))
               (org-level-3 :weight normal :foreground ,dracula-pink
                            ,@(when dracula-enlarge-headings
                                (list :height dracula-height-title-3)))
               (org-level-4 :weight normal :foreground ,dracula-yellow)
               (org-level-5 :weight normal :foreground ,dracula-cyan)
               (org-level-6 :weight normal :foreground ,dracula-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,dracula-fg)
               (org-link :foreground ,dracula-cyan :underline t)
               (org-priority :foreground ,dracula-cyan)
               (org-quote :foreground ,dracula-yellow :slant italic)
               (org-scheduled :foreground ,dracula-green)
               (org-scheduled-previously :foreground ,dracula-yellow)
               (org-scheduled-today :foreground ,dracula-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,dracula-yellow)
               (org-table :foreground ,dracula-purple)
               (org-tag :foreground ,dracula-pink :weight bold :background ,bg2)
               (org-todo :foreground ,dracula-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,dracula-yellow)
               (org-verbatim :inherit org-quote)
               (org-warning :weight bold :foreground ,dracula-pink)
               ;; outline
               (outline-1 :foreground ,dracula-pink)
               (outline-2 :foreground ,dracula-purple)
               (outline-3 :foreground ,dracula-green)
               (outline-4 :foreground ,dracula-yellow)
               (outline-5 :foreground ,dracula-cyan)
               (outline-6 :foreground ,dracula-orange)
               ;; perspective
               (persp-selected-face :weight bold :foreground ,dracula-pink)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,dracula-fg)
               (rainbow-delimiters-depth-2-face :foreground ,dracula-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,dracula-purple)
               (rainbow-delimiters-depth-4-face :foreground ,dracula-pink)
               (rainbow-delimiters-depth-5-face :foreground ,dracula-orange)
               (rainbow-delimiters-depth-6-face :foreground ,dracula-green)
               (rainbow-delimiters-depth-7-face :foreground ,dracula-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,dracula-orange)
               ;; rst (reStructuredText)
               (rst-level-1 :foreground ,dracula-pink :weight bold)
               (rst-level-2 :foreground ,dracula-purple :weight bold)
               (rst-level-3 :foreground ,dracula-green)
               (rst-level-4 :foreground ,dracula-yellow)
               (rst-level-5 :foreground ,dracula-cyan)
               (rst-level-6 :foreground ,dracula-orange)
               (rst-level-7 :foreground ,other-blue)
               (rst-level-8 :foreground ,dracula-fg)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,dracula-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,dracula-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,dracula-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,dracula-orange
                     :strike-through t :slant oblique)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,dracula-yellow :background ,dracula-bg)
                        ;; :inherit variable-pitch)
               (tab-bar-tab :foreground ,dracula-yellow :background ,dracula-bg)
               (tab-bar-tab-inactive :foreground ,dracula-comment :background ,dracula-bg)
               (tab-line :background ,bg2
                         :height 1.0 :inherit variable-pitch
                         :box (:line-width -1 :color ,bg2))
               (tab-line-tab :foreground ,dracula-yellow :background ,dracula-bg
                             :box (:line-width 2 :color ,dracula-bg))
               (tab-line-tab-inactive :foreground ,dracula-comment :background ,bg2
                                      :box (:line-width 2 :color ,bg2))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,dracula-red)
               ;; term
               (term :foreground ,dracula-fg :background ,dracula-bg)
               (term-color-black :foreground ,dracula-bg :background ,dracula-comment)
               (term-color-blue :foreground ,dracula-purple :background ,dracula-purple)
               (term-color-cyan :foreground ,dracula-cyan :background ,dracula-cyan)
               (term-color-green :foreground ,dracula-green :background ,dracula-green)
               (term-color-magenta :foreground ,dracula-pink :background ,dracula-pink)
               (term-color-red :foreground ,dracula-red :background ,dracula-red)
               (term-color-white :foreground ,dracula-fg :background ,dracula-fg)
               (term-color-yellow :foreground ,dracula-yellow :background ,dracula-yellow)
               ;; tree-sitter
               (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
               (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
               (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
               (tree-sitter-hl-face:constant.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:constructor :inherit font-lock-constant-face)
               (tree-sitter-hl-face:escape :foreground ,dracula-pink)
               (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
               (tree-sitter-hl-face:function.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face
                                                  :weight normal)
               (tree-sitter-hl-face:function.macro :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:function.special :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:punctuation :foreground ,dracula-pink)
               (tree-sitter-hl-face:punctuation.bracket :foreground ,dracula-fg)
               (tree-sitter-hl-face:punctuation.delimiter :foreground ,dracula-fg)
               (tree-sitter-hl-face:punctuation.special :foreground ,dracula-pink)
               (tree-sitter-hl-face:string :inherit font-lock-string-face)
               (tree-sitter-hl-face:string.special :foreground ,dracula-red)
               (tree-sitter-hl-face:tag :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:type :inherit font-lock-type-face)
               (tree-sitter-hl-face:type.parameter :foreground ,dracula-pink)
               (tree-sitter-hl-face:variable :inherit font-lock-variable-name-face)
               (tree-sitter-hl-face:variable.parameter :inherit tree-sitter-hl-face:variable
                                                       :weight normal)
               ;; web-mode
               (web-mode-builtin-face :inherit font-lock-builtin-face)
               (web-mode-comment-face :inherit font-lock-comment-face)
               (web-mode-constant-face :inherit font-lock-constant-face)
               (web-mode-css-property-name-face :inherit font-lock-constant-face)
               (web-mode-doctype-face :inherit font-lock-comment-face)
               (web-mode-function-name-face :inherit font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,dracula-purple)
               (web-mode-html-attr-value-face :foreground ,dracula-green)
               (web-mode-html-tag-face :foreground ,dracula-pink :weight bold)
               (web-mode-keyword-face :foreground ,dracula-pink)
               (web-mode-string-face :foreground ,dracula-yellow)
               (web-mode-type-face :inherit font-lock-type-face)
               (web-mode-warning-face :inherit font-lock-warning-face)
               ;; which-func
               (which-func :inherit font-lock-function-name-face)
               ;; which-key
               (which-key-key-face :inherit font-lock-builtin-face)
               (which-key-command-description-face :inherit default)
               (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               (which-key-local-map-description-face :foreground ,dracula-green)
               ;; whitespace
               (whitespace-big-indent :background ,dracula-red :foreground ,dracula-red)
               (whitespace-empty :background ,dracula-orange :foreground ,dracula-red)
               (whitespace-hspace :background ,bg3 :foreground ,dracula-comment)
               (whitespace-indentation :background ,dracula-orange :foreground ,dracula-red)
               (whitespace-line :background ,dracula-bg :foreground ,dracula-pink)
               (whitespace-newline :foreground ,dracula-comment)
               (whitespace-space :background ,dracula-bg :foreground ,dracula-comment)
               (whitespace-space-after-tab :background ,dracula-orange :foreground ,dracula-red)
               (whitespace-space-before-tab :background ,dracula-orange :foreground ,dracula-red)
               (whitespace-tab :background ,bg2 :foreground ,dracula-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit font-lock-builtin-face)
               (yard-directive-face :inherit font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'dracula
         (let ((expand-with-func
                (lambda (func spec)
                  (let (reduced-color-list)
                    (dolist (col colors reduced-color-list)
                      (push (list (car col) (funcall func col))
                            reduced-color-list))
                    (eval `(let ,reduced-color-list
                             (backquote ,spec))))))
               whole-theme)
           (pcase-dolist (`(,face . ,spec) faces)
             (push `(,face
                     ((((min-colors 16777216)) ; fully graphical envs
                       ,(funcall expand-with-func 'cadr spec))
                      (((min-colors 256))      ; terminal withs 256 colors
                       ,(if dracula-use-24-bit-colors-on-256-colors-terms
                            (funcall expand-with-func 'cadr spec)
                          (funcall expand-with-func 'caddr spec)))
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme))

  (apply #'custom-theme-set-variables
         'dracula
         (let ((get-func
                (pcase (display-color-cells)
                  ((pred (<= 16777216)) 'car) ; fully graphical envs
                  ((pred (<= 256)) 'cadr)     ; terminal withs 256 colors
                  (_ 'caddr))))               ; should be only tty-like envs
           `((ansi-color-names-vector
              [,(funcall get-func (alist-get 'dracula-bg colors))
               ,(funcall get-func (alist-get 'dracula-red colors))
               ,(funcall get-func (alist-get 'dracula-green colors))
               ,(funcall get-func (alist-get 'dracula-yellow colors))
               ,(funcall get-func (alist-get 'dracula-comment colors))
               ,(funcall get-func (alist-get 'dracula-purple colors))
               ,(funcall get-func (alist-get 'dracula-cyan colors))
               ,(funcall get-func (alist-get 'dracula-fg colors))])))))

;; mode-line
(with-eval-after-load 'flymake
  (setq-default flymake-mode-line-counter-format
                '("" flymake-mode-line-error-counter
                  flymake-mode-line-warning-counter
                  flymake-mode-line-note-counter "")
                flymake-mode-line-format
                '(" "
                  flymake-mode-line-exception
                  flymake-mode-line-counters)))
(setq-default global-mode-string nil
              mode-line-end-spaces
              '(" " mode-line-misc-info "  "
                (vc-mode vc-mode) " Ln %l, %p %m "))
(setq-default mode-line-format
              '(;; (:eval evil-mode-line-tag)
                (:eval (when (mode-line-window-selected-p)
                         (meow-indicator)))
                "%e "
                (:eval (if (buffer-modified-p)
                           (propertize " %b " 'face '(:slant italic :inverse-video t)
                                       'help-echo (buffer-file-name))
                         (propertize " %b " 'help-echo (buffer-file-name))))
                (:eval (when (and (bound-and-true-p flymake-mode)
                                  (mode-line-window-selected-p))
                         flymake-mode-line-format))
                mode-line-format-right-align
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

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dracula-theme.el ends here

