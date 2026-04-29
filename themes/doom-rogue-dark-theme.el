;;; doom-rogue-dark-theme.el --- Muted dark theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-rogue-dark-theme nil
  "Options for the `doom-rogue-dark' theme."
  :group 'doom-themes)

(defcustom doom-rogue-brighter-modeline t
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-rogue-dark-theme
  :type 'boolean)

(defcustom doom-rogue-dark-variable-heading-face "EtBembo"
  "Variable pitch font for headings and displays."
  :type 'string
  :group 'doom-rogue-dark-theme)

(defcustom doom-rogue-dark-variable-body-face "Merriweather"
  "Variable pitch font for regular text body."
  :type 'string
  :group 'doom-rogue-dark-theme)

(defcustom doom-rogue-dark-variable-label-face "Source Sans Pro"
  "Variable pitch font for tags, labels, etc."
  :type 'string
  :group 'doom-rogue-dark-theme)

(defcustom doom-rogue-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-rogue-dark-theme
  :type '(or integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-rogue-dark
  "Muted dark theme with subtle blue accents."
  :family 'doom-rogue
  :background-mode 'dark

  ;; name        default    256        16
  ((bg           '("#1f1f1f" "black"    "black"))
   (fg           '("#d6d3ce" "#c6c6c6"  "brightwhite"))

   (bg-alt       '("#262626" "black"    "black"))
   (fg-alt       '("#a8a49e" "#a8a8a8"  "brightblack"))

   (base0        '("#171717" "black"    "black"))
   (base1        '("#202020" "#1e1e1e"  "black"))
   (base2        '("#2b2b2b" "#2a2a2a"  "brightblack"))
   (base3        '("#3b3b3b" "#3a3a3a"  "brightblack"))
   (base4        '("#65625e" "#626262"  "brightblack"))
   (base5        '("#8a8680" "#868686"  "brightblack"))
   (base6        '("#b2aea7" "#b0b0b0"  "white"))
   (base7        '("#cbc7c0" "#c8c8c8"  "white"))
   (base8        '("#e3dfd8" "#e0e0e0"  "white"))

   ;; Core palette (intentionally muted and near-monochrome)
   (primary        '("#8ea0b3" "#8ea0b3" "blue"))
   (primary-dark   '("#738496" "#738496" "brightblue"))
   (secondary      '("#948c82" "#8c8c8c" "brightblack"))
   (secondary-dark '("#797168" "#747474" "brightblack"))

   (grey       base4)
   (red        '("#bc8f8d" "#bc8f8d" "red"))
   (orange     '("#ba9b7f" "#ba9b7f" "yellow"))
   (green      '("#91a790" "#91a790" "green"))
   (teal       '("#86a39c" "#86a39c" "cyan"))
   (yellow     '("#b5a983" "#b5a983" "yellow"))
   (blue       primary)
   (dark-blue  primary-dark)
   (magenta    '("#a392a8" "#a392a8" "magenta"))
   (violet     '("#9f97b4" "#9f97b4" "magenta"))
   (cyan       '("#88a8af" "#88a8af" "cyan"))
   (dark-cyan  '("#6f9097" "#6f9097" "cyan"))

   (highlight      base6)
   (vertical-bar   (doom-lighten base2 0.1))
   (selection      (doom-lighten base2 0.05))
   (builtin        primary-dark)
   (comments       base4)
   (doc-comments   (doom-lighten comments 0.05))
   (constants      secondary)
   (functions      fg)
   (keywords       base8)
   (methods        dark-cyan)
   (operators      base6)
   (type           primary)
   (strings        base5)
   (variables      base7)
   (numbers        orange)
   (region         (doom-lighten base1 0.1))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-rogue-brighter-modeline)
   (-modeline-pad
    (when doom-rogue-padded-modeline
      (if (integerp doom-rogue-padded-modeline) doom-rogue-padded-modeline 4)))

   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if -modeline-bright (doom-darken blue 0.45) (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if -modeline-bright (doom-darken blue 0.4) (doom-darken bg-alt 0.05)))
   (modeline-bg-inactive     (doom-darken bg-alt 0.2))
   (modeline-bg-alt-inactive `(,(car bg-alt) ,@(cdr base1))))

  ;;;; Base theme face overrides
  (((font-lock-doc-face &override) :slant 'italic)
   ((font-lock-keyword-face &override) :weight 'bold)
   ((font-lock-builtin-face &override) :weight 'bold)
   ((font-lock-type-face &override) :slant 'italic)
   ((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground base7)
   ((rainbow-delimiters-depth-1-face &override) :foreground primary-dark)
   ((rainbow-delimiters-depth-2-face &override) :foreground secondary)
   ((rainbow-delimiters-depth-3-face &override) :foreground primary-dark)
   ((rainbow-delimiters-depth-4-face &override) :foreground secondary)
   ((rainbow-delimiters-depth-5-face &override) :foreground primary-dark)
   ((rainbow-delimiters-depth-6-face &override) :foreground secondary)
   ((rainbow-delimiters-depth-7-face &override) :foreground primary-dark)
   ((rainbow-delimiters-depth-8-face &override) :foreground secondary)
   ((rainbow-delimiters-depth-9-face &override) :foreground primary-dark)
   ((completions-common-part &override) :foreground primary :weight 'bold)
   ((company-tooltip-common &override) :foreground primary :weight 'bold)
   (show-paren-match :inherit 'highlight)
   (hl-line :background base1)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground base8)
   (shadow :foreground base4)
   (tooltip :background base1 :foreground fg)
   ((link &override) :foreground fg :weight 'normal :underline t)
   ((button &override) :box '(:style released-button) :inherit 'fixed-pitch)
   (child-frame-border :background fg)

   ;;;; doom-modeline
   (doom-modeline-bar :background modeline-bg)
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-blend red bg 0.18))
   (ediff-current-diff-B        :foreground green :background (doom-blend green bg 0.18))
   (ediff-current-diff-C        :foreground blue  :background (doom-blend blue bg 0.18))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-blend teal bg 0.18))
   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground keywords :weight 'bold)
   (lsp-ui-doc-background :background base0)
   ;;;; magit
   (magit-blame-heading :foreground base6 :background bg-alt)
   ((magit-section-heading &override) :foreground primary)
   (magit-diff-removed :foreground (doom-lighten red 0.1) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.18) :weight 'bold)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground primary)
   ((markdown-code-face &override) :background base1)
   (mmm-default-submode-face :background base1)
   ;;;; mu4e
   (mu4e-header-highlight-face :inherit 'vertico-current)
   (mu4e-highlight-face :inherit 'bold)
   (mu4e-thread-folding-child-face :extend t :background base1 :underline nil)
   (mu4e-thread-folding-root-folded-face :inherit nil)
   (mu4e-thread-folding-root-unfolded-face :extend t :background base3 :overline nil :underline nil)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground base8)
   ((outline-2 &override) :foreground base6)
   ;;;; org <built-in>
   ((org-block &override) :background bg :extend nil)
   ((org-block-begin-line &override) :foreground fg-alt :background base1 :slant 'italic)
   ((org-block-end-line &override) :foreground fg-alt :background base1)
   ((org-code &override) :foreground primary-dark)
   (org-ellipsis :underline nil :background bg :foreground primary)
   ((org-quote &override) :background base1)
   ((org-document-title &override) :family doom-rogue-dark-variable-heading-face :height 2.5 :foreground fg :weight 'unspecified)
   ((org-level-1 &override) :family doom-rogue-dark-variable-heading-face :height 1.9 :weight 'bold)
   ((org-level-2 &override) :family doom-rogue-dark-variable-heading-face :height 1.6 :weight 'bold)
   ((org-level-3 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic :weight 'bold)
   ((org-level-4 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-5 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-6 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-7 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-8 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-todo &override) :foreground primary :weight 'bold)
   (org-headline-done :strike-through t :foreground grey)
   (org-table :foreground fg)
   (org-date :foreground secondary)
   ;;;; treemacs
   ((treemacs-async-loading-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-directory-collapsed-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-directory-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-file-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-fringe-indicator-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-root-face &override) :family doom-rogue-dark-variable-label-face :weight 'bold :foreground fg)
   ((treemacs-git-added-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-git-conflict-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-git-ignored-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-git-modified-face &override) :family doom-rogue-dark-variable-label-face :foreground primary)
   ((treemacs-git-renamed-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-git-unmodified-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-git-untracked-face &override) :family doom-rogue-dark-variable-label-face)
   ((treemacs-tags-face &override) :family doom-rogue-dark-variable-label-face :height 0.8 :foreground primary-dark)
   ;;;; vertico
   (vertico-current :background base2)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; whitespace
   ((whitespace-tab &override) :background (if (not (default-value 'indent-tabs-mode)) base0 'unspecified))
   ((whitespace-indentation &override) :background (if (default-value 'indent-tabs-mode) base0 'unspecified))
   ;;;; eros
   (eros-result-overlay-face :background bg-alt :foreground fg :box `(:line-width -1 :color ,fg-alt))
   ((indent-guide-face &override) :foreground base4 :slant 'normal)
   ;;;; flycheck-overlay
   (flycheck-overlay-error :background (doom-blend error bg 0.35)
                           :foreground fg
                           :height 0.9
                           :weight 'normal)
   (flycheck-overlay-warning :background (doom-blend warning bg 0.35)
                             :foreground fg
                             :height 0.9
                             :weight 'normal)
   (flycheck-overlay-info :background (doom-blend success bg 0.35)
                          :foreground fg
                          :height 0.9
                          :weight 'normal)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-alt-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt-inactive))))
  ;;;; Base theme variable overrides
  ((flycheck-overlay-info-icon "")
   (flycheck-overlay-warning-icon "")
   (flycheck-overlay-error-icon "")
   (flycheck-overlay-icon-left-padding 0.4)))

(provide 'doom-rogue-dark-theme)

;;; doom-rogue-dark-theme.el ends here
