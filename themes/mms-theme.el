;; -*- lexical-binding: t; -*-

(deftheme mms
  "Created 2025-05-01.")

(let ((bg      "#f2ebd6")
      (fg      "#352f19")
      (comment "#7d7165")
      (black   "#000000")
      (header  "#e8dfc8")
      (red     "#ff0000"))

  (custom-theme-set-faces
   'mms

   ;; Core
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,fg))))
   `(highlight ((t (:background ,fg :foreground ,bg))))
   `(region ((t (:inherit highlight))))
   `(minibuffer-prompt ((t (:weight bold))))
   `(secondary-selection ((t (:extend t :background "yellow1"))))
   `(trailing-whitespace ((t (:inherit highlight))))

   ;; Font lock
   `(font-lock-builtin-face ((t (:weight bold))))
   `(font-lock-comment-face ((t (:slant italic :foreground ,comment))))
   `(font-lock-constant-face ((t (:weight bold :slant italic))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-function-name-face ((t (:weight bold))))
   `(font-lock-keyword-face ((t (:weight bold))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-number-face ((t nil)))
   `(font-lock-operator-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:slant italic))))
   `(font-lock-punctuation-face ((t nil)))
   `(font-lock-string-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:slant italic))))
   `(font-lock-variable-name-face ((t (:foreground ,black))))
   `(font-lock-warning-face ((t (:underline (:color ,red :style wave) :foreground ,black))))

   ;; UI
   `(link ((t (:underline t))))
   `(link-visited ((t (:foreground "magenta4" :inherit link))))
   `(header-line ((t (:background ,header :foreground ,fg))))
   `(tooltip ((t (:foreground "black" :background "lightyellow" :inherit variable-pitch))))
   `(mode-line ((t (:inherit highlight))))
   `(mode-line-active ((t (:inherit highlight))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background "#c2c2c2"))))
   `(isearch ((t (:inherit highlight :weight bold))))
   `(lazy-highlight ((t nil)))
   `(completions-common-part ((t (:underline t))))
   `(help-key-binding ((t (:box (:line-width (-1 . -1) :color ,fg)))))

   ;; Tabs
   `(tab-bar ((t (:inherit variable-pitch))))
   `(tab-line ((t (:height 0.9 :foreground "black" :background "grey85" :inherit variable-pitch))))
   `(tab-bar-tab ((t (:inherit highlight))))
   `(tab-bar-tab-inactive ((t (:box (:line-width (1 . 1) :color ,fg :style flat-button)))))

   ;; Org
   `(org-document-title ((t (:inherit default :height 2.0))))
   `(org-level-1 ((t (:inherit default :height 1.75))))
   `(org-level-2 ((t (:inherit default :height 1.5))))
   `(org-level-3 ((t (:inherit default :height 1.25))))
   `(org-level-4 ((t (:inherit default :height 1.1))))
   `(org-level-5 ((t (:inherit default :height 1.0))))

   ;; Notmuch / Email
   `(notmuch-tag-face ((t (:weight bold))))
   `(notmuch-tag-flagged ((t nil)))
   `(notmuch-tag-unread ((t (:underline (:color foreground-color :style double-line) :weight extra-bold))))
   `(notmuch-hello-logo-background ((t nil)))
   `(notmuch-message-summary-face ((t (:inherit highlight))))
   `(message-header-cc ((t nil)))
   `(message-header-subject ((t (:underline t))))
   `(message-header-name ((t (:underline (:color foreground-color :style dots)))))
   `(message-header-other ((t nil)))
   `(message-header-to ((t (:weight bold))))

   ;; Mastodon
   `(mastodon-display-name-face ((t (:weight bold))))
   `(mastodon-boosted-face ((t nil)))
   `(mastodon-cw-face ((t nil)))
   `(mastodon-boost-fave-face ((t (:weight bold))))

   ;; ERC
   `(erc-notice-face ((t (:weight semi-bold))))
   `(erc-prompt-face ((t (:weight bold))))
   `(erc-my-nick-face ((t (:underline t))))
   `(erc-timestamp-face ((t nil)))
   `(erc-current-nick-face ((t (:inherit highlight))))
   `(erc-direct-msg-face ((t (:slant oblique))))
   `(erc-input-face ((t nil)))
   `(erc-pal-face ((t (:inherit highlight))))
   `(erc-nick-prefix-face ((t nil)))
   `(erc-nick-default-face ((t nil)))

   ;; Magit
   `(magit-branch-remote-head ((t (:box nil))))
   `(magit-branch-remote ((t nil)))
   `(magit-branch-local ((t (:inherit highlight))))
   `(magit-section-heading ((t (:underline (:color foreground-color :style dashes) :weight bold))))
   `(magit-diff-context-highlight ((t nil)))
   `(magit-section-highlight ((t (:inherit highlight))))

   ;; Jira
   `(jira-face-info ((t (:inherit highlight))))
   `(jira-face-tag ((t nil)))
   `(jira-face-time ((t nil)))))

(provide-theme 'mms)
