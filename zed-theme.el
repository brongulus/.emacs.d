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
                            "#fafafa"
                          "#282c33"))
      (foreground-color (if load-theme-light
                            "black"
                          "#c8ccd3"))
      (green-color (if load-theme-light
                       "#50a14f"
                     "#a7bf87"))
      (blue-color (if load-theme-light
                      "#6079db"
                    "#80ace3"))
      (cyan-color (if load-theme-light
                      "#6594bd"
                    "#7db2bd"))
      (red-color (if load-theme-light
                     "#c56655"
                   "#c47779"))
      (magenta-color (if load-theme-light
                         "#9950a6"
                       "#ab7bca"))
      (yellow-color (if load-theme-light
                        "#986801"
                      "#d9c18c"))
      (light-yellow-color (if load-theme-light
                              "#ddc184"
                            "#d9c18c"))
      (_x-border-color "#a5a5a5"))
  (custom-theme-set-faces
   'zed
   ;; Basics
   `(cursor ((t (:background ,blue-color))))
   `(default ((t (:background ,background-color :foreground ,foreground-color))))
   `(border-glyph ((t (nil))))
   `(info-node ((t (:italic t :bold t))))
   `(info-xref ((t (:bold t))))
   `(left-margin ((t (nil))))
   `(right-margin ((t (nil))))
   `(pointer ((t (nil))))

   ;; Frame
   `(fringe ((t (:background ,background-color))))
   `(header-line ((t (:background ,background-color))))
   `(mode-line ((t (:background ,modeline-color :foreground ,foreground-color
                                ;; :underline ,`(:color ,border-color :position -2)
                                ;; :overline ,border-color
                                :box (:line-width 4 :style flat-button)))))
   `(mode-line-highlight ((t (:box (:line-width 2 :color ,inactive-color)))))
   `(mode-line-inactive ((t (:background ,modeline-color :foreground ,inactive-color
                                         ;; :underline ,`(:color ,border-color :position -2)
                                         ;; :overline ,border-color
                                         :box (:line-width 4 :style flat-button)))))

   ;; Parens
   `(show-paren-match ((t (:background ,selection-color))))
   `(show-paren-mismatch ((t (:foreground ,foreground-color :background ,red-color))))

   ;; Highlighting
   `(hl-line ((t (:background ,modeline-color))))
   `(highline-face ((t (:background ,subtle-color))))
   `(highlight ((t (:background ,highlight-color))))
   `(highlight-symbol-face ((t (:background ,secondary-color))))
   `(isearch ((t (:background ,highlight-color))))
   `(lazy-highlight ((t (:background ,secondary-color))))
   `(primary-selection ((t (:background ,selection-color))))
   `(region ((t (:background ,selection-color :extend nil))))
   `(secondary-selection ((t (:background ,secondary-color :extend nil))))
   `(match ((t (:inherit highlight))))
   `(rectangle-preview ((t (:inherit region))))
   `(shadow ((t (:foreground "grey50"))))

   ;; Font-lock
   `(font-lock-builtin-face ((t (:foreground ,blue-color))))
   `(font-lock-comment-face ((t (:foreground "#5e636e"))))
   `(font-lock-constant-face ((t (:foreground ,red-color))))
   `(font-lock-number-face ((t (:inherit font-lock-constant-face))))
   `(font-lock-doc-string-face ((t (:foreground "#1A93AE" :background "#F4F9FE"))))
   `(font-lock-function-name-face ((t (:foreground ,blue-color))))
   `(font-lock-keyword-face ((t (:foreground ,blue-color))))
   `(font-lock-preprocessor-face ((t (:foreground ,magenta-color))))
   `(font-lock-property-name-face ((t (:foreground ,yellow-color))))
   `(font-lock-reference-face ((t (:foreground "#4E279A" :background "#F3F2FF"))))
   `(font-lock-string-face ((t (:foreground ,green-color))))
   `(font-lock-type-face ((t (:foreground ,cyan-color))))
   `(font-lock-variable-name-face ((t (:inherit default))))
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
   
   `(avy-lead-face ((t (:foreground ,yellow-color :inverse-video t :weight bold))))
   `(avy-lead-face-0 ((t (:foreground ,yellow-color :inverse-video t :weight bold))))
   `(avy-lead-face-1 ((t (:foreground ,yellow-color :inverse-video t :weight bold))))
   `(avy-lead-face-2 ((t (:foreground ,yellow-color :inverse-video t :weight bold))))
   
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
   
   `(completions-highlight ((t (:inherit highlight :extend t))))
   `(dired-directory ((t (:foreground ,yellow-color))))
   `(dired-header ((t (:foreground ,green-color :height 1.2))))
   `(fixed-pitch ((t (:inherit default))))
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
   `(orderless-match-face-1 ((t (:foreground ,red-color :weight bold))))
   `(orderless-match-face-2 ((t (:foreground ,green-color :weight bold))))
   `(orderless-match-face-3 ((t (:foreground ,yellow-color :weight bold))))
   `(popper-echo-area ((t (:inherit mode-line))))
   `(pulse-highlight-start-face ((t (:background ,light-yellow-color))))
   `(pulse-highlight-face ((t (:background ,light-yellow-color :extend t))))
   `(vertical-border ((t (:foreground ,border-color))))
   `(whitespace-space ((t (:inherit font-lock-comment-face))))
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
   `(shr-text ((t (:inherit default))))
   `(gnus-group-mail-1 ((t (:foreground ,red-color :weight bold))))
   `(gnus-group-mail-2 ((t (:foreground ,red-color :weight bold))))
   `(gnus-group-mail-3 ((t (:foreground ,red-color :weight bold))))
   `(gnus-group-mail-low ((t (:foreground ,red-color :weight bold))))
   `(gnus-group-mail-1-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-2-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-3-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-mail-low-empty ((t (:foreground "#5e636e"))))
   `(gnus-group-news-1 ((t (:foreground ,red-color :weight bold))))
   `(gnus-group-news-2 ((t (:foreground ,red-color :weight bold))))
   `(gnus-group-news-3 ((t (:foreground ,red-color :weight bold))))
   `(gnus-group-news-low ((t (:foreground ,red-color :weight bold))))
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
   
   `(meow-beacon-indicator
     ((t (:background ,background-color :foreground ,green-color :inverse-video t))))
   `(meow-normal-indicator
     ((t (:background ,foreground-color :foreground ,selection-color :inverse-video t))))
   `(meow-motion-indicator
     ((t (:background ,foreground-color :foreground ,selection-color :inverse-video t))))
   `(meow-keypad-indicator
     ((t (:background ,foreground-color :foreground ,selection-color :inverse-video t))))
   `(meow-insert-indicator
     ((t (:background ,background-color :foreground ,yellow-color :inverse-video t))))
   `(minibuffer-prompt ((t (:foreground ,yellow-color))))
   
   `(org-block ((t (:inherit default :background ,subtle-color :extend t))))
   `(org-block-begin-line ((t (:inherit org-meta-line :background ,subtle-color :extend t))))
   `(org-block-end-line ((t (:inherit org-meta-line :background ,subtle-color :extend t))))
   `(org-document-title ((t (:height 1.26))))
   `(org-document-info ((t (:height 1.26))))
   `(org-drawer ((t (:inherit shadow))))
   `(org-level-1 ((t (:weight bold :height 1.24))))
   `(org-level-2 ((t (:height 1.2))))
   `(org-level-3 ((t (:weight bold :height 1.16))))
   `(org-agenda-structure ((t (:foreground ,magenta-color :height 1.2))))
   `(org-agenda-date ((t (:foreground ,blue-color :height 1.1))))
   `(org-time-grid ((t (:inherit font-lock-comment-face))))
   `(org-agenda-current-time ((t (:foreground ,foreground-color))))
   `(org-imminent-deadline ((t (:foreground ,red-color :weight bold))))
   `(org-todo ((t (:foreground ,yellow-color :weight bold))))
   `(org-scheduled ((t (:foreground ,green-color))))
   `(org-scheduled-today ((t (:inherit org-scheduled))))
   `(org-date ((t (:inherit link))))
   `(org-verbatim ((t (:foreground ,green-color))))
   `(org-habit-clear-face ((t (:foreground ,background-color))))
   `(org-habit-clear-future-face ((t (:foreground ,background-color))))
   `(org-habit-alert-face ((t (:foreground ,yellow-color :inverse-video t))))
   `(org-habit-alert-future-face ((t (:foreground ,yellow-color :inverse-video t))))
   `(org-habit-overdue-face ((t (:foreground ,red-color :inverse-video t))))
   `(org-habit-overdue-future-face ((t (:foreground ,red-color :inverse-video t))))
   `(org-habit-ready-face ((t (:foreground ,green-color))))
   `(org-habit-ready-future-face ((t (:foreground ,green-color))))
   
   `(tab-bar ((t (:background ,subtle-color :box ,border-color))))
   `(tab-bar-tab ((t (:background ,background-color :foreground ,foreground-color
                                  :underline ,`(:color ,background-color :position -1)))))
   `(tab-bar-tab-inactive
     ((t (:background ,subtle-color :foreground ,inactive-color
                      :underline ,`(:color ,border-color :position -1)))))
   ;; TODO make it solaire-compatible
   `(tab-line ((t (:background ,background-color :box ,border-color :italic nil))))
   `(tab-line-tab ((t (:background ,subtle-color :foreground ,foreground-color :italic nil
                                   :underline ,`(:color ,subtle-color :position -1)))))
   `(tab-line-tab-inactive
     ((t (:background ,background-color :foreground ,inactive-color :italic nil
                      :underline ,`(:color ,border-color :position -1)))))
   `(tab-line-tab-current ((t (:inherit tab-line-tab))))
   `(tab-line-highlight ((t (:inherit tab-line-tab))))

   `(why-this-face ((t (:inherit font-lock-comment-face))))
   `(which-func ((t (:foreground ,inactive-color))))))

;; Mode-line
(setq global-mode-string nil
      ;; clean eglot
      eglot-menu-string (char-to-string #x2699)
      which-func-non-auto-modes '(erc-mode)
      which-func-unknown ""
      ;; which-func-modes '(text-mode prog-mode)
      which-func-format
      `(:propertize which-func-current face which-func)
      ;; dont show which-func information in misc-info
      mode-line-misc-info (delete
                           '(which-function-mode
                             (which-func-mode
                              ("" which-func-format " ")))
                           mode-line-misc-info)
      ;; all the stuff that will be right-aligned
      mode-line-end-spaces
      `("%n " mode-line-misc-info
        (:eval (propertize (format " %s" (upcase (if (stringp mode-name)
                                                     mode-name
                                                   (car mode-name))))
                           'help-echo "Mouse-1: Show major-mode-menu"
                           'local-map mode-line-major-mode-keymap))
        (:eval (when (bound-and-true-p vc-mode)
                 (propertize (concat " " vc-mode) 'face ;; vc-display-status 'no-backend
                             '(:inherit success :weight bold))))
        (:eval (when (bound-and-true-p flymake-mode)
                 (let ((flymake-mode-line-counter-format
                        '(" " flymake-mode-line-error-counter
                          flymake-mode-line-warning-counter
                          flymake-mode-line-note-counter "")))
                   (flymake--mode-line-counters))))
        (:eval (propertize " %l:%C" 'face 'which-func))))

(defun my/ml-padding ()
  "Adding padding to the modeline so that spme elements can be right aligned."
  (let ((r-length (length (format-mode-line mode-line-end-spaces))))
    (propertize " "
                'display `(space :align-to (- right ,r-length)))))

(setq-default
 mode-line-format
 '((:eval (when (and (bound-and-true-p meow-global-mode)
                     (mode-line-window-selected-p))
            (let* ((state (meow--current-state))
                   (indicator-face (alist-get state meow-indicator-face-alist))
                   (buffer-state (cond ((file-remote-p default-directory)
                                        (concat " " (file-remote-p
                                                     default-directory 'host)
                                                " "))
                                       (t
                                        " %p "))))
              (propertize buffer-state
                          'face `(,indicator-face
                                  :inverse-video t
                                  :box (:style flat-button))))))
   "%e"
   (:eval (propertize " %b " 'face (if (and (buffer-modified-p)
                                            (or (derived-mode-p 'prog-mode)
                                                (derived-mode-p 'text-mode)))
                                       '(:inherit font-lock-warning-face :weight bold)
                                     '(:weight bold))
                      'help-echo (buffer-file-name)))
   (which-function-mode (which-func-mode
                         ("" which-func-format " ")))
   (:eval (if (string> emacs-version "29.4")
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

;; Tabs
(with-eval-after-load 'tab-line
  (setq tab-line-close-button
        (propertize (concat " " (make-string 1 #x00D7) " ") 'close-tab t)))

(with-eval-after-load 'tab-bar
  (defun zed-tab-name (tab _i)
    "A cleaner tab name emulating atom one."
    (let* ((selected-p (eq (car tab) 'current-tab))
           (name (alist-get 'name tab))
           (name (concat " " name " "))
           (face (if selected-p
                     'tab-bar-tab
                   'tab-bar-tab-inactive)))
      (concat (propertize " " 'face `(:background ,(face-foreground 'vertical-border nil t))
                          'display '(space :width (1)))
              ;; show dot if buffer modified else " "
              (if (and selected-p (buffer-modified-p)
                       (or (derived-mode-p 'prog-mode)
                           (derived-mode-p 'text-mode)))
                  (propertize (concat " " (make-string 1 #x23fA))
                              'face `(:inherit tab-bar-tab :foreground ,(face-background 'cursor nil t))
                              'display '(raise 0.2))
                (apply 'propertize " " `(tab ,tab ,@(if selected-p '(selected t))
                                             face ,face
                                             display (raise 0.2))))
              ;; apply face to buffer name
              (apply 'propertize
                     (concat
                      (propertize (string-replace "%" "%%" name) ;; (bug#57848)
                                  'follow-link 'ignore)
                      (if (and selected-p tab-bar-close-button-show)
                          tab-bar-close-button
                        " ")
                      "")
                     `(tab ,tab
                           ,@(if selected-p '(selected t))
                           face ,face
                           display (raise 0.2)))
              
              (propertize " " 'face `(:background ,(face-foreground 'vertical-border nil t))
                          'display '(space :width (1))))))

  (defun tab-bar-format-menu-bar ()
    "Produce the Menu button for the tab bar that shows the menu bar."
    `((menu-bar menu-item ,(propertize " ≡ " 'display '((raise 0.1)
                                                        (height 1.1)))
                tab-bar-menu-bar :help "Menu Bar")))

  (defun tab-bar-format-history ()
    "Produce back and forward buttons for the tab bar."
    (when tab-bar-history-mode
      `((sep-history-back menu-item ,(tab-bar-separator) ignore)
        (history-back
         menu-item ,zed-tab-bar-back-button tab-bar-history-back
         :help "Click to go back in tab history")
        (sep-history-forward menu-item ,(tab-bar-separator) ignore)
        (history-forward
         menu-item ,zed-tab-bar-forward-button tab-bar-history-forward
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
        zed-tab-bar-forward-button
        (propertize "⏵" 'display '(raise 0.2))
        zed-tab-bar-back-button
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
