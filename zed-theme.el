;;; zed-theme.el -- Zed theme for Emacs.

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
                          "#292c32"))
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
      (x-border-color "#a5a5a5"))
  (custom-theme-set-faces
   'zed
   ;; Basics
   `(cursor ((t (:background ,blue-color))))
   `(default ((t (:background ,background-color :foreground ,foreground-color))))
   `(blue ((t (:foreground "blue"))))
   `(bold ((t (:bold t))))
   `(bold-italic ((t (:italic t :bold t))))
   `(border-glyph ((t (nil))))
   `(green ((t (:foreground "green"))))
   `(info-node ((t (:italic t :bold t))))
   `(info-xref ((t (:bold t))))
   `(italic ((t (:italic t))))
   `(left-margin ((t (nil))))
   `(pointer ((t (nil))))
   `(red ((t (:foreground "red"))))
   `(right-margin ((t (nil))))
   `(underline ((t (:underline t))))
   `(yellow ((t (:foreground "yellow"))))

   ;; Frame
   `(fringe ((t (:background ,background-color))))
   `(header-line ((t (:background ,background-color))))
   `(mode-line ((t (:background ,modeline-color :foreground ,foreground-color :box (:line-width 4 :style flat-button)))))
   `(mode-line-highlight ((t (:box (:line-width 2 :color ,inactive-color)))))
   `(mode-line-inactive ((t (:background ,modeline-color :foreground ,inactive-color :box (:line-width 4 :style flat-button)))))

   ;; Parens
   `(show-paren-match ((t (:background ,selection-color))))
   `(show-paren-mismatch ((t (:foreground "#F9F2CE" :background ,red-color))))

   ;; Highlighting
   `(hl-line ((t (:background ,subtle-color))))
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
   `(font-lock-constant-face ((t (:foreground ,yellow-color))))
   `(font-lock-number-face ((t (:inherit font-lock-constant-face))))
   `(font-lock-doc-string-face ((t (:foreground "#1A93AE" :background "#F4F9FE"))))
   `(font-lock-function-name-face ((t (:foreground ,blue-color))))
   `(font-lock-keyword-face ((t (:foreground ,magenta-color))))
   `(font-lock-preprocessor-face ((t (:foreground ,magenta-color))))
   `(font-lock-property-name-face ((t (:foreground ,red-color))))
   `(font-lock-reference-face ((t (:foreground "#4E279A" :background "#F3F2FF"))))
   `(font-lock-string-face ((t (:foreground ,green-color))))
   `(font-lock-type-face ((t (:foreground ,cyan-color))))
   `(font-lock-variable-name-face ((t (:inherit default))))
   `(font-lock-warning-face ((t (:foreground ,red-color :weight bold))))

   ;; Diff Mode
   `(diff-header ((t (:inherit secondary-selection))))
   `(diff-file-header ((t (:bold t :inherit diff-header))))
   ;; `(diff-added ((t (:background "#DDFFDD"))))
   ;; `(diff-removed ((t (:background "#FFDDDD"))))
   ;; `(diff-changed ((t (:background "#FFFFDD"))))
   ;; `(diff-refine-change ((t (:background "#DDDDFF"))))

   ;; Ediff
   `(ediff-odd-diff-A ((t (:inherit secondary-selection :extend t))))
   `(ediff-odd-diff-B ((t (:inherit secondary-selection :extend t))))
   `(ediff-odd-diff-C ((t (:inherit secondary-selection :extend t))))
   `(ediff-even-diff-A ((t (:inherit secondary-selection :extend t))))
   `(ediff-even-diff-B ((t (:inherit secondary-selection :extend t))))
   `(ediff-even-diff-C ((t (:inherit secondary-selection :extend t))))
   
   ;; Magit
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
   `(flymake-note ((t (:underline ,`(style line :position -1 :color ,green-color)))))
   `(flymake-warning ((t (:underline ,`(style line :position -1 :color ,yellow-color)))))
   `(flymake-error ((t (:underline ,`(style line :position -1 :color ,red-color)))))
   `(diff-hl-insert ((t (:foreground ,green-color))))
   `(diff-hl-change ((t (:foreground ,blue-color))))
   `(diff-hl-delete ((t (:foreground ,red-color))))
   
   `(completions-highlight ((t (:inherit highlight :extend t))))
   `(dired-directory ((t (:foreground ,yellow-color))))
   `(dired-header ((t (:foreground ,green-color))))
   `(fixed-pitch ((t (:inherit default))))
   `(info-menu-star ((t (:foreground ,red-color))))
   `(icomplete-selected-match ((t (:inherit highlight :extend t))))
   `(link ((t (:foreground ,cyan-color))))
   `(line-number ((t (:foreground "grey50"))))
   `(line-number-current-line ((t (:inherit hl-line :foreground ,foreground-color))))
   `(popper-echo-area ((t (:inherit mode-line))))
   `(vertical-border ((t (:foreground ,border-color))))
   `(solaire-default-face ((t (:inherit default :background ,subtle-color))))
   
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
   
   `(org-agenda-date ((t (:foreground "steel blue"))))
   `(org-agenda-structure ((t (:foreground "steel blue"))))
   `(org-time-grid ((t (:foreground "grey"))))
   `(org-agenda-current-time ((t (:foreground "black"))))
   `(org-imminent-deadline ((t (:foreground "orange red" :weight bold))))
   
   `(tab-bar ((t (:background ,subtle-color :box ,border-color))))
   `(tab-bar-tab ((t (:background ,background-color :foreground ,foreground-color
                                  :underline ,`(:color ,background-color :position -1)))))
   `(tab-bar-tab-inactive
     ((t (:background ,subtle-color :foreground ,inactive-color
                      :underline ,`(:color ,border-color :position -1)))))
   `(which-func ((t (:foreground ,blue-color))))))

;; Mode-line
(setq global-mode-string nil
      mode-line-end-spaces
      `("%n " mode-line-misc-info
        (:eval (when (bound-and-true-p flymake-mode)
                 (let ((flymake-mode-line-counter-format
                        '(""flymake-mode-line-error-counter
                          flymake-mode-line-warning-counter
                          flymake-mode-line-note-counter "")))
                   (flymake--mode-line-counters))))
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

;; Tabs
(with-eval-after-load 'tab-bar
  (defun zed-tab-name (tab i)
    "A cleaner tab name emulating atom one."
    (let* ((buffer-p (bufferp tab))
           (selected-p (if (eq (car tab) 'current-tab)
                           t
                         nil))
           (name (alist-get 'name tab))
           (name (concat " " name " "))
           (face (if selected-p
                     'tab-bar-tab
                   'tab-bar-tab-inactive)))
      (concat (if (display-graphic-p)
                  (propertize " " 'face `(:background ,(face-foreground 'vertical-border nil t))
                              'display '(space :width (1))))
              ;; show dot if buffer modified else " "
              (if (and selected-p (buffer-modified-p))
                  (propertize (concat " " (make-string 1 #x23fA))
                              'face `(:inherit 'tab-bar-tab :foreground ,(face-background 'cursor nil t))
                              'display '(raise 0.2))
                (apply 'propertize " " `(tab ,tab ,@(if selected-p '(selected t))
                                             face ,face
                                             display (raise 0.2))))
              ;; apply face to buffer name
              (apply 'propertize
                     (concat
                      (propertize (string-replace "%" "%%" name) ;; (bug#57848)
                                  'help-echo (if selected-p "Current tab"
                                               "Click to select tab")
                                  'follow-link 'ignore)
                      (if (and selected-p tab-bar-close-button-show)
                          tab-bar-close-button
                        " ")
                      "")
                     `(tab ,tab
                           ,@(if selected-p '(selected t))
                           face ,face
                           display (raise 0.2)))
              
              (if (display-graphic-p)
                  (propertize " " 'face `(:background ,(face-foreground 'vertical-border nil t))
                              'display '(space :width (1)))))))

  (defun tab-bar-format-menu-bar ()
    "Produce the Menu button for the tab bar that shows the menu bar."
    `((menu-bar menu-item ,(propertize " ≡ " 'display '((raise 0.1)
                                                        (height 1.3)))
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
