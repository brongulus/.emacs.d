;;; alabaster-theme.el -- Alabaster theme for Emacs.

;; Author: Chris Etheridge (theme originally by Nikita Tonsky)
;; URL: https://github.com/chris-etheridge/alabaster-emacs
;; Package-Version: 20160525.0001
;; Version: 1.0

;;; Commentary:
;;
;; Alabaster is a theme originally created by Nikita Tonsky for Light Table.
;; Source: <https://github.com/tonsky/alabaster-lighttable-skin>

(deftheme alabaster
  "Alabster skin.")

(let ((selection-color (if (featurep 'ns) "ns_selection_color" "#C9D0D9"))
      (highlight-color "#FFD863");; "#EEE00A")
      (secondary-color "#FBE9AD")
      (active-color "#EEEEEE")
      (passive-color "#AAAAAA")
      (subtle-color "#EEEEEE")
      (error-color "#F93232")
      (border-color "#a5a5a5"))
  (custom-theme-set-faces
   'alabaster
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
   '(diff-hl-insert ((t (:foreground "lime green"))))
   '(diff-hl-delete ((t (:foreground "red"))))
   '(diff-hl-change ((t (:foreground "blue"))))
   '(line-number ((t (:foreground "grey50"))))
   '(line-number-current-line ((t (:foreground "black"))))
   '(vertical-border ((t (:foreground "grey"))))
   '(meow-beacon-indicator ((t (:background "black" :foreground "medium spring green" :inverse-video t))))
   `(meow-normal-indicator ((t (:background "black" :foreground ,selection-color :inverse-video t))))
   `(meow-motion-indicator ((t (:background "black" :foreground ,selection-color :inverse-video t))))
   `(meow-keypad-indicator ((t (:background "black" :foreground ,selection-color :inverse-video t))))
   `(meow-insert-indicator ((t (:background "black" :foreground ,highlight-color :inverse-video t))))
   `(tab-bar ((t (:background "#E8E8E8"))))
   `(tab-bar-tab ((t (:background "grey99" :foreground "black"))))
   `(tab-bar-tab-inactive ((t (:background "#E8E8E8" :foreground "grey20" :box ,`(:line-width (1 . -1) :color "#E8E8E8")))))
   ))

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
            (hi-lock-face-phrase-buffer "todo" 'hi-green)
            (hi-lock-face-phrase-buffer "fixme" 'hi-salmon)))

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
                      'face `(:background "grey99" :foreground "black" :slant italic :box ,`(:line-width ,(/ (line-pixel-height) 8) :color "grey99")))
        (propertize (concat padstr name padstr)
                    'face `(:background "#E8E8E8" :foreground "grey20" :box ,`(:line-width ,(/ (line-pixel-height) 8) :color "#E8E8E8"))))))

  (advice-add 'tab-bar-tab-name-format-default :override #'clean-tab-name)

  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-show 1
        tab-bar-separator ""))
        ;; tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        ;; tab-bar-tab-name-truncated-max 12))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'alabaster)
;;; alabaster-theme.el ends here
