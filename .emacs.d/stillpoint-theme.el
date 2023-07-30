;;; stillpoint-theme.el --- -*- lexical-binding: t; -*-
(deftheme stillpoint "A minimalist theme.")

(defun stillpoint-balance-mode-line (left right)
  "Return a string with LEFT and RIGHT at edges of current window."
  (format (format "%%s %%%ds" (- (window-total-width) (length left) 2))
          left right))

(custom-theme-set-variables
 'stillpoint
 '(fringe-mode 0)
 '(scroll-bar-mode nil)
 '(widget-image-enable nil)
 '(default-frame-alist
   '((internal-border-width . 20)))
 '(mode-line-format
   '(:eval
     (stillpoint-balance-mode-line
      (format-mode-line
       (list " " mode-line-buffer-identification
             " " mode-line-modified
             " " mode-name))
      (format-mode-line
       (list minor-mode-alist " " mode-line-misc-info))))))

;; '(diff-hl-delete
;;   ((((background light)) (:background "pink"))
;;    (((background dark)) (:background "red4"))))
;; '(diff-hl-insert
;;   ((((background light)) (:background "light green"))
;;    (((background dark)) (:background "dark green"))))

(let ((off-white "#fffffc")     (empty-black "#1a1a1a")
      (faint-red "#ffe8e4")     (deep-red "#681924")
      (dark-blue "#4a567a")     (faint-blue "#d0e4ff")
      (dim-light "#b3b3b3")     (dim-dark "#1e1e1e")
      (light-shadow "#4d4d4d")  (dark-shadow "#bfbfbf")
      (yellow-hl "#ffffe0")
      (paper "#fdfcf5")
      (faint-gray "#f5f5f5")
      (select-light "#b3d7ff")  (select-dark "#3f638b")
      (match-light "#ffffe0")   (match-dark "#4b5d6b")
      (attn-light "cyan")       (attn-dark "red")
      (emph-light "cyan")       (emph-dark "red")
      (empty-light "#faf9f8")   (empty-dark "#141414")
      (hover-light "#c0ddf3")   (hover-dark "SlateGray")
      (string-light "#4a567a")  (string-dark "#d0e4ff")
      (item-light "#f3f3f3")    (item-dark "#555555"))
  (custom-theme-set-faces
   'stillpoint
   `(default
     ((((type tty))            nil)
      (((background dark))     (:foreground "white" :background ,empty-black))
      (t                       (:foreground "black" :background ,off-white))))
   `(cursor
     ((((background light))     (:background "black"))
      (((background dark))      (:background "white"))))
   `(escape-glyph
     ((default                  (:foreground "brown"))
      (((background dark))      (:foreground "cyan"))))
   `(homoglyph
     ((t                        nil)))
   `(minibuffer-prompt
     ((t                        (:weight bold))))
   `(highlight
     ((((background light))     (:background ,select-light))
      (((background dark))      (:background ,dark-blue))))
   `(region
     ((default                  (:extend t))
      (((type tty))             (:background "blue"))
      (((background light))     (:background ,select-light))
      (((background dark))      (:background ,select-dark))))
   `(shadow
     ((((background light))     (:foreground ,light-shadow))
      (((background dark))      (:foreground ,dark-shadow))))
   `(secondary-selection
     ((((type tty))             (:inverse-video t))
      (((background light))     (:background ,faint-blue))
      (((background dark))      (:background ,dark-blue))))
   `(trailing-whitespace
     ((t                        (:inverse-video t))))
   `(font-lock-builtin-face
     ((t                        nil)))
   `(font-lock-comment-delimiter-face
     ((t                        (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face
     ((t                        (:inherit (shadow) :slant italic))))
   `(font-lock-constant-face
     ((t                        nil)))
   `(font-lock-doc-face
     ((t                        (:inherit (font-lock-string-face) :slant italic))))
   `(font-lock-function-name-face
     ((((background dark))      (:background "gray15"))
      (((background light))     (:background "gray96"))))
   `(font-lock-keyword-face
     ((t                        (:weight bold))))
   `(font-lock-negation-char-face
     ((t                        nil)))
   `(font-lock-preprocessor-face
     ((t                        nil)))
   `(font-lock-regexp-grouping-backslash
     ((t                        nil)))
   `(font-lock-regexp-grouping-construct
     ((t                        nil)))
   `(font-lock-string-face
     ((((background light))     (:foreground ,string-light))
      (((background dark))      (:foreground ,string-dark))))
   `(font-lock-type-face
     ((t                        nil)))
   `(font-lock-variable-name-face
     ((t                        nil)))
   `(font-lock-warning-face
     ((((background dark))      (:background ,deep-red :weight bold))
      (t                        (:background ,faint-red :weight bold))))
   `(button
     ((t                        (:inherit (link)))))
   `(link
     ((t                        (:inherit (underline)))))
   `(link-visited
     ((default                  (:inherit (link)))
      (((background dark))      (:foreground "violet"))))
   `(fringe
     ((default                  (:background ,empty-light))
      (((background dark))      (:background ,empty-dark))))
   `(header-line
     ((default                  (:inherit (mode-line) :weight bold))
      (((type tty))             (:underline (:color foreground-color :style line)))))
   `(mode-line
     ((((type tty))             (:inverse-video t))
      (t                        (:overline t))))
   `(mode-line-inactive
     ((((type tty))             (:inherit (shadow)))
      (t                        (:foreground ,dim-light :inherit (mode-line)))))
   `(mode-line-buffer-id
     ((t                        (:weight bold))))
   `(mode-line-emphasis
     ((t                        (:weight bold))))
   `(mode-line-highlight
     ((t                        (:inherit (highlight)))))
   `(isearch
     ((t                        (:inverse-video t))))
   `(isearch-fail
     ((t                        (:inverse-video t))))
   `(lazy-highlight
     ((((background light))     (:background ,yellow-hl))
      (((background dark))      (:background ,match-dark))))
   `(match
     ((default                  (:background ,yellow-hl))
      (((background dark))      (:background ,match-dark))))
   `(next-error
     ((t                        (:inherit (region)))))
   `(query-replace
     ((t                        (:inherit (isearch)))))
   `(custom-button
     ((default                  (:box (:line-width (2 . 2) :style flat-button)))
      (((background light))     (:foreground "black" :background ,item-light))
      (((background dark))      (:foreground "white" :background ,item-dark))))
   `(custom-button-mouse
     ((((background light))     (:foreground "black" :background ,select-light))
      (((background dark))      (:foreground "white" :background ,select-dark))))
   `(custom-button-pressed
     ((t                        (:inherit (custom-button) :inverse-video t))))))

(provide-theme 'stillpoint)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; stillpoint-theme.el ends here
