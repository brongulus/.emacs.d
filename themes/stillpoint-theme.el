;; -*- lexical-binding: t; -*-

(deftheme stillpoint "A minimalist theme.")

(add-to-list 'default-frame-alist
             '(internal-border-width . 15))

(custom-theme-set-variables
 'stillpoint
 '(mode-line-format
   '("%e " mode-line-front-space
     mode-line-buffer-identification " "
     mode-line-modified " "
     mode-line-position
     mode-line-format-right-align
     (project-mode-line project-mode-line-format)
     (vc-mode vc-mode) " "
     mode-line-modes " " mode-line-misc-info
     mode-line-end-spaces)))

(custom-theme-set-faces
 'stillpoint
 '(default
   ((((type tty))           nil)
    (((background light))   (:foreground "black" :background "white"))
    (((background dark))    (:foreground "white" :background "grey10"))))
 '(region
   ((default                (:extend t))
    (((background light))   (:background "lightskyblue2"))
    (((background dark))    (:background "steelblue4"))))
 '(font-lock-keyword-face
   ((t                      (:weight bold))))
 '(font-lock-variable-name-face
   ((t                      nil)))
 '(font-lock-string-face
   ((((background light))   (:foreground "paleturquoise4"))
    (((background dark))    (:foreground "paleturquoise1"))))
 '(font-lock-comment-face
   ((t                      (:inherit (shadow) :slant italic))))
 '(mode-line
   ((t                      (:overline t))))
 '(mode-line-inactive
   ((t                      (:inherit (mode-line shadow)))))
 '(tty-menu-enabled-face
   ((((background light))   (:background "grey80" :weight bold))
    (((background dark))    (:background "grey30" :weight bold))))
 '(tty-menu-disabled-face
   ((((background light))   (:background "grey70"))
    (((background dark))    (:background "grey20"))))
 '(tty-menu-selected-face
   ((t                      (:inherit hi-aquamarine :foreground "black"))))
 '(sh-quoted-exec
   ((t                      nil))))

(provide-theme 'stillpoint)
