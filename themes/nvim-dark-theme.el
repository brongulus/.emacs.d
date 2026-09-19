;;; nvim-dark-theme.el --- An Emacs theme that looks like Neovim's default theme -*- lexical-binding: t; -*-

(deftheme nvim-dark
  "An Emacs theme that looks like Neovim's default theme")

;; NvimDarkBlue     #004c63     rgb(0,76,115)       RGB_(0x00, 0x4c, 0x73)
;; NvimDarkCyan     #007373     rgb(0,115,115)      RGB_(0x00,0x73,0x73)
;; NvimDarkGreen    #005523     rgb(0,85,35)        RGB_(0x00,0x55,0x23)
;; NvimDarkMagenta  #470045     rgb(71,0,69)        RGB_(0x47,0x00,0x45)
;; NvimDarkRed      #590008     rgb(89,0,8)         RGB_(0x59,0x00,0x08)
;; NvimDarkYellow   #6b5300     rgb(107,83,0)       RGB_(0x6b,0x53,0x00)
;; NvimLightBlue    #A6DBFF     rgb(166,219,255)    RGB_(0xa6,0xdb,0xff)
;; NvimLightCyan    #8cf8f7     rgb(140,248,247)    RGB_(0x8c,0xf8,0xf7)
;; NvimLightGreen   #b4f6c0     rgb(179, 246, 192)  RGB_(0xb3, 0xf6, 0xc0)
;; NvimLightMagenta #FFCAFF     rgb(255, 202, 255)  RGB_(0xff, 0xca, 0xff)
;; NvimLightRed     #FFC0B9     rgb(255, 192, 185)  RGB_(0xff, 0xc0, 0xb9)
;; NvimLightYellow  #FCE094     rgb(252, 224, 148)  RGB_(0xfc, 0xe0, 0x94)
;; NvimLightGrey1   #EEF1F8     rgb(238,241,248)    RGB_(0xee, 0xf1, 0xf8)
;; NvimLightGrey2   #E0E2EA     rgb(224, 226, 234)  RGB_(0xe0, 0xe2, 0xea)
;; NvimLightGrey3   #C4C6CD     rgb(196, 198, 205)  RGB_(0xc4, 0xc6, 0xcd)
;; NvimLightGrey4   #9b9ea4     rgb(155, 158, 164)  RGB_(0x9b, 0x9e, 0xa4)
;; NvimDarkGrey4    #4f5258     rgb(79,82,88)       RGB_(0x4f,0x52,0x58)
;; NvimDarkGrey3    #2c2e33     rgb(44,46,51)       RGB_(0x2c,0x2e,0x33)
;; NvimDarkGrey2    #14161B     rgb(20,22,27)       RGB_(0x2c,0x2e,0x33)
;; NvimDarkGrey1    #07080D     rgb(7,8,13)         RGB_(0x07,0x08,0x0d)

(let ((bg        "#14161B")
      (fg        "#E0E2EA")
      (tabbg     "#2c2e33")
      (tabfg     "#C4C6CD")
      (cursor    "#e0e2ea")
      (comment   "#9b9ea4")
      (keyword)
      (string    "#b4f6c0")
      (function  "#8cf8f7")
      (variable)
      (type)
      (constant)
      (region    "#4f5258")
      (line      "#2c2e33")
      (warning   "#FCE094")
      (error     "#FFC0B9")
      (mismatch  "#590008"))

  (custom-theme-set-faces
   'nvim-dark

   ;; default
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,cursor))))
   `(region ((t (:background ,region))))
   `(highlight ((t (:background ,region))))
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(minibuffer-prompt ((t (:foreground ,keyword :weight bold))))

   ;; line number
   `(line-number ((t (:background ,bg :foreground ,comment))))
   `(line-number-current-line ((t (:background ,line :foreground ,fg :weight bold))))

   ;; syntax highlighting
   `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,keyword :weight bold))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-function-name-face ((t (:foreground ,function))))
   `(font-lock-function-call-face ((t (:foreground ,function))))
   `(font-lock-variable-name-face ((t (:foreground ,variable))))
   `(font-lock-type-face ((t (:foreground ,type))))
   `(font-lock-constant-face ((t (:foreground ,constant))))
   `(font-lock-builtin-face ((t (:foreground ,keyword))))
   `(font-lock-warning-face ((t (:foreground ,warning :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,keyword :weight bold))))

   ;; mode line
   `(mode-line ((t (:background ,region :foreground ,fg))))
   `(mode-line-inactive ((t (:background ,line :foreground ,comment))))

   ;; tab-bar-mode
   `(tab-bar ((t (:background ,tabbg))))
   `(tab-bar-tab ((t (:background ,bg :foreground ,fg :weight bold))))
   `(tab-bar-tab-inactive ((t (:background ,tabbg :foreground ,tabfg))))

   ;; tab-line-mode
   `(tab-line ((t (:background ,tabbg))))
   `(tab-line-tab-current ((t (:background ,bg :foreground ,fg :weight bold))))
   `(tab-line-tab-inactive ((t (:background ,tabbg :foreground ,tabfg))))
  
   ;; paren match
   `(show-paren-match ((t (:background ,region :foreground ,fg :weight bold))))
   `(show-paren-mismatch ((t (:background ,mismatch :foreground ,fg))))

   ;; search
   `(isearch ((t (:background ,warning :foreground ,bg :weight bold))))
   `(lazy-highlight ((t (:background ,region :foreground ,fg))))
   `(ido-only-match ((t (:foreground ,string :weight bold))))

   ;; error/warning
   `(error ((t (:foreground ,error :weight bold))))
   `(warning ((t (:foreground ,warning :weight bold))))
   `(success ((t (:foreground ,string :weight bold))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'nvim-dark)

;;; nvim-dark-theme.el ends here
