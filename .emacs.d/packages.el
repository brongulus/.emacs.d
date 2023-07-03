(push (expand-file-name "el-get/el-get" user-emacs-directory) load-path)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(with-eval-after-load 'el-get-git
  (setq el-get-git-shallow-clone t))

(push "~/.emacs.d/el-get-user/recipes" el-get-recipe-path)
(setq el-get-is-lazy t)

(el-get-bundle undo-fu)
(el-get-bundle undo-fu-session)
(el-get-bundle elpa:evil)
(el-get-bundle evil-escape)

(el-get-bundle compat) ;; for minad's pkgs
(el-get-bundle vertico)
(el-get-bundle marginalia)
(el-get-bundle orderless)
(el-get-bundle consult)
;; Helpfuls's requires aren't being pulled by el-get
(el-get-bundle f)
(el-get-bundle s)
(el-get-bundle elisp-refs)
(el-get-bundle helpful)

(el-get-bundle corfu)
(el-get-bundle popon)
(el-get-bundle corfu-terminal)
(el-get-bundle cape)
(el-get-bundle which-key)

(el-get-bundle git-gutter)
(el-get-bundle magit/transient)
(el-get-bundle magit/ghub)
(el-get-bundle magit/magit-popup)
(el-get-bundle magit/with-editor)
(el-get-bundle magit/magit)
(el-get-bundle magit/forge)
;; terminal stuff, taken from doom (cursor, cliboard)
(el-get-bundle xclip)
(el-get-bundle evil-terminal-cursor-changer)
(el-get-bundle tempel)
(el-get-bundle expand-region)
;; Remove project import after 29
(el-get-bundle! project
  :url "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/progmodes/project.el")
(el-get-bundle rustic)
(el-get-bundle reformatter)
(el-get-bundle zig-mode)
(el-get-bundle external-completion)
(el-get-bundle eldoc-box)
(el-get-bundle! eglot)

(el-get-bundle! damiencollard/nice-citation) ;; gnus
(el-get-bundle dirvish)
(el-get-bundle jdtsmith/outli)

(el-get-bundle log4e)
(el-get-bundle aio)
(el-get-bundle graphql)
(el-get-bundle leetcode)
(el-get-bundle! smart-compile
  :url "https://raw.githubusercontent.com/zenitani/elisp/master/smart-compile.el")
;; (el-get-bundle engrave-faces)
(el-get 'sync)
