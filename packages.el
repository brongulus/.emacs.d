;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
;
;;##########PACKAGES###########
(unpin! doom-themes)
;; (package! solaire-mode :disable t)

(package! gruber-darker-theme)
(package! org-fragtog)
(package! anki-editor)
(package! geiser)
(package! elvish-mode)
(package! dirvish)
(package! oj)
(package! guix)
(package! iscroll)
(package! ox-hugo)
(package! info-colors)
(package! yequake)
(package! devdocs)
(package! debbugs)
(package! conllu-mode)
(package! graphviz-dot-mode)
(package! imenu-list)
(package! nano-agenda)
;; (package! leetcode)
;; (package! pocket-reader)
;; (package! vuiet)
;; (package! emms)
;; (package! org-ql)
(package! lexic
  :recipe (:host github :repo "tecosaur/lexic"))
(package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))
;; FIXME / TESTING
(package! pdf-continuous-scroll-mode
  :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el"))
(package! org-ol-tree
  :recipe (:host github :repo "Townk/org-ol-tree"))
(package! movie
  :recipe (:host github :repo "larsmagne/movie.el"))
(package! pvr
  :recipe (:host github :repo "larsmagne/pvr.el"))
(package! imdb
  :recipe (:host github :repo "larsmagne/imdb.el"))
(package! touchgrid
  :recipe (:host github :repo "larsmagne/touchgrid.el"))
(package! info-variable-pitch
  :recipe (:type git :host github
   :repo "kisaragi-hiu/info-variable-pitch"))
;; If I end up testing gccemacs
(package! webkit
 :recipe (:type git :host github :repo "akirakyle/emacs-webkit"
          :branch "main" :files (:defaults "*.js" "*.css" "*.so")
          :pre-build ("make")))


;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
