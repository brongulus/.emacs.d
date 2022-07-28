;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (vertico +icons)
       (company +childframe)

       :ui
       doom
       hl-todo
       indent-guides
       modeline
       ophints
       (popup +defaults)
       (treemacs + lsp)
       vc-gutter
       workspaces

       :editor
       (evil +everywhere)
       fold
       word-wrap

       :emacs
       dired
       (undo +tree)
       vc

       :term
       vterm

       :checkers
       syntax

       :tools
       (debugger +lsp)
       direnv
       editorconfig
       (eval +overlay)
       (lookup +docsets)
       (lsp +peek)
       (magit +forge)
       make
       tree-sitter

       :lang
       (cc +lsp +tree-sitter)
       emacs-lisp
       (python +lsp +pyright +tree-sitter)
       (sh +tree-sitter)

       :config
       (default +bindings +smartparens)
       )
