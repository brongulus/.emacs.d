;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(doom/set-frame-opacity 95)

(setq-default
 auth-sources '("~/.authinfo")
 doom-theme 'doom-nord
 bookmark-file "~/.doom.d/bookmarks"
 display-line-numbers-type 'relative
 tab-width 2
 large-file-warning-threshold nil
 evil-split-window-below t
 evil-vsplit-window-right t)

;;FIXME
(add-hook! 'after-init-hook
  (add-hook 'post-command-hook #'recenter nil t)
  (remove-hook 'post-command-hook #'recenter t))

(map! :i "C-y" #'evil-paste-after)

(use-package windmove
  :bind
  (("S-<left>" . windmove-left)
   ("S-<right>" . windmove-right)
   ("S-<up>" . windmove-up)
   ("S-<down>" . windmove-down)))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(setq org-support-shift-select 'always)

(after! doom-modeline
  (remove-hook! 'doom-modeline-mode-hook #'column-number-mode)
  (setq-default doom-modeline-major-mode-color-icon t
                doom-modeline-buffer-file-name-style 'relative-to-project
                doom-modeline-buffer-encoding nil
                line-number-mode nil))

(custom-set-faces!
  '(mode-line :font "Open Sans" :weight regular))

;; TODO Popup Rules

;;; LSP

(setq lsp-ui-doc-enable t
      lsp-ui-doc-use-childframe t
      lsp-ui-doc-use-webkit nil
      lsp-enable-folding t)

(after! lsp-mode
  (require 'dap-cpptools))

(setq tramp-default-method "mosh"
      remote-file-name-inhibit-cache nil
      vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)
      tramp-verbose 6)

;; TODO Make it so that format on save is disabled for c++ buffers only over tramp
(setq +format-on-save-enabled-modes '(not c-mode c++-mode cpp-mode emacs-lisp-mode sql-mode tex-mode latex-mode org-msg-edit-mode))

;;; DAP

;; Resources
;; https://code.visualstudio.com/docs/editor/variables-reference
;; https://code.visualstudio.com/docs/editor/debugging

;; The best way to explain the difference between launch and attach is to think
;; of a launch configuration as a recipe for how to start your app in debug mode
;; before VS Code attaches to it, while an attach configuration is a recipe for
;; how to connect VS Code's debugger to an app or process that's already running.

(after! dap-cpptools
        (dap-register-debug-template "kinara-klib"
                                (list :type "cppdbg"
                                      :request "launch"
                                      :dap-compilation "make dir=cndw_5x5_p1_s2_ualgn_w32m_h1m_c1m_u8b_rel6 clean; make dir=cndw_5x5_p1_s2_ualgn_w32m_h1m_c1m_u8b_rel6 dis; make dir=cndw_5x5_p1_s2_ualgn_w32m_h1m_c1m_u8b_rel6 gdb"
                                      :dap-compilation-dir "/mosh:iprashant@vendor03.dc.kinara.ai:/auto/xwork/iprashant/klib/ara2/"))

        (dap-register-debug-template "test"
                                (list :type "cppdbg"
                                      :request "launch"
                                      :program "${fileDirname}${fileBasenameNoExtension}"
                                      :cwd "${workspaceFolder}"
                                      :dap-compilation "g++ -g ${file} -o ${fileBasenameNoExtension}"
                                      :dap-compilation-dir "${fileDirname}"
                                      ;; FIXME
                                      )))
