;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;; Everything's from example configs at this point

(use-package! corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-preselect-first nil)    ;; Disable Candidate Preselection
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :hook (doom-first-input . corfu-global-mode)

  ;;:init
  ;; (when (featurep! +lsp)
  ;;   ())

  :config
  )

;; (add-hook 'eshell-mode-hook
;;         (lambda ()
;;           (setq-local corfu-cycle t
;;                       corfu-auto t
;;                       corfu-quit-at-boundary t
;;                       corfu-quit-no-match t)
;;           (corfu-mode)))


(use-package! corfu-doc
  :after corfu
  :hook ((corfu-mode-hook . corfu-doc-mode))
  :bind
  (:map corfu-map
        ("M-p" . corfu-doc-scroll-down)
        ("M-n" . corfu-doc-scroll-up)
        ("M-d" . corfu-doc-toggle)))

(use-package! kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (setq svg-lib-icons-dir
        (expand-file-name (concat user-emacs-directory ".local/cache/svg-lib/")))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
 )
