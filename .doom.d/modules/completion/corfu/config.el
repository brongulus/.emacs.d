;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;; Everything's from example configs at this point

(use-package! corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-preselect-first nil)    ;; Disable Candidate Preselection
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary 'separator)     ;; Automatically quit at word boundary
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
        ("C-j" . corfu-next) ;; Org down element clashing
        ("S-TAB" . corfu-previous)
        ("C-k" . corfu-previous)
        ([backtab] . corfu-previous)
        ("SPC" . corfu-insert-separator))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :hook (doom-first-input . global-corfu-mode)

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

;; Add extensions

;; FIXME Company dependency and cape--bounds undefined
;; (defun cape--yasnippet-snippets (prefix)
;;   "Return all snippets from yasnippet matching PREFIX"
;;   (require 'yasnippet)
;;   ;; `company-yasnippet--candidates'
;;   (cl-loop with tables = (yas--get-snippet-tables)
;;            for key-prefix in (company-yasnippet--key-prefixes)
;;            ;; Only consider keys at least as long as the symbol at point.
;;            when (>= (length key-prefix) (length prefix))
;;            thereis (company-yasnippet--completions-for-prefix prefix
;;                                                               key-prefix
;;                                                               tables)))

;; (defvar cape--yasnippet-properties
;;   (list :annotation-function (lambda (_) " snippet")
;;         :company-kind (lambda (_) 'snippet)
;;         :exclusive 'no)
;;   "Completion extra properties for `cape-yasnippet'.")

;; (defun cape-yasnippet (&optional interactive)
;;   "Complete snippet at point with yasnippet.
;; If INTERACTIVE is nil the function acts like a Capf."
;;   (interactive (list t))
;;   (if interactive
;;       (cape--interactive #'cape-yasnippet)
;;     (let ((bounds (cape--bounds 'word)))
;;       `(,(car bounds) ,(cdr bounds)
;;         ,(cape--table-with-properties
;;           (cape--cached-table (car bounds) (cdr bounds) #'cape--yasnippet-snippets 'substring)
;;           :category 'cape-yasnippet)
;;         ,@cape--yasnippet-properties))))

;; (setq-local completion-at-point-functions '(cape-yasnippet cape-symbol))


(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c c p" . completion-at-point) ;; capf
         ("C-c c t" . complete-tag)        ;; etags
         ("C-c c d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)        ;; Eshell, Comint or miibuffer history
         ("C-c c f" . cape-file)
         ("C-c c k" . cape-keyword)
         ("C-c c s" . cape-symbol)         ;; Elisp symbols doc
         ("C-c c a" . cape-abbrev)
         ("C-c c i" . cape-ispell)
         ("C-c c l" . cape-line)
         ("C-c c w" . cape-dict)
         ("C-c c \\" . cape-tex)           ;; Unicode char from TeX command
         ("C-c c _" . cape-tex)
         ("C-c c ^" . cape-tex)
         ("C-c c &" . cape-sgml)
         ("C-c c r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-yasnippet) ;; FIXME
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; Use Company backends as Capfs.
  (cl-loop for backend in '(company-yasnippet company-shell)
             do (add-hook 'completion-at-point-functions
                          (cape-company-to-capf backend)))
  ;; (setq-local completion-at-point-functions
  ;;       (mapcar #'cape-company-to-capf
  ;;               (list #'company-files #'company-ispell #'company-dabbrev)))
)

;; Extras

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
  (setq kind-icon-blend-background nil
        kind-icon-blend-frac 0.0)
  (setq svg-lib-icons-dir
        (expand-file-name (concat user-emacs-directory ".local/cache/svg-lib/")))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
 )

(unless (display-graphic-p)
  (corfu-terminal-mode +1))
