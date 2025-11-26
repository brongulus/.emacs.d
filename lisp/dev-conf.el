;;;; dev-conf -*- lexical-binding: t -*-
(when t
  (defvar package-quickstart)
  (setq package-quickstart t))

(if package-quickstart
    (let ((load-source-file-function nil))
      (package-activate-all))
  (package-initialize))

(with-eval-after-load 'package
  (setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
        package-archive-priorities '(("gnu" . 3) ("nongnu" . 2)
                                     ("melpa" . 1) ("melpa-stable" . 0)))
  (setq package-native-compile t
        package-install-upgrade-built-in t
        package-check-signature nil))

;; Auto-install function
(defun my/ensure-package-installed (&rest packages)
  "Ensure PACKAGES are installed, install if missing."
  (mapcar
   (lambda (package)
     (let* ((pkg-name (if (listp package) (car package) package))
            (is-vc-package (or (stringp package)
                               (and (listp package) (plist-get (cdr package) :url)))))
       (unless (locate-library (symbol-name pkg-name))
         ;; (package-installed-p pkg-name)
         (if is-vc-package
             (if (listp package)
                 (package-vc-install (plist-get (cdr package) :url))
               (package-vc-install package))
           (progn
             (unless package-archive-contents
               (package-refresh-contents))
             (package-install package))))))
   packages))

(my/ensure-package-installed ;; 'diff-hl
 'consult-eglot 'corfu 'markdown-mode 'dape 'ox-hugo 'zig-mode 'nov 'pr-review 'eldoc-box)

;; pr-review needs (info "(forge) Setup for Githubcom")
;; C-c C- {c (comment) s (action) e (edit) d (ediff) f (goto file)
;;         j (react) l (label) o (browser) q (req-review) v (view file)}
(setq pr-review-ghub-username "takoverflow")
(with-eval-after-load 'viper-ex
  (nconc ex-token-alist
         '(("prr" (call-interactively 'pr-review))
           ("prs" (call-interactively 'pr-review-search))
           ("prn" (call-interactively 'pr-review-notification)))))
(add-to-list 'browse-url-default-handlers
             '(pr-review-url-parse . pr-review-open-url))
(with-eval-after-load 'pr-review
  (define-key pr-review-mode-map (kbd "SPC") ctl-x-map))
(with-eval-after-load 'project
  (when (locate-library "magit")
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))))
(with-eval-after-load 'magit
  (transient-bind-q-to-quit)
  (defun magit-forge-kill-buffers () ;src: manuel-uberti
    "Restore window configuration and kill all Magit & Forge buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers))
          (forge-buffers
           (seq-filter (lambda (buf)
                         (with-current-buffer buf
                           (member major-mode '(forge-pullreq-mode
                                                forge-issue-mode
                                                forge-topics-mode
                                                forge-post-mode
                                                forge-motifications-mode
                                                forge-repository-list-mode))))
                       (buffer-list))))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)
      (mapc #'kill-buffer forge-buffers)))
  (define-key magit-status-mode-map (kbd "q") #'magit-forge-kill-buffers))

(define-key (current-global-map) (kbd "C-x S") #'consult-eglot-symbols)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map [remap xref-find-apropos] #'consult-eglot-symbols))
(with-eval-after-load 'ox
  (require 'ox-hugo))
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-header-line-format nil)
(with-eval-after-load 'nov
  (define-key nov-mode-map (kbd "SPC") ctl-x-map)
  (define-key nov-mode-map (kbd "#") #'definition-at-point))

(with-eval-after-load 'corfu
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map [tab] #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map [backtab] #'corfu-previous)
  (dolist (fn '("<next-line>" "<forward-line>" "<backward-char>" "<previous-line>"))
    (keymap-unset corfu-map (concat "<remap> " fn)))
  (add-hook 'eshell-mode-hook #'(lambda () (setq-local corfu-auto nil) (corfu-mode)))
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  (setq completion-ignore-case t)
  (with-eval-after-load 'dabbrev
    (push 'pdf-view-mode dabbrev-ignored-buffer-modes))
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.5
        corfu-separator 32
        corfu-max-width 80
        corfu-preselect 'prompt
        corfu-quit-no-match t
        corfu-quit-at-boundary 'separator
        corfu-preview-current nil
        corfu-popupinfo-delay '(0.5 . 0.3)
        corfu-preselect-first nil))

(setq eldoc-box-clear-with-C-g t)
(define-key (current-global-map) (kbd "s-<mouse-1>") #'my/eldoc-get-help)
(defun my/eldoc-get-help ()
  (interactive)
  (if (derived-mode-p 'emacs-lisp-mode)
      (describe-symbol (symbol-at-point))
    (if (and (display-graphic-p)
             (package-installed-p 'eldoc-box))
        (eldoc-box-help-at-point)
      (eldoc-doc-buffer t))))
(with-eval-after-load 'eldoc
  (with-eval-after-load 'eldoc-box
    (define-key (current-global-map) (kbd "C-;")
                (lambda nil (interactive) (eldoc-box-scroll-up 5)))
    (define-key (current-global-map) (kbd "C-'")
                (lambda nil (interactive) (eldoc-box-scroll-down 5)))
    (setq eldoc-box-max-pixel-width 800
          eldoc-box-max-pixel-height 700
          eldoc-box-only-multi-line t)))

;; Src: https://github.com/joaotavora/eglot/discussions/1238#discussioncomment-13365314
(defun my-markdown-follow-help-or-link-at-point-advice (orig-fun &rest args)
  "Prefer to use the help-echo property as `browse-url' target."
  (let* ((event-win (posn-window (event-start last-input-event)))
         (help-echo (with-selected-frame (window-frame event-win)
                      (with-current-buffer (window-buffer event-win)
                        (get-text-property (point) 'help-echo))))
         (help-is-url (url-type (url-generic-parse-url help-echo))))
    (message "if %s (browse-url %S)" help-is-url help-echo)
    (if help-is-url
        (browse-url help-echo)
      (apply orig-fun args))))

(push '("\\..?md\\'" . markdown-mode) auto-mode-alist)
(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-follow-link-at-point
              :around #'my-markdown-follow-help-or-link-at-point-advice)
  (add-hook 'markdown-mode-hook #'(lambda nil
                                    (visual-line-mode t)
                                    (when (display-graphic-p) (markdown-toggle-inline-images))))
  (dolist (level '("1" "2" "3" "4" "5" "6"))
    (set-face-attribute (intern (concat "markdown-header-face-" level)) nil :height 1.1 :inherit 'bold))
  (setq markdown-fontify-code-blocks-natively t
        markdown-max-image-size '(800 . 800)))

(setq dape-key-prefix "a")
(setq dape-debug t)
(define-key (current-global-map) (kbd "C-x a d") #'dape)
(autoload 'dape-breakpoint-toggle "dape")
(define-key (current-global-map) (kbd "C-x a b") #'dape-breakpoint-toggle)
(with-eval-after-load 'dape
  (define-key (dape-global-map) (kbd "f7") #'dape-step-in)
  (define-key (dape-global-map) (kbd "f8") #'dape-next)
  (define-key (dape-global-map) (kbd "f9") #'dape-continue)
  (add-hook 'dape-start-hook #'repeat-mode)
  (setq dape-breakpoint-margin-string (make-string 1 #x23fA)
        dape-inlay-hints t
        dape-buffer-window-arrangement 'right)
  (set-face-attribute 'dape-breakpoint-face nil :inherit 'compilation-mode-line-fail
                      :background (face-background 'default))
  (add-to-list 'dape-configs
               `(dlv-custom
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 command "dlv"
                 command-cwd dape-command-cwd
                 command-insert-stderr t
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 port :autoport
                 fn (lambda (config) ;; FIXME: check for different formats
                      (let* ((input (read-string "Config: " "dlv debug"))
                             (parts (split-string input "dlv debug" t))
                             (program (string-trim (car (split-string (cadr parts) "--" t))))
                             (args-string (cadr (split-string (cadr parts) "--" t)))
                             (args (if (string-empty-p args-string) []
                                     (vconcat (split-string args-string " " t))))
                             (env-string (car parts))
                             (env-vars
                              (if (string-empty-p env-string) nil
                                (let ((env-list '()))
                                  (dolist (pair (split-string env-string " " t))
                                    (let ((kv (split-string pair "=" t)))
                                      (when (>= (length kv) 2)
                                        (push (cadr kv) env-list)
                                        (push (intern (concat ":" (car kv))) env-list))))
                                  env-list))))
                        (when env-vars (plist-put config :env env-vars))
                        (plist-put config :program program)
                        (when (> (length args) 0) (plist-put config :args args))))
                 :request "launch"
                 :type (lambda() (if (string-suffix-p "_test.go" (buffer-name)) "test" "debug"))
                 :cwd dape-command-cwd)))

;; (dolist (hook '(prog-mode-hook conf-mode-hook))
;;   (add-hook hook #'turn-on-diff-hl-mode)
;;   (add-hook hook #'diff-hl-margin-mode)
;;   (add-hook hook #'diff-hl-show-hunk-mouse-mode))
;; (with-eval-after-load 'vc
;;   (define-key vc-prefix-map "*" #'diff-hl-show-hunk))
;; (with-eval-after-load 'diff-hl
;;   (dolist (pair '(("q" . diff-hl-inline-popup-hide)
;;                   ("r" . diff-hl-show-hunk-revert-hunk)))
;;     (let ((key (car pair))
;;           (fn (cdr pair)))
;;       (define-key diff-hl-inline-popup-transient-mode-map
;;                   (kbd key)
;;                   (lambda nil
;;                     "Clean up the littering diff-hl does by leaving its buffers after quitting."
;;                     (interactive)
;;                     (funcall fn)
;;                     (let ((diff-hl-buffers
;;                            (seq-filter
;;                             (lambda (buf)
;;                               (with-current-buffer buf
;;                                 (and (eq major-mode 'diff-mode)
;;                                      (string-match-p "*diff-hl-.*" (buffer-name buf)))))
;;                             (buffer-list))))
;;                       (mapc #'kill-buffer diff-hl-buffers))))))
;;   ;; (diff-hl-flydiff-mode t)
;;   (when (package-installed-p 'magit)
;;     (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
;;     (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
;;   (setq vc-git-diff-switches '("--histogram")
;;         diff-hl-flydiff-delay 0.5
;;         diff-hl-update-async nil;t
;;         diff-hl-show-staged-changes nil
;;         diff-hl-margin-symbols-alist '((insert . "█")
;;                                        (delete . "█")
;;                                        (change . "█"))
;;         diff-hl-draw-borders nil))


;; (autoload #'howm-menu "howm.el")
;; (define-key (current-global-map) (kbd "C-x , ;") #'howm-menu)
;; (setq howm-prefix (kbd "C-x ,")
;;       howm-directory "~/Dropbox/denote"
;;       howm-home-directory howm-directory
;;       howm-file-name-format "%Y%m%dT%H%M%S.org"
;;       howm-view-title-header "#+title:") ; "*"
;;       ;; howm-view-title-regexp "^#\\+[tT][iI][tT][lL][eE]:\\( +\\(.*\\)\\|\\)$"
;;       ;; howm-view-title-regexp-grep "^(#\\+[tT][iI][tT][lL][eE]:) +")
;; (with-eval-after-load 'howm
;;   ;; (setq howm-view-summary-sep "│"
;;   ;;       howm-menu-reminder-format "❱ %s │ %s"
;;   ;;       howm-menu-list-format
;;   ;;       (let* ((path (format-time-string howm-file-name-format))
;;   ;;              (width (length (file-name-sans-extension
;;   ;;                              (file-name-nondirectory path)))))
;;   ;;         (concat "❱ %-" (format "%s" width) "s │ %s"))
;;   ;;       howm-menu-list-regexp "^\\(❱\\([^│\r\n]*│\\)\\) +\\(.*\\)$")

;;   (advice-add 'howm-menu-copy-skel
;;               :filter-args
;;               (lambda (args)
;;                 (list (replace-regexp-in-string "^-\\{2,\\}$"
;;                   "─────────────────────────────────────────────────"
;;                   (car args))))))

;;; Kanata-mode?
(define-generic-mode kbd-mode
  '(";;" ("#|" . "|#"))
  nil
  '(("(\\(def[a-zA-Z-]+\\)\\>" 1 font-lock-builtin-face))
  '("\\.kbd\\'")
  (list (lambda () (run-hooks 'prog-mode-hook)))
  "Simple mode for kanata files.")

(add-to-list 'auto-mode-alist '("\\.kbd\\'" . kbd-mode))
