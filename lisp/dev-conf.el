;;;; dev-conf -*- lexical-binding: t -*-

(if package-quickstart
    (let ((load-source-file-function nil))
      (package-activate-all))
  (package-initialize))

(with-eval-after-load 'package
  (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (push '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)
  (setq package-native-compile t
        package-install-upgrade-built-in t
        package-check-signature nil))

;; Auto-install function
(defun my/ensure-package-installed (&rest packages)
  "Ensure PACKAGES are installed, install if missing."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (unless package-archive-contents
         (package-refresh-contents))
       (package-install package)))
   packages))

(my/ensure-package-installed 'corfu 'eldoc-box 'markdown-mode 'dape 'ox-hugo) ;; 'diff-hl

(with-eval-after-load 'corfu
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map [tab] #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map [backtab] #'corfu-previous)
  (dolist (fn '("<next-line>" "<forward-line>" "<backward-char>" "<previous-line>"))
    (keymap-unset corfu-map (concat "<remap> " fn)))
  (add-hook 'eshell-mode #'(lambda () (setq-local corfu-auto nil) (corfu-mode)))
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  (setq completion-ignore-case t)
  (with-eval-after-load 'dabbrev
    (push 'pdf-view-mode dabbrev-ignored-buffer-modes))
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.2
        corfu-separator 32
        corfu-max-width 80
        corfu-preselect 'prompt
        corfu-quit-no-match t
        corfu-quit-at-boundary 'separator
        corfu-preview-current nil
        corfu-popupinfo-delay '(0.2 . 0.1)
        corfu-preselect-first nil))

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
    (setq eldoc-box-max-pixel-width 800
          eldoc-box-max-pixel-height 700
          eldoc-box-only-multi-line t)
    (setq eldoc-doc-buffer-separator
          (concat "\n"
                  (propertize "-" 'display '(space :align-to right)
                              'face '(:strike-through t)
                              'font-lock-face '(:strike-through t))
                  "\n"))))

(push '("\\.md\\'" . markdown-mode) auto-mode-alist)
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'(lambda nil
                                    (when (display-graphic-p) (markdown-toggle-inline-images))))
  (setq markdown-fontify-code-blocks-natively t
        markdown-max-image-size '(800 . 800)))

(with-eval-after-load 'ox
  (require 'ox-hugo))

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

