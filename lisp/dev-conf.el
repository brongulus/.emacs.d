;;;; dev-conf -*- lexical-binding: t -*-

(use-package package
  :ensure nil
  :init
  (if package-quickstart
      (let ((load-source-file-function nil))
        (package-activate-all))
    (package-initialize))
  :config
  (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (push '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)
  (setq package-native-compile t
        package-install-upgrade-built-in t
        package-check-signature nil))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :hook ((corfu-mode . corfu-popupinfo-mode))
         ;; (meow-insert-exit . corfu-quit))
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :config
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
        corfu-auto-delay 0.3
        corfu-separator 32
        corfu-max-width 80
        corfu-preselect 'prompt
        corfu-quit-no-match t
        corfu-quit-at-boundary 'separator
        corfu-preview-current nil
        corfu-popupinfo-delay '(0.5 . 0.1)
        corfu-preselect-first nil))

(use-package eldoc-box
  :after eldoc
  :ensure t
  :commands eldoc-box-help-at-point my/eldoc-get-help
  :bind (("s-<mouse-1>" . my/eldoc-get-help))
  :config
  (defun my/eldoc-get-help ()
    (interactive)
    (if (derived-mode-p 'emacs-lisp-mode)
        (describe-symbol (symbol-at-point))
      (if (and (display-graphic-p)
               (package-installed-p 'eldoc-box))
          (eldoc-box-help-at-point)
        (eldoc-doc-buffer t))))
  (setq eldoc-box-max-pixel-width 800
        eldoc-box-max-pixel-height 700
        eldoc-box-only-multi-line t)
  (setq eldoc-doc-buffer-separator
        (concat "\n"
                (propertize "-" 'display '(space :align-to right)
                            'face '(:strike-through t)
                            'font-lock-face '(:strike-through t))
                "\n")))

(use-package diff-hl
  :ensure t
  :hook (((prog-mode conf-mode) . turn-on-diff-hl-mode)
         ((prog-mode conf-mode) . diff-hl-margin-mode)
         ((prog-mode conf-mode) . diff-hl-show-hunk-mouse-mode))
  :config
  (dolist (pair '(("q" . diff-hl-inline-popup-hide)
                  ("r" . diff-hl-show-hunk-revert-hunk)))
    (let ((key (car pair))
          (fn (cdr pair)))
      (define-key diff-hl-inline-popup-transient-mode-map
                  (kbd key)
                  (lambda nil
                    "Clean up the littering diff-hl does by leaving its buffers after quitting."
                    (interactive)
                    (funcall fn)
                    (let ((diff-hl-buffers
                           (seq-filter
                            (lambda (buf)
                              (with-current-buffer buf
                                (and (eq major-mode 'diff-mode)
                                     (string-match-p "*diff-hl-.*" (buffer-name buf)))))
                            (buffer-list))))
                      (mapc #'kill-buffer diff-hl-buffers))))))
  (diff-hl-flydiff-mode t)
  (when (package-installed-p 'magit)
    (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (setq vc-git-diff-switches '("--histogram")
        diff-hl-flydiff-delay 0.5
        diff-hl-update-async nil;t
        diff-hl-show-staged-changes nil
        diff-hl-margin-symbols-alist '((insert . "█")
                                       (delete . "█")
                                       (change . "█"))
        diff-hl-draw-borders nil))

(use-package markdown-mode
  :ensure t
  ;; :hook (markdown-mode . visual-line-mode)
  :config
  (add-hook 'markdown-mode-hook #'(lambda nil
                                    (when (display-graphic-p) (markdown-toggle-inline-images))))
  (setq markdown-fontify-code-blocks-natively t
        markdown-max-image-size '(800 . 800)))
