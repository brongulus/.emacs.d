;; -*- coding: utf-8; lexical-binding: t -*-
;;; --------------------------
;;; Defaults? Better? Maybe...
;;; --------------------------
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(use-package package
  :ensure nil
  :init (package-initialize)
  :config
  ;; (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (unless package-archive-contents
    (package-refresh-contents))
  (setq package-native-compile t
        package-install-upgrade-built-in t
        package-check-signature nil
        use-package-always-ensure t
        use-package-always-defer t
        use-package-enable-imenu-support t
        use-package-expand-minimally t))

;; Android
(when (string-equal system-type "android")
  (let ((termuxpath "/data/data/com.termux/files/usr/"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath "bin"))
    (setenv "LD_LIBRARY_PATH" (concat (getenv "LD_LIBRARY_PATH") ":" termuxpath "lib"))
    (push (concat termuxpath "bin") exec-path))
  (setq overriding-text-conversion-style nil)
  (set-frame-font "monospace 16" nil t))

(when (member "VictorMono Nerd Font Mono" (font-family-list))
  (set-frame-font "VictorMono Nerd Font Mono 16" nil t))
;; (set-face-attribute
;;  'variable-pitch nil :family "Roboto" :height 180)

(setq-default default-frame-alist
              '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars)
                (left-fringe . 0)
                (right-fringe . 0)
                (internal-border-width . 20)
                (fullscreen . fullboth))
              cursor-in-non-selected-windows nil
              bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right
              ;; misc
              line-spacing 3
              cursor-type 'bar
              tab-width 2
              indent-tabs-mode nil
              enable-recursive-minibuffers t
              show-paren-delay 0
              custom-safe-themes t
              ring-bell-function 'ignore
              use-short-answers t
              initial-major-mode 'fundamental-mode
              ;; dired
              dired-dwim-target t
              dired-auto-revert-buffer t
              dired-mouse-drag-files t
              mouse-drag-and-drop-region-cross-program t
              dired-kill-when-opening-new-dired-buffer t
              dired-recursive-deletes 'always
              dired-recursive-copies 'always
              ;; tab-bar
              tab-bar-close-button-show nil
              tab-bar-new-button-show nil
              tab-bar-show 1
              tab-bar-separator "")

;;; Misc
(setq inhibit-startup-screen t
      load-prefer-newer t
      make-backup-files nil
      create-lockfiles nil
      uniquify-buffer-name-style 'forward
      global-auto-revert-non-file-buffers t
      flymake-suppress-zero-counters t)

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep
        xref-auto-jump-to-first-xref nil ; 'move
        xref-show-definitions-function 'xref-show-definitions-completing-read
        xref-show-xrefs-function 'xref-show-definitions-completing-read))
;; #'dabbrev-completion resets the global variables first so we do the same
(advice-add #'dabbrev-capf :before #'dabbrev--reset-global-variables)
(add-hook 'completion-at-point-functions #'dabbrev-capf 100)

(fset 'display-startup-echo-area-message'ignore)
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))
(add-hook 'prog-mode-hook #'flymake-mode)
(set-display-table-slot standard-display-table 'truncation 32) ;; hides $
(set-display-table-slot standard-display-table 'wrap 32) ;; hides \
(save-place-mode 1)
(savehist-mode)
(blink-cursor-mode -1)
(global-auto-revert-mode t)
(global-set-key [remap kill-buffer] 'kill-this-buffer)
;; Load theme based on the time of the day
(let ((hour (substring (current-time-string) 11 13)))
  (if (and (string-lessp hour "17") (string-greaterp hour "08"))
      (load-theme 'alabaster :no-confirm)
    (load-theme 'dracula :no-confirm)))
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "M-s r") #'query-replace)
(global-set-key (kbd "M-s R") #'query-replace-regexp)
(global-set-key (kbd "C-h '") #'describe-face)
(global-set-key (kbd "C--") #'(lambda () (interactive) (dired "/data/data/com.termux/files/home/")))
(global-set-key (kbd "C-'") #'(lambda () (interactive)
                                (term "/data/data/com.termux/files/usr/bin/fish")))
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'ediff
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-local display-line-numbers nil))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

;;; -------------------------------------------------------------
;;; Built-in packages (icomplete, project, recentf, dired, ediff)
;;; -------------------------------------------------------------
(use-package icomplete
  :init
  (fido-vertical-mode)
  :bind (:map icomplete-fido-mode-map
              ("<backspace>" . icomplete-fido-backward-updir)
              ("TAB" . icomplete-forward-completions)
              ("<backtab>" . icomplete-backward-completions))
  :bind (:map icomplete-minibuffer-map
              ("C-," . embark-act))
  :hook (icomplete-minibuffer-setup . (lambda ()
                                        (setq-local completion-styles '(orderless basic))))
  :config ;; src: https://www.reddit.com/r/emacs/comments/zl6amy/
  (defun completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
   Use as a value for `completion-in-region-function'."
    (let* ((initial (buffer-substring-no-properties start end))
           (all (completion-all-completions initial collection predicate
                                            (length initial)))
           (completion (catch 'done
                         (atomic-change-group
                           (let ((completion
                                  (completing-read "Completion: "
                                                   collection predicate nil initial)))
                             (throw 'done completion))))))
      (cond (completion (completion--replace start end completion) t)
            (t (message "No completion") nil))))
  (setq completion-in-region-function #'completing-read-in-region
        tab-always-indent 'complete
        icomplete-in-buffer t
        completions-group t
        icomplete-compute-delay 0))

(use-package vc
  :defer 1
  :ensure nil
  :bind (("C-x v f" . (lambda () (interactive)
                        (vc-git-push t)))
         ("C-x v e" . vc-ediff)
         ("C-x v R" . vc-interactive-rebase))
  :config
  (defun vc-interactive-rebase ()
    (interactive)
    (with-current-buffer (eshell)
      (eshell-return-to-prompt)
      (insert "git rebase -i")
      (eshell-send-input)))
  
  (setq vc-handled-backends '(Git)
        vc-follow-symlinks t
        project-vc-merge-submodules nil
        vc-annotate-background-mode t)
  
  (if (string= (car custom-enabled-themes) "dracula")
      (add-hook 'vc-annotate-mode-hook
                (lambda ()
                  (face-remap-add-relative 'default :foreground "black")))))

(use-package isearch
  :ensure nil
  :custom
  (isearch-wrap-pause 'no)
  (isearch-lazy-count t)
  (search-whitespace-regexp ".*?"))

(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :bind ("C-x f" . #'recentf-open)
  :custom
  (recentf-max-menu-items 25)
  (recentf-auto-cleanup 'never))

(use-package tramp
  :config
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=\~/.ssh/control/ssh-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes")
        tramp-default-method "ssh"
        remote-file-name-inhibit-cache nil
        remote-file-name-inhibit-locks t
        tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
;;; ---------------------------------------------------------------------------------
;;; ELPA packages (popper, orderless, marginalia, embark, meow, eat, undo-fu+session)
;;; ---------------------------------------------------------------------------------
(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          "magit:.\*"
          "\\*eat\\*"
          "vc-git :.\*"
          "\\*Warnings\\*"
          "\\*Occur\\*"
          compilation-mode
          "^\\*eshell.*\\*$" eshell-mode
          "^\\*term.*\\*$" term-mode)
        popper-mode-line nil
        popper-window-height 0.33)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package orderless
  :after minibuffer
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :init (marginalia-mode)
  :config
  (setq marginalia-annotator-registry
      (assq-delete-all 'file marginalia-annotator-registry)))

(use-package embark
  :after minibuffer
  :bind ("C-," . embark-act)
  :config
  (setq embark-prompter 'embark-completing-read-prompter
        embark-indicators (delete 'embark-mixed-indicator embark-indicators)))

(use-package undo-fu
  :demand t
  :bind (("C-x u" . undo-fu-only-undo)
         ("C-z" . undo-fu-only-redo))
  :config
  (setq undo-limit 67108864
        undo-strong-limit 100663296
        undo-outer-limit 1006632960))

(use-package undo-fu-session
  :demand t
  :init
  (unless (string-equal system-type "android")
    (undo-fu-session-global-mode)))

(use-package eat
  :bind ("C-." . #'eat)
  :custom
  (eat-kill-buffer-on-exit t))

(use-package with-editor
  :defer 1
  :hook ((eshell-mode . with-editor-export-git-editor)
         (eshell-mode . with-editor-export-editor)))

(defun my-jk () ;; src: wasamasa
  (interactive)
  (let* ((initial-key ?j)
         (final-key ?k)
         (timeout 0.5)
         (event (read-event nil nil timeout)))
    (if event ;; timeout met
        (if (and (characterp event) (= event final-key))
            (meow-insert-exit)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(use-package meow
  :demand t
  :init (meow-global-mode 1)
  :config
  (defun meow-setup()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
          meow-keypad-leader-dispatch ctl-x-map)
    (define-key meow-insert-state-keymap (kbd "j") #'my-jk)
    (with-eval-after-load 'dired
      (define-key dired-mode-map "-" 'dired-up-directory)
      (define-key dired-mode-map "E" 'wdired-change-to-wdired-mode))
    (global-set-key (kbd "M-<right>") 'windmove-right)
    (global-set-key (kbd "M-<left>") 'windmove-left)
    (global-set-key (kbd "M-<up>") 'windmove-up)
    (global-set-key (kbd "M-<down>") 'windmove-down)
    (push '(eat-mode . insert) meow-mode-state-list)
    (push '(eshell-mode . insert) meow-mode-state-list)
    (push '(log-edit-mode . insert) meow-mode-state-list)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . meow-comment)
     '("d" . meow-kill)
     '("D" . meow-page-down)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-visit)
     '("t" . meow-till)
     '("u" . undo-fu-only-undo)
     '("U" . meow-page-up)
     '("v" . meow-line)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-delete)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("Z" . undo-fu-only-redo)
     '("+" . meow-block)
     '("-" . negative-argument)
     '("\\" . dired-jump)
     '("*" . isearch-forward-symbol-at-point)
     '("/" . isearch-forward)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup))

;;; -------------------------------------------------
;;; Competitive programming setup (snippets and foxy)
;;; -------------------------------------------------
(define-skeleton cpp-header "Base c++ template for competitive programming." nil
  "#include<bits/stdc++.h>\n"
  "using namespace std;\n"
  "\n#ifdef LOCAL\n"
  "#include \"algo/debug.h\"\n"
  "#else\n"
  "#define debug(...) 42\n"
  "#endif\n"
  "\nint main () {\n"
  "\tios::sync_with_stdio(0);\n"
  "\tcin.tie(0);\n"
  "\t" _ "\n"
  "}")

(define-skeleton cpp-for-loop
  "Insert a C++ for loop with user-defined iterator and termination variable." ""
  > "for (int " (setq iterator (read-char "Iterator variable: ")) " = 0; " iterator " < "
  > (read-char "Termination: ") "; " iterator "++) {\n\t\t"
  > _ "\n}"
  > (indent-according-to-mode)
  > (delete-char -1))

(define-skeleton cpp-tests "Run multiple testcases." ""
  > "int tt = 0; cin >> tt;\n\t"
  > "while(tt--) {\n\t\t"
  > _ "\n}"
  > (indent-according-to-mode)
  > (delete-char -1))

(define-skeleton cpp-all "Run from beginning to end of iterator." ""
  > (setq var (read-from-minibuffer "")) ".begin(), " var ".end()"
  > (forward-char)
  > _)

(setq save-abbrevs nil)
(defun init-c++-abbrevs ()
  (define-abbrev c++-mode-abbrev-table "gtc" "" 'cpp-header)
  (define-abbrev c++-mode-abbrev-table "forl" "" 'cpp-for-loop)
  (define-abbrev c++-mode-abbrev-table "all" "" 'cpp-all)
  (define-abbrev c++-mode-abbrev-table "ttt" "" 'cpp-tests))
(add-hook 'c++-mode-hook 'abbrev-mode)
(add-hook 'c++-mode-hook 'init-c++-abbrevs)

(load "~/.emacs.d/foxy" nil t)
(setq foxy-compile-command "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion -O2 ")

(setq delete-active-region t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(benchmark-init with-editor avy eat howm undo-fu undo-fu-session embark marginalia meow orderless popper)))
