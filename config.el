;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; TODO Fix treemacs symbols, indent guide continuation,
;; font size, variable pitch modeline, faces for palenight

;;; Commentary:
;; Hello there weary traveller? What brings you to these wild parts?
;; Stop and turn around, what lies ahead is madness and a pit of
;; despair and disappointments. Consider yourself warned.

;;; Basics
(setq-default
 user-full-name "Prashant Tak"
 user-mail-address "prashantrameshtak@gmail.com"
 forge-owned-accounts '(("brongulus"))
 auth-sources '("/home/prashant/.authinfo" "/home/prashant/.emacs.d/.local/etc/authinfo.gpg" "~/.authinfo.gpg")
 doom-font "Hack:pixelsize=15"
 doom-theme 'doom-palenight
 org-directory "/home/prashant/Dropbox/org/"
 bookmark-file "~/.doom.d/bookmarks"
 evil-escape-mode 1
 display-line-numbers-type 'relative
 tab-width 2
 doom-fallback-buffer-name "*doom*"
 which-key-idle-delay 0.5
 large-file-warning-threshold nil
 custom-file (expand-file-name ".custom.el" doom-private-dir)
 org-latex-toc-command "\\tableofcontents \\clearpage"
 window-resize-pixelwise nil
 evil-split-window-below t
 evil-vsplit-window-right t
 frame-resize-pixelwise nil
 treemacs-width 30
 doom-themes-treemacs-theme "doom-colors"
 +treemacs-git-mode 'deferred)

;; HACK Fixes font sizing issue
(add-to-list 'default-frame-alist '(font . "Hack-8"))

(when (file-exists-p custom-file)
  (load custom-file))

(add-hook! 'writeroom-mode-hook
  (if writeroom-mode
      (add-hook 'post-command-hook #'recenter nil t)
    (remove-hook 'post-command-hook #'recenter t)))

(setq yequake-frames
      '(("Yequake & scratch" .
         ((width . 0.75)
          (height . 0.5)
          (alpha . 0.90)
          (buffer-fns . ("~/doom-emacs/.local/straight/build-27.1/yequake/yequake.el"
                         split-window-horizontally
                         "*scratch*"))
          (frame-parameters . ((undecorated . t)))))))

(defun emacs-run-launcher ()
"Create and select a frame called emacs-run-launcher which
 consists only of a minibuffer and has specific dimensions.
 Run counsel-linux-app on that frame, which is an emacs
 command that prompts you to select an app and open it in a
 dmenu like behaviour. Delete the frame after that command has exited"
        (interactive)
        (with-selected-frame
            (make-frame '((name . "emacs-run-launcher")
                          (minibuffer . only)
                          (width . 120)
                          (height . 11)))
            (counsel-linux-app)
            (delete-frame)))


(add-to-list 'load-path "~/.doom.d/selectric")
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/movie.el")
(load! "~/.doom.d/openwith")
;; (require 'selectric-mode)
;; (selectric-mode 1)
(require 'movie)
(require 'openwith)
(add-hook 'dired-mode-hook 'openwith-mode 1)
(add-hook! 'ranger-mode-hook
  (setq hl-line-mode 1))

(defun hugo-build ()
  (interactive)
  (eshell-command "cd /mnt/manjaro/home/prashant/blog & hugo" nil))

(setq eshell-visual-commands
      '("spt" "ncmpcpp" "nvim" "vim" "vi" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm"))

(map! :i "C-y" #'evil-paste-after)
(map! :map image-mode-map
      :ni "r" #'image-rotate)

(after! evil
  (setq evil-move-cursor-back nil))

(after! warnings
        (add-to-list 'warning-suppress-types '(defvaralias)))

(add-hook! 'window-setup-hook
  (select-frame-set-input-focus (selected-frame)))

(when (featurep! :ui zen)
  (after! writeroom-mode
    (setq +zen-text-scale 0)
    (setq display-line-numbers nil)))

(defun save-and-close ()
  (interactive)
  (call-interactively 'save-buffer)
  (call-interactively 'kill-current-buffer))

(map! :n "SPC b w" #'save-and-close)

;; (doom/set-frame-opacity 98)
;; (add-hook! 'writeroom-mode-hook
;;   (doom/set-frame-opacity (if writeroom-mode 98 100)))

;;;; Outline minor mode
(defvar-local outline-folded nil)

(defun toggle-outline-entry (&optional arg)
        (interactive)
        (if (setq outline-folded (not outline-folded))
            (outline-show-subtree)
          (outline-hide-subtree)))

(add-hook! 'emacs-lisp-mode-hook #'outline-minor-mode)
(map! :map outline-minor-mode-map
      :n [backtab] #'outline-hide-body
      :n [tab] #'toggle-outline-entry
      :n "SPC m o" #'consult-outline)

(setq-default imenu-list-position 'left)
(setq imenu-list-position 'left)
(map! :map doom-leader-map
      "s i" nil
      "s i" #'imenu-list)

(add-hook! 'imenu-list-major-mode-hook #'hide-mode-line-mode)

;;; Webkit

(map! :n "SPC o w" 'webkit)

;; HACK add checks for last window then dont close workspace
;; TODO insert mode, vertico freeze fix
(defun webkit-close-tab(&optional arg)
  (interactive)
  (progn
    (kill-current-buffer)
    (+workspace/close-window-or-workspace)))

(map! :map webkit-mode-map :n "q" #'webkit-close-tab)

(use-package! webkit
  :init
  (when (eq window-system 'x)
        (modify-frame-parameters nil '((inhibit-double-buffering . t))))
  (require 'ol)
  ;; (setq webkit-own-window t) ;; Pls no
  :config
  (setq browse-url-browser-function 'webkit-browse-url
        webkit-cookie-file "~/.doom.d/webkit/cookies"
        webkit-history-file "~/.doom.d/webkit/history"
        webkit-browse-url-force-new t)
  (defun webkit--display-progress (progress)
  (setq webkit--progress-formatted
        (if (equal progress 100.0)
            ""
          (format "%s%.0f%%  " (all-the-icons-faicon "spinner") progress)))
  (force-mode-line-update)))

(use-package webkit-dark)

(use-package evil-collection-webkit
  :config
  (evil-collection-xwidget-setup))

;;; Modeline
(map! :n "SPC t m" #'hide-mode-line-mode)

(remove-hook! 'doom-modeline-mode-hook #'size-indication-mode)

(after! doom-modeline
  (remove-hook! 'doom-modeline-mode-hook #'column-number-mode)
  (doom-modeline-def-segment buffer-name
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-name)))
  (setq-default doom-modeline-major-mode-icon t
                doom-modeline-enable-word-count nil
                doom-modeline-buffer-encoding nil
                doom-modeline-buffer-file-name-style 'relative-to-project
                line-number-mode nil
                column-number-mode nil
                size-indication-mode nil))

;;; Org

(unless (display-graphic-p)
  (map! :map org-mode-map
        :ni "C-c C-<down>" '+org/insert-item-below
        :ni "C-c C-<up>" '+org/insert-item-above
        :ni "C-c C-<left>" 'org-insert-heading
        :ni "C-c C-<right>" 'org-insert-subheading))

(add-hook! 'org-mode-hook #'org-fragtog-mode)
(add-hook 'org-mode-hook
          (λ! (yas-minor-mode)
              (yas-activate-extra-mode 'latex-mode)))

(add-hook! 'org-mode-hook
    (add-hook 'post-command-hook #'recenter nil t))

(add-hook! 'org-mode-hook #'mixed-pitch-mode)

(setq org-fontify-quote-and-verse-blocks t
      yas-triggers-in-field t
      org-startup-with-inline-images t
      +latex-viewers nil
      flycheck-global-modes '(not LaTeX-mode latex-mode)
      org-latex-pdf-process '("tectonic -Z shell-escape --outdir=%o %f")
      org-preview-latex-default-process 'dvisvgm)

(after! org
  (plist-put org-format-latex-options :background "Transparent")
  (setq org-src-block-faces '(("latex" (:inherit default :extend t))))
  (setq org-format-latex-options '(:foreground default :background "Transparent" :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  )
(add-hook! 'doom-load-theme-hook
  (setq org-preview-latex-image-directory
        (concat doom-cache-dir "org-latex/" (symbol-name doom-theme) "/"))
  (dolist (buffer (doom-buffers-in-mode 'org-mode (buffer-list)))
    (with-current-buffer buffer
      (+org--toggle-inline-images-in-subtree (point-min) (point-max) 'refresh)
      (org-clear-latex-preview (point-min) (point-max))
      (org--latex-preview-region (point-min) (point-max))
      )))

(map! :after evil-org
      :map evil-org-mode-map
      :ni "C-RET"   #'+org/insert-item-below
      :ni "C-S-RET" #'+org/insert-item-above)

(setq org-agenda-start-with-log-mode t
      org-log-done t
      org-log-into-drawer t
      org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-files
      '("~/Dropbox/org/inbox.org"
        "~/Dropbox/org/todo.org"))

(setq org-agenda-custom-commands
      '(("A" "My agenda"
         ((todo "TODO" (
                        (org-agenda-overriding-header "⚡ TODAY:\n")
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format " %-15b")
                        (org-agenda-todo-keyword-format "")))
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "-1d")
                      (org-agenda-span 3)
                      (org-agenda-overriding-header "⚡ SCHEDULE:\n")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format " %-15b%t %s")
                      (org-agenda-todo-keyword-format "")
                      ;;         (org-agenda-time)
                      (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                      (org-agenda-scheduled-leaders '("" ""))
                      ;;       (org-agenda-deadline-leaders '("" ""))
                      (org-agenda-time-grid (quote ((today require-timed remove-match) (0800 1100 1400 1700 2000) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
                      )
                  )
          ;;(todo "NEXT" (
          ;;              (org-agenda-overriding-header "⚡ THIS WEEK:\n")
          ;;              (org-agenda-prefix-format " %b")
          ;;              (org-agenda-todo-keyword-format "")))
          ))))

(defun my-org-agenda-format-date-aligned (DATE)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name DATE 1 nil))
         (day (cadr DATE))
         (month (car DATE))
         (monthname (calendar-month-name month 1))
         ;;   (year (nth 2 DATE))
         )
    (format " %-2s. %2d %s"
            dayname day monthname)))

(setq org-agenda-format-date 'my-org-agenda-format-date-aligned
      org-agenda-block-separator (string-to-char " ")
      org-agenda-hidden-separator "‌‌ "
      +org-capture-readings-file "~/Dropbox/org/links.org"
      +org-capture-todo-file "~/Dropbox/org/inbox.org")

(after! org-capture
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "todo")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Notes")
           "* %u %?\n%i\n%a" :prepend t)
          ("r" "Readings" entry
           (file+headline +org-capture-readings-file "Readings")
           "* " :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n** What happened \n** What is going through your mind? \n** What emotions are you feeling? \n** What thought pattern do you recognize? \n** How can you think about the situation differently? " :prepend t)
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))
        ))

;;; Faces
;; Flatwhite
(custom-theme-set-faces! 'doom-flatwhite
  '(font-lock-comment-face  :background "#fcf2bf")
  '(hi-yellow :background "#d9c6c3")
  '(org-block :background "#fff9db")
  '(org-block-begin-line :background "#fff9db" :extend t)
  '(org-block-end-line :background "#fff9db" :extend t)
  '(cursor :background "#614c61")
  )

(custom-set-faces!
  '(mode-line :font "Open Sans" :weight regular)
  '(default :background nil)
  '(ein:cell-input-area :background "bg-alt" :extend t)
  '((font-lock-comment-face font-lock-doc-face) :slant italic)
  '(treemacs-git-unmodified-face :inherit treemacs-file-face)
  '(org-table :inherit 'fixed-pitch))

;;; Readers

(setq rmh-elfeed-org-files '("~/.doom.d/elfeed.org"))
(set-popup-rule! "^\\*elfeed-entry" :ignore t)
(after! elfeed
  (setq elfeed-search-filter "@2-month-ago")
  (add-hook! elfeed-show-mode-hook #'mixed-pitch-mode)
  (set-popup-rule! "^\\*elfeed-entry" :ignore t))
(add-hook! 'elfeed-show-mode-hook
  (setq left-margin-width 2))
(defun =elfeed ()
  (interactive)
  (elfeed)
  )

(add-hook! elfeed-show-mode-hook #'mixed-pitch-mode)
(map! :n "SPC o l" #'=elfeed)
(map! :map elfeed-search-mode-map :localleader "u" #'elfeed-update)

;; FIXME
(after! pocket-reader
  (set-evil-initial-state! 'pocket-reader-mode
    'insert))
(setq pocket-reader-open-url-default-function #'eww
      pocket-reader-pop-to-url-default-function #'eww)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(map! :map pdf-view-mode-map :localleader "h" #'pdf-annot-add-highlight-markup-annotation)

;;; Dashboard

(setq fancy-splash-image "~/.doom.d/sink.png")
(setq +doom-dashboard-menu-sections
      '(("Reload last session"
         :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
         :when (cond ((require 'persp-mode nil t)
                      (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                     ((require 'desktop nil t)
                      (file-exists-p (desktop-full-file-name))))
         :face (:inherit (doom-dashboard-menu-title bold))
         :action doom/quickload-session)
        ("Open notmuch"
         :icon (all-the-icons-octicon "mention" :face 'doom-dashboard-menu-title)
         :face (:inherit (doom-dashboard-menu-title bold))
         :action notmuch)
        ("Open browser"
         :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
         :face (:inherit (doom-dashboard-menu-title bold))
         :action webkit)
        ("Open Agenda"
         :icon (all-the-icons-octicon "check" :face 'doom-dashboard-menu-title)
         :face (:inherit (doom-dashboard-menu-title bold))
         :action org-agenda))
      )
(add-hook! '+doom-dashboard-mode-hook #'hide-mode-line-mode)

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'info-variable-pitch-mode)
;;; Window Management

;; FIXME
(set-popup-rule! "^\\*info\\*$" :ignore t)

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

(set-popup-rules!
  '(("^\\*Completions" :ignore t)
    ("^\\*[Hh]elp"
     :slot 2 :vslot -8 :size 0.35 :select t)
    ;; ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
    ;;  :vslot -11 :size 0.35 :select t)
    ;; ("^\\*info\\*$"  ; `Info-mode'
    ;;  :slot 2 :vslot 2 :size 0.45 :select t)
    ;;    ))
    ;;'(
    ("^\\*OrgOutlineTree\\:$" :side left)
    ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail\\|info\\|eww\\)\\*$" :ignore t)))

;;; Mail

;;(setq +notmuch-sync-backend 'mbsync)
(autoload 'notmuch "notmuch" "notmuch mail" t)
;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent
      user-mail-address "prashantrameshtak@gmail.com"
      user-full-name "Prashant Tak"
      ;; postponed message is put in the following draft directory
      message-auto-save-directory "~/.mail/gmail/draft"
      ;; change the directory to store the sent mail
      message-directory "~/.mail/gmail/")

(map! :n "SPC o n" 'notmuch)

;;; Code
;;;; LSP Test
(setq lsp-ui-doc-enable t
      lsp-ui-doc-use-childframe t
      lsp-ui-doc-use-webkit nil)

(defun lsp-ui-popup-focus(&optional arg)
  (interactive)
  (progn
    (lsp-ui-doc-show)
    (lsp-ui-doc-focus-frame)))

(defun lsp-ui-popup-kill(&optional arg)
  (interactive)
  (progn
    (lsp-ui-doc-unfocus-frame)
    (lsp-ui-doc-hide)))

(map! :map lsp-mode-map :localleader "k" #'lsp-ui-popup-focus)
(map! :map lsp-mode-map
      "ESC" nil
      "ESC" #'lsp-ui-popup-kill)

(after! lsp-mode
  (set-lookup-handlers! 'lsp-mode
    :documentation '(lsp-ui-doc-show :async t)))

;;;; Languages 
(defun java-ide-view-enable ()
  (interactive)
  (progn
    (lsp-treemacs-symbols)
    (lsp-treemacs-java-deps-list)))

(map! :map java-mode-map
      :n "SPC m p" #'java-ide-view-enable)

(set-file-template! "\\.pl$" :trigger "__pl" :mode 'perl-mode)
(add-to-list 'evil-insert-state-modes 'perl-mode)

(setq oj-home-dir "/mnt/Data/Documents/problems")

(defun end-process ()
  (interactive)
  (progn
    (delete-process (get-buffer-process (current-buffer)))
    (kill-current-buffer)
    (+workspace/close-window-or-workspace)))

(map! :map comint-mode-map
      :ni "q" #'end-process)
(map! :map compilation-mode-map
      :ni "q" #'+workspace/close-window-or-workspace
      :ni "<escape>" #'+workspace/close-window-or-workspace)

(add-hook! c++-mode
  ;; FIXED (Finally) Disable naive completion of angle brackets <>
  (sp-local-pair 'c++-mode "<" ">" :actions :rem)
  ;; Disable built-in "smart" completion of tags
  (map! :map c++-mode-map
        "<" nil
        ">" nil))

;; Start c++ files in insert state, why would one want it any other way...
(add-to-list 'evil-insert-state-modes 'c++-mode)

(setq dap-cpptools-extension-version "1.5.1")

  (with-eval-after-load 'lsp-rust
    (require 'dap-cpptools))

  (with-eval-after-load 'dap-cpptools
    ;; Add a template specific for debugging Rust programs.
    ;; It is used for new projects, where I can M-x dap-edit-debug-template
    (dap-register-debug-template "Rust::CppTools Run Configuration"
                                 (list :type "cppdbg"
                                       :request "launch"
                                       :name "Rust::Run"
                                       :MIMode "gdb"
                                       :miDebuggerPath "rust-gdb"
                                       :environment []
                                       :program "${workspaceFolder}/target/debug/hello / replace with binary"
                                       :cwd "${workspaceFolder}"
                                       :console "external"
                                       :dap-compilation "cargo build"
                                       :dap-compilation-dir "${workspaceFolder}")))

  (with-eval-after-load 'dap-mode
    (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
    (dap-auto-configure-mode +1))

;; Compile Command
(defun cpp-compile-command (f-name)
  (when f-name
    (setq compile-command
          (concat "g++ -std=c++17 -O2 -o "
                  (shell-quote-argument (file-name-sans-extension f-name))
                  " "
                  (shell-quote-argument f-name)
                  " -Wall;"))))

(add-hook! 'c++-mode-hook
  (setq-local compile-command (cpp-compile-command (buffer-file-name))))

(add-hook! 'python-mode-hook
  (setq-local compile-command (concat "python "(shell-quote-argument buffer-file-name))))

(after! projectile
(projectile-register-project-type 'cpp '("*.cpp")
                                  :compile "g++ -std=c++17 -O2 -o "))
(setq lsp-enable-folding t)

(after! scheme
  ;;(put 'test-group 'scheme-indent-function 1)
  (setq geiser-mode-start-repl-p t))
;;; Insert Package Name here

(load! "./agenda-sidebar")

(add-to-list 'evil-insert-state-modes 'nano-agenda-mode)

(add-hook! 'nano-agenda-mode-hook #'hide-mode-line-mode #'display-line-numbers-mode '+org-pretty-mode)
