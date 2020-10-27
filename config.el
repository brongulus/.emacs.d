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

;;(eshell-command "setq stringvar $XDG_CURRENT_DESKTOP")
;;(if (string= stringvar "EXWM")
;;   (load! "+exwm")
;;nil
;;)

(setq-default
 user-full-name "Prashant Tak"
 user-mail-address "prashantrameshtak@gmail.com"
 doom-font (font-spec :family "Source Code Pro" :size 16)
 doom-variable-pitch-font (font-spec :family "iA Writer Quattro S")
 doom-serif-font (font-spec :family "iA Writer Quattro S" :weight 'regular)
 doom-theme 'doom-shades-of-purple
 org-directory "/mnt/Data/Documents/org/"
 evil-escape-mode 1
 display-line-numbers-type 'relative
 tab-width 8
 which-key-idle-delay 0.5
 large-file-warning-threshold nil
 org-latex-toc-command "\\tableofcontents \\clearpage"
 )

(custom-set-faces! '(default :background nil))

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(add-hook! 'window-setup-hook
  (select-frame-set-input-focus (selected-frame)))

(setq window-resize-pixelwise nil
      frame-resize-pixelwise nil)

(after! company
  (setq company-idle-delay 0.2))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(when (featurep! :ui zen)
  (after! writeroom-mode
    (setq +zen-text-scale 0)))

(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-modal-icon nil))

(display-time-mode 1)

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

(add-hook 'org-mode-hook 'org-fragtog-mode)
(add-hook 'org-mode-hook
          (λ! (yas-minor-mode)
              (yas-activate-extra-mode 'latex-mode)))
;; (add-hook 'org-mode-hook 'lsp-completion-mode)

(setq yas-triggers-in-field t)

(setq flycheck-global-modes '(not LaTeX-mode latex-mode))

(setq org-preview-latex-default-process 'dvisvgm)

(after! org
  (map! :map org-mode-map
        :localleader
        :desc "View exported file" "v" #'org-view-output-file)

  (defun org-view-output-file (&optional org-file-path)
    "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
    (interactive)
    (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
           (dir (file-name-directory org-file-path))
           (basename (file-name-base org-file-path))
           (output-file nil))
      (dolist (ext org-view-output-file-extensions)
        (unless output-file
          (when (file-exists-p
                 (concat dir basename "." ext))
            (setq output-file (concat dir basename "." ext)))))
      (if output-file
          (if (member (file-name-extension output-file) org-view-external-file-extensions)
              (browse-url-xdg-open output-file)
            (pop-to-buffer (or (find-buffer-visiting output-file)
                               (find-file-noselect output-file))))
        (message "No exported file found")))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")

(after! elfeed
  (setq elfeed-search-filter "@2-month-ago"))
(defun =elfeed ()
  (interactive)
  (elfeed)
  )
(map! :n "SPC o e" #'=elfeed)
(after! elfeed
  (map! :n "u" #'elfeed-update))

(add-hook 'pdf-view-mode-hook (lambda ()
        (pdf-view-midnight-minor-mode)))
(add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
;;(setq pdf-view-midnight-colors '("#839496" . "#002b36" ))

;; (setq fancy-splash-image "~/.doom.d/doom_grin.png")
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
    ("Open elfeed"
     :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
     :face (:inherit (doom-dashboard-menu-title bold))
     :action =elfeed)
    )
  )

(use-package windmove
  :bind
  (("S-<left>". windmove-left)
   ("S-<right>". windmove-right)
   ("S-<up>". windmove-up)
   ("S-<down>". windmove-down)))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(setq org-support-shift-select 'always)

(setq +notmuch-sync-backend 'mbsync)
(autoload 'notmuch "notmuch" "notmuch mail" t)
;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "prashantrameshtak@gmail.com"
      user-full-name "Prashant Tak")
;; smtp config
(setq smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'message-smtpmail-send-it)

;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/.mail/gmail/draft")
(setq message-kill-buffer-on-exit t)
;; change the directory to store the sent mail
(setq message-directory "~/.mail/gmail/")

(after! notmuch
(set-popup-rule! "^\\*notmuch-hello" :ignore t))
(map! :n "SPC o n" 'notmuch)

(setq notmuch-saved-searches
      '((:name "inbox"    :query "tag:inbox not tag:trash"    :key "i")
        (:name "personal" :query "tag:personal"               :key "p")
        (:name "bits"     :query "tag:bits"                   :key "b")
        (:name "unread"   :query "tag:unread"                 :key "u")
        (:name "flagged"  :query "tag:flagged"                :key "f")
        (:name "sent"     :query "tag:sent"                   :key "s")
        )
      )

;;(use-package emms
;;:ensure t
;;:config
;;(require 'emms-setup)
;;(require 'emms-player-mplayer)
;;(emms-all)
;;(setq emms-player-list '(
;;                         emms-player-mpg321
;;                         emms-player-ogg123
;;                         emms-player-mplayer
;;                         ))
;;(defun emms-player-mplayer-volume(amount)
;;  (process-send-string
;;   emms-player-simple-process-name
;;   (format "volume %d\n" amount)))
;;(setq emms-volume-change-function 'emms-player-mplayer-volume)
;;(setq emms-source-file-default-directory "D:/Music/")
;;(setq emms-playlist-buffer-name "*Music*")
;;(emms-add-directory-tree emms-source-file-default-directory)
;;)

;;(after! cc-mode
;;  (set-company-backend! 'c-mode
;;    '(:separate company-irony-c-headers company-irony)))
;;Windows
;;(after! lsp-mode
;;  (set-lsp-priority! 'clangd 1))
;;
;;Linux
;;(after! lsp-mode
;;  (require 'dap-cpptools)
;;  (yas-global-mode)
;;  )

(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))

(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))

(load! "~/.doom.d/openwith")
(require 'openwith)
(add-hook 'dired-mode-hook 'openwith-mode 1)

;;(load! "~/.emacs.d/elegant-emacs/sanity")
;;(load! "~/.emacs.d/elegant-emacs/elegance")

(setq counsel-spotify-client-id "d9d7e317a9b344a886d71643341cb796"
      counsel-spotify-client-secret "6e91aa96e0c1451ab8ad0da705983082"
      )

(map! :n "SPC a t" #'counsel-spotify-toggle-play-pause
      :n "SPC a <" #'counsel-spotify-previous
      :n "SPC a >" #'counsel-spotify-next
      :n "SPC a s" #'counsel-spotify-search-track
      :n "SPC a p" #'counsel-spotify-search-playlist
      )

(use-package! el-secretario-org
  :after (el-secretario))
(use-package! el-secretario-notmuch
  :after (el-secretario))

(use-package! el-secretario
  :config
  (defun my/dailyreview-secretary ()
    (list

     ;; First take care of email
     (el-secretario-notmuch-make-source "tag:unread")
     ;; Then Take care of inbox
     (el-secretario-org-make-source nil ("/mnt/Data/Documents/org/index.org"))

     ;; Check if any waiting items are done
    ;;(el-secretario-org-make-source (todo "WAITING") ("~/org/orgzly/Todo.org"))
     ;; Go through TODOs
    ;; (el-secretario-org-make-source (todo "TODO") ("~/org/orgzly/Todo.org"))
     )
    )
  ;; Create a function to start the review
  (defun el-secretario-daily-review ()
    (interactive)
    (el-secretario-start-session (my/dailyreview-secretary)))
  :commands (el-secretario-daily-review)
  )
