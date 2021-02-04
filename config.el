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
 doom-font (font-spec :family "JetBrains Mono" :size 18) ;Nerd Font Mono
 doom-variable-pitch-font (font-spec :family "iA Writer Quattro S")
 doom-serif-font (font-spec :family "iA Writer Quattro S" :weight 'regular)
 doom-theme 'doom-dracula
 org-directory "/home/prashant/Dropbox/org/"
 evil-escape-mode 1
 display-line-numbers-type nil
 tab-width 8
 which-key-idle-delay 0.5
 large-file-warning-threshold nil
 org-latex-toc-command "\\tableofcontents \\clearpage"
 )

;; (setq projectile-switch-project-action #'projectile-dired)

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

;;(defun display-workspaces-in-minibuffer ()
;;  (with-current-buffer " *Minibuf-0*"
;;    (erase-buffer)
;;    (insert (+workspace--tabline))))
;;(run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;(+workspace/display)

(defun save-and-close ()
  (interactive)
  (call-interactively 'save-buffer)
  (call-interactively 'kill-current-buffer))

(map! :n "SPC b w" #'save-and-close)

(add-hook 'after-change-major-mode-hook
(lambda ()
(hl-line-mode -1)))

(setq auth-sources '("/home/prashant/.authinfo" "/home/prashant/.emacs.d/.local/etc/authinfo.gpg" "~/.authinfo.gpg"))

(display-time-mode 1)

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

(add-hook! 'org-mode-hook #'org-fragtog-mode)
(after! org
  (add-hook! 'org-mode-hook #'writeroom-mode))
(add-hook 'org-mode-hook
          (λ! (yas-minor-mode)
              (yas-activate-extra-mode 'latex-mode)))
;; (add-hook 'org-mode-hook 'lsp-completion-mode)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(custom-set-faces!
  '(org-table :inherit 'fixed-pitch))
;;(set-face-attribute 'org-table nil :inherit 'fixed-pitch)

(setq yas-triggers-in-field t)

(setq flycheck-global-modes '(not LaTeX-mode latex-mode))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  )
(setq org-preview-latex-default-process 'dvisvgm)

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-fragments'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-fragments))

(setq org-agenda-start-with-log-mode t
      org-log-done t
      org-log-into-drawer t
      org-agenda-breadcrumbs-separator " ❱ ")

(setq org-agenda-files
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
                      ;;           (org-agenda-skip-scheduled-if-done t)
                      ;;           (org-agenda-skip-timestamp-if-done t)
                      ;;           (org-agenda-skip-deadline-if-done t)
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

(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)

(setq org-agenda-block-separator (string-to-char " "))

(setq org-agenda-hidden-separator "‌‌ ")

(use-package! appt
  :defer-incrementally t
  :config

  (appt-activate t)

  ;; use appointment data from org-mode
  (defun my-org-agenda-to-appt ()
    (interactive)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))

  (setq appt-message-warning-time 5) ; Show notification 5 minutes before event
  (setq appt-display-interval appt-message-warning-time) ; Disable multiple reminders
  (setq appt-display-mode-line nil)

  ;; update alarms when starting emacs
  (my-org-agenda-to-appt)
  ;; (2) ... Everyday at 12:05am (useful in case you keep Emacs always on)
  (run-at-time "12:05am" (* 24 3600) 'my-org-agenda-to-appt)

  ;; (3) ... When TODO.org is saved
  (add-hook 'after-save-hook
            '(lambda ()
               (if (string= (buffer-file-name) (concat (getenv "HOME") "~/Dropbox/org/todo.org"))
                   (my-org-agenda-to-appt))))

  ;; TODO Display appointments as a window manager notification (incorporate the script within elisp)
  (setq appt-disp-window-function 'my-appt-display)
  (setq appt-delete-window-function (lambda () t))

  (setq my-appt-notification-app "~/appt-notification.sh")

  (defun my-appt-display (min-to-app new-time msg)
    (if (atom min-to-app)
        (start-process "my-appt-notification-app" nil my-appt-notification-app min-to-app msg)
      (dolist (i (number-sequence 0 (1- (length min-to-app))))
        (start-process "my-appt-notification-app" nil my-appt-notification-app (nth i min-to-app) (nth i msg)))))
  )

(after! org-capture
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Notes")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)
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

(after! elfeed
  (setq elfeed-search-filter "@2-month-ago"))
(defun =elfeed ()
  (interactive)
  (elfeed)
  )
(add-hook! elfeed-show-mode 'variable-pitch-mode)
(map! :n "SPC o e" #'=elfeed)
(after! elfeed
  (map! :n "u" #'elfeed-update))

;; FIXME
(after! pocket-reader
  (set-evil-initial-state! 'pocket-reader-mode
    'insert))
(setq pocket-reader-open-url-default-function #'eww
      pocket-reader-pop-to-url-default-function #'eww)

(add-hook 'pdf-view-mode-hook (lambda ()
                                (pdf-view-midnight-minor-mode)))
(add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
;;(setq pdf-view-midnight-colors '("#839496" . "#002b36" ))
(add-hook 'pdf-view-mode-hook #'hide-mode-line-mode)

;;(map! pdf-view-mode-map
;;      :niv "h" #'pdf-annot-add-markup-annotation)

(add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)

(after! pdf-tools
  (map! :map pdf-view-mode-map
        ;; "j" nil
        ;; "k" nil
        :n "M-j" #'pdf-continuous-scroll-forward
        :n "M-k" #'pdf-continuous-scroll-backward))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq fancy-splash-image "~/.doom.d/splash.png")
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
        ("Open Agenda"
         :icon (all-the-icons-octicon "check" :face 'doom-dashboard-menu-title)
         :face (:inherit (doom-dashboard-menu-title bold))
         :action org-agenda)
        )
      )
(add-hook! '+doom-dashboard-mode-hook #'hide-mode-line-mode)

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

;;(setq +notmuch-sync-backend 'mbsync)
(autoload 'notmuch "notmuch" "notmuch mail" t)
;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "prashantrameshtak@gmail.com"
      user-full-name "Prashant Tak")
;; smtp config
;;(setq smtpmail-smtp-server "smtp.gmail.com"
;;      message-send-mail-function 'message-smtpmail-send-it)

;; report problems with the smtp server
;;(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
;;(setq message-defNotmault-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/.mail/gmail/draft")
;;(setq message-kill-buffer-on-exit t)
;; change the directory to store the sent mail
(setq message-directory "~/.mail/gmail/")

;;(after! notmuch
;;(set-popup-rule! "^\\*notmuch-hello" :ignore t))
(map! :n "SPC o n" 'notmuch)
;;(add-hook 'notmuch-hello-refresh-hook
;;              (lambda ()
;;                (if (and (eq (point) (point-min))
;;                         (search-forward "Saved searches:" nil t))
;;                    (progn
;;                     (forward-line)
;;                      (widget-forward 1))
;;                  (if (eq (widget-type (widget-at)) 'editable-field)
;;                      (beginning-of-line)))))

;;(after! notmuch
;;  (setq notmuch-saved-searches
;;        '((:name "inbox"    :query "tag:inbox not tag:trash"    :key "i")
;;          (:name "personal" :query "tag:personal"               :key "p")
;;          (:name "bits"     :query "tag:bits"                   :key "b")
;;          (:name "unread"   :query "tag:unread"                 :key "u")
;;          (:name "flagged"  :query "tag:flagged"                :key "f")
;;          (:name "sent"     :query "tag:sent"                   :key "s")
;;          )
;;        )
;;  )

;;FIXME (add-hook! 'notmuch-search-mode-hook #'notmuch-tree-mode)
;;(setq mm-text-html-renderer 'shr
;;      notmuch-multipart/alternative-discouraged '("text/plain" ;;"multipart/related")
;;      shr-use-colors nil
;;      gnus-blocked-images nil
;;      )
;; inline images?
;;(if (not (fboundp 'gnus-blocked-images))
;;    (defun gnus-blocked-images () nil))

;;FIXME
;;(setq notmuch-search-result-format
;;      '(("date" . "%12s | ")
;;        ("authors" . "%-20s | ")
;;        ("subject" . "%-54s")
;;        ("tags" . ":%s:")
;;        ))
;;(after! notmuch
;;  (setq notmuch-hello-sections
;;        '(notmuch-hello-insert-header +notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)
;;        notmuch-message-headers-visible nil))
;; Look for alternate methods of centering, writeroom destroys formatting
;;(add-hook! 'notmuch-show-mode-hook #'writeroom-mode)

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

;;(setq lsp-julia-default-environment "~/.julia/environments/v1.0")
(setq lsp-enable-folding t)

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

(require 'xwidget)

(when
    (featurep 'xwidget-internal)
  (easy-menu-define my-xwidget-tools-menu nil "Menu for Xwidget Webkit."
    `("Xwidget Webkit" :visible
      (featurep 'xwidget-internal)
      ["Browse Url ..." xwidget-webkit-browse-url :help "Ask xwidget-webkit to browse URL"]
      ["End Edit Textarea" xwidget-webkit-end-edit-textarea :help "End editing of a webkit text area"]))
  (easy-menu-add-item menu-bar-tools-menu nil my-xwidget-tools-menu 'separator-net)
  (easy-menu-define my-xwidget-menu xwidget-webkit-mode-map "Menu for Xwidget Webkit."
    '("Xwidget Webkit"
      ["Browse Url" xwidget-webkit-browse-url :help "Ask xwidget-webkit to browse URL"]
      ["Reload" xwidget-webkit-reload :help "Reload current url"]
      ["Back" xwidget-webkit-back :help "Go back in history"]
      "--"
      ["Insert String" xwidget-webkit-insert-string :help "current webkit widget"]
      ["End Edit Textarea" xwidget-webkit-end-edit-textarea :help "End editing of a webkit text area"]
      "--"
      ["Scroll Forward" xwidget-webkit-scroll-forward :help "Scroll webkit forwards"]
      ["Scroll Backward" xwidget-webkit-scroll-backward :help "Scroll webkit backwards"]
      "--"
      ["Scroll Up" xwidget-webkit-scroll-up :help "Scroll webkit up"]
      ["Scroll Down" xwidget-webkit-scroll-down :help "Scroll webkit down"]
      "--"
      ["Scroll Top" xwidget-webkit-scroll-top :help "Scroll webkit to the very top"]
      ["Scroll Bottom" xwidget-webkit-scroll-bottom :help "Scroll webkit to the very bottom"]
      "--"
      ["Zoom In" xwidget-webkit-zoom-in :help "Increase webkit view zoom factor"]
      ["Zoom Out" xwidget-webkit-zoom-out :help "Decrease webkit view zoom factor"]
      "--"
      ["Fit Width" xwidget-webkit-fit-width :help "Adjust width of webkit to window width"]
      ["Adjust Size" xwidget-webkit-adjust-size :help "Manually set webkit size to width W, height H"]
      ["Adjust Size Dispatch" xwidget-webkit-adjust-size-dispatch :help "Adjust size according to mode"]
      ["Adjust Size To Content" xwidget-webkit-adjust-size-to-content :help "Adjust webkit to content size"]
      "--"
      ["Copy Selection As Kill" xwidget-webkit-copy-selection-as-kill :help "Get the webkit selection and put it on the kill-ring"]
      ["Current Url" xwidget-webkit-current-url :help "Get the webkit url and place it on the kill-ring"]
      "--"
      ["Show Element" xwidget-webkit-show-element :help "Make webkit xwidget XW show a named element ELEMENT-SELECTOR"]
      ["Show Id Element" xwidget-webkit-show-id-element :help "Make webkit xwidget XW show an id-element ELEMENT-ID"]
      ["Show Id Or Named Element" xwidget-webkit-show-id-or-named-element :help "Make webkit xwidget XW show a name or element id ELEMENT-ID"]
      ["Show Named Element" xwidget-webkit-show-named-element :help "Make webkit xwidget XW show a named element ELEMENT-NAME"]
      "--"
      ["Cleanup" xwidget-cleanup :help "Delete zombie xwidgets"]
      ["Event Handler" xwidget-event-handler :help "Receive xwidget event"]
      "--"
      ["Xwidget Webkit Mode" xwidget-webkit-mode :style toggle :selected xwidget-webkit-mode :help "Xwidget webkit view mode"])))
