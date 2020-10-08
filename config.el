;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;#############EXWM##################
(eshell-command "setq stringvar $XDG_CURRENT_DESKTOP")
(if (string= stringvar "EXWM")
    (use-package exwm
      :config
      (require 'exwm-config)
      (exwm-config-default)
      (require 'exwm-randr)
      (setq exwm-randr-workspace-monitor-plist '(0 "DP-4"))
      (add-hook 'exwm-randr-screen-change-hook
                (lambda()
                  (start-process-shell-command
                   "xrandr" nil "xrandr --output DP-4 --mode 1920x1080 --pos 0x0 --rotate normal")))
      (exwm-randr-enable)
      (require 'exwm-systemtray)
      (exwm-systemtray-enable)
      )
nil
)

;;###########DEFAULTS##################
;;
(setq-default
      user-full-name "Prashant Tak"
      user-mail-address "prashantrameshtak@gmail.com"
      doom-font (font-spec :family "Source Code Pro" :size 16)
      doom-theme 'doom-challenger-deep
      org-directory "/mnt/Data/Documents/org/"
      evil-escape-mode 1
      display-line-numbers-type 'relative
      tab-width 8
      which-key-idle-delay 0.5
      large-file-warning-threshold nil
      org-latex-toc-command "\\tableofcontents \\clearpage"
      )

(display-time-mode 1)

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

;; ensure emacs gets focused after restart
(add-hook! 'window-setup-hook
  (select-frame-set-input-focus (selected-frame)))

;;; Info Colors
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)


(after! company
  (setq company-idle-delay 0.2))

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;;; org

(add-hook 'org-mode-hook 'org-fragtog-mode)

(add-hook 'org-mode-hook
          (λ! (yas-minor-mode)
              (yas-activate-extra-mode 'latex-mode)))

;; (add-hook 'org-mode-hook 'lsp-completion-mode)


(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; automatically turns on midnight-mode for pdf
(add-hook 'pdf-view-mode-hook (lambda ()
        (pdf-view-midnight-minor-mode)))

(add-hook 'pdf-view-mode-hook 'doom-modeline-mode)
;;(setq pdf-view-midnight-colors '("#839496" . "#002b36" ))


;;#############WINDMOVE###############
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

;;##############MAIL##################

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

;;############EMMS####################
;;
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

;;################C++########################3
;;
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

;; DICTIONARY
;;
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

;;###################LOAD EL FILES######################
;;
;;(load! "~/.emacs.d/elegant-emacs/sanity")
;;(load! "~/.emacs.d/elegant-emacs/elegance")
(load! "~/.doom.d/openwith")
(require 'openwith)
(add-hook 'dired-mode-hook 'openwith-mode 1)

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
