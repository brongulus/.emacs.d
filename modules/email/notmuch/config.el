;;; email/notmuch/config.el -*- lexical-binding: t; -*-

(defvar +notmuch-sync-backend 'mbsync
  "Backend to use; ifflineimap, mbsync or nil")

;; TODO shift it to a case basis in config section
(defvar +notmuch-sync-command nil)

(defvar +notmuch-mail-folder "~/.mail/")



;;; Packages

(use-package! notmuch
  :defer t
  :init
  (after! org
    (add-to-list 'org-modules 'ol-notmuch))
  :config

;;  (pixel-scroll-mode)

  ;; advice
  (advice-add #'notmuch-search-insert-field :override #'+notmuch-search-insert-field-a)
  (advice-add #'notmuch-tree-quit :after #'notmuch-bury-or-kill-this-buffer)

  (set-company-backend! 'notmuch-message-mode
    'notmuch-company '(company-ispell company-yasnippet))

  (set-popup-rule! "^\\*notmuch-hello" :ignore t)

;;  (set-evil-initial-state! '(notmuch-show-mode notmuch-tree-mode notmuch-search-mode) 'insert)

  (setq notmuch-fcc-dirs nil

        ;;  might change this
        notmuch-show-logo nil
        notmuch-message-headers-visible nil
        notmuch-search-oldest-first nil
        message-defNotmault-mail-headers "Cc: \nBcc: \n"
        message-kill-buffer-on-exit t

        ;;  smtp
        smtpmail-smtp-server "smtp.gmail.com"
        message-send-mail-function 'message-smtpmail-send-it
        smtpmail-debug-info t

        ;; html stuff TODO add different colour for links for improved visibility
        mm-text-html-renderer 'gnus-w3m
        notmuch-multipart/alternative-discouraged
        '("text/plain" "multipart/related")
        notmuch-show-text/html-blocked-images nil
        gnus-max-image-proportion 0.7
        ;;shr-use-colors nil

        ;; Image hacks
        pixel-resolution-fine-flag t
        mouse-wheel-progressive-speed nil
;;        mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-follow-mouse 't

        ;; search results TODO convert to tree format and fix the stuff with subject
        notmuch-search-result-format
        '(("date" . "%12s ")
          ("authors" . "%-20s")
          ("subject" . "%-120s")
          ("tags" . "[%s]"))
        notmuch-tree-result-format
        '(("date" . "%12s  ")
         ("authors" . "%-20s")
         ((("tree" . "%s")
           ("subject" . "%s"))
          . " %-120s ") ;;-54s
         ("tags" . "[%s]"))

        notmuch-hello-sections
        '(notmuch-hello-insert-header notmuch-hello-insert-saved-searches
        notmuch-hello-insert-search notmuch-hello-insert-recent-searches
        notmuch-hello-insert-alltags notmuch-hello-insert-footer)

        ;;  tags
        notmuch-saved-searches
        '((:name "inbox"    :query "tag:inbox not tag:trash"    :key "i")
          (:name "personal" :query "tag:personal"               :key "p")
          (:name "bits"     :query "tag:bits"                   :key "b")
          (:name "unread"   :query "tag:unread"                 :key "u")
          (:name "flagged"  :query "tag:flagged"                :key "f")
          (:name "sent"     :query "tag:sent"                   :key "s")
          )
        notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread)))
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-tree-show-out t
        )

  ;; hooks

  (add-hook 'doom-real-buffer-functions #'notmuch-interesting-buffer)

  (add-hook 'notmuch-hello-refresh-hook
              (lambda ()
                (if (and (eq (point) (point-min))
                         (search-forward "Saved searches:" nil t))
                    (progn
                      (forward-line)
                      (widget-forward 1))
                  (if (eq (widget-type (widget-at)) 'editable-field)
                      (beginning-of-line)))))

  (add-hook! '(notmuch-show-mode-hook
               notmuch-tree-mode-hook
               notmuch-search-mode-hook)
             #'hide-mode-line-mode)

;;  (add-hook! 'notmuch-search-hook #'notmuch-tree-from-search-current-query)

  (add-hook! 'notmuch-show-hook #'variable-pitch-mode #'writeroom-mode #'iscroll-mode)
  ;; TEST setup


;;  (setq notmuch-show-insert-text/plain-hook
;;  '(notmuch-wash-wrap-long-lines
;;    notmuch-wash-tidy-citations
;;    notmuch-wash-elide-blank-lines
;;    notmuch-wash-excerpt-citations)

  ;; FIXME mappings

;;  (map! :localleader
;;        :map (notmuch-search-mode-map notmuch-tree-mode-map notmuch-show-mode-map)
;;        :desc "Compose email"   "c" #'+notmuch/compose
;;        :desc "Fetch new email" "u" #'+notmuch/update
;;        :desc "Quit notmuch"    "q" #'+notmuch/quit
;;        :map notmuch-search-mode-map
;;        :desc "Mark as deleted" "d" #'+notmuch/search-delete
;;        :desc "Mark as spam"    "s" #'+notmuch/search-spam
;;       :map notmuch-tree-mode-map
;;        :desc "Mark as deleted" "d" #'+notmuch/tree-delete
;;        :desc "Mark as spam"    "s" #'+notmuch/tree-spam)
  )

(use-package! org-mime
  :after (org notmuch)
  :config (setq org-mime-library 'mml))


(use-package! counsel-notmuch
  :when (featurep! :completion ivy)
  :commands counsel-notmuch
  :after notmuch)


(use-package! helm-notmuch
  :when (featurep! :completion helm)
  :commands helm-notmuch
  :after notmuch)
