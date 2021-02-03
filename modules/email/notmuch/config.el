;;; email/notmuch/config.el -*- lexical-binding: t; -*-

(defvar +notmuch-sync-backend 'mbsync)

(defvar +notmuch-sync-command nil)

(defvar +notmuch-mail-folder "~/.mail/")


;;; Packages

(use-package! notmuch
  :defer t
  :init
  (after! org
    (add-to-list 'org-modules 'ol-notmuch))
  :config

  (set-company-backend! 'notmuch-message-mode
    'notmuch-company '(company-ispell company-yasnippet))

  (set-popup-rule! "^\\*notmuch-hello" :ignore t)

  (setq notmuch-fcc-dirs nil
        ;;  might change this
        notmuch-show-logo nil
        notmuch-message-headers-visible nil
        notmuch-search-oldest-first nil
        message-defNotmault-mail-headers "Cc: \nBcc: \n"
        ;;  smtp
        smtpmail-smtp-server "smtp.gmail.com"
        message-send-mail-function 'message-smtpmail-send-it
        smtpmail-debug-info t
        ;; html stuff
        mm-text-html-renderer 'shr
        notmuch-multipart/alternative-discouraged
        '("text/plain" "multipart/related")
        shr-use-colors nil
        gnus-blocked-images nil ;; not needed maybe
        ;; search results
;;        notmuch-search-result-format
;;        '(("date" . "%12s | ")
;;          ("authors" . "%-20s | ")
;;          ("subject" . "%-54s")
;;          ("tags" . ":%s:"))
        notmuch-hello-sections
        '(notmuch-hello-insert-header +notmuch-hello-insert-saved-searches
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

  (add-hook! 'notmuch-search-hook #'notmuch-tree-from-search-current-query)

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
