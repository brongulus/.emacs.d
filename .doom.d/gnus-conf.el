;;; ../doom-configs/.doom.d/gnus-conf.el -*- lexical-binding: t; -*-

;; SOURCE:
;; https://gluer.org/blog/2023/trying-gnus-as-an-email-client/

(setq gnus-select-method '(nntp "news.gwene.org"))
(setq gnus-secondary-select-methods
      '((nnimap "personal"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port "993")
                (nnimap-stream ssl)
                (nnir-search-engine imap)
                (nnmail-expiry-target "nnimap+personal:[Imap]/Trash")
                (nnmail-expiry-wait 'immediate))
        (nnimap "uni"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port "993")
                (nnimap-stream ssl)
                (nnir-search-engine imap)
                (nnmail-expiry-target "nnimap+uni:[Imap]/Trash")
                (nnmail-expiry-wait 'immediate))))

(setq gnus-asynchronous t
      gnus-use-cache t
      gnus-cache-remove-articles nil
      gnus-fetch-old-headers t
      ;; gnus-blocked-images t
      ;; gnus-inhibit-images nil
      gnus-article-x-face-too-ugly ".*"
      gnus-interactive-exit nil
      gnus-novice-user nil
      gnus-expert-user nil)

;; SOURCE: https://www.bounga.org/tips/2020/05/03/multiple-smtp-accounts-in-gnus-without-external-tools/
(setq gnus-posting-styles
        '((".*"
                (address "prashantrameshtak@gmail.com")
                (signature "Prashant Tak")
                ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 prashantrameshtak@gmail.com"))
          ("uni"
                (address "f20181050@pilani.bits-pilani.ac.in")
                (signature "Prashant Tak\n(2018B4A81050P)")
                ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 f20181050@pilani.bits-pilani.ac.in"))))

(setq message-send-mail-function 'message-use-send-mail-function
      send-mail-function 'smtpmail-send-it)

(setq gnus-parameters
      '(("personal"
	 (gcc-self . "nnimap+personal:Sent")
         (display . all))
      ("uni"
	 (gcc-self . "nnimap+uni:Sent")
         (display . all))))


;; summary
(setq gnus-sum-thread-tree-false-root " "
      gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-root "r "
      ;; gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-vertical        "|"
      gnus-sum-thread-tree-leaf-with-other "├─► %s"
      gnus-sum-thread-tree-single-leaf     (concat "╰─► " "%S") ; FIXME:

      gnus-user-date-format-alist '(((gnus-seconds-today) . " %H:%M")
                                    (t . "%b %d"))

      ; SOURCE: all-the-icons-gnus
      gnus-topic-line-format (concat "%i "
                                     (propertize
                                      (all-the-icons-material "folder")
                                      'display '(raise 0.0))
                                     " %(%{%n - %A%}%) %v\n")

      gnus-group-line-format (concat "%1M%1S%5y "
                                  (propertize ;(all-the-icons-faicon "envelope-o")
                                   (all-the-icons-material "mail")
                                   'display '(raise 0.0))
                                  " : %(%-50,50G%)\n")

      ;; │06-Jan│  Sender Name  │ Email Subject
      gnus-summary-line-format (concat "%0{%U%R%z%}"
				       "%3{│%}" "%1{%&user-date;%}" "%3{│%}"
				       " "
                                       (propertize
                                        (all-the-icons-material "person")
                                        'display '(raise 0.0))
				       "%4{%-16,16f%}"
				       " "
				       "%3{│%}"
				       " "
				       "%1{%B%}"
				       "%s\n"))

;; xdg directories
(setq user-emacs-config-directory (concat (getenv "HOME") "/.doom.d")
      user-emacs-data-directory (concat (getenv "HOME") "/.emacs.d/.local")
      user-emacs-cache-directory (concat (getenv "HOME") "/.cache/emacs"))

(setq gnus-directory (concat user-emacs-data-directory "/gnus")
      ;; all of them depend on the `gnus-directory` variable
      gnus-startup-file (concat gnus-directory "/.newsrc")
      gnus-cache-directory (concat gnus-directory  "/news/cache")
      gnus-article-save-directory (concat gnus-directory "/news")
      gnus-kill-files-directory (concat gnus-directory "/news")
      nndraft-directory (concat gnus-directory "/mail/draft")
      nnfolder-directory (concat gnus-directory "/mail/archive"))

;; SOURCE 2:
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

(eval-after-load 'gnus-topic
  '(progn
    (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
      (setq gnus-topic-topology '(("Gnus" visible)
                                  (("uni" visible nil nil))
                                  (("personal" visible nil nil))
                                  (("news.gwene.org" visible nil nil))))

      ;; key of topic is specified in my sample ".gnus.el"
      (setq gnus-topic-alist '(("uni" ; the key of topic
                                "nnimap+uni:INBOX"
                                "nnimap+uni:[Gmail]/Sent Mail"
                                "nnimap+uni:[Gmail]/Starred")
                               ("personal" ; the key of topic
                                "nnimap+personal:INBOX"
                                "nnimap+personal:[Gmail]/Sent Mail"
                                "nnimap+personal:[Gmail]/Starred")
                               ("news.gwene.org")
                               ("Gnus")))))

(add-hook! 'gnus-group-mode-hook #'gnus-topic-mode) ; #'(lambda ()(gnus-group-list-all-groups))

(setq-hook! 'gnus-article-mode-hook hl-line-mode -1)

(after! gnus
        (map! :map gnus-group-mode-map
                :ni "t" nil
                :ni "t" #'gnus-topic-mode
                :ni "g" nil
                :ni "g" #'gnus-group-get-new-news)
        (map! :map gnus-summary-mode-map
              :ni "J" nil
              :ni "J" 'evil-window-down)
        (map! :map gnus-article-mode-map
              :ni "K" nil
              :ni "K" 'evil-window-up))

;; Using in doom-modeline for now
;; (after! gnus
;;   (gnus-demon-add-handler 'gnus-demon-scan-news 1 1))
