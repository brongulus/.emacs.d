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
      gnus-use-header-prefetch t
      gnus-blocked-images t
      gnus-inhibit-images nil
      gnus-article-x-face-too-ugly ".*")

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
                                  (("RSS" visible nil nil))
                                  (("misc" visible))
                                 ))

      ;; key of topic is specified in my sample ".gnus.el"
      (setq gnus-topic-alist '(("uni" ; the key of topic
                                "nnimap+uni:INBOX"
                                "nnimap+uni:[Gmail]/Sent Mail"
                                "nnimap+uni:[Gmail]/Starred"
                                "nnimap+uni:[Gmail]/Drafts")
                               ("personal" ; the key of topic
                                "nnimap+personal:INBOX"
                                "nnimap+personal:[Gmail]/Sent Mail"
                                "nnimap+personal:[Gmail]/Starred"
                                "nnimap+personal:[Gmail]/Drafts")
                               ("misc" ; the key of topic
                                "nnfolder+archive:sent.2015-12"
                                "nnfolder+archive:sent.2016"
                                "nndraft:drafts")
                               ("RSS"
                                "nntp+news.gwene.org:gwene.com.youtube.feeds.videos.xml.user.ethoslab"
                                "nntp+news.gwene.org:gwene.rs.lobste"
                                "nntp+news.gwene.org:gwene.net.lwn.headlines.comments")
                               ("Gnus")))))

;; SOURCE: https://www.reddit.com/r/emacs/comments/lx04yu/comment/gpl92oa/
(defvar my/gnus-rss-list
  '("nntp+news.gwene.org:gwene.com.youtube.feeds.videos.xml.user.ethoslab"
    "nntp+news.gwene.org:gwene.rs.lobste"
    "nntp+news.gwene.org:gwene.net.lwn.headlines.comments"))
;; (add-to-list 'gnus-topic-alist (add-to-list 'my/gnus-rss-list "RSS") t)

(add-hook! 'gnus-group-mode-hook #'(lambda ()(gnus-group-list-all-groups)) #'gnus-topic-mode)

(defun gnus-get-news-async (&optional arg)
        (interactive)
        (make-thread (lambda()
                        (gnus-group-get-new-news))
                     "Get the new gnus news asynchronously"))

(after! gnus
        (map! :map gnus-group-mode-map
                :ni "t" nil
                :ni "t" #'gnus-topic-mode
                :ni "g" nil
                :ni "g" #'gnus-get-news-async))

(gnus-demon-add-handler 'gnus-demon-scan-news 1 t)
