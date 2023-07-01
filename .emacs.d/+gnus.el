;;; ../doom-configs/.emacs.d/gnus.el -*- lexical-binding: t; -*-
;; TODO: nnrss backend
;; REFS:
;; https://gluer.org/blog/2023/trying-gnus-as-an-email-client/
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;; https://www.bounga.org/tips/2020/05/03/multiple-smtp-accounts-in-gnus-without-external-tools/

;; (with-delayed-execution
    (with-eval-after-load 'gnus
      ;; See: #52735
      (defun srb-gnus-group-get-new-news (&optional arg one-level)
        (interactive "P")
        (with-timeout (1 (kill-buffer (nntp-find-connection-buffer nntp-server-buffer))
		                     (gnus-group-get-new-news))
          (gnus-group-get-new-news arg one-level)))
      (define-key gnus-group-mode-map "g" 'srb-gnus-group-get-new-news)
      
      ;; mode-line unread indicator
      (defun my/gnus-unread-count ()
        (interactive)
        (let ((uc (+ (cdr (nth 1 gnus-topic-unreads))    ;; uni uc
                     (cdr (nth 2 gnus-topic-unreads))))) ;; personal uc
          (if (eq uc 0)
              ""
            (format " 📥 %s " uc))))
      ;; FIXME: box not rendering completely
      (setq global-mode-string 
            (append global-mode-string 
                    (list '(:eval (propertize 
                                   (my/gnus-unread-count)
                                   'help-echo "Gnus : Unread"
                                   'face '(:box '(:line-width -1 :color "blue")))))))
      ;; Better UI
      (gnus-add-configuration
       '(article
         (horizontal 1.0
                     (vertical 25
                               (group 1.0))
                     (vertical 1.0
                               (summary 0.25 point)
                               (article 1.0)))))
      (gnus-add-configuration
       '(summary
         (horizontal 1.0
                     (vertical 25
                               (group 1.0))
                     (vertical 1.0
                               (summary 1.0 point)))))
      (delete 'gnus-article-mode viper-vi-state-mode-list)
      (with-eval-after-load 'gnus-summary
        (keymap-set gnus-summary-mode-map "u" #'gnus-summary-put-mark-as-unread)
        (keymap-set gnus-summary-mode-map "j" #'next-line)
        (keymap-set gnus-summary-mode-map "k" #'previous-line))
      (keymap-set gnus-article-mode-map "j" #'next-line)
      (keymap-set gnus-article-mode-map "k" #'previous-line)
      (keymap-set gnus-article-mode-map "q" #'kill-buffer-and-window))
      ;; (gnus-demon-add-handler 'gnus-demon-scan-news 5 1))
      ;; (add-hook 'gnus-article-mode-hook #'visual-line-mode)
    
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
					;; gnus-fetch-old-headers t
          mm-text-html-renderer 'shr
					gnus-article-x-face-too-ugly ".*"
					gnus-interactive-exit 'quiet
					gnus-novice-user nil
					gnus-expert-user nil
          gnus-auto-select-first nil
          gnus-summary-display-arrow nil
					gnus-thread-sort-functions
					'(gnus-thread-sort-by-most-recent-date
						(not gnus-thread-sort-by-number)))
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
		(setq gnus-sum-thread-tree-false-root ""
					gnus-sum-thread-tree-indent " "
					gnus-sum-thread-tree-root ""
					gnus-sum-thread-tree-single-indent ""
					gnus-sum-thread-tree-vertical        "│"
					gnus-sum-thread-tree-leaf-with-other "├─►"
					gnus-sum-thread-tree-single-leaf     "╰─►"
					gnus-user-date-format-alist '(((gnus-seconds-today) . " %H:%M")
																				(t . "%b %d"))
					gnus-topic-line-format (concat "%i%(%{%n - %A%}%) %v\n")
					gnus-group-line-format (concat "%S%3y : %(%-50,50G%)\n") ;; %E (gnus-group-icon-list)
					;;  06-Jan   Sender Name    Email Subject
					gnus-summary-line-format (concat "%0{%U%R%z%}"
																					 "%3{ %}" "%1{%&user-date;%}" "%3{ %}" " "
																					 "%4{%-16,16f%}" " "
																					 "%3{ %}" " "
																					 "%1{%B%}" "%S\n"))

		(setq user-emacs-config-directory (concat (getenv "HOME") "/doom-configs/.emacs.d")
					user-emacs-data-directory (concat (getenv "HOME") "/.emacs.d/.local")
					user-emacs-cache-directory (concat (getenv "HOME") "/.cache/emacs")
		      gnus-directory (concat user-emacs-data-directory "/gnus")
					;; all of them depend on the `gnus-directory` variable
					gnus-startup-file (concat gnus-directory "/.newsrc")
					gnus-cache-directory (concat gnus-directory  "/news/cache")
					gnus-article-save-directory (concat gnus-directory "/news")
					gnus-kill-files-directory (concat gnus-directory "/news")
					nndraft-directory (concat gnus-directory "/mail/draft")
					nnfolder-directory (concat gnus-directory "/mail/archive"))

	(with-eval-after-load 'gnus-topic
				 (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
				 (setq gnus-topic-topology '(("Unread" visible)
																		 (("📩 Uni" visible nil nil))
																		 (("📥 Personal" visible nil nil))
																		 (("📰 News" visible nil nil))))
				 (setq gnus-topic-alist '(("📩 Uni" ; the key of topic
																	 "nnimap+uni:INBOX"
																	 "nnimap+uni:[Gmail]/Sent Mail"
																	 "nnimap+uni:Sent"
																	 "nnimap+uni:PS2"
																	 "nnimap+uni:Placement"
																	 "nnimap+uni:SOP"
																	 "nnimap+uni:Thesis"
																	 "nnimap+uni:[Gmail]/Starred")
																	("📥 Personal" ; the key of topic
																	 "nnimap+personal:INBOX"
																	 "nnimap+personal:[Gmail]/Sent Mail"
																	 "nnimap+personal:Sent"
																	 "nnimap+personal:sent.2023"
																	 "nnimap+personal:[Gmail]/Starred")
																	("📰 News"
                                   "gmane.emacs.devel"
																	 "gwene.com.youtube.feeds.videos.xml.user.ethoslab"
																	 "gmane.comp.web.qutebrowser"
																	 "gmane.comp.web.elinks.user"
																	 "gwene.app.rsshub.leetcode.articles"
																	 "gwene.com.arcan-fe"
																	 "gwene.io.github.matklad"
																	 "gwene.net.lwn.headlines"
																	 "gwene.org.quantamagazine"
																	 "gwene.org.bitlbee.news.rss")
																	("Unread"))))

		(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
;; )
