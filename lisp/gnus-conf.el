;;;; Gnus-conf -*- lexical-binding: t -*-

(use-package gnus
  :ensure nil
  :hook (gnus-exit-gnus . tab-bar-close-tab)
  :hook (gnus-summary-mode . turn-on-gnus-mailing-list-mode)
  :bind (:map gnus-article-mode-map
              ("q" . kill-buffer-and-window)
              ("RET" . gnus-summary-scroll-up)
              ("C-<return>" . gnus-summary-scroll-down)
              :map gnus-summary-mode-map
              ("R" . (lambda nil (interactive)
                       (gnus-summary-mark-article nil ?R))))
  :preface
  (setq gnus-directory (concat "~/.emacs.d" "/gnus")
        gnus-startup-file (concat "~/.emacs.d" "/.newsrc")
        gnus-use-dribble-file nil
        gnus-always-read-dribble-file nil)
  :config
  (advice-add 'gnus-splash :before #'tab-bar-new-tab)
  (setq gnus-select-method '(nntp "news.gwene.org"))
  (setq gnus-secondary-select-methods
        '((nnimap "personal"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "993")
                  (nnimap-stream ssl)
                  (nnir-search-engine imap)
                  (nnmail-expiry-target "nnimap+personal:[Imap]/Trash")
                  (nnmail-expiry-wait 'immediate))
          (nnrss ""))
        ;; opts
        gnus-check-new-newsgroups nil ;; disable first time you use gnus
        gnus-asynchronous t
        gnus-use-cache t
        gnus-cache-remove-articles nil
        gnus-large-newsgroup 200
        gnus-blocked-images nil
        gnus-treat-hide-boring-headers t
        mm-text-html-renderer 'shr ;; w3m
        mm-inline-large-images 'resize
        shr-use-colors nil
        shr-max-width fill-column
        shr-indentation 2
        gnus-article-x-face-too-ugly ".*"
        gnus-interactive-exit nil
        gnus-novice-user nil
        gnus-expert-user nil
        gnus-auto-select-first nil
        gnus-auto-select-next 'quietly
        gnus-summary-display-arrow nil
        gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))
  ;; Better UI
  (gnus-add-configuration
   '(article (vertical 1.0 (summary 0.2 point) (article 1.0))))
  (setq gnus-unread-mark #x2022 ;; dot
        gnus-unseen-mark 32 ;; space
        gnus-read-mark 32
        gnus-del-mark ?\
        gnus-ancient-mark 32
        gnus-replied-mark 32
        gnus-cached-mark 32
        gnus-ticked-mark ?!
        gnus-low-score-mark #x2193 ;; down arrow
        ;; see (info "(gnus) Summary Score Commands")
        gnus-use-adaptive-scoring t
        gnus-summary-expunge-below 0
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-single-indent ""
        gnus-sum-thread-tree-vertical        "│"
        gnus-sum-thread-tree-leaf-with-other "├─►"
        gnus-sum-thread-tree-single-leaf     "╰─►"
        gnus-user-date-format-alist '(((gnus-seconds-today) . " %H:%M")
                                      (t . "%b %d"))
        gnus-topic-line-format (concat "%(%{%n - %A%}%) %v\n")
        gnus-group-uncollapsed-levels 2
        gnus-group-line-format (concat "%S%4y: %(%-40,40c%)\n") ;; %E (gnus-group-icon-list)
        ;;  06-Jan   Sender Name    Email Subject
        gnus-summary-line-format (concat " %0{%U%R%}"
                                         ;; "%1{%-4,4i%}" " "
                                         "%1{%&user-date;%}" "%3{ %}" " "
                                         "%4{%-16,16f%}" " "
                                         "%3{ %}" " "
                                         "%1{%B%}" "%S\n"))
  (setq gnus-message-archive-group '((format-time-string "sent.%Y"))))

(use-package gnus-group
  :ensure nil     ; use G R to subscribe to rss feeds
  :after gnus
  :hook (gnus-group-mode . gnus-topic-mode)
  :config
  (with-eval-after-load 'gnus-art
    (set-face-attribute 'gnus-header nil :height (face-attribute 'default :height)))
  
  (with-eval-after-load 'gnus-cite
    (defun gnus-clean-citation nil
      (save-excursion
        (let ((replacement "▎ "))
          (put-text-property 0 2 'face 'font-lock-comment-face replacement)
          (replace-regexp-in-region
           "\\(>[ ]?\\)" replacement (point-min) (point-max)))
        (replace-regexp-in-region "\\([^\s\n]\\)▎ " "\\1>" (point-min) (point-max))))

    (nconc gnus-treatment-function-alist
           '((t gnus-clean-citation))))

  (add-hook 'gnus-article-mode-hook
            (lambda nil
              (setq left-margin-width 4)))
  (with-eval-after-load 'gnus-topic
    (setq gnus-topic-topology '(("Unread" visible)
                                (("📥 Personal" visible nil nil))
                                (("📰 News" visible nil nil))))
    (setq gnus-topic-alist '(("📥 Personal" ; the key of topic
                              "nnimap+personal:INBOX"
                              "nnimap+personal:[Gmail]/Sent Mail"
                              ;; "nnimap+personal:Sent"
                              ;; "nnimap+personal:sent.2023"
                              "nnimap+personal:[Gmail]/Starred")
                             ("📰 News"
                              ;; "nnrss:Prot Codelog" "nnrss:HLTV.org"
                              "gwene.com.blogspot.petr-mitrichev" "gwene.me.tonsky.blog"
                              "gmane.emacs.announce" "gwene.com.rubyweekly"
                              ;; "gmane.emacs.devel" "gmane.emacs.tramp" "gmane.emacs.bugs"
                              ;; "gmane.emacs.gnus.general" "gmane.emacs.gnus.user"
                              "gwene.org.perlmonks.headlines" "gwene.com.perlweekly.perlweekly"
                              "gmane.comp.lang.go.general" "gwene.com.iximiuz" "gwene.net.cheney.dave"
                              "gwene.com.golangweekly" "gwene.org.golang.blog"
                              "gwene.com.thisweekinrust" "gwene.org.rust-lang.blog"
                              "gwene.com.youtube.feeds.videos.xml.user.ethoslab"
                              "gmane.comp.web.qutebrowser" "gmane.comp.web.elinks.user"
                              "gwene.io.kubernetes" "gwene.app.rsshub.leetcode.articles"
                              "gwene.rs.lobste" "gwene.org.hnrss.newest.points"
                              "gwene.net.lwn.headlines" "gwene.com.arcan-fe"
                              "gwene.io.github.matklad" "gwene.net.openmymind"
                              "gwene.org.quantamagazine" "gwene.com.tedinski"
                              "gwene.org.bitlbee.news.rss")
                             ("Unread")))))
