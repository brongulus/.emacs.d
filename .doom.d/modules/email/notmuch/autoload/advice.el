;;; email/notmuch/autoload/advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +notmuch-search-insert-field-a (field format-string result)
  (setq sub-str (notmuch-sanitize(plist-get result :subject)))
  (cond
   ((string-equal field "date")
    (insert (propertize (format format-string (plist-get result :date_relative))
			'face 'notmuch-search-date)))
   ((string-equal field "count")
    (insert (propertize (format format-string
				(format "%s" (plist-get result :total)))
			'face 'notmuch-search-count)))
   ((string-equal field "subject")
    (insert (propertize (format format-string
				(truncate-string-to-width sub-str 120 nil nil t))
			'face 'notmuch-search-subject)))
   ((string-equal field "authors")
    (notmuch-search-insert-authors
     format-string (notmuch-sanitize (plist-get result :authors))))
   ((string-equal field "tags")
    (let ((tags (plist-get result :tags))
	  (orig-tags (plist-get result :orig-tags)))
      (insert (format format-string (notmuch-tag-format-tags tags orig-tags)))))))


;;(defun +notmuch-hello-insert-saved-searches ()
;;  "Insert the saved-searches section."
;;  (let ((searches (notmuch-hello-query-counts
;;		   (if notmuch-saved-search-sort-function
;;		       (funcall notmuch-saved-search-sort-function
;;				notmuch-saved-searches)
;;		     notmuch-saved-searches)
;;		   :show-empty-searches notmuch-show-empty-saved-searches)))
;;    (when searches
;;      (widget-insert "Saved searches: ")
;;      (widget-create 'push-button
;;		     :notify (lambda (&rest ignore)
;;			       (customize-variable 'notmuch-saved-searches))
;;		     "edit")
;;     (widget-insert "\n\n")
;;      (let ((start (point)))
;;	(notmuch-hello-insert-buttons searches)
;;	(indent-rigidly start (point) notmuch-hello-indent)))))

;; TODO add an unread count in modeline

;;(defvar notmuch-unread-string "")
;;(defvar notmuch-unread-mail-count nil)




;; advice
;;(defun +notmuch/truncate (str)
;;  (truncate-string-to-width str 80 nil nil t))
;;(advice-add 'notmuch-sanitize :after #'+notmuch/truncate)
