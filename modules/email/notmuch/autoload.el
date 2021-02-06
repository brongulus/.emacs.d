;;; email/notmuch/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +notmuch-hello-insert-saved-searches ()
  "Insert the saved-searches section."
  (let ((searches (notmuch-hello-query-counts
		   (if notmuch-saved-search-sort-function
		       (funcall notmuch-saved-search-sort-function
				notmuch-saved-searches)
		     notmuch-saved-searches)
		   :show-empty-searches notmuch-show-empty-saved-searches)))
    (when searches
      (widget-insert "Saved searches: ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (customize-variable 'notmuch-saved-searches))
		     "edit")
      (widget-insert "\n\n")
      (let ((start (point)))
	(notmuch-hello-insert-buttons searches)
	(indent-rigidly start (point) notmuch-hello-indent)))))

;; TODO add an unread count in modeline

(defvar notmuch-unread-string "")
(defvar notmuch-unread-mail-count nil)
