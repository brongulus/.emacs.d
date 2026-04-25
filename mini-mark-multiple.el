;;; mini-mark-multiple.el --- Minimal mark-multiple -*- lexical-binding: t -*-
;;; Commentary:
;;  Inspired by https://github.com/magnars/mark-multiple.el
;;  Regurgitated by Opus 4.6, taking the above as input ^
;;; Code:

(defface mmm/master-face '((t :inherit region)) "Master region." :group 'mini-mark-multiple)
(defface mmm/mirror-face '((t :inherit secondary-selection)) "Mirror region." :group 'mini-mark-multiple)

(defvar-local mmm/master nil)
(defvar-local mmm/mirrors nil)
(defvar-local mmm/--last nil)

(defvar mmm/--unnarrow
  '(mmm/mark-next-like-this mmm/mark-previous-like-this mmm/mark-all-like-this
                            mmm/mark-all-in-defun mmm/clear-all keyboard-quit))

(defvar mmm/keymap (let ((m (make-sparse-keymap))) (define-key m (kbd "RET") #'mmm/clear-all) m))

(defun mmm/--ov (beg end face &rest props)
  (let ((o (make-overlay beg end nil nil t)))
    (overlay-put o 'priority 100) (overlay-put o 'face face)
    (while props (overlay-put o (pop props) (pop props))) o))

(defun mmm/clear-all ()
  "End session, remove overlays and hooks."
  (interactive) (widen)
  (when mmm/master (when (overlayp mmm/master) (delete-overlay mmm/master))
        (mapc #'delete-overlay mmm/mirrors))
  (setq mmm/master nil mmm/mirrors nil mmm/--last nil)
  (remove-hook 'pre-command-hook #'mmm/--pre t)
  (remove-hook 'post-command-hook #'mmm/--post t))

(defun mmm/add-mirror (beg end) (push (mmm/--ov beg end 'mmm/mirror-face) mmm/mirrors))

(defun mmm/create-master (beg end)
  (mmm/clear-all)
  (setq mmm/master (mmm/--ov beg end 'mmm/master-face 'keymap mmm/keymap)
        mmm/mirrors nil mmm/--last (buffer-substring beg end))
  (add-hook 'pre-command-hook #'mmm/--pre nil t)
  (add-hook 'post-command-hook #'mmm/--post nil t))

(defun mmm/--undo-p ()
  (and (symbolp this-command) (string-match-p "undo" (symbol-name this-command))))

(defun mmm/--quit-p ()
  (or (eq this-command 'keyboard-quit)
      (eq this-command (command-remapping 'keyboard-quit))))

(defun mmm/--pre ()
  (when (and mmm/master (overlay-buffer mmm/master))
    (cond
     ((mmm/--quit-p) (mmm/clear-all))
     ((and mmm/mirrors (not (memq this-command mmm/--unnarrow)) (not (mmm/--undo-p)))
      (setq inhibit-redisplay t)
      (narrow-to-region (overlay-start mmm/master) (overlay-end mmm/master))))))

(defun mmm/--post ()
  (widen) (setq inhibit-redisplay nil)
  (if (or (null mmm/master) (not (overlay-buffer mmm/master)))
      (mmm/clear-all)
    (let ((cur (buffer-substring (overlay-start mmm/master) (overlay-end mmm/master))))
      (unless (or (string= cur mmm/--last) (mmm/--undo-p))
        (when mmm/mirrors
          (let ((cg (when (listp buffer-undo-list) (prepare-change-group))))
            (save-excursion
              (dolist (m (seq-sort-by #'overlay-start #'> mmm/mirrors))
                (when (overlay-buffer m)
                  (goto-char (overlay-start m))
                  (delete-region (overlay-start m) (overlay-end m))
                  (insert cur))))
            (when cg (undo-amalgamate-change-group cg)))))
      (setq mmm/--last cur))))

;;; --- Helpers ---

(defun mmm/--ensure ()
  (when (and mmm/master (not (overlay-buffer mmm/master))) (mmm/clear-all))
  (unless (or mmm/master (region-active-p))
    (let ((b (find-tag-default-bounds)))
      (unless b (user-error "No region or symbol at point"))
      (mmm/create-master (car b) (cdr b))))
  (unless mmm/master (mmm/create-master (region-beginning) (region-end))))

(defun mmm/--search (forward skip remove)
  (mmm/--ensure)
  (let* ((acc (if forward #'overlay-end #'overlay-start))
         (cmp (if forward #'> #'<))
         (best (lambda () (car (seq-sort-by acc cmp mmm/mirrors)))))
    (cond
     (remove (unless mmm/mirrors (user-error "No mirrors"))
             (let ((m (funcall best))) (setq mmm/mirrors (delq m mmm/mirrors)) (delete-overlay m)))
     (t (when skip
          (unless mmm/mirrors (user-error "No mirrors"))
          (let ((m (funcall best))) (setq mmm/mirrors (delq m mmm/mirrors)) (delete-overlay m)))
        (save-excursion
          (goto-char (if forward
                         (apply #'max (overlay-end mmm/master) (mapcar #'overlay-end mmm/mirrors))
                       (apply #'min (overlay-start mmm/master) (mapcar #'overlay-start mmm/mirrors))))
          (let* ((case-fold-search nil)
                 (s (buffer-substring (overlay-start mmm/master) (overlay-end mmm/master))))
            (unless (funcall (if forward #'search-forward #'search-backward) s nil t)
              (user-error "No more \"%s\"" s))
            (mmm/add-mirror (match-beginning 0) (match-end 0))))))))

;;; --- Interactive commands ---

;;;###autoload
(defun mmm/mark-next-like-this (arg)
  "Mark next match.  Negative ARG removes, zero skips then marks."
  (interactive "p") (mmm/--search t (= arg 0) (< arg 0)))

;;;###autoload
(defun mmm/mark-previous-like-this (arg)
  "Mark previous match.  Negative ARG removes, zero skips then marks."
  (interactive "p") (mmm/--search nil (= arg 0) (< arg 0)))

;;;###autoload
(defun mmm/mark-all-like-this ()
  "Mark every match in buffer."
  (interactive) (mmm/--ensure)
  (mapc #'delete-overlay mmm/mirrors) (setq mmm/mirrors nil)
  (let ((case-fold-search nil)
        (s (buffer-substring (overlay-start mmm/master) (overlay-end mmm/master)))
        (ms (overlay-start mmm/master)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward s nil t)
        (unless (= (match-beginning 0) ms)
          (mmm/add-mirror (match-beginning 0) (match-end 0))))))
  (deactivate-mark))

;;;###autoload
(defun mmm/mark-all-in-defun ()
  "Mark all matches within current defun."
  (interactive)
  (let* ((bounds (if (use-region-p) (cons (region-beginning) (region-end))
                   (find-tag-default-bounds)))
         (s (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (pt (car bounds)))
    (unless s (user-error "No region or symbol at point"))
    (save-excursion
      (let ((beg (progn (beginning-of-defun) (point)))
            (end (progn (end-of-defun) (point))))
        (mmm/clear-all)
        (mmm/create-master pt (+ pt (length s)))
        (goto-char beg)
        (let ((case-fold-search nil))
          (while (search-forward s end t)
            (unless (= (match-beginning 0) pt)
              (mmm/add-mirror (match-beginning 0) (match-end 0)))))))
    (unless mmm/master (user-error "No match for \"%s\"" s))
    (deactivate-mark) (goto-char (overlay-start mmm/master))))

(provide 'mini-mark-multiple)
;;; mini-mark-multiple.el ends here
