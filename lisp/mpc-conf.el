;;;; Mpc-conf -*- lexical-binding: t -*-

(dolist (hook '(mpc-status-mode-hook mpc-songs-mode-hook mpc-tagbrowser-dir-mode-hook))
  (add-hook hook (lambda nil (face-remap-add-relative 'header-line 'highlight))))
(add-hook 'mpc-status-mode-hook (lambda nil (setq-local mode-line-format nil)))
(add-hook 'mpc-tagbrowser-dir-mode-hook
          (lambda nil (setq-local mode-line-format default-mode-line-format)))

(with-eval-after-load 'mpc
  (defun mpc-cmd-list-change-arrow (orig-fun tag &optional other-tag value)
    "Change directory arrow character."
    (let ((result (funcall orig-fun tag other-tag value)))
      (if (eq tag 'Directory)
          (mapcar (lambda (dir)
                    (when (get-text-property 0 'display dir)
                      (let ((display (get-text-property 0 'display dir)))
                        (when (string-match-p "↪" display)
                          (put-text-property 0 (1+ (string-match "/" dir))
                                             'display (replace-regexp-in-string
                                                       "↪" "└─" display)
                                             dir))))
                    dir)
                  result)
        result)))
  (advice-add 'mpc-cmd-list :around #'mpc-cmd-list-change-arrow)

  (defun my-mpc-force-cover-refresh (orig-fun)
    "Force cover image refresh when song changes."
    (let ((old-file (cdr (assq 'file mpc-status))))
      (funcall orig-fun)
      (let ((new-file (cdr (assq 'file mpc-status))))
        (when (and old-file new-file (not (equal old-file new-file)))
          ;; Song changed, force status buffer refresh
          (let ((buf (mpc-proc-buffer (mpc-proc) 'status)))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer))
                (mpc-status-buffer-refresh))))))))

  (advice-add 'mpc--status-callback :around #'my-mpc-force-cover-refresh)

  (defun mpc-cover-image-find (file)
    "Extract embedded cover art from FILE using ffmpeg."
    (when-let* ((full-path (expand-file-name (concat mpc-mpd-music-directory "/" file)))
                ((file-exists-p full-path))
                ((executable-find "ffmpeg")))
      (let* ((cache-dir (expand-file-name "covers" (mpc-data-directory)))
             (cache-file (expand-file-name
                          (concat (md5 file) ".jpg")
                          cache-dir)))
        (unless (file-directory-p cache-dir)
          (make-directory cache-dir t))
        (unless (file-exists-p cache-file)
          (with-temp-buffer
            (when (zerop
                   (call-process "ffmpeg" nil nil nil
                                 "-i" full-path
                                 "-an" "-c:v" "copy"
                                 cache-file))
              cache-file)))
        (when (file-exists-p cache-file)
          cache-file))))
  (defun my-mpc-tagbrowser-toggle ()
    "Toggle directory at point."
    (interactive)
    (let ((name (buffer-substring (line-beginning-position)
                                  (line-end-position)))
          (prop (if (stringp mpc-tag) 
                    (intern mpc-tag) 
                  mpc-tag))  ; mpc-tag might already be a symbol
          (proc (mpc-proc)))
      (if (not (member name (process-get proc prop)))
          (process-put proc prop
                       (cons name (process-get proc prop)))
        (let ((new (delete name (process-get proc prop))))
          (setq name (concat name "/"))
          (process-put proc prop
                       (delq nil
                             (mapcar (lambda (x)
                                       (if (string-prefix-p name x)
                                           nil x))
                                     new)))))
      (mpc-tagbrowser-refresh)))

  (defun mpc-change-vol (inc)
    (interactive)
    (let* ((curvol (string-to-number (cdr (assq 'volume mpc-status))))
           (newvol (max 0 (min 100 (if inc (+ curvol 5) (- curvol 5))))))
      (mpc-proc-cmd (list "setvol" newvol) #'mpc-status-refresh)))
  (dolist (map (list mpc-tagbrowser-dir-mode-map mpc-status-mode-map mpc-songs-mode-map))
    (define-key map (kbd "SPC") ctl-x-map)
    (define-key map (kbd "-") (lambda nil (interactive) (mpc-change-vol nil)))
    (define-key map (kbd "+") (lambda nil (interactive) (mpc-change-vol t)))
    (define-key map (kbd "p") 'mpc-toggle-play)
    (define-key map (kbd "U") 'mpc-update))
  (define-key mpc-tagbrowser-mode-map (kbd "TAB") 'my-mpc-tagbrowser-toggle)
  (define-key mpc-tagbrowser-mode-map (kbd "RET") 'mpc-play-at-point))
