;;;; Org-conf -*- lexical-binding: t -*-

(with-eval-after-load 'org
  (define-key (current-global-map) (kbd "C-x y") #'yank-media)
  (define-key org-mode-map (kbd "C-'") #'avy-goto-char-timer)
  (define-key org-mode-map (kbd "C-,") #'my-scroll-other-down)
  (define-key org-mode-map (kbd "C-c C-x C-m") #'my-toggle-org-markers)

  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-mode-hook (lambda () (org-cycle-hide-drawers 'all)))

  (setq org-modules '(ol-info ol-eww org-habit))
  (defun my-toggle-org-markers nil (interactive)
         (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
         (revert-buffer-quick))
  ;; Taken from rougier: org-outer-indent
  (defun org-outer-indent--compute-prefixes ()
    "Compute prefix strings for regular text and headlines."
    (setq org-indent--heading-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (setq org-indent--inlinetask-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (setq org-indent--text-line-prefixes
          (make-vector org-indent--deepest-level nil))
    ;; Find the lowest headline level (FIXME)
    (let* (;; (headline-levels (or (org-element-map
           ;;                          (org-element-parse-buffer) 'headline
           ;;                        #'(lambda (item)
           ;;                            (org-element-property :level item)))
           ;;                      '()))
           ;; (max-level (seq-max (if headline-levels
           ;;                         headline-levels
           ;;                       '(0))))
           (line-indentation (+ 3 4))
           (headline-indentation))
      (dotimes (level org-indent--deepest-level)
        (setq headline-indentation
              (max 0 (- line-indentation (+ 1 level))))
        (aset org-indent--inlinetask-line-prefixes level
              (make-string line-indentation ?\s))
        (aset org-indent--text-line-prefixes level
              (make-string line-indentation ?\s))
        (aset org-indent--heading-line-prefixes level
              (make-string headline-indentation ?\s))))
    (setq-local org-hide-leading-stars nil))

  (advice-add 'org-indent--compute-prefixes :override
              #'org-outer-indent--compute-prefixes)

  ;; configure <s template for org-src-blocks
  (require 'org-tempo)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (or (char-equal c ?\[)
                                     (char-equal c ?<))
                                 t
                               (,electric-pair-inhibit-predicate c))))))

  (org-clock-persistence-insinuate)
  (setq org-global-properties ; org clock in effort times default
        '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00"))
        org-clock-history-length 23
        org-clock-persist t
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t)
  
  (setq org-directory (concat "~/Dropbox/" "org")
        org-use-sub-superscripts '{}
        ;; org-export-with-sub-superscripts nil
        org-ellipsis "…" ; "  ·"
        org-pretty-entities t
        org-startup-indented t
        org-startup-truncated nil
        org-adapt-indentation t
        org-special-ctrl-a/e nil
        org-M-RET-may-split-line '((item . nil))
        org-insert-heading-respect-content t
        org-fold-catch-invisible-edits 'show-and-error
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-fontify-quote-and-verse-blocks t
        org-fontify-done-headline nil
        org-src-fontify-natively t
        ;; tectonic
        org-highlight-latex-and-related '(latex)
        org-preview-latex-default-process 'tectonic
        org-preview-latex-process-alist
        '((tectonic :programs
                    ("tectonic" "convert")
                    :description "pdf > png"
                    :message "you need install the programs: tectonic and imagemagick."
                    :image-input-type "pdf"
                    :image-output-type "png"
                    :image-size-adjust (1.0 . 1.0)
                    :latex-compiler
                    ("tectonic -Z shell-escape-cwd=%o -Z continue-on-errors --outfmt pdf --outdir %o %f")
                    :image-converter
                    ("magick convert -density %D -trim -antialias %f -quality 300 %O")))
        org-latex-compiler "tectonic"
        org-latex-pdf-process
        '("tectonic -X compile -Z shell-escape -Z continue-on-errors --outdir=%o %f")))

(define-key (current-global-map) (kbd "C-c o a")
            #'(lambda nil (interactive) (org-agenda nil "n")))
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "q") #'org-agenda-exit)
  (set-face-attribute 'org-agenda-clocking nil :inherit 'highlight)
  (set-face-attribute 'org-agenda-date nil :inherit 'org-agenda-date-today)
  (set-face-attribute 'org-time-grid nil :foreground (face-foreground 'font-lock-comment-face))

  (add-to-list 'display-buffer-alist
               '("\\*Calendar\\*"
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (window-parameters (height . 0.33))))

  (setq org-agenda-files (list org-directory)
        org-agenda-ignore-properties '(effort appt stats category)
        org-agenda-dim-blocked-tasks nil
        org-agenda-use-tag-inheritance nil
        org-agenda-inhibit-startup t
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-start-with-log-mode t
        org-agenda-log-mode-add-notes nil
        org-agenda-remove-tags t
        org-agenda-show-all-dates nil
        org-agenda-start-on-weekday 0
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-include-deadlines t)

  (defun elegant-agenda--title nil ;; src: elegant-agenda-mode
    (when-let* ((title (when (and org-agenda-redo-command
                                  (stringp (cadr org-agenda-redo-command)))
                         (format "─  %s "
                                 (mapconcat
                                  #'identity
                                  (split-string-and-unquote
                                   (cadr org-agenda-redo-command) "")
                                  ""))))
                (width (window-width)))
      (face-remap-set-base 'header-line :height 1.4)
      (setq-local header-line-format
                  (format "%s %s" title (make-string (- width (length title)) ?─ t)))))
  (add-hook 'org-agenda-finalize-hook #'elegant-agenda--title)

  (setq my/org-grid-w 31)
  ;; TODO https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
  (defun my/org-agenda-clean-clockin (orig-fun &rest args)
    "Reformat clock entries to show time ranges after task name."
    (let ((result (apply orig-fun args)))
      (when (and result (stringp result))
        (cond
         ((string-match
           "\\([0-9]+:[0-9]+\\)-\\([0-9]+:[0-9]+\\)Clocked:\\s-+(\\([^)]+\\))\\(.+\\)$" result)
          (let* ((start-time (match-string 1 result))
                 (duration (concat "(" (match-string 3 result) ")"))
                 (task (string-trim (match-string 4 result)))
                 (prefix (substring result 0 (match-beginning 1)))
                 (padlen (max 0 (- (1- my/org-grid-w) (length task))))
                 (pad (make-string padlen ?┄ t)))
            (format "%s%s %7s %s %s" prefix start-time duration task pad)))
         ((string-match
           "\\([0-9]+:[0-9]+\\)\\s-+Closed:\\s-+\\(.+\\)$" result)
          (let* ((time (match-string 1 result))
                 (task (string-trim (match-string 2 result)))
                 (prefix (substring result 0 (match-beginning 1)))
                 (padlen (max 0 (- (+ 2 my/org-grid-w) (length task))))
                 (pad (make-string padlen ?┄ t)))
            (format "%s%s %7s ✓ %s %s" prefix time "" task pad)))
         ((string-match
           "\\([0-9]+:[0-9]+\\)\\s-+Clocked:\\s-+\\(.+\\)$" result)
          (let* ((time (match-string 1 result))
                 (task (string-trim (match-string 2 result)))
                 (prefix (substring result 0 (match-beginning 1)))
                 (padlen (max 0 (- (1- my/org-grid-w) (length task))))
                 (pad (make-string padlen ?┄ t)))
            (format "%s%s %7s %s %s" prefix time "" task pad)))
         (t result)))))
  (advice-add 'org-agenda-format-item :around #'my/org-agenda-clean-clockin)

  (setq org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-todo-keyword-format "%-1s"
        org-agenda-use-time-grid t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-scheduled-leaders '("" "")
        org-agenda-deadline-leaders '("" "")
        org-agenda-todo-keyword-format ""
        org-agenda-block-separator (string-to-char " ")
        org-agenda-current-time-string
        (concat "← now " (make-string (- my/org-grid-w 6) ?─ t))
        org-agenda-time-grid
        `((daily today require-timed remove-matched)
          (800 1200 1600 2000)
          ,(make-string 9 ?  t) ,(make-string my/org-grid-w ?┄ t))
        org-agenda-prefix-format
        '((agenda . " %i %-16b%t%s")
          (todo . " %i %?-16b"))))

(with-eval-after-load 'org-habit
  ;; :after org-agenda
  (setq org-habit-show-habits-only-for-today t
        org-habit-show-done-always-green t
        org-habit-show-all-today t
        org-habit-missed-glyph ?o ;?◌ ;; 9676
        org-habit-completed-glyph ?* ;?● ;; 9679
        org-habit-today-glyph ?o ;?○ ;; 9675
        org-habit-following-days 1
        org-habit-preceding-days 21)

  (defun add-missed-day-glyph (graph)
    (dotimes (i (length graph))
      (when (char-equal ?\s (aref graph i))
        (let ((face (get-char-property i 'face graph)))
          (aset graph i org-habit-missed-glyph)
          (put-text-property i (1+ i) 'face face graph))))
    graph)

  (advice-add 'org-habit-build-graph :filter-return #'add-missed-day-glyph)
  (set-face-attribute 'org-habit-clear-face nil :background 'unspecified 
                      :foreground (face-foreground 'font-lock-comment-face))
  (set-face-attribute 'org-habit-clear-future-face nil :background 'unspecified 
                      :foreground (face-foreground 'font-lock-comment-face))
  (set-face-attribute 'org-habit-alert-face nil :background 'unspecified 
                      :foreground (face-foreground 'ansi-color-yellow))
  (set-face-attribute 'org-habit-alert-future-face nil :background 'unspecified 
                      :foreground (face-foreground 'ansi-color-yellow))
  (set-face-attribute 'org-habit-overdue-face nil :background 'unspecified 
                      :foreground (face-foreground 'ansi-color-red))
  (set-face-attribute 'org-habit-overdue-future-face nil :background 'unspecified 
                      :foreground (face-foreground 'ansi-color-red))
  (set-face-attribute 'org-habit-ready-face nil :background 'unspecified 
                      :foreground (face-foreground 'ansi-color-green))
  (set-face-attribute 'org-habit-ready-future-face nil :background 'unspecified 
                      :foreground (face-foreground 'ansi-color-green)))

(define-key (current-global-map) (kbd "C-c o c") #'org-capture)
(with-eval-after-load 'org-capture
  ;; :hook (org-capture-mode . meow-insert)
  (add-hook 'org-capture-mode-hook
            (lambda nil
              (setq-local header-line-format nil)))
  (setq org-capture-file
        (concat org-directory "/inbox.org")
        org-joural-file
        (concat org-directory "/journal.org")
        org-capture-templates
        '(("t" "TODO" entry
           (file+headline org-capture-file "Tasks")
           "* TODO %?\n%<%d %b '%g %R>%i %a" :prepend t)
          ("n" "Note" entry
           (file+headline org-capture-file "Notes")
           "* %?\n%i %a" :prepend t)
          ;; https://www.twelvety.net/2024/12/styling-a-markdown-one-line-journal-in-emacs
          ("j" "Journal" plain
           (file+datetree org-joural-file)
           "%<%d %b, %a> | %?" :tree-type month :empty-lines 1)
          ("h" "Habit" entry
           (file+headline org-capture-file "Habit")
           "* TODO %?\n:PROPERTIES:\n:STYLE: habit\n:END:"
           :prepend t))))
