;;;; Org-conf -*- lexical-binding: t -*-

(use-package org
  :ensure nil
  :bind (("C-x y" . yank-media)
         :map org-mode-map
         ("C-'" . avy-goto-char-timer)
         ("C-," . my/scroll-other-window))
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode))
  :config
  (setq org-modules '(ol-info ol-eww org-habit))
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

  (setq org-directory (concat "~/Dropbox/" "org")
        org-use-sub-superscripts '{}
        ;; org-export-with-sub-superscripts nil
        org-ellipsis "…"
        org-pretty-entities t
        org-startup-indented t
        org-startup-truncated nil
        org-adapt-indentation t
        org-special-ctrl-a/e nil
        org-M-RET-may-split-line '((item . nil))
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

(use-package org-agenda
  :ensure nil
  :bind (("C-c o a" . (lambda nil (interactive)
                        (org-agenda nil "n")))
         :map org-agenda-mode-map
         ("q" . org-agenda-exit))
  :config
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
        org-agenda-show-all-dates nil
        org-log-done t
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

  (setq org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-todo-keyword-format "%-1s"
        org-agenda-use-time-grid t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-scheduled-leaders '("" "")
        org-agenda-deadline-leaders '("" "")
        org-agenda-todo-keyword-format ""
        org-agenda-block-separator (string-to-char " ")
        org-agenda-current-time-string "← now ─────────"
        org-agenda-time-grid
        '((daily today require-timed remove-matched)
          (800 1200 1600 2000)
          "       " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-prefix-format
        '((agenda . " %i %-12b%t%s")
          (todo . " %i %?-12b"))))

(use-package org-habit
  :after org-agenda
  :ensure nil
  :config
  (setq org-habit-show-habits-only-for-today t
        org-habit-show-done-always-green t
        org-habit-show-all-today t
        org-habit-missed-glyph ?◌;; 9676
        org-habit-completed-glyph ?● ;; 9679
        org-habit-today-glyph ?○ ;; 9675
        org-habit-following-days 1
        org-habit-preceding-days 21)

  (defun add-missed-day-glyph (graph)
    (dotimes (i (length graph))
      (when (char-equal ?\s (aref graph i))
        (let* ((face (get-char-property i 'face graph))
               (rep-str (propertize (char-to-string org-habit-missed-glyph)
                                    'face face)))
          (aset graph i (string-to-char rep-str)))))
    graph)

  (advice-add 'org-habit-build-graph :filter-return #'add-missed-day-glyph))

(use-package org-capture
  :ensure nil
  :bind ("C-c o c" . org-capture)
  ;; :hook (org-capture-mode . meow-insert)
  :config
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
