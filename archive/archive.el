;;; archive.el -*- lexical-binding: t; -*-

;;; Comments/Extras

;; (setq-default
;;  org-indent-mode t
;;  left-margin-width 2)

;; (setq projectile-switch-project-action #'projectile-dired)


;; (after! emacs-everywhere
;;   ;; Easier to match with a bspwm rule:
;;   ;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
;;   (setq emacs-everywhere-frame-name-format "emacs-anywhere")

;;   ;; The modeline is not useful to me in the popup window. It looks much nicer
;;   ;; to hide it.
;;   (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

;;   ;; Semi-center it over the target window, rather than at the cursor position
;;   ;; (which could be anywhere).
;;   ;; (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
;;   ;;   :override #'emacs-everywhere-set-frame-position
;;   ;;   (cl-destructuring-bind (x y width height)
;;   ;;       (emacs-everywhere-window-geometry window-info)
;;   ;;     (set-frame-position frame
;;   ;;                         (+ x (/ width 2) (- (/ width 2)))
;;   ;;                         (+ y (/ height 2)))))
;;   )

;; (after! company
;;   (setq company-idle-delay 0.2))

;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; (add-hook 'dired-mode-hook 'writeroom-mode)

;;(defun display-workspaces-in-minibuffer ()
;;  (with-current-buffer " *Minibuf-0*"
;;    (erase-buffer)
;;    (insert (+workspace--tabline))))
;;(run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;(+workspace/display)

;; (after! org
;; (add-hook! 'org-mode-hook #'writeroom-mode))

;; (add-hook 'org-mode-hook 'lsp-completion-mode)

;; (setq org-latex-listings 'minted) ;; Doesn't work with tectonic fml
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)

;; (use-package graphviz-dot-mode
;;   :config
;;   (setq graphviz-dot-indent-width 4))

;; (use-package company-graphviz-dot)

;; (use-package! appt
;;   :defer-incrementally t
;;   :config

;;   (appt-activate t)

;;   ;; use appointment data from org-mode
;;   (defun my-org-agenda-to-appt ()
;;     (interactive)
;;     (setq appt-time-msg-list nil)
;;     (org-agenda-to-appt))

;;   (setq appt-message-warning-time 5) ; Show notification 5 minutes before event
;;   (setq appt-display-interval appt-message-warning-time) ; Disable multiple reminders
;;   (setq appt-display-mode-line nil)

;;   ;; update alarms when starting emacs
;;   (my-org-agenda-to-appt)
;;   ;; (2) ... Everyday at 12:05am (useful in case you keep Emacs always on)
;;   (run-at-time "12:05am" (* 24 3600) 'my-org-agenda-to-appt)

;;   ;; (3) ... When TODO.org is saved
;;   (add-hook 'after-save-hook
;;             #'(lambda ()
;;                (if (string= (buffer-file-name) (concat (getenv "HOME") "~/Dropbox/org/todo.org"))
;;                    (my-org-agenda-to-appt))))

;;   ;; TODO Display appointments as a window manager notification (incorporate the script within elisp)
;;   (setq appt-disp-window-function 'my-appt-display)
;;   (setq appt-delete-window-function (lambda () t))

;;   (setq my-appt-notification-app "~/appt-notification.sh")

;;   (defun my-appt-display (min-to-app new-time msg)
;;     (if (atom min-to-app)
;;         (start-process "my-appt-notification-app" nil my-appt-notification-app min-to-app msg)
;;       (dolist (i (number-sequence 0 (1- (length min-to-app))))
;;         (start-process "my-appt-notification-app" nil my-appt-notification-app (nth i min-to-app) (nth i msg)))))
;; )

;; (add-hook! 'treemacs-mode-hook #'hl-todo-mode #'org-fragtog-mode #'org-mode)

;;(add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)

;;(map! pdf-view-mode-map
;;      :niv "h" #'pdf-annot-add-markup-annotation)

;; (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)

;; (map! :map c++-mode-map
;;       :localleader "c" (cmd! (compile (concat "g++ -std=c++17 -O2" buffer-file-name " -Wall"))))

;; HACK DOESN'T WORK LMFAO For now, disabling adding codeforces dir to lsp, but look into it later
;; (with-eval-after-load 'projectile
;;   (add-to-list 'projectile-globally-ignored-directories "/mnt/Data/Documents/code/codeforces"))

;; (defun proj-cpp-compile-command ()
;;   (cond
;;    ((and (eq major-mode 'c++-mode)
;;          (not (string-match-p (regexp-quote "\\.*/atcoder/\\.*") (buffer-file-name (current-buffer)))))
;;     "./g++ -std=c++17 -O2 -o ")))

;; (after! 'c++-mode
;;   (set (make-local-variable 'compile-command)
;;        (concat "g++ -std=c++17 -O2 " buffer-file-name " -Wall")))
;; (set-file-template! "/main\\.c\\(?:c\\|pp\\)$" :trigger "__main.cpp" :mode 'c++-mode)

;;(after! cc-mode
;;  (set-company-backend! 'c-mode
;;    '(:separate company-irony-c-headers company-irony)))
;;Windows
;;(after! lsp-mode
;;  (set-lsp-priority! 'clangd 1))
;;
;;Linux
;;(after! lsp-mode
;;  (require 'dap-cpptools)
;;  (yas-global-mode)
;;  )

;; (setq lsp-julia-default-environment "~/.julia/environments/v1.0")
;; (after! julia-mode
;;   (add-hook 'julia-mode-hook #'rainbow-delimiters-mode-enable)
;;   (add-hook! 'julia-mode-hook
;;     (setq-local lsp-enable-folding t
;;                 lsp-folding-range-limit 100)))

;;(eshell-command "setq stringvar $XDG_CURRENT_DESKTOP")
;;(if (string= stringvar "EXWM")
;;   (load! "+exwm")
;;nil
;;)

;; smtp config
;;(setq smtpmail-smtp-server "smtp.gmail.com"
;;      message-send-mail-function 'message-smtpmail-send-it)

;; report problems with the smtp server
;;(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
;;(setq message-defNotmault-mail-headers "Cc: \nBcc: \n")

;;(setq message-kill-buffer-on-exit t)

;;(after! notmuch
;;(set-popup-rule! "^\\*notmuch-hello" :ignore t))

;;(add-hook 'notmuch-hello-refresh-hook
;;              (lambda ()
;;                (if (and (eq (point) (point-min))
;;                         (search-forward "Saved searches:" nil t))
;;                    (progn
;;                     (forward-line)
;;                      (widget-forward 1))
;;                  (if (eq (widget-type (widget-at)) 'editable-field)
;;                      (beginning-of-line)))))

;;(after! notmuch
;;  (setq notmuch-saved-searches
;;        '((:name "inbox"    :query "tag:inbox not tag:trash"    :key "i")
;;          (:name "personal" :query "tag:personal"               :key "p")
;;          (:name "bits"     :query "tag:bits"                   :key "b")
;;          (:name "unread"   :query "tag:unread"                 :key "u")
;;          (:name "flagged"  :query "tag:flagged"                :key "f")
;;          (:name "sent"     :query "tag:sent"                   :key "s")
;;          )
;;        )
;;  )

;;FIXME (add-hook! 'notmuch-search-mode-hook #'notmuch-tree-mode)
;;(setq mm-text-html-renderer 'shr
;;      notmuch-multipart/alternative-discouraged '("text/plain" ;;"multipart/related")
;;      shr-use-colors nil
;;      gnus-blocked-images nil
;;      )
;; inline images?
;;(if (not (fboundp 'gnus-blocked-images))
;;    (defun gnus-blocked-images () nil))

;;FIXME
;;(setq notmuch-search-result-format
;;      '(("date" . "%12s | ")
;;        ("authors" . "%-20s | ")
;;        ("subject" . "%-54s")
;;        ("tags" . ":%s:")
;;        ))
;;(after! notmuch
;;  (setq notmuch-hello-sections
;;        '(notmuch-hello-insert-header +notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)
;;        notmuch-message-headers-visible nil))
;; Look for alternate methods of centering, writeroom destroys formatting
;;(add-hook! 'notmuch-show-mode-hook #'writeroom-mode)

;;(setq lsp-file-watch-threshold 2000)
;; (use-package! leetcode
;;   :commands leetcode)
