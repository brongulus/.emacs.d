;;; ui/doom-dashboard/config.el -*- lexical-binding: t; -*-

(use-package! dashboard
  :hook (doom-init-modules . dashboard-setup-startup-hook)
  :hook (doom-init-modules . dashboard-open)
  :init
  (setq dashboard-center-content t
        dashboard-startup-banner 'logo
        dashboard-items '((agenda . 10)
                          (bookmarks . 5))
        dashboard-set-heading-icons t
        dashboard-week-agenda t)
  :config
  (dashboard-setup-startup-hook)
  (dashboard-open))

(setq doom-fallback-buffer-name "*dashboard*")
