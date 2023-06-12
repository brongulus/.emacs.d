;;; early-init.el -*- lexical-binding: t; -*-
(menu-bar-mode -1)
(setq gc-cons-threshold most-positive-fixnum)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(setq package-enable-at-startup nil
      load-prefer-newer t
      make-backup-files nil
      create-lockfiles nil)

(provide 'early-init)
;;; early-init.el ends here
