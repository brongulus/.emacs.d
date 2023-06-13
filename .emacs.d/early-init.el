;;; early-init.el -*- lexical-binding: t; -*-

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-lines . 0) default-frame-alist)
(scroll-bar-mode -1)
;; (tooltip-mode -1)

(setq gc-cons-threshold (* 128 1024 1024)
      garbage-collection-messages nil)

(setq package-enable-at-startup nil
      load-prefer-newer t
      auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil
      create-lockfiles nil)

(provide 'early-init)
;;; early-init.el ends here
