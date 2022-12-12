;;; agenda-sidebar.el --- Show agenda menu / inbox headlines -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Brongulus & Contributors

;; Author: Brongulus (2022)
;; Version: 0.9
;; Homepage: https://github.com/brongulus/agenda-sidebar
;; Package-Requires: ((emacs "28.1")(org "9.6"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; TODO
;;  Inbox
;;  Today
;;  Upcoming
;;  Anytime
;;  Someday
;;  Logbook

;; Keybinds -> Create minor mode?

;;; Commentary:
;; Shows a list of menu items for org agenda that allows for quick
;; overview.
;; For optimal experience, use patched font all-the-icons, add support
;; for terminal/non-doom fallback icons.

;;; Code:

(require 'ol)
(require 'svg-lib)
(require 'all-the-icons)

(defconst agenda-sidebar-buffer-name "*Agendalist*"
  "Name of the buffer that is used to display the sidebar entries.")

(defgroup agenda-sidebar nil
  "Variables for 'agenda-sidebar' package."
  :group 'org-agenda)

(defcustom agenda-sidebar-size 0.3
  "Size of the agenda-sidebar buffer."
  :type 'number)

(defcustom agenda-sidebar-position 'left
  "Fudge your docstring."
  :type '(choice (const above)
                 (const below)
                 (const left)
                 (const right)))

(defun agenda-sidebar-split-size ()
  "Convert `agenda-sidebar-size' to proper argument for `split-window'."
  (let ((frame-size (if (member agenda-sidebar-position '(left right))
                        (frame-width)
                      (frame-height))))
    (cond ((integerp agenda-sidebar-size) (- agenda-sidebar-size))
          (t (- (round (* frame-size agenda-sidebar-size)))))))

;;;###autoload
(defun agenda-sidebar-create-menu ()
  (interactive)
  (with-current-buffer agenda-sidebar-buffer-name
    (goto-char (point-max))
    (newline)
    (insert(concat
            (insert (all-the-icons-octicon "inbox" :v-adjust 0.1 :face 'all-the-icons-blue))
            (propertize "  Inbox" 'face '(:height 1.3 :inherit 'variable-pitch))))
    (newline)
            (insert-image (svg-lib-tag "check" nil :stroke 0 :background "#FFFFFF" :foreground "#000000" :margin 1 :radius 0 :scale 1))
            (newline)
            (insert-image (svg-lib-progress-pie 0.4 nil :margin 0 :stroke 2 :padding 1))
            (insert (propertize "  40% Completed" 'face '(:height 1.1 :inherit 'variable-pitch)))
    (newline)
    (insert(concat
            (insert (all-the-icons-faicon "star" :v-adjust 0.1 :face 'all-the-icons-yellow))
            (propertize "  Today" 'face '(:height 1.3 :inherit 'variable-pitch))))
    (newline)
    (insert(concat
            (insert (all-the-icons-faicon "calendar" :v-adjust 0.1 :face 'all-the-icons-red))
            (propertize "  Upcoming" 'face '(:height 1.3 :inherit 'variable-pitch))))
    (newline)
    (insert(concat
            (insert (all-the-icons-faicon "stack-overflow" :v-adjust 0.1 :face 'all-the-icons-dgreen))
            (propertize "  Anytime" 'face '(:height 1.3 :inherit 'variable-pitch))))
    (newline)
    (insert(concat
            (insert (all-the-icons-faicon "dropbox" :v-adjust 0.1 :face 'all-the-icons-dyellow))
            (propertize "  Someday" 'face '(:height 1.3 :inherit 'variable-pitch))))
    (newline)
    (newline)
    (insert(concat
            (insert (all-the-icons-faicon "book" :v-adjust 0.1 :face 'all-the-icons-green))
            (propertize "  Logbook" 'face '(:height 1.3 :inherit 'variable-pitch))))

    (newline)
    (insert (propertize "─────────" 'face '(:height 1.3 :face 'all-the-icons-dsilver)))
    ))

(defun agenda-sidebar-buffer-create ()
  "Return the imenu-list buffer. If it doesn't exist, create it."
  (or (get-buffer agenda-sidebar-buffer-name)
      (let ((buffer (get-buffer-create agenda-sidebar-buffer-name)))
        (split-window-right)
        (agenda-sidebar-create-menu)
        (with-current-buffer buffer
          (agenda-sidebar-major-mode)
          buffer))))


;;;###autoload
(defun agenda-sidebar-show ()
  "Show the agenda sidebar. If it doesn't exist, create it."
  (interactive)
  (progn
    (agenda-sidebar-buffer-create)
    (pop-to-buffer agenda-sidebar-buffer-name)
    (setq-local left-margin-width 4)
    (hide-mode-line-mode)
    ;; FIXME
    (window-resize (selected-window) -70 t)
    (evil-insert-state)))

(defvar agenda-sidebar-minor-mode)

(defun agenda-sidebar-quit ()
  (interactive)
    (if (one-window-p)
        (kill-this-buffer)
      (kill-buffer-and-window)))

(defvar agenda-sidebar-major-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'agenda-sidebar-quit)
    map))

(define-derived-mode agenda-sidebar-major-mode special-mode "Agendalist"
  (read-only-mode 1))

(provide 'agenda-sidebar)
;;; agenda-sidebar.el ends here
