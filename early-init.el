;;; early-init.el -*- lexical-binding: t; -*-
(defvar my/saved-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      load-prefer-newer noninteractive
      garbage-collection-messages nil)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold (* 50 1024 1024)
                    gc-cons-percentage 0.1
                    file-name-handler-alist my/saved-file-name-handler-alist)))

;; src: skangas
(when (>= emacs-major-version 27)
  (defun gc-on-last-frame-out-of-focus ()
    "GC if all frames are inactive."
    (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
        (garbage-collect)))
  (add-function :after after-focus-change-function
                #'gc-on-last-frame-out-of-focus))

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (garbage-collect)
              (setq gc-cons-threshold (* 60 1024 1024))))

;; Android
(when (string-equal system-type "android")
  (let ((termuxpath "/data/data/com.termux/files/usr/"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath "bin"))
    (setenv "LD_LIBRARY_PATH" (concat (getenv "LD_LIBRARY_PATH") ":" termuxpath "lib"))
    (push (concat termuxpath "bin") exec-path))
  (setq overriding-text-conversion-style nil)
  (set-frame-font "monospace 16" nil t))

(setq-default default-frame-alist
              '((menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars)
                (left-fringe . 15)
                (right-fringe . 15)
                (internal-border-width . 00)
                (fullscreen . fullboth)
                ;; (font . "VictorMono Nerd Font Mono-16"))
                (font . "Zed Mono-17"))
              fringe-indicator-alist
              (assq-delete-all 'truncation fringe-indicator-alist)
              cursor-in-non-selected-windows nil
              bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right)

(fset 'display-startup-echo-area-message 'ignore)

(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil right-curly-arrow))

(when (file-exists-p (locate-user-emacs-file "package-quickstart.el"))
  (defvar package-quickstart)
  (setq package-quickstart t))

(setq package-enable-at-startup nil
      redisplay-skip-fontification-on-input t
      window-combination-resize t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(defun ar/show-welcome-buffer () ;; xendoium (centering issues...)
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path (fancy-splash-image-file))
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (and image (car size)))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (max (window-width) 195) width) 2)))
           (prompt-title (format "%d packages loaded in %s"
                                 (length package-activated-list)
                                 (format "%.2f seconds"
                                         (float-time
                                          (time-subtract after-init-time before-init-time))))))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (max (window-width) 195) (string-width prompt-title)) 2)) ?\ ))
      (insert prompt-title))
    (setq-local cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)
    (local-set-key (kbd "RET") 'kill-this-buffer)))

(add-hook 'emacs-startup-hook (lambda ()
                                (if (or (> (length command-line-args) 2)
                                        (file-exists-p (locate-user-emacs-file ".emacs.desktop")))
                                    (message (format "%d packages loaded in %s"
                                                     (length package-activated-list)
                                                     (format "%.2f seconds"
                                                             (float-time
                                                              (time-subtract after-init-time before-init-time)))))
                                  (ar/show-welcome-buffer))))

(if (eq system-type 'darwin)
    (setq mac-option-modifier 'meta)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))

(provide 'early-init)
;;; early-init.el ends here
