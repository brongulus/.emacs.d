;;; popup.el --- Enable popup emacs frames -*- lexical-binding: t; -*-
;; Also check ~/.config/yabai/yabai-emacs.sh
(defun popup-frame-delete (&rest _)
  "Kill selected frame if it has parameter `popup-frame'."
  (when (frame-parameter nil 'popup-frame))
  (delete-frame))

(defmacro popup-frame-define (command title &optional delete-frame)
  "Define interactive function to call COMMAND in frame with TITLE."
  `(defun ,(intern (format "popup-frame-%s" command)) ()
     (interactive)
     (let* ((display-buffer-alist '(("")
                                    (display-buffer-full-frame)))
            (frame (make-frame
                    '((title . ,title)
                      (window-system . ns)
                      (popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " popup-frame-hidden-buffer")
       (condition-case nil
           (progn
             (call-interactively ',command)
             (delete-other-windows))
         (error (delete-frame frame)))
       (when ,delete-frame
         (sit-for 0.2)
         (delete-frame frame)))))

(popup-frame-define org-capture "capture-popup")
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-after-finalize-hook #'popup-frame-delete))

(defun my/agenda nil (interactive)
  (org-agenda nil "n"))
(popup-frame-define my/agenda "large-popup")
(with-eval-after-load 'org-agenda
  (advice-add #'org-agenda-exit :after #'popup-frame-delete))
