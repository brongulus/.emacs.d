;;; foxy.el --- (foc-cc) FastOlympicCoding Competitive Companion Helper  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Prashant Tak

;; Author: Prashant Tak <prashantrameshtak@gmail.com>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: competitive programming

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; If you want richer diffs between output and answer, install diff-lisp package
;; I have opted to make that choice optional as I don't want external packages.

;; _,-=._              /|_/|
;; `-.}   `=._,.-=-._.,  @ @._,
;;    `._ _,-.   )      _,.-'
;;       `    G.m-"^m`m'        by Dmytro O. Redchuk

;; TODO:
;; Add run timeout per testcase
;; Error handling while fetching ip/op
;; Add new testcases function
;; C-a C-d remove tc

;;; Code:
(require 'json)
;; (require 'subr)
(require 'timer)
(require 'cl-lib)
(require 'files)

;; (require 'widget)
;; (require 'wid-edit)
(if (package-installed-p 'diff-lisp)
    (require 'diff-lisp))

;;;; Competitive Companion
;; https://stackoverflow.com/a/6200347
(defvar foxy-compile-command ;"g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion -O2 -DLOCAL "
  "The command used to compile the source file.")

(make-variable-buffer-local 'foxy-compile-command)

(defvar foxy-listen-port 27121
    "Port of the server.")

(defvar foxy-listen-host "127.0.0.1"
  "Host of the server.")

(defvar foxy-currently-listening nil)

(defun foxy-listen-start nil
    "Start the competitive-companion tcp client listener."
    (interactive)
    (if foxy-currently-listening
        (foxy-listen-stop)
      (progn
        (setq foxy-currently-listening t))
      (message "Started listening for competitive-companion")
      (make-network-process :name "Fetch Contest" :buffer "*fetch*" :family nil
                            :server t :host foxy-listen-host :service foxy-listen-port
                            :sentinel 'foxy-listen-sentinel :filter 'foxy-listen-filter)))

(defun foxy-listen-stop nil
  "Stop the competitive-companion tcp listener."
  (interactive)
  (delete-process "Fetch Contest")
  (kill-buffer "*fetch*")
  (message "Stopped listening to competitive-companion")
  (setq foxy-currently-listening nil))

(defun foxy-listen-filter (proc string)
  "Parses the incoming JSON data (STRING) from competitive-companion
and populates the testcase files."
  (let* ((json-input (json-read-from-string
                     (replace-regexp-in-string ".*\r\n" ""
                                               string)))
         (prob-name (replace-regexp-in-string
                     ".*/" ""
                     (cdr (assoc 'url json-input))))
         (tests (cdr (assoc 'tests json-input)))
         (i 1))
    (make-directory prob-name)
    ;; iterate over number of testcases
    (while (< i (1+ (length tests)))
      ;; populate input file
      (find-file (concat default-directory prob-name "/in"
                         (format "%s" i)".txt"))
      (insert (format "%s" (cdr (assoc 'input
                                       (aref (cdr (assoc 'tests json-input)) (1- i))))))
      (basic-save-buffer)
      (kill-buffer)
      ;; populate output file
      (find-file (concat default-directory prob-name "/ans"
                           (format "%s" i) ".txt"))
      (insert (format "%s" (cdr (assoc 'output
                                       (aref (cdr (assoc 'tests json-input)) (1- i))))))
      (basic-save-buffer)
      (kill-buffer)
      (setq i (1+ i)))
      (message "Testcases added for %s" prob-name)))

(defun foxy-listen-sentinel (proc msg)
  "Echo client quit when MSG says broken connection."
  (when (string= msg "connection broken by remote peer\n")
    (message (format "Client %s has quit" proc))))

(defun foxy-start-server-with-timer nil
  "Start the server that listens to competitive-companion for 1 minute."
  (interactive)
  (foxy-listen-start)
  (run-at-time 60 nil #'foxy-listen-stop))

(defun foxy-read-file (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-string)))

(defun foxy-run-all-tests nil
  "Run all the available testcases for the current problem."
  (interactive)
  (let* ((file-ext (format "%s" (file-name-extension buffer-file-name)))
        (bin-name
         (if (string-equal file-ext "py")
             (concat "python " (file-relative-name (buffer-file-name) default-directory))
           "./a.out"))
        (tests (length (directory-files
                         default-directory nil "ans.*txt")))
         (i 1)
         (results ""))
    ;; Compile if cpp/rs file
    (unless (string-equal file-ext "py")
                                        ;(file-exists-p "a.out")
      (compile (concat foxy-compile-command buffer-file-name))
      (sleep-for 5)) ;; FIXME!!!
    (while (< i (1+ tests)) ;; TODO: What if there's RTE, TLE?
      ;; Get the output, debug data and the answer as strings
      (let* ((test-output ;; (2>./deb.txt handles debug output)
             (shell-command-to-string (concat bin-name " < ./in"
                                              (format "%s" i) ".txt 2> ./deb.txt")))
            (test-debug (foxy-read-file "./deb.txt"))
            (test-ans (foxy-read-file (concat "./ans"
                                           (format "%s" i) ".txt"))))
        ;; If output is same as test, continue else show all three strings
        (if (string-equal test-output test-ans)
            (setq results (concat results "Testcase " (format "%s" i) " passed!\n"))
          (progn
            (if (fboundp 'diff-lisp-diff-strings)
                (setq results (concat results (diff-lisp-diff-strings
                                               test-output test-ans
                                               (concat "Testcase "
                                                       (format "%s" i) " mismatch!"))))
              (setq results (concat results
                                    "Testcase " (format "%s" i)
                                    " mismatch!\nOutput:\n" test-output
                                    "\nAns:\n" test-ans "\n")))
            (unless (string-equal "" test-debug)
              (setq results (concat results "Debug:\n" test-debug))))))
      (delete-file "./deb.txt")
      (setq i (1+ i)))
    ;; Populate the results buffer and show in a side window
    (with-current-buffer (get-buffer-create "*Results*")
      (let ((display-buffer-mark-dedicated t))
        (display-buffer (current-buffer)
                        '(display-buffer-in-side-window
                          (side . right)
                          (direction . right)
                          (slot . 0)
                          (window-width . 40)
                          (window-parameters
                           (dedicated . t)
                           (no-delete-other-windows . t)))))
        (let ((inhibit-read-only t)) (erase-buffer))
        (remove-overlays)
        (insert (format "%s" results))
        (diff-mode)
        (use-local-map
         (make-composed-keymap
          (list (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "q") 'kill-buffer-and-window)
                  (define-key map (kbd "C-M-b") 'window-toggle-side-windows)
                  (if (bound-and-true-p evil-mode)
                      (evil-make-overriding-map map 'normal))
                  map))
          widget-keymap))))
  (windmove-right))

(defun elem-index (item seq)
  (cl-position item seq :test #'string-equal))

(defun foxy-cycle-files (&optional step)
  "Go STEP steps in the current directory listing.
Given a step of 1 (the default), will go to the next file. -1 means previous file."
  (interactive)
  (when (not buffer-file-name)
    (user-error "Not visiting a file - cannot cycle"))
  (let* ((file-ext (format "%s" (file-name-extension buffer-file-name)))
         (arg (or step 1))
         (dir (substring default-directory 0 -1))
         (dirs (seq-filter #'file-directory-p
                (directory-files
                 (file-name-directory (directory-file-name
                                       (file-name-directory default-directory)))
                 t directory-files-no-dot-files-regexp nil)))
         (index (elem-index dir dirs))
         (new-index (mod (+ arg index) (length dirs))))
   (find-file (concat (nth new-index dirs) "/main." file-ext))))

(global-set-key (kbd "C-M-b") #'foxy-run-all-tests)
(global-set-key (kbd "C-M-c") #'foxy-cycle-files)
(global-set-key (kbd "C-M-l") #'foxy-start-server-with-timer)

;;;; TODO: Widget

;; (defface persistent-variable '((t :inherit custom-variable-tag
;;                                 :height 1.2
;;                                 :weight semi-bold))
;;   "Face for Persistent menu headers.")

;; (custom-set-faces
;;  `(widget-button ((t (:foreground unspecified)))))

;; (defmacro with-visible-org-buffer (body)
;;   `(if-let
;;        ((win (seq-find (lambda (w)
;;                          (eq
;;                           (buffer-mode (window-buffer w))
;;                          'org-mode))
;;               (window-list))))
;;        (with-current-buffer (window-buffer win)
;;         (progn ,body))
;;      (message "No org-buffer visible!")))

;; (defmacro persistent-choice (desc val choices notify-func)
;;   `(progn
;;     (widget-create 'menu-choice
;;      :format
;;      (concat "%{%t%}"
;;       (propertize " " 'display
;;        '(space :align-to 20))
;;       "%[%v%]")
;;      :tag ,desc
;;      :sample-face 'persistent-variable
;;      :value ,val
;;      ;; :help-echo "Choose color theme"
;;      :notify #',notify-func
;;      ,@(cl-loop for (choice-tag . choice-val) in choices
;;         collect
;;         `'(choice-item :tag ,choice-tag :value ,choice-val)))
;;     (widget-insert "\n")))

;; (defmacro persistent-toggler (desc var &optional var-values on-string off-string)
;;   `(progn
;;      (widget-insert (propertize ,desc 'face 'persistent-variable))
;;      (widget-insert (propertize " " 'display '(space :align-to 20)))
;;      ,(if (not var-values)
;;           `(widget-create 'toggle
;;             :value (with-visible-org-buffer ,var)
;;             :on (concat
;;                  (propertize ,(or on-string " on ")
;;                   'face '(:inherit success :box t
;;                           :weight semi-bold :slant italic
;;                           :height 1.2)))
;;             :off (concat
;;                   (propertize ,(or off-string " off ")
;;                    'face '(:inherit error :box t
;;                            :weight semi-bold
;;                            :height 1.2 )))
;;             :notify
;;             (lambda (widget &rest ignore)
;;               (with-visible-org-buffer
;;                (if (commandp ',var)
;;                    (,var (if (widget-value widget) 1 0))
;;                  (setq ,var (widget-value widget))))))
;;         `(widget-create 'toggle
;;           :value (eq ,var ',(caar var-values))
;;           :on
;;           (concat
;;            (propertize ,(cdr (car var-values))
;;             'face '(:box t :weight semi-bold :slant italic
;;                     :inherit success :height 1.2))
;;            "  "
;;            (propertize ,(cdr (cadr var-values))
;;             'face 'shadow
;;                   ;; '(:box t :weight semi-bold
;;                   ;;   :inherit shadow :height 1.2)
;;             ))
;;           :off (concat
;;                 (propertize ,(cdr (car var-values))
;;                  'face 'shadow
;;                        ;; '(:box t :weight semi-bold
;;                        ;;   :inherit shadow :height 1.2)
;;                        )
;;                 "  "
;;                 (propertize ,(cdr (cadr var-values))
;;                  'face '(:box t :weight semi-bold :slant italic
;;                          :inherit success :height 1.2)))
;;           ;; :notify (lambda (widget &rest _)
;;           ;;           (setq org-latex-preview-default-process
;;           ;;            (if (widget-value widget)
;;           ;;                ',(caar var-values) ',(caadr var-values))))
;;           ))
;;      (widget-insert "\n")))

;; (defun persistent-toggle ()
;;   "Show or hide the persistent menu."
;;   (interactive)
;;   (if-let ((win (cl-some (lambda (w)
;;                            (and (string= (buffer-name (window-buffer w))
;;                                    "*persistent*")
;;                                 w))
;;                          (window-list))))
;;       (delete-window win)
;;     (persistent-make-buffer)))

;; (define-key emacs-lisp-mode-map (kbd "<f6>") #'persistent-toggle)

;; (defun persistent-make-buffer ()
;;   "Create the test buffer."
;;   (interactive)
;;   (with-current-buffer (get-buffer-create "*tests*")
;;     (let ((display-buffer-mark-dedicated t))
;;       (display-buffer (current-buffer)
;;                       '(display-buffer-in-side-window
;;                         (slot . -20)
;;                         (direction . right)
;;                         (side . right)
;;                         (window-width . 40)
;;                         (window-parameters
;;                          (dedicated . t)
;;                          (no-delete-other-windows . t)))))
;;     (let ((inhibit-read-only t)) (erase-buffer))
;;     (remove-overlays)
;;     (widget-insert "\n           ")
;;     (widget-create 'push-button
;;                    :format "%{%[[PREVIEW!]%]%}"
;;                    ;; :button-prefix "       "
;;                    :sample-face '(:height 2.0 :box (:line-width 2))
;;                    :help-echo "Preview LaTeX fragments in document"
;;                    :notify (lambda (widget &rest _)
;;                              (with-visible-org-buffer
;;                               (org-latex-preview '(16)))))
;;     (widget-insert "\n\n ")
;;     ;; hide testcase
;;     (widget-create 'push-button
;;                    :format "%{%[[test 0]%]%}"
;;                    :help-echo "Show/hide testcase input"
;;                    :notify (lambda (widget &rest _)))
;;     (widget-insert "  ")
;;     ;; edit tc
;;     (widget-create 'push-button
;;                    :format "%{%[[edit]%]%}"
;;                    :value "in1.txt"
;;                    :help-echo "Edit the current testcase"
;;                    :notify (lambda (widget &rest ignore)
;;                              (message "Switch to: %s"
;;                                       (widget-value widget))
;;                              (display-buffer
;;                               (find-file-noselect
;;                                (file-name-concat
;;                                 "/mnt/Data/Documents/problems/Codeforces/1842/a/"
;;                                 (widget-value widget)))
;;                               '((display-buffer-reuse-window
;;                                  display-buffer-reuse-mode-window
;;                                  display-buffer-use-some-window)))))
;;     (widget-insert "  ")
;;     ;; run tc
;;     (widget-create 'push-button
;;                    :format "%{%[[run]%]%}"
;;                    :value "in1.txt"
;;                    :help-echo "Run the current testcase"
;;                    :notify (lambda (widget &rest ignore)
;;                              (message "Running testcase")
;;                              (compile
;;                               (concat "g++ "
;;                                "/mnt/Data/Documents/problems/Codeforces/1842/a/a.cpp"
;;                                       " && ./a.out < "
;;                                (file-name-concat
;;                                 "/mnt/Data/Documents/problems/Codeforces/1842/a/"
;;                                 (widget-value widget))))))
;;     (widget-insert "\n")
;;     (widget-create 'editable-field ;; FIXME (widget-insert)
;;                    :format "%{%v%}"
;;                    :value-face 'font-lock-comment-face
;;                    :value
;;                     (foxy-read-file
;;                      "/mnt/Data/Documents/problems/Codeforces/1842/a/in1.txt")
;;                    :indent 2
;;                     )

;;     ;; TODO: Time (ms) and next test
;;     (widget-insert "\n\n\n\n")
   
;;     (use-local-map
;;      (make-composed-keymap
;;       (list (let ((map (make-sparse-keymap)))
;;               (define-key map (kbd "<mouse-1>") 'widget-button-click)
;;               (define-key map (kbd "RET") 'widget-button-press)
;;               (define-key map (kbd "q") 'kill-buffer-and-window)
;;               (define-key map (kbd "<f6>") 'kill-buffer-and-window)
;;               (if (bound-and-true-p evil-mode)
;;                   (evil-make-overriding-map map 'normal))
;;               map))
;;       widget-keymap))
;;     (widget-setup)))

(provide 'foxy)
;;; foxy.el ends here
