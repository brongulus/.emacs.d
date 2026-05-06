;;; ac-lite.el --- Child-frame completion popup -*- lexical-binding: t -*-
;;; Minimal corfu alternative. Works with any CAPF. Requires Emacs 29+.

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(defgroup ac nil "Auto completion popup." :group 'completion)
(defcustom ac-count 8 "Max visible candidates." :type 'natnum)
(defcustom ac-min-width 15 "Min popup width in chars." :type 'natnum)
(defcustom ac-max-width 80 "Max popup width in chars." :type 'natnum)
(defcustom ac-auto-delay 0.3 "Auto-trigger idle delay." :type 'float)
(defcustom ac-auto-prefix 2 "Min chars before auto-trigger." :type 'natnum)
(defcustom ac-border-width 1 "Border pixels." :type 'natnum)
(defcustom ac-doc-delay 1.0 "Doc popup delay (nil to disable)."
  :type '(choice float (const nil)))
(defcustom ac-doc-max-width 70 "Doc popup max width in chars." :type 'natnum)
(defcustom ac-doc-max-height 12 "Doc popup max lines." :type 'natnum)

(defface ac-default '((t :inherit highlight :extend t)) "Popup face.")
(defface ac-current '((t :inherit region)) "Selected candidate face.")
(defface ac-border `((t :background ,(face-foreground 'shadow)))  "Border face.")

;;; State
(defvar ac--frame nil)
(defvar ac--doc-frame nil)
(defvar ac--candidates nil)
(defvar ac--raw nil)
(defvar ac--total 0)
(defvar ac--index -1)
(defvar ac--scroll 0)
(defvar ac--base "")
(defvar ac--auto-timer nil)
(defvar ac--doc-timer nil)

;;; Child frame infrastructure
(defvar ac--frame-params
  '((no-accept-focus . t) (no-focus-on-map . t) (min-width . t) (min-height . t)
    (border-width . 0) (outer-border-width . 0)
    (internal-border-width . 0) (child-frame-border-width . 0)
    (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
    (left-fringe . 0) (right-fringe . 0)
    (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
    (no-other-frame . t) (unsplittable . t) (undecorated . t)
    (cursor-type . nil) (no-special-glyphs . t) (minibuffer . nil)
    (inhibit-double-buffering . t) (desktop-dont-save . t)))

(defvar ac--buffer-params
  '((mode-line-format . nil) (header-line-format . nil) (tab-line-format . nil)
    (truncate-lines . t) (cursor-in-non-selected-windows . nil)
    (cursor-type . nil) (show-trailing-whitespace . nil)
    (display-line-numbers . nil) (left-fringe-width . 0) (right-fringe-width . 0)
    (left-margin-width . 0) (right-margin-width . 0) (line-spacing . 0)
    (buffer-read-only . t)))

(defun ac--make-buffer (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (dolist (v ac--buffer-params) (set (make-local-variable (car v)) (cdr v)))
      (setq-local view-read-only nil))
    buf))

(defun ac--show-frame (frame buf parent x y w h)
  (let* ((window-min-height 1) (window-min-width 1)
         (b ac-border-width)
         (params `((parent-frame . ,parent)
                   (background-color . ,(face-attribute 'ac-default :background nil 'default))
                   (font . ,(frame-parameter parent 'font))
                   (internal-border-width . ,b) (child-frame-border-width . ,b)
                   ,@ac--frame-params)))
    (unless (and (frame-live-p frame) (eq (frame-parent frame) parent))
      (when (frame-live-p frame) (delete-frame frame))
      (setq frame (make-frame `((visibility . nil) (width . 0) (height . 0) ,@params))))
    (let ((win (frame-root-window frame)))
      (set-window-buffer win buf)
      (set-window-start win 1)
      (set-window-point win 1)
      (set-window-dedicated-p win t)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'no-other-window t))
    (set-face-background 'internal-border (face-attribute 'ac-border :background nil 'default) frame)
    (set-face-background 'child-frame-border (face-attribute 'ac-border :background nil 'default) frame)
    (set-frame-size frame w h t)
    (set-frame-position frame x y)
    (redirect-frame-focus frame parent)
    (make-frame-visible frame)
    frame))

(defun ac--hide-frame (f)
  (when (and (frame-live-p f) (frame-visible-p f)) (make-frame-invisible f)))

;;; Completion popup
(defun ac--popup-show (xpos ypos)
  (let* ((cw (default-font-width))
         (buf (ac--make-buffer " *ac*"))
         (count (min ac-count ac--total)))
    (cond ((< ac--index ac--scroll) (setq ac--scroll ac--index))
          ((>= ac--index (+ ac--scroll count)) (setq ac--scroll (- ac--index count -1))))
    (let* ((cands (seq-subseq ac--candidates ac--scroll
                              (min (+ ac--scroll count) ac--total)))
           (maxw (cl-loop for c in cands maximize (length c)))
           (width (min ac-max-width (max ac-min-width (+ maxw 2))))
           (pw (* width cw))
           (parent (window-frame))
           (edge (window-inside-pixel-edges))
           (ch (default-line-height))
           (x (+ (car edge) (or (car (posn-x-y xpos)) 0)))
           (yb (+ (cadr edge) (or (cdr (posn-x-y ypos)) 0) ch)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dotimes (i (length cands))
            (let* ((s (truncate-string-to-width (concat " " (nth i cands)) width nil ?\s))
                   (sel (= (+ i ac--scroll) ac--index)))
              (insert (propertize s 'face (if sel 'ac-current 'ac-default)))
              (unless (= i (1- (length cands))) (insert "\n"))))
          (goto-char (point-min))))
      (let* ((lh (with-current-buffer buf (default-line-height)))
             (ph (* (length cands) lh))
             (y (if (> (+ yb ph ch) (frame-pixel-height)) (- yb ph ch) yb))
             (x (max 0 (min x (- (frame-pixel-width) pw)))))
        (setq ac--frame (ac--show-frame ac--frame buf parent x y pw ph))))))

(defun ac--popup-hide ()
  (ac--hide-frame ac--frame)
  (ac--doc-hide))

;;; Doc popup
(defun ac--doc-hide ()
  (when (timerp ac--doc-timer) (cancel-timer ac--doc-timer) (setq ac--doc-timer nil))
  (ac--hide-frame ac--doc-frame))

(defun ac--doc-schedule ()
  (ac--doc-hide)
  (when (and ac-doc-delay (>= ac--index 0))
    (let ((buf (current-buffer)))
      (setq ac--doc-timer
            (run-with-timer ac-doc-delay nil
                            (lambda () (when (buffer-live-p buf)
                                         (with-current-buffer buf (ac--doc-show)))))))))

(defun ac--doc-show ()
  (when-let* (((frame-live-p ac--frame))
              (cand (nth ac--index ac--candidates))
              (doc (ac--doc-get cand)))
    (let* ((buf (ac--make-buffer " *ac-doc*"))
           (cw (default-font-width))
           (lh (with-current-buffer buf (default-line-height)))
           (fe (frame-edges ac--frame 'outer-edges))
           (rx (nth 2 fe)) (ry (nth 1 fe))
           (fw (frame-pixel-width))
           (avr (- fw rx 4)) (avl (- (nth 0 fe) 4))
           (pw (if (>= avr (* cw 20)) (min (* cw ac-doc-max-width) avr)
                 (min (* cw ac-doc-max-width) avl)))
           (x (if (>= avr (* cw 20)) rx (max 0 (- (nth 0 fe) pw))))
           (fill-col (max 20 (/ pw cw))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer) (insert doc)
          (let ((fill-column fill-col))
            (goto-char (point-min))
            (while (not (eobp))
              (let ((start (point))
                    (end (or (and (re-search-forward "\n[ \t]*\n" nil t)
                                  (match-beginning 0))
                             (point-max))))
                (when (> (- end start) fill-col)
                  (fill-region start end))
                (goto-char end)
                (skip-chars-forward "\n\t "))))
          (goto-char (point-min))
          (put-text-property (point-min) (point-max) 'face 'ac-default)))
      (let ((ph (min (* lh ac-doc-max-height)
                     (* lh (with-current-buffer buf
                             (count-lines (point-min) (point-max)))))))
        (when (> pw (* cw 15))
          (setq ac--doc-frame
                (ac--show-frame ac--doc-frame buf (window-frame) x ry pw ph)))))))

(defun ac--doc-get (candidate)
  (or
   (ignore-errors
     (when-let* ((raw (nth ac--index ac--raw))
                 (props (nth 4 completion-in-region--data))
                 (fun (plist-get props :company-doc-buffer))
                 (res (cl-letf (((symbol-function #'help-buffer)
                                 (lambda () (get-buffer-create " *ac-help-tmp*")))
                                (inhibit-message t)
                                (message-log-max nil)
                                (inhibit-redisplay t))
                        (funcall fun raw))))
       (with-current-buffer (or (car-safe res) res)
         (let ((s (string-trim (buffer-string))))
           (and (not (string-empty-p s)) s)))))
   (ignore-errors
     (when (and (fboundp 'eglot-current-server) (eglot-current-server))
       (when-let* ((raw (nth ac--index ac--raw))
                   (item (get-text-property 0 'eglot--lsp-item raw))
                   (res (jsonrpc-request (eglot-current-server)
                                         :completionItem/resolve item :timeout 1))
                   (doc (plist-get res :documentation)))
         (cond ((stringp doc) doc) ((listp doc) (plist-get doc :value))))))
   (when (derived-mode-p 'emacs-lisp-mode)
     (when-let* ((sym (intern-soft candidate))
                 (d (or (ignore-errors (documentation sym t))
                        (ignore-errors
                          (documentation-property sym 'variable-documentation t)))))
       (and (stringp d) (not (string-empty-p d))
            (substring d 0 (min (length d) 800)))))))

(defun ac-doc-scroll-up () (interactive)
       (when (and (frame-live-p ac--doc-frame) (frame-visible-p ac--doc-frame))
         (let ((win (frame-root-window ac--doc-frame)))
           (set-window-start win
                             (with-current-buffer (window-buffer win)
                               (save-excursion
                                 (goto-char (window-start win))
                                 (forward-line 3)
                                 (point)))
                             t))))

(defun ac-doc-scroll-down () (interactive)
       (when (and (frame-live-p ac--doc-frame) (frame-visible-p ac--doc-frame))
         (let ((win (frame-root-window ac--doc-frame)))
           (set-window-start win
                             (with-current-buffer (window-buffer win)
                               (save-excursion
                                 (goto-char (window-start win))
                                 (forward-line -3)
                                 (point)))
                             t))))

;;; Core completion logic
(defun ac--strip-base (all)
  (let* ((last (last all)) (base (cdr last)))
    (when base (setcdr last nil))
    (cons all (or base 0))))

(defun ac--in-region (beg end table pred)
  (if (minibufferp)
      (completion--in-region beg end table pred)
    (barf-if-buffer-read-only)
    (let* ((inhibit-message t)
           (temp-buffer-show-function #'ignore)
           (pt (- (point) beg))
           (str (buffer-substring-no-properties beg end))
           (md (completion-metadata (substring str 0 pt) table pred))
           (raw (completion-all-completions str table pred pt md)))
      (when raw
        (pcase-let ((`(,all . ,base) (ac--strip-base raw)))
          (let ((end-marker (copy-marker end t)))
            (setq completion-in-region--data
                  (list beg end-marker table pred completion-extra-properties)
                  ac--base (substring str 0 base)
                  ac--candidates (mapcar #'substring-no-properties all)
                  ac--raw all
                  ac--total (length ac--candidates)
                  ac--index 0)
            (completion-in-region-mode 1)
            (ac--install-hooks)
            (ac--exhibit)))))))

(defun ac--refresh-table ()
  "Re-query CAPF to get fresh table without touching markers."
  (when-let* ((result (run-hook-with-args-until-success
                       'completion-at-point-functions)))
    (when (and (consp result) (integerp (car result)))
      (let ((table (nth 2 result)))
        ;; Only update the table, keep beg/end intact
        (setcar (cddr completion-in-region--data) table)
        t))))

(defun ac--exhibit ()
  ;; On typing, always refresh the table from CAPF for fresh results
  (when (memq this-command '(self-insert-command delete-backward-char
                                                 backward-delete-char-untabify delete-char))
    (ac--refresh-table))
  (pcase-let ((`(,beg ,end ,table ,pred . ,_) completion-in-region--data))
    (let* ((str (buffer-substring-no-properties beg end))
           (pt (- (point) beg))
           (md (completion-metadata (substring str 0 pt) table pred))
           (raw (completion-all-completions str table pred pt md)))
      (if (not raw)
          (ac-quit)
        (pcase-let ((`(,all . ,base) (ac--strip-base raw)))
          (let ((new-cands (mapcar #'substring-no-properties all)))
            (unless (equal new-cands ac--candidates)
              (setq ac--index 0 ac--scroll 0))
            (setq ac--base (substring str 0 base)
                  ac--candidates new-cands
                  ac--raw all
                  ac--total (length ac--candidates)
                  ac--index (min ac--index (1- ac--total))))
          (when (< ac--index 0) (setq ac--index 0))
          (when-let ((pos (posn-at-point))
                     (xpos (posn-at-point (+ beg base))))
            (ac--popup-show xpos pos)
            (ac--doc-schedule)))))))

;;; Post-command dispatch
(defun ac--post-command ()
  (condition-case nil
      (if (and completion-in-region-mode (ac--continue-p))
          (cond
           ((memq this-command '(ac-doc-scroll-up ac-doc-scroll-down
                                                  mwheel-scroll mac-mwheel-scroll))
            nil)
           ((memq this-command '(ac-next ac-prev))
            (when-let ((xpos (pcase-let ((`(,beg . ,_) completion-in-region--data))
                               (posn-at-point (+ beg (length ac--base)))))
                       (ypos (posn-at-point)))
              (ac--popup-show xpos ypos)
              (ac--doc-schedule)))
           (t (ac--exhibit)))
        (ac-quit))
    (error (ac-quit))))

(defun ac--continue-p ()
  (and (pcase-let ((`(,beg ,end . ,_) completion-in-region--data))
         (and beg end (<= beg (point) (marker-position end))))
       (or (memq this-command
                 '(self-insert-command delete-backward-char
                                       backward-delete-char-untabify delete-char
                                       ac-next ac-prev ac-accept
                                       indent-for-tab-command completion-at-point
                                       mwheel-scroll mac-mwheel-scroll
                                       ac-doc-scroll-up ac-doc-scroll-down))
           (string-prefix-p "ac-" (symbol-name (or this-command 'ignore))))))

;;; Commands
(defun ac-next () (interactive)
       (when (> ac--total 0) (setq ac--index (mod (1+ ac--index) ac--total))))

(defun ac-prev () (interactive)
       (when (> ac--total 0) (setq ac--index (mod (1- ac--index) ac--total))))

(defun ac-accept () (interactive)
       (if (< ac--index 0) (ac-quit)
         (pcase-let ((`(,beg ,end . ,_) completion-in-region--data))
           (let ((str (concat ac--base (nth ac--index ac--candidates))))
             (unless (equal str (buffer-substring-no-properties beg end))
               (completion--replace beg end str))
             (ac-quit)
             (when-let ((exit (plist-get completion-extra-properties :exit-function)))
               (funcall exit str 'finished))))))

(defun ac-quit () (interactive)
       (ac--popup-hide)
       (setq ac--candidates nil ac--raw nil ac--total 0 ac--index -1 ac--scroll 0)
       (when completion-in-region-mode (completion-in-region-mode -1)))

;;; Keymap
(defvar-keymap ac-map
  "RET" #'ac-accept
  "TAB" #'ac-next "S-TAB" #'ac-prev
  "<tab>" #'ac-next "<backtab>" #'ac-prev
  "C-g" #'ac-quit "<escape>" #'ac-quit
  "<remap> <scroll-other-window>" #'ac-doc-scroll-up
  "<remap> <scroll-other-window-down>" #'ac-doc-scroll-down)

;;; Hooks and lifecycle
(defun ac--window-change (_)
  (unless (and completion-in-region-mode
               (pcase-let ((`(,_beg ,end . ,_) completion-in-region--data))
                 (and end (eq (current-buffer) (marker-buffer end)))))
    (ac-quit)))

(defun ac--setup ()
  (add-hook 'post-command-hook #'ac--post-command nil 'local)
  (add-hook 'window-selection-change-functions #'ac--window-change nil 'local)
  (add-hook 'window-buffer-change-functions #'ac--window-change nil 'local)
  (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) ac-map))

(defun ac--teardown ()
  (remove-hook 'post-command-hook #'ac--post-command 'local)
  (remove-hook 'window-selection-change-functions #'ac--window-change 'local)
  (remove-hook 'window-buffer-change-functions #'ac--window-change 'local)
  (ac--popup-hide))

(defun ac--install-hooks ()
  (let ((sym (make-symbol "ac--cir-teardown"))
        (buf (current-buffer)))
    (fset sym (lambda ()
                (unless completion-in-region-mode
                  (remove-hook 'completion-in-region-mode-hook sym)
                  (when (buffer-live-p buf)
                    (with-current-buffer buf (ac--teardown))))))
    (add-hook 'completion-in-region-mode-hook sym)
    (ac--setup)))

;;; Auto trigger
(defun ac--auto-post-command ()
  (when (and (eq this-command 'self-insert-command)
             (not completion-in-region-mode)
             (not (minibufferp)))
    (when (timerp ac--auto-timer) (cancel-timer ac--auto-timer))
    (setq ac--auto-timer
          (run-with-idle-timer ac-auto-delay nil #'ac--auto-trigger (current-buffer)))))

(defun ac--auto-trigger (buf)
  (setq ac--auto-timer nil)
  (when (and (buffer-live-p buf) (not completion-in-region-mode) (not (minibufferp buf)))
    (with-current-buffer buf
      (let* ((line (buffer-substring-no-properties (line-beginning-position) (point)))
             (prefix (when (string-match "[[:alnum:]_-]+\\'" line)
                       (match-string 0 line))))
        (when (and prefix (>= (length prefix) ac-auto-prefix))
          (let ((inhibit-message t)
                (temp-buffer-show-function #'ignore))
            (completion-at-point)))))))

;;; Minor mode
(defun ac--suppress-help (fn &rest args)
  (let ((display-buffer-alist
         (cons '("\\*Help\\*\\|\\*Completions\\*" (display-buffer-no-window))
               display-buffer-alist)))
    (apply fn args)))

;;;###autoload
(define-minor-mode ac-mode
  "Auto-completion popup using CAPF."
  :global nil :lighter " ac"
  (if ac-mode
      (progn
        (setq-local completion-in-region-function #'ac--in-region)
        (push '(completion-in-region-mode . nil) minor-mode-overriding-map-alist)
        (add-hook 'post-command-hook #'ac--auto-post-command)
        (advice-add 'completion-at-point :around #'ac--suppress-help))
    (kill-local-variable 'completion-in-region-function)
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'completion-in-region-mode minor-mode-overriding-map-alist))
    (remove-hook 'post-command-hook #'ac--auto-post-command)
    (advice-remove 'completion-at-point #'ac--suppress-help)))

(provide 'ac-lite)
;;; ac-lite.el ends here
