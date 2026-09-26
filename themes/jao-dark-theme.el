;;; jao-dark-theme.el --- a dark theme with blue-green accent -*- lexical-binding: t; -*-

;; Author: jao <mail@jao.io>
;; Keywords: themes

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

(require 'jao-themes "~/.emacs.d/themes/jao-themes.el")

(defvar jao-theme-dark-bold
  (if (> emacs-major-version 28) 'medium 'semibold))

(jao-define-custom-theme jao-dark
  (:names (bg "#282b33" nil nil)
          (bg-alt "#1f2024" nil nil)
          (fg "#c6c6c6")
          (fg-0.5 "#a6a6a6")
          (fg-1 "#868686")
          (fg-2 "#666666")
          (blue "#819cd6")
          (blueish "#6e7899")
          (dark-blue "#616c96")
          (green "#5b94ab")
          (yellow "antiquewhite3")
          (dark-yellow "antiquewhite4")
          (orange "#a6c1e0")
          (orange2 "#dd8844")
          (red "#e1c1ee" "#ff6655" "red")
          (comments "azure4" "lightblue4")
          (constants "honeydew4")
          (dimm-line-fg "#3f3f3f")
          (dimm-line "#7f7f7f")
          (error "#e1c1ee")
          (functions "#7ebebd")
          (functions2 "#44b9b1")
          (keywords "cadetblue3")
          (keywords2 "#51afef")
          (modeline-bg "#22242b")
          (modeline-bg-inactive "#24262d")
          ;;(region "#41454b")
          (region "#31353b")
          (strings "honeydew3")
          (success "#5b94ab")
          (warning "#cfcf9c")
          (warning2 "#ECBE7B"))
  (:face-family "Fira Code")
  (:face-size 9)
  (:bold-weight jao-theme-dark-bold)
  (:palette (fg "#c6c6c6")
            (bg "#1f2024")
            (box "grey25")
            (button (p f12) bx nul)
            (hilite (c nil region) ex)
            (link (ul dark-blue)) ;; (ul dimm-line)
            (visited-link (ul dark-blue))
            (tab-sel (c 252 232) nbf)
            (tab-unsel (c 245 232))
            (comment (c comments))
            (keyword (c keywords))
            (function (c functions))
            (type (c constants))
            (variable-name (c nil))
            (constant (c constants))
            (string (c strings) it)
            (error (c warning2))
            (warning (c warning))
            (success (c green))
            (dimm (c dimm-line))
            (gnus-mail (c nil))
            (gnus-news (c nil))
            (outline-1 (c strings) nbf)
            (outline-2 (c functions) nbf)
            (outline-3 (c blue) nbf)
            (outline-4 (c functions2) nbf)
            (outline-5 (c nil))
            (f00 (c "lightskyblue3"))
            (f01 (c "lightskyblue4"))
            (f02 (c "lightblue3"))
            (f10 (c "lightblue4"))
            (f11 (c "lightblue2"))
            (f12 (c "azure3")))
  (:x-faces (clojure-keyword-face (c "powder blue"))
            (corfu-background (c "white" "grey20"))
            (corfu-bar (c nil "grey20")) ;; moving part of the bar
            (corfu-border (~ corfu-background)) ;; background of the bar
            (corfu-current (c "white" "grey50") nbf nit nul)
            (corfu-echo (~ default))
            (cursor (c warning2 warning2))
            (diff-hl-change (c green))
            (diff-hl-delete (c orange2) bf)
            (diff-hl-insert (c dark-blue))
            (fill-column-indicator (c "black") :inherit nil)
            (font-lock-doc-face (~ font-lock-comment-face) it)
            (fringe (p dimm))
            (gnus-button (c "lightyellow3"))
            (gnus-cite-1 (c fg-0.5))
            (gnus-cite-2 (c fg-1))
            (gnus-cite-3 (c fg-2))
            (gnus-cite-4 (c fg-2))
            ;; (gnus-group-mail-3 (c yellow))
            ;; (gnus-group-mail-3-empty (c fg-0.5))
            ;; (gnus-group-news-3 (~ gnus-group-mail-3))
            ;; (gnus-group-news-3-empty (~ gnus-group-mail-3-empty))
            ;; (gnus-group-mail-low (p f10))
            ;; (gnus-group-mail-low-empty (~ gnus-group-mail-3-empty))
            (gnus-summary-cancelled  (c "dark slate gray" nil) st)
            (gnus-summary-selected (p warning) nul nbf)
            (header-line (p hilite))
            (magit-diff-context-highlight (p hilite) ex)
            (mode-line (c "grey60" modeline-bg)
                       :box (:line-width 1 :color "#282b33"))
            (mode-line-inactive (c "grey35" modeline-bg-inactive)
                                :box (:line-width 1 :color "#282b33"))
            (mode-line-buffer-id (c yellow))
            (org-hide (c 0 nil))
            (org-code (c yellow))
            (scroll-bar (c bg))
            (show-paren-match (c "darkseagreen1" "#5f5f5f"))
            (shr-link (c blue) (ul dark-blue))
            (vertical-border (c "black") :inherit nil)
            (vertico-group-separator (p dimm) :strike-through "#6f6f6f")))

;; (enable-theme 'jao-dark)
;; (jao-mode-line-adjust-faces)


(provide 'jao-dark-theme)
