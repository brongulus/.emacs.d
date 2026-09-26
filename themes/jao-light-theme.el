;;; jao-light-theme.el --- a light theme  -*- lexical-binding: t; -*-

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

(jao-define-custom-theme jao-light
  (:names (bg-lightest "gray98")
          (bg-light "#efefef")

          (light-gray "gray80")

          (black "black")
          (dark-gray "gray30")
          (fg-light "gray40")

          (hl "#f2f2f2")
          (dimm "lemonchiffon4")

          (warning "orange4")
          (red "burlywood4")
          (blue "#023770")
          (pale-blue "honeydew3")
          (green "#005555")
          (lightgreen "darkgreen")
          (yellow "lightyellow"))
  (:bold-weight 'semibold)
  (:palette (fg "black")
            (bg "white")
            (box "gray80")
            (hilite (c nil hl))
            (button (c fg-light bg-lightest) (ul light-gray))
            (link (c green) nbf (ul light-gray))
            (visited-link (ul light-gray) nbf)
            (tab-sel (~ mode-line))
            (tab-unsel (~ mode-line-inactive))
            (comment (c fg-light) it)
            (keyword (c blue) bf)
            (type (c blue) nbf)
            (function (c green))
            (variable-name (c black))
            (constant (c dark-gray))
            (string (c blue) nit)
            (warning (c warning))
            (error (c red) bf)
            (dimm (c dimm))
            (gnus-mail (c "black"))
            (gnus-news (c "black"))
            (outline (c "black") bf)
            (outline-1 (c green) bf)
            (outline-2 (c green) nbf)
            (outline-3 (c blue) nul nbf)
            (outline-4 (c blue) nul nbf)
            (outline-5 (c blue) nul nbf)
            (f00 (c green))
            (f01 (c blue))
            (f02 (c dark-gray))
            (f10 (p f00))
            (f11 (p f01))
            (f12 (p f02)))
  (:x-faces (compilation-info (c "#223142" nil) nbf)
            (completions-group-separator (c nil ni) (st "grey80"))
            (corfu-default (~ default) (c "black" "grey95"))
            (corfu-bar (c nil "grey80")) ;; moving part of the bar
            (corfu-border (~ corfu-background)) ;; background of the bar
            (corfu-current (c "black" "grey95") nbf nit (ul "grey70"))
            (cursor (c "sienna3" "sienna3"))
            (diff-hl-change (c "white" "honeydew2"))
            (diff-hl-insert (c "white" "lemonchiffon2"))
            (diff-hl-delete (c "white" "wheat1"))
            (diff-hl-margin-change (c pale-blue))
            (diff-hl-margin-insert (c pale-blue))
            (diff-hl-margin-delete (c "wheat1"))
            (fill-column-indicator (c "grey80"))
            (fringe (c "grey70" nil))
            (gnus-button (c blue))
            (gnus-cite-1 (c "darkslategray" nil))
            (gnus-cite-2 (c "slate gray" nil))
            (gnus-cite-3 (c "slate gray" nil))
            (gnus-cite-4 (c "slate gray" nil))
            (gnus-face-4 (p f11) :family "Triplicate C4c")
            (gnus-header-name (c fg-light))
            (gnus-summary-selected (c green) nbf)
            (gnus-summary-cancelled (c "sienna3" nil) st)
            (header-line (c dark-gray bg-lightest)
                         :box (:line-width 1 :color "grey90"))
            (magit-diff-context-highlight (c nil yellow) ex)
            (magit-diff-hunk-heading-highlight (c nil hl) it bf)
            (message-header-subject (p warning) nbf)
            (mode-line
             (c "seashell3" nil)
             :box (:line-width 1 :color "#dfdfdf" :style flat-button))
            (mode-line-active
             (c "#3f3f3f" "gray96")
             :box (:line-width 1 :color "#dfdfdf" :style flat-button))
            (mode-line-inactive
             (c "#afafaf" nil)
             :box (:line-width 1 :color "#efefdf" :style flat-button))
            (mode-line-buffer-id (~ default) (c nil nil) nit)
            (mode-line-emphasis (c green nil))
            (mode-line-highlight (c green nil))
            (org-agenda-calendar-event (p f02))
            (org-agenda-structure (c nil "#efefef") ex)
            (org-agenda-date (~ default))
            (org-agenda-date-today (~ org-agenda-structure) ex)
            (org-link (p link) (ul "grey80"))
            (org-time-grid (c "grey60" nil))
            (tab-bar (~ header-line) :family "Source Code Pro")
            (scroll-bar (c "grey80" nil))
            (show-paren-match (c nil "grey85"))
            (shr-text (c nil nil))
            (shr-link (~ link) (ul light-gray))
            (shr-code (c blue nil))
            (success (c green))
            (vertical-border (c "#dfdfdf" nil))
            (vterm-color-yellow (c "darkgoldenrod4" yellow))
            (window-divider (c "grey99" nil))
            (widget-button (~ default) nit (ul "grey80"))))

;; (enable-theme 'jao-light)
;; (jao-mode-line-adjust-faces)

(provide 'jao-light-theme)
