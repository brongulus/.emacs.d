;;; jao-light-term-theme.el --- a light theme  -*- lexical-binding: t; -*-

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

(jao-define-custom-theme jao-light-term
  (:names (bg-lightest "gray98")
          (bg-light "gray95")

          (light-gray "gray80")

          (black "black")
          (dark-gray "gray30")
          (fg-light "gray40")

          ;; (hl "#f2f2f2")
          (hl "ivory2")
          (dimm "lemonchiffon4")

          (warning "orange4")
          (red "burlywood4")
          (blue "#023770")
          (green "#005555")
          (lightgreen "darkgreen")
          (yellow "lightyellow"))
  (:face-size 9)
  (:face-family "DejaVu Sans Mono")
  (:bold-weight 'bold)
  (:palette (fg "#000000")
            (bg "#ffffff")
            (box "gray80")
            (hilite (c nil hl))
            (link (c green) nbf nul)
            (visited-link (c green))
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
            (outline-1 (c green) nbf nul ex)
            (outline-2 (c blue) nbf)
            (outline-3 (c lightgreen) nbf)
            (outline-4 (c blue) nul nbf)
            (outline-5 (c blue) nul nbf)
            (f00 (c green))
            (f01 (c blue))
            (f02 (c dark-gray))
            (f10 (p f00))
            (f11 (p f01))
            (f12 (p f02)))
  (:x-faces (button (c blue bg-lightest))
            (compilation-info (c "#223142" nil) nbf)
            (completions-group-separator (c nil ni) (st "grey80"))
            (corfu-default (~ default) (c "black" "grey95"))
            (corfu-bar (c nil "grey80")) ;; moving part of the bar
            (corfu-border (~ corfu-background)) ;; background of the bar
            (corfu-current (c "black" "grey95") nbf nit (ul "grey70"))
            (cursor (c "sienna3" "sienna3"))
            (diff-hl-margin-change (c "lightcyan2" nil))
            (diff-hl-margin-insert (c "honeydew2" nil))
            (diff-hl-margin-delete (c "wheat1" nil))
            (eww-form-text (p hilite))
            (fill-column-indicator (c "grey80"))
            (fringe (c "grey70" nil))
            (gnus-button (c blue))
            (gnus-cite-1 (c "darkslategray" nil))
            (gnus-cite-2 (c "slate gray" nil))
            (gnus-cite-3 (c "slate gray" nil))
            (gnus-cite-4 (c "slate gray" nil))
            (gnus-header-name (c fg-light))
            (gnus-summary-selected (c green) nbf)
            (gnus-summary-cancelled (c "sienna3" nil) st)
            (header-line (c dark-gray bg-lightest)
                         :box (:line-width 1 :color "grey90"))
            (magit-diff-context-highlight (c nil hl) ex)
            (magit-diff-hunk-heading-highlight (c nil hl) it bf)
            (mode-line (c "grey20" "gray90") nbf)
            (mode-line-inactive (c "grey40" "gray95"))
            (mode-line-buffer-id (~ default) (c dark-blue-2 nil) nit)
            (mode-line-emphasis (c green nil))
            (mode-line-highlight (c green nil))
            (org-link (p link) (ul "grey80"))
            (scroll-bar (c "grey90" nil))
            (show-paren-match (c nil "grey85"))
            (shr-text (c nil nil))
            (shr-link (~ link) (ul light-gray))
            (shr-code (c blue nil))
            (success (c green))
            (vertical-border (c "grey70" nil))
            (vertico-current (c nil yellow) nul ext)
            (widget-button (c blue nil) nit nul)
            (widget-field (c nil bg-light) nit nul)
            (whitespace-tag (p hilite))))

;; (enable-theme 'jao-light-term)
;; (jao-mode-line-adjust-faces)

(provide 'jao-light-term-theme)
