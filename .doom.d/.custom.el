(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-preview-latex-process-alist
   '((dvipng :programs
      ("latex" "dvipng")
      :description "dvi > png" :message "you need to install the programs: latex/tectonic and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
      (1.0 . 1.0)
      :latex-compiler
      ("tectonic -X compile %f --outdir=%o -Z shell-escape")
      :image-converter
      ("dvipng -D %D -T tight -o %O %f")
      :transparent-image-converter
      ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
     (dvisvgm :programs
      ("latex" "dvisvgm")
      :description "dvi > svg" :message "you need to install the programs: latex/tectonic and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
      (1.7 . 1.5)
      :latex-compiler
      ("tectonic -X compile %f --outdir=%o -Z shell-escape")
      :image-converter
      ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
      ("latex" "convert")
      :description "pdf > png" :message "you need to install the programs: latex/tectonic and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
      (1.0 . 1.0)
      :latex-compiler
      ("tectonic -X compile %f --outdir=%o -Z shell-escape")
      :image-converter
      ("convert -density %D -trim -antialias %f -quality 100 %O")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-input-area ((t (:background "bg-alt" :extend t))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-doc-face ((t (:slant italic))))
 '(org-table ((t (:inherit 'fixed-pitch))))
 '(tooltip ((t (:font "VictorMono:pixelsize=18"))))
 '(treemacs-git-unmodified-face ((t (:inherit treemacs-file-face)))))
