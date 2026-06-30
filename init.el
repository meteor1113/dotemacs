;; (add-to-list 'load-path "~/.emacs.d")
;; (add-to-list 'load-path "/Volumes/data/Projects/00.common/dotemacs")
;; (add-to-list 'load-path "d:/Projects/00.common/dotemacs")
;; (or (load "init-loader" 'noerror) (load "~/.emacs.d/init-loader" 'noerror))
;; (load "init-loader")
(load (expand-file-name "init-loader.el" (file-name-directory load-file-name)))

;; (add-to-list 'load-path "d:/Projects/00.common/note")
;; (load "init-note" 'noerror)

;; (load "~/dotemacs/sample/proj" t)
