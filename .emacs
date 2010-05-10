;; if you want to use offical cedet
;; (let ((cedet-path "~/.emacs.d/cedet-1.0pre7")
;;       (packages '("common" "eieio" "semantic" "srecode"
;;                   "ede" "speedbar" "cogre" "contrib")))
;;   (add-to-list 'load-path cedet-path)
;;   (dolist (package packages)
;;     (add-to-list 'load-path (expand-file-name package cedet-path))))

(add-to-list 'load-path "~/.emacs.d/dotemacs")
;; (add-to-list 'load-path "e:/common/dotemacs")
(require 'init-basic nil t)
(require 'init-site nil t)
(require 'init-misc nil t)

;; (add-to-list 'load-path "e:/common/note")
;; (require 'init-note nil t)

;; (load "~/dotemacs/sample-proj")
