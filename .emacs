;; for portable emacs:
;; copy this file to site-lisp and rename to site-start.el,
;; then uncomment the following line.
;; (setenv "HOME" (file-name-directory (or load-file-name (buffer-file-name))))

;; if you want to use offical cedet
(let ((default-directory (expand-file-name "~/.emacs.d/cedet-1.0pre7")))
  (when (file-exists-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(add-to-list 'load-path "~/.emacs.d/dotemacs")
;; (add-to-list 'load-path "e:/common/dotemacs")
(require 'init-basic nil t)
(require 'init-site nil t)
(require 'init-misc nil t)
(require 'init-toolbar nil t)
(require 'init-test nil t)

;; (add-to-list 'load-path "e:/common/note")
;; (require 'init-note nil t)

;; (load "~/dotemacs/sample-proj")
