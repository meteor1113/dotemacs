;; portable emacs: copy this file to site-lisp and rename to site-start.el.
(when (and load-file-name
           (equal (file-name-nondirectory load-file-name) "site-start.el"))
  (setenv "HOME" (file-name-directory load-file-name)))

;; if you want to use offical cedet
(let ((default-directory (expand-file-name "~/.emacs.d/cedet-1.0pre7")))
  (when (file-exists-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(add-to-list 'load-path "~/.emacs.d/dotemacs")
;; (add-to-list 'load-path "e:/common/dotemacs")
(load "init-basic" 'noerror)
(load "init-site" 'noerror)
(load "init-misc" 'noerror)
(load "init-toolbar" 'noerror)
(load "init-test" t)

;; (add-to-list 'load-path "e:/common/note")
(load "init-note" 'noerror)

;; (load "~/dotemacs/sample-proj")
