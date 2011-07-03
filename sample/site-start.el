;; portable emacs: copy this file to site-lisp.
;; (when (and load-file-name
;;            (equal (file-name-nondirectory load-file-name) "site-start.el"))
;;   (setenv "HOME" (file-name-directory load-file-name)))
(setenv "HOME" (file-name-directory (or load-file-name (buffer-file-name))))
