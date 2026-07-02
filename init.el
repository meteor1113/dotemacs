;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(let ((config-dir
       (or (cl-find-if (lambda (dir) (file-exists-p (expand-file-name "init-loader.el" dir)))
                       '("~/Projects/emacs.d"
                         "/Volumes/data/Projects/00.common/emacs.d"
                         "d:/Projects/00.common/emacs.d"))
           user-emacs-directory)))
  (message "Using config directory: %s" config-dir)
  (load (expand-file-name "init-loader" config-dir)))

(provide 'init)

;;; init.el ends here
