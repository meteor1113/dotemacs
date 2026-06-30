(let ((config-dir
       (or (cl-find-if (lambda (dir) (file-exists-p (expand-file-name "init-loader.el" dir)))
                       '("/Volumes/data/Projects/00.common/dotemacs"
                         "d:/Projects/00.common/dotemacs"))
           user-emacs-directory)))
  (message "Using config directory: %s" config-dir)
  (load (expand-file-name "init-loader" config-dir)))

(provide 'init)

;;; init.el ends here
