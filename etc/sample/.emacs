(setq emacs-load-start-time (current-time))

(add-to-list 'load-path "~/.emacs.d/dotemacs")
(add-to-list 'load-path "d:/common/dotemacs")
(add-to-list 'load-path "e:/common/dotemacs")
(load "init-basic" 'noerror)
(load "init-site" 'noerror)
(load "init-misc" 'noerror)
(load "init-toolbar" 'noerror)
(load "init-test" t)

(add-to-list 'load-path "d:/common/note")
(add-to-list 'load-path "e:/common/note")
(load "init-note" 'noerror)

;; (load "~/dotemacs/sample/proj" t)

(message "Emacs startup time: %g seconds."
         (float-time (time-since emacs-load-start-time)))
