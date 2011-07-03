(add-to-list 'load-path "~/.emacs.d/dotemacs")
;; (add-to-list 'load-path "e:/common/dotemacs")
(load "init-basic" 'noerror)
(load "init-site" 'noerror)
(load "init-misc" 'noerror)
(load "init-toolbar" 'noerror)
(load "init-test" t)

;; (add-to-list 'load-path "e:/common/note")
(load "init-note" 'noerror)

;; (load "~/dotemacs/sample/proj" t)
