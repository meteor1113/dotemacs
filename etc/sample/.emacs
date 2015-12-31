(add-to-list 'load-path "~/.emacs.d/dotemacs")
(add-to-list 'load-path "d:/common/dotemacs")
(add-to-list 'load-path "e:/common/dotemacs")
(add-to-list 'load-path "d:/project/common/dotemacs")
(add-to-list 'load-path "e:/project/common/dotemacs")
(load "init" 'noerror)

(add-to-list 'load-path "d:/common/note")
(add-to-list 'load-path "e:/common/note")
(add-to-list 'load-path "d:/project/common/note")
(add-to-list 'load-path "e:/project/common/note")
(load "init-note" 'noerror)

;; (load "~/dotemacs/sample/proj" t)
