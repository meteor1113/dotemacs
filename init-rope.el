;;;
;; Copyright (C) 2008-2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2009-02-17


;;; pymacs setting
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)


;;; ropemacs setting
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)


(provide 'init-rope)
