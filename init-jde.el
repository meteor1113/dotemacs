;;;
;; Copyright (C) 2008-2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2009-02-17


;;; jde setting
(require 'jde nil t)
(setq jde-enable-abbrev-mode t)
(if (boundp 'ac-modes)
    (add-to-list 'ac-modes 'jde-mode))


(provide 'init-jde)
