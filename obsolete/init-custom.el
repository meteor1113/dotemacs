;;; -*- mode: emacs-lisp; mode: goto-address; coding: utf-8; -*-
;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @date 2015-03-10
;; @URL http://git.oschina.net/meteor1113/dotemacs


;; user information
(setq user-full-name "Liu Xin")
(setq user-mail-address "meteor1113@qq.com")

;; calendar
(setq holiday-other-holidays '((holiday-chinese 6 23 "李浩斌生日")
                               (holiday-fixed 10 16 "李可生日(1981)")
                               (holiday-chinese 8 15 "爸妈生日(1955)")
                               (holiday-fixed 4 9 "戚生日(1983)")
                               (holiday-fixed 10 6 "奇奇阳历生日(2010)")
                               (holiday-chinese 8 29 "奇奇阴历生日(2010)")
                               (holiday-fixed 9 11 "妙妙阳历生日(2014)")
                               (holiday-chinese 8 18 "妙妙阴历生日(2014)")
                               (holiday-fixed 12 8 "刘阳历生日(1981)")
                               (holiday-chinese 11 13 "刘阴历生日(1981)")))
(eval-after-load "calendar"
  '(when (require 'cal-china-x nil 'noerror)
     (setq cal-china-x-priority2-holidays holiday-other-holidays)))

(provide 'init-custom)
