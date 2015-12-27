;;; -*- mode: emacs-lisp; mode: goto-address; coding: utf-8; -*-
;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @date 2009-08-08
;; @URL http://git.oschina.net/meteor1113/dotemacs

(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

;; psvn
;; (autoload 'svn-status "psvn" nil t)
(eval-after-load "vc-svn"
  '(require 'psvn nil 'noerror))

;; magit
;; (autoload 'magit-status "magit" nil t)

(provide 'init-vc)
