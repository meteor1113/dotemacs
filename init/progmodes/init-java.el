;;; -*- mode: emacs-lisp; coding: utf-8; -*-
;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @date 2015-12-26
;; @URL http://git.oschina.net/meteor1113/dotemacs

;; java-mode
(add-hook 'java-mode-hook
          '(lambda ()
             (c-set-style "java")))

;; jde
(add-hook 'java-mode-hook
          '(lambda ()
             (when (require 'jde nil 'noerror)
               (setq jde-enable-abbrev-mode t))))

(provide 'init-java)