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

;; text-mode
(defvar text-imenu-generic-expression
  `((nil ,"^ \\{0,4\\}\\([一二三四五六七八九十]+[、. )]\\)+ *[^,。，]+?$" 0)
    (nil ,"^ \\{0,4\\}\\([0-9]+[、. )]\\)+ *[^,。，]+?$" 0)))

(add-hook 'text-mode-hook
          '(lambda ()
             (setq imenu-generic-expression text-imenu-generic-expression)
             (imenu-add-menubar-index)))

(provide 'init-text)
