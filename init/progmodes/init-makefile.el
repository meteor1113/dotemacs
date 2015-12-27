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

(add-hook 'makefile-mode-hook
          (lambda ()
            (when (fboundp 'whitespace-mode)
              (whitespace-mode t))
            (linum-mode 1)
            (imenu-add-menubar-index)))

(provide 'init-makefile)
