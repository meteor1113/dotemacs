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

;; sh-mode
(add-hook 'sh-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (ignore-errors (whitespace-mode t))
             (linum-mode 1)
             (hs-minor-mode t)
             (ignore-errors (imenu-add-menubar-index))))

;; shell-mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(provide 'init-sh)
