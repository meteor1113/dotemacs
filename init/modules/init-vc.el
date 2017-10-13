;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @URL https://github.com/meteor1113/dotemacs

;;; Commentary:

;;; Code:

;; change-log-mode
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

;; psvn
;; (autoload 'svn-status "psvn" nil t)
(eval-after-load "vc-svn"
  '(require 'psvn nil 'noerror))

;; magit
;; (autoload 'magit-status "magit" nil t)

(provide 'init-vc)

;;; init-vc.el ends here
