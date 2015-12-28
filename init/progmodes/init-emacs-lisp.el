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

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (ignore-errors (whitespace-mode t))
             (turn-on-eldoc-mode)))

;; hl-defined
(autoload 'hdefd-highlight-mode "hl-defined" nil t)
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (when (require 'hl-defined nil 'noerror)
               (hdefd-highlight-mode 1)))
          'APPEND)

(provide 'init-emacs-lisp)
