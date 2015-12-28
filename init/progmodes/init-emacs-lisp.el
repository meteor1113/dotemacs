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
             (setq indent-tabs-mode nil)
             (ignore-errors (whitespace-mode t))
             (linum-mode 1)
             (hs-minor-mode t)
             (ignore-errors (imenu-add-menubar-index))
             (turn-on-eldoc-mode)))

;; hl-defined
(autoload 'hdefd-highlight-mode "hl-defined" nil t)
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (when (require 'hl-defined nil 'noerror)
               (hdefd-highlight-mode 1)))
          'APPEND)

(provide 'init-emacs-lisp)
