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

;; company
(setq company--disabled-backends '(company-pysmell))
;; (autoload 'company-mode "company" nil t)
;; (autoload 'global-company-mode "company" nil t)

(eval-after-load "company"
  '(progn
     (setq company-idle-delay nil)
     ;; (setq company-idle-delay t
     ;;       company-minimum-prefix-length 1
     ;;       company-begin-commands '(self-insert-command c-electric-lt-gt))
     (define-key company-mode-map (kbd "M-n") 'company-select-next)
     (define-key company-mode-map (kbd "M-p") 'company-select-previous)))

(provide 'init-company)
