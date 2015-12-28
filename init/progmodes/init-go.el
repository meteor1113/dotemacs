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

;; go-mode
(add-hook 'go-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook #'gofmt-before-save)
             (local-set-key (kbd "M-.") #'godef-jump)
             (local-set-key (kbd "M-,") #'pop-tag-mark)))

;; go-doc
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; go-snippets

;; go-autocomplete
(add-hook 'go-mode-hook
          '(lambda ()
             (when (require 'go-autocomplete nil 'noerror)
               (local-set-key (kbd "M-n") 'ac-complete-go))))

(provide 'init-go)
