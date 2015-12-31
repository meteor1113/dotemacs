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

;; flycheck
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (global-flycheck-mode t))))

(eval-after-load "flycheck"
  '(progn
     (define-key flycheck-mode-map (kbd "C-c <f4>") 'flycheck-next-error)
     (define-key flycheck-mode-map (kbd "C-c <S-f4>") 'flycheck-previous-error)
     (define-key flycheck-mode-map (kbd "C-c <C-f4>") 'flycheck-list-errors)))

;; flycheck-rust
(add-hook 'flycheck-mode-hook
          #'(lambda ()
              (ignore-errors (flycheck-rust-setup))))

(provide 'init-flycheck)
