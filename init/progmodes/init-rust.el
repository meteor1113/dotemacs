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

;; rust-mode

;; rustfmt
(eval-after-load "rust-mode"
  '(progn
     (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer)
     (define-key rust-mode-map [M-f8] #'rustfmt-format-buffer)))
(add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

;; cargo
(add-hook 'rust-mode-hook #'cargo-minor-mode)

;; ac-racer
(add-hook 'racer-mode-hook
          '(lambda ()
             (ac-racer-setup)
             (local-set-key (kbd "M-n") 'ac-complete-racer)))

;; racer
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(provide 'init-rust)
