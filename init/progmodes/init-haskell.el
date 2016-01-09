;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @URL http://git.oschina.net/meteor1113/dotemacs

;;; Commentary:

;;; Code:

;; haskell-mode
(add-hook 'haskell-mode-hook
          '(lambda ()
             (haskell-doc-mode)
             (haskell-indentation-mode)
             (interactive-haskell-mode +1)))

(provide 'init-haskell)

;;; init-haskell.el ends here
