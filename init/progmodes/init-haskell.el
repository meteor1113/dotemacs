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

;; ghc
(setq ghc-display-error 'minibuffer)
(add-hook 'haskell-mode-hook
          '(lambda ()
             (ignore-errors (ghc-init))))

;; company-ghc
(add-hook 'haskell-mode-hook
          '(lambda ()
             (when (fboundp 'company-mode)
               (company-mode 1)
               (define-key haskell-mode-map (kbd "M-n") 'company-select-next)
               (define-key haskell-mode-map "\ep" 'company-select-previous)))
          'append)
(eval-after-load "company"
  '(progn
     (when (fboundp 'company-ghc)
       (add-to-list 'company-backends 'company-ghc))))

(provide 'init-haskell)

;;; init-haskell.el ends here
