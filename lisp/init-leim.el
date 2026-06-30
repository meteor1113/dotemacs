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

;; chinese-wbim
(setq chinese-wbim-use-tooltip nil)
(autoload 'chinese-wbim-use-package "chinese-wbim" nil t)

(register-input-method
 "chinese-wbim" "euc-cn" 'chinese-wbim-use-package
 "Wubi" "chinese-wbim" "wb.txt")
(setq default-input-method 'chinese-wbim)

(eval-after-load "chinese-wbim"
  '(progn
     (require 'chinese-wbim-extra)
     (global-set-key ";" 'chinese-wbim-insert-ascii)))

;; ibus
(when (require 'ibus nil 'noerror)
  ;; (dolist (key '((f6) (f7) (f8) (shift f8) (f9) (f10) (f11) (f12)))
  ;;   (setq ibus-common-function-key-list
  ;;         (delete key ibus-common-function-key-list)))
  (setq ibus-common-function-key-list '((control " ")))
  (add-hook 'after-init-hook 'ibus-mode-on))

;; scim
(when (require 'scim-bridge nil 'noerror)
  (setq scim-common-function-key-list '((control " ")))
  (add-hook 'after-init-hook 'scim-mode-on))

(provide 'init-leim)

;;; init-leim.el ends here
