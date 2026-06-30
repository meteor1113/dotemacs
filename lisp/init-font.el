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

;; cnfonts
(add-hook 'after-init-hook
          '(lambda ()
             (when (and (require 'cnfonts nil 'noerror)
                        (file-readable-p (cnfonts--return-config-file-path)))
               (cnfonts-enable))))

(provide 'init-font)

;;; init-font.el ends here
