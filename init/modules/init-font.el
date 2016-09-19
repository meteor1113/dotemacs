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

;; chinese-fonts-setup
(setq cfs-verbose nil)
(add-hook 'after-init-hook
          '(lambda ()
             (when (and (require 'chinese-fonts-setup nil 'noerror)
                        (file-exists-p cfs-profiles-directory)
                        cfs--current-profile
                        cfs--profiles-steps)
               (chinese-fonts-setup-enable))))

(provide 'init-font)

;;; init-font.el ends here
