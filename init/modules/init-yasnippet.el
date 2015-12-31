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

;; yasnippet
(setq yas-wrap-around-region t)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (yas-global-mode 1))))

(eval-after-load "yasnippet"
  '(let ((root-dir (if (boundp 'dotemacs-root-dir)
                       dotemacs-root-dir
                     (file-name-directory
                      (directory-file-name
                       (file-name-directory
                        (directory-file-name
                         (file-name-directory
                          (or load-file-name buffer-file-name)))))))))
     (add-to-list 'yas-snippet-dirs
                  (expand-file-name "etc/snippets" root-dir))))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
