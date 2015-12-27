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

;; yasnippet
(setq yas-wrap-around-region t)
(add-hook 'after-init-hook '(lambda () (ignore-errors (yas-global-mode 1))))

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

(eval-after-load "org"
  '(add-hook 'org-mode-hook
             (let ((original-command (lookup-key org-mode-map [tab])))
               `(lambda ()
                  (setq yas-fallback-behavior
                        '(apply ,original-command))
                  (local-set-key [tab] 'yas-expand)))))

(provide 'init-yasnippet)
