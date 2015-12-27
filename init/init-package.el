;;; -*- mode: emacs-lisp; coding: utf-8; -*-
;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @date 2015-04-28
;; @URL http://git.oschina.net/meteor1113/dotemacs

(let ((root-dir (if (boundp 'dotemacs-root-dir)
                    dotemacs-root-dir
                  (file-name-directory
                   (directory-file-name
                    (file-name-directory
                     (or load-file-name buffer-file-name)))))))
  (setq package-user-dir (expand-file-name "elpa" root-dir)))

(when (require 'package nil 'noerror)
  ;; (add-to-list 'package-archives
  ;;              '("marmalade" . "https://marmalade-repo.org/packages/"))
  ;; (add-to-list 'package-archives
  ;;              '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(defun compile-all-packages ()
  "Byte-compile all installed packages."
  (interactive)
  (dolist (elt package-alist)
    (package--compile (car (cdr elt)))))

(provide 'init-package)
