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

;; dired
(setq dired-dwim-target t)
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;;             (define-key dired-mode-map (kbd "^")
;;               (lambda () (interactive) (find-alternate-file "..")))))

(defadvice dired-find-file (around dired-find-file-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-file-for-visit)))
    ad-do-it
    (when (and (file-directory-p filename)
               (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

(defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer)))
    ad-do-it
    (kill-buffer orig)))

;; dired+
(when window-system
  (eval-after-load "dired"
    '(when (require 'dired+ nil 'noerror)
       (define-key dired-mode-map [mouse-2] 'diredp-mouse-find-file)
       (diredp-toggle-find-file-reuse-dir 1))))

(provide 'init-dired)
