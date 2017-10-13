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

;; server
(when (and window-system (not (daemonp)))
  (require 'server)

  (when (and (>= emacs-major-version 23)
             (equal window-system 'w32))
    (unless (file-exists-p server-auth-dir)
      (make-directory server-auth-dir))
    (defun server-ensure-safe-dir (dir) "Noop" t))

  (unless (server-running-p)
    (server-start)))

(provide 'init-server)

;;; init-server.el ends here
