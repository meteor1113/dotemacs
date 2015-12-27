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

;; org
(setq org-log-done 'time)
(setq org-export-with-archived-trees t)
(setq org-startup-truncated nil)
(setq org-src-fontify-natively t)

(add-hook 'org-mode-hook
          (lambda ()
            (setq comment-start nil)
            (setq indent-tabs-mode nil)
            ;; (when (fboundp 'whitespace-mode)
            ;;   (whitespace-mode 1))
            ;; (auto-fill-mode t)
            (imenu-add-menubar-index)))

(eval-after-load "org"
  `(progn
     (define-key org-mode-map [(control tab)] nil)
     (define-key org-mode-map (kbd "<C-S-iso-lefttab>")
       'org-force-cycle-archived)
     (define-key org-mode-map (kbd "<C-S-tab>") 'org-force-cycle-archived)))

(provide 'init-org)
