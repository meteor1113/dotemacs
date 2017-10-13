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

;; org
(setq org-log-done 'time)
(setq org-export-with-archived-trees t)
(setq org-startup-truncated nil)
(setq org-src-fontify-natively t)

(add-hook 'org-mode-hook
          '(lambda ()
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

(eval-after-load "org"
  '(eval-after-load "yasnippet"
     '(add-hook 'org-mode-hook
                (let ((original-command (lookup-key org-mode-map [tab])))
                  `(lambda ()
                     (setq yas-fallback-behavior
                           '(apply ,original-command))
                     (local-set-key [tab] 'yas-expand))))))

(provide 'init-org)

;;; init-org.el ends here
