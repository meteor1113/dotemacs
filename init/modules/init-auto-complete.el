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


;; auto-complete
(setq ac-use-comphist nil)
(setq ac-disable-faces nil)
;; (global-set-key (kbd "M-n") 'auto-complete)
(add-hook 'after-init-hook '(lambda () (ignore-errors (ac-config-default))))

(eval-after-load "auto-complete"
  '(progn
     (define-key ac-completing-map [return] 'ac-complete)
     (setq ac-modes
           (append ac-modes
                   '(org-mode objc-mode csharp-mode jde-mode sql-mode
                              plsql-mode sqlplus-mode eshell-mode
                              inferior-emacs-lisp-mode change-log-mode
                              text-mode xml-mode nxml-mode html-mode
                              tex-mode latex-mode plain-tex-mode
                              conf-unix-mode conf-windows-mode
                              conf-colon-mode conf-space-mode
                              conf-javaprop-mode inetd-conf-generic-mode
                              etc-services-generic-mode etc-passwd-generic-mode
                              etc-fstab-generic-mode etc-sudoers-generic-mode
                              resolve-conf-generic-mode
                              etc-modules-conf-generic-mode
                              apache-conf-generic-mode apache-log-generic-mode
                              samba-generic-mode reg-generic-mode
                              fvwm-generic-mode ini-generic-mode
                              x-resource-generic-mode
                              hosts-generic-mode inf-generic-mode
                              bat-generic-mode javascript-generic-mode
                              vrml-generic-mode java-manifest-generic-mode
                              java-properties-generic-mode
                              alias-generic-mode rc-generic-mode
                              makefile-gmake-mode makefile-bsdmake-mode
                              autoconf-mode makefile-automake-mode)))

     (defadvice ac-update-word-index-1 (around exclude-hidden-buffer activate)
       "Exclude hidden buffer, hack for eim."
       (unless (string= (substring (buffer-name) 0 1) " ")
         ad-do-it))))

(eval-after-load "auto-complete-config"
  '(progn
     (add-hook 'ielm-mode-hook 'ac-emacs-lisp-mode-setup)
     (add-hook 'eshell-mode-hook 'ac-emacs-lisp-mode-setup)
     (add-hook 'auto-complete-mode-hook
               '(lambda ()
                  (add-to-list 'ac-sources 'ac-source-yasnippet)))))

(provide 'init-auto-complete)
