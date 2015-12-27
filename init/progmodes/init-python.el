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

(add-hook 'python-mode-hook 'prog-common-function)

(eval-after-load "auto-complete-config"
  '(progn
     (setq ac-source-ropemacs              ; Redefine ac-source-ropemacs
           '((candidates . (lambda ()
                             (setq ac-ropemacs-completions-cache
                                   (mapcar
                                    (lambda (completion)
                                      (concat ac-prefix completion))
                                    (ignore-errors
                                      (rope-completions))))))
             (prefix . c-dot)
             (requires . 0)))
     (defun ac-complete-ropemacs ()
       (interactive)
       (auto-complete '(ac-source-ropemacs)))
     (defun ac-ropemacs-setup ()
       (when (locate-library "pymacs")
         (ac-ropemacs-require)
         ;; (setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
         (local-set-key (kbd "M-n") 'ac-complete-ropemacs)))
     (ac-ropemacs-initialize)))

(provide 'init-python)
