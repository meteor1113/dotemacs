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

;; auto-complete
(setq ac-use-comphist nil)
(setq ac-disable-faces nil)
;; (global-set-key (kbd "M-n") 'auto-complete)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (ac-config-default))))
;; (run-with-idle-timer 3 nil #'ac-config-default)

(eval-after-load "auto-complete"
  '(progn
     (defun auto-complete-mode-maybe ()
       (unless (minibufferp (current-buffer))
         (auto-complete-mode 1)))

     (define-key ac-completing-map [return] 'ac-complete)

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

;;; init-auto-complete.el ends here
