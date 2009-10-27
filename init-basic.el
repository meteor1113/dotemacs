;;;
;; Copyright (C) 2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2009-08-08


;;; basic setting
(setq user-full-name "Meteor Liu")
(setq user-mail-address "meteor1113@gmail.com")

(tool-bar-mode t)
(set-scroll-bar-mode 'right)
(cua-mode t)
(setq cua-keep-region-after-copy t)
(ido-mode t)
(icomplete-mode t)
(column-number-mode t)
(display-time-mode t)
(show-paren-mode t)
(ffap-bindings)
(global-auto-revert-mode t)
(which-function-mode t)
(setq bookmark-save-flag 1)
(setq-default show-trailing-whitespace t)
;; (global-highlight-changes-mode t)
;; (global-hl-line-mode t)
(require 'saveplace)
(setq-default save-place t)
(savehist-mode t)
(recentf-mode t)
(desktop-save-mode t)

(setq org-log-done 'time)
(add-hook 'org-mode-hook
          (lambda ()
            (imenu-add-menubar-index)
            (setq comment-start nil)
            (auto-fill-mode t)))

(when (require 'nxml-mode nil t)
  (add-to-list 'auto-mode-alist
               '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (add-hook 'nxml-mode-hook
            '(lambda ()
               (require 'sgml-mode)
               (set-syntax-table sgml-mode-syntax-table))))

(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)


;;; program setting
(defun program-common-function ()
  (setq indent-tabs-mode nil)
  (hs-minor-mode t)
  (imenu-add-menubar-index))

(add-hook 'c-mode-common-hook 'program-common-function)

(add-to-list 'auto-mode-alist '("\\.[ch]\\'" . c++-mode))
(add-hook 'c-mode-hook (lambda () (c-set-style "stroustrup")))

(add-hook 'c++-mode-hook (lambda () (c-set-style "stroustrup")))

(add-hook 'java-mode-hook (lambda () (c-set-style "java")))

(add-hook 'objc-mode-hook (lambda () (c-set-style "stroustrup")))

(add-hook 'emacs-lisp-mode-hook 'program-common-function)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'python-mode-hook 'program-common-function)
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

(add-hook 'perl-mode-hook 'program-common-function)
(add-to-list 'auto-mode-alist
             '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-hook 'cperl-mode-hook
          '(lambda ()
             (program-common-function)
             (cperl-set-style "PerlStyle")
             (setq cperl-continued-brace-offset -4)
             (abbrev-mode t)))


(provide 'init-basic)
