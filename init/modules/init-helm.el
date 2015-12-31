;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @URL http://git.oschina.net/meteor1113/dotemacs

;;; Commentary:

;;; Code:

;; helm
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(add-hook 'after-init-hook
          '(lambda ()
             (when (require 'helm-config nil 'noerror)
               (helm-mode 1)

               (global-set-key (kbd "C-c h") 'helm-command-prefix)
               (global-unset-key (kbd "C-x c"))

               (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
               (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
               (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
               )))

(provide 'init-helm)

;;; init-helm.el ends here
