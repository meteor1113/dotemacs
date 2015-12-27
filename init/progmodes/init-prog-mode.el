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

(defun chmod+x ()
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (not (file-executable-p buffer-file-name))
       (shell-command (format "chmod +x '%s'" buffer-file-name))
       (kill-buffer "*Shell Command Output*")))
(when (executable-find "chmod")
  (add-hook 'after-save-hook 'chmod+x))

(defun prog-common-function ()
  (setq indent-tabs-mode nil)
  ;; (local-set-key (kbd "<return>") 'newline-and-indent)
  (when (fboundp 'whitespace-mode)
    (whitespace-mode t))
  (linum-mode 1)
  (hs-minor-mode t)
  (ignore-errors (imenu-add-menubar-index)))

;; ggtags
(when (executable-find "global")
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

;; smart-compile
;; (autoload 'smart-compile "smart-compile" nil t)
(global-set-key [C-f7] 'smart-compile)

;; xcscope
(add-hook 'after-init-hook
          '(lambda ()
             (when (executable-find "cscope")
               (when (require 'xcscope nil 'noerror)
                 (define-key cscope-list-entry-keymap [mouse-1]
                   'cscope-mouse-select-entry-other-window)))))

(provide 'init-prog-mode)
