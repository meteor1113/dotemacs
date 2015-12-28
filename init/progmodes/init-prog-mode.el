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

;; ggtags
(when (executable-find "global")
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (ggtags-mode 1))))

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
