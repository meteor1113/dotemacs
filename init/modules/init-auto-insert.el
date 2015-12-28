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

;; auto-insert
(auto-insert-mode 1)
;; (setq auto-insert t)
;; (setq auto-insert-query nil)
(let ((root-dir (if (boundp 'dotemacs-root-dir)
                    dotemacs-root-dir
                  (file-name-directory
                   (directory-file-name
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name)))))))))
  (setq auto-insert-directory
        ;; (file-name-as-directory
        (expand-file-name "etc/templates" root-dir)))

(setq auto-insert-expansion-alist
      '(("(>>>DIR<<<)" . (file-name-directory buffer-file-name))
        ("(>>>FILE<<<)" . (file-name-nondirectory buffer-file-name))
        ("(>>>FILE_SANS<<<)" . (file-name-sans-extension
                                (file-name-nondirectory buffer-file-name)))
        ("(>>>FILE_UPCASE<<<)" . (upcase
                                  (file-name-sans-extension
                                   (file-name-nondirectory buffer-file-name))))
        ("(>>>FILE_UPCASE_INIT<<<)" . (upcase-initials
                                       (file-name-sans-extension
                                        (file-name-nondirectory buffer-file-name))))
        ("(>>>FILE_EXT<<<)" . (file-name-extension buffer-file-name))
        ("(>>>FILE_EXT_UPCASE<<<)" . (upcase (file-name-extension buffer-file-name)))
        ("(>>>DATE<<<)" . (format-time-string "%d %b %Y"))
        ("(>>>TIME<<<)" . (format-time-string "%T"))
        ("(>>>VC_DATE<<<)" . (let ((ret ""))
                               (set-time-zone-rule "UTC")
                               (setq ret (format-time-string "%Y/%m/%d %T"))
                               (set-time-zone-rule nil)
                               ret))
        ("(>>>YEAR<<<)" . (format-time-string "%Y"))
        ("(>>>ISO_DATE<<<)" . (format-time-string "%Y-%m-%d"))
        ("(>>>AUTHOR<<<)" . (or user-mail-address
                                (and (fboundp 'user-mail-address)
                                     (user-mail-address))
                                (concat (user-login-name) "@" (system-name))))
        ("(>>>USER_NAME<<<)" . (or (and (boundp 'user-full-name)
                                        user-full-name)
                                   (user-full-name)))
        ("(>>>LOGIN_NAME<<<)" . (user-login-name))
        ("(>>>HOST_ADDR<<<)" . (or (and (boundp 'mail-host-address)
                                        (stringp mail-host-address)
                                        mail-host-address)
                                   (system-name)))))

(defun auto-insert-expand ()
  (dolist (val auto-insert-expansion-alist)
    (let ((from (car val))
          (to (eval (cdr val))))
      (goto-char (point-min))
      (replace-string from to))))

(define-auto-insert "\\.\\([Hh]\\|hh\\|hpp\\)\\'"
  ["h.tpl" auto-insert-expand])
(define-auto-insert "\\.\\([Cc]\\|cc\\|cpp\\)\\'"
  ["cpp.tpl" auto-insert-expand])
(define-auto-insert "\\.java\\'"
  ["java.tpl" auto-insert-expand])
(define-auto-insert "\\.py\\'"
  ["py.tpl" auto-insert-expand])

(provide 'init-auto-insert)
