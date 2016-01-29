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

;; calendar
(setq holiday-local-holidays '((holiday-fixed 2 14 "情人节")
                               (holiday-fixed 3 8 "妇女节")
                               (holiday-fixed 3 12 "植树节")
                               (holiday-fixed 5 4 "青年节")
                               (holiday-float 5 0 2 "母亲节")   ;5月的第二个星期天
                               (holiday-float 6 0 3 "父亲节")
                               (holiday-fixed 6 1 "儿童节")
                               (holiday-fixed 9 10 "教师节")
                               (holiday-chinese 1 15 "元宵节(正月十五)")
                               (holiday-chinese 7 7 "七夕节")
                               (holiday-chinese 9 9 "重阳节(九月初九)")))
(setq calendar-chinese-all-holidays-flag t)
(setq mark-holidays-in-calendar t)
;; (setq calendar-week-start-day 1)
;; (setq mark-diary-entries-in-calendar t)
(setq diary-file "~/.emacs.d/diary")
;; (add-hook 'diary-hook 'appt-make-list)
;; (setq appt-display-format 'window)
;; (setq appt-display-mode-line t)
(setq appt-display-diary nil)
(setq appt-message-warning-time 0)
(setq appt-display-duration (* 365 24 60 60))

(unless (daemonp)
  ;; (add-hook 'after-init-hook
  ;;           (lambda ()
  ;;             (appt-activate 1))))
  (run-with-idle-timer 2 nil #'appt-activate 1))

;; cal-china-x
(eval-after-load "calendar"
  '(when (require 'cal-china-x nil 'noerror)
     (setq cal-china-x-priority1-holidays
           (append holiday-local-holidays
                   cal-china-x-chinese-holidays
                   cal-china-x-japanese-holidays))
     (setq calendar-holidays
           (append calendar-holidays
                   cal-china-x-chinese-holidays
                   cal-china-x-japanese-holidays))))

;; calfw
(autoload 'cfw:open-org-calendar "calfw-org" nil t)
(autoload 'cfw:open-diary-calendar "calfw-cal" nil t)
;; (autoload 'cfw:open-calendar-buffer "calfw" nil t)
;; (eval-after-load "calfw"
;;   '(when (require 'calfw-org nil 'noerror)
;;      (cfw:install-org-schedules)))

(provide 'init-calendar)

;;; init-calendar.el ends here
