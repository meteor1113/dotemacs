;;; cal-china-x-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "cal-china-x" "cal-china-x.el" (22661 30075
;;;;;;  0 0))
;;; Generated autoloads from cal-china-x.el

(autoload 'cal-china-x-birthday-from-chinese "cal-china-x" "\
Return birthday date this year in Gregorian form.

LUNAR-MONTH and LUNAR-DAY are date number used in chinese lunar
calendar.

\(fn LUNAR-MONTH LUNAR-DAY)" t nil)

(autoload 'holiday-lunar "cal-china-x" "\
Like `holiday-fixed', but with LUNAR-MONTH and LUNAR-DAY.

When there are multiple days(like Run Yue or 闰月, e.g.,
2006-08-30, which is 07-07 in lunar calendar, the chinese
valentine's day), we use NUM to define which day(s) as
holidays. The rules are:

NUM = 0, only the earlier day.
NUM = 1, only the later day.
NUM with other values(default), all days(maybe one or two).

emacs23 introduces a similar `holiday-chinese', a quick test
shows that it does not recognize Run Yue at all.

\(fn LUNAR-MONTH LUNAR-DAY STRING &optional NUM)" nil nil)

(autoload 'holiday-solar-term "cal-china-x" "\
A holiday(STR) on SOLAR-TERM day.
See `cal-china-x-solar-term-name' for a list of solar term names .

\(fn SOLAR-TERM STR)" nil nil)

;;;***

;;;### (autoloads nil nil ("cal-china-x-pkg.el") (22661 30075 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cal-china-x-autoloads.el ends here
