;;; cnfonts-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cnfonts" "cnfonts.el" (0 0 0 0))
;;; Generated autoloads from cnfonts.el

(autoload 'cnfonts-set-font "cnfonts" "\
使用已经保存的字号设置字体.
如果 FRAME 是 non-nil, 设置对应的 FRAME 的字体。

\(fn &optional FRAME)" t nil)

(autoload 'cnfonts-reset-fontsize "cnfonts" "\
使用 `cnfonts-default-fontsize' 重置字号." t nil)

(autoload 'cnfonts-decrease-fontsize "cnfonts" "\
Cnfonts 减小字体." t nil)

(autoload 'cnfonts-increase-fontsize "cnfonts" "\
Cnfonts 增大字体." t nil)

(autoload 'cnfonts-switch-profile "cnfonts" "\
切换 cnfonts profile." t nil)

(autoload 'cnfonts-next-profile "cnfonts" "\
选择下一个字体设置 profile.

\(fn &optional _)" t nil)

(declare-function cnfonts-ui "cnfonts-ui")

(autoload 'cnfonts-regenerate-profile "cnfonts" "\
重新生成当前 profile." t nil)

(defvar cnfonts-mode nil "\
Non-nil if Cnfonts mode is enabled.
See the `cnfonts-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `cnfonts-mode'.")

(custom-autoload 'cnfonts-mode "cnfonts" nil)

(autoload 'cnfonts-mode "cnfonts" "\
cnfonts mode.

This is a minor mode.  If called interactively, toggle the
`Cnfonts mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='cnfonts-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'cnfonts-enable "cnfonts" "\
启用 cnfonts, 建议使用 `cnfonts-mode'." t nil)

(autoload 'cnfonts-disable "cnfonts" "\
警用 cnfonts, 建议使用 `cnfonts-mode'." t nil)

(register-definition-prefixes "cnfonts" '("cnfonts-"))

;;;***

;;;### (autoloads nil "cnfonts-ui" "cnfonts-ui.el" (0 0 0 0))
;;; Generated autoloads from cnfonts-ui.el

(register-definition-prefixes "cnfonts-ui" '("cnfonts-ui"))

;;;***

;;;### (autoloads nil nil ("cnfonts-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cnfonts-autoloads.el ends here
