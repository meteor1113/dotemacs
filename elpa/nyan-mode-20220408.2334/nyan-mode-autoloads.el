;;; nyan-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nyan-mode" "nyan-mode.el" (0 0 0 0))
;;; Generated autoloads from nyan-mode.el

(defvar nyan-mode nil "\
Non-nil if Nyan mode is enabled.
See the `nyan-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nyan-mode'.")

(custom-autoload 'nyan-mode "nyan-mode" nil)

(autoload 'nyan-mode "nyan-mode" "\
Use NyanCat to show buffer size and position in mode-line.
You can customize this minor mode, see option `nyan-mode'.

This is a minor mode.  If called interactively, toggle the `Nyan
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='nyan-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "nyan-mode" '("nyan-"))

;;;***

;;;### (autoloads nil nil ("nyan-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nyan-mode-autoloads.el ends here
