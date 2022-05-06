;;; racer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "racer" "racer.el" (0 0 0 0))
;;; Generated autoloads from racer.el

(autoload 'racer-find-definition "racer" "\
Run the racer find-definition command and process the results." t nil)

(autoload 'racer-find-definition-other-window "racer" "\
Run the racer find-definition command and process the results." t nil)

(autoload 'racer-find-definition-other-frame "racer" "\
Run the racer find-definition command and process the results." t nil)

(autoload 'racer-mode "racer" "\
Minor mode for racer.

This is a minor mode.  If called interactively, toggle the `racer
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `racer-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "racer" '("racer-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; racer-autoloads.el ends here
