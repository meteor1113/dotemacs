;;; mode-icons-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mode-icons" "mode-icons.el" (0 0 0 0))
;;; Generated autoloads from mode-icons.el

(defvar mode-icons-mode nil "\
Non-nil if Mode-Icons mode is enabled.
See the `mode-icons-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mode-icons-mode'.")

(custom-autoload 'mode-icons-mode "mode-icons" nil)

(autoload 'mode-icons-mode "mode-icons" "\
Replace the name of the current major mode with an icon.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mode-icons" '("mode-icons")))

;;;***

;;;### (autoloads nil nil ("mode-icons-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mode-icons-autoloads.el ends here
