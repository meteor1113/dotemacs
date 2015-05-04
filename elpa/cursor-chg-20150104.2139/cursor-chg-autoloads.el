;;; cursor-chg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cursor-chg" "cursor-chg.el" (21828 4424 0
;;;;;;  0))
;;; Generated autoloads from cursor-chg.el

(defvar curchg-change-cursor-on-input-method-flag t "\
*Non-nil means to use a different cursor when using an input method.")

(custom-autoload 'curchg-change-cursor-on-input-method-flag "cursor-chg" t)

(defvar curchg-change-cursor-on-overwrite/read-only-flag t "\
*Non-nil means use a different cursor for overwrite mode or read-only.")

(custom-autoload 'curchg-change-cursor-on-overwrite/read-only-flag "cursor-chg" t)

(defvar curchg-default-cursor-color (or (cdr (assq 'cursor-color default-frame-alist)) "Red") "\
*Default text cursor color for non-special frames.")

(custom-autoload 'curchg-default-cursor-color "cursor-chg" t)

(defvar curchg-default-cursor-type 'bar "\
*Default text cursor type.")

(custom-autoload 'curchg-default-cursor-type "cursor-chg" t)

(defvar curchg-idle-cursor-type 'box "\
*Text cursor type when Emacs is idle.")

(custom-autoload 'curchg-idle-cursor-type "cursor-chg" t)

(defvar curchg-input-method-cursor-color "Orange" "\
*Default cursor color if using an input method.
This has no effect if `curchg-change-cursor-on-input-method-flag' is nil.")

(custom-autoload 'curchg-input-method-cursor-color "cursor-chg" t)

(defvar curchg-overwrite/read-only-cursor-type 'box "\
*Default text cursor type for overwrite mode or read-only buffer.
This applies only to non-special frames.  This has no effect if
`curchg-change-cursor-on-overwrite/read-only-flag' is nil.")

(custom-autoload 'curchg-overwrite/read-only-cursor-type "cursor-chg" t)

(autoload 'curchg-set-cursor-type "cursor-chg" "\
Set the cursor type of the selected frame to CURSOR-TYPE.
When called interactively, prompt for the type to use.
To get the frame's current cursor type, use `frame-parameters'.

\(fn CURSOR-TYPE)" t nil)

(defalias 'toggle-cursor-type-when-idle 'curchg-toggle-cursor-type-when-idle)

(autoload 'curchg-toggle-cursor-type-when-idle "cursor-chg" "\
Turn on or off automatically changing cursor type when Emacs is idle.
When on, use `curchg-idle-cursor-type' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

\(fn &optional ARG)" t nil)

(autoload 'curchg-change-cursor-when-idle-interval "cursor-chg" "\
Set wait until automatically change cursor type when Emacs is idle.
Whenever Emacs is idle for this many seconds, the cursor type will
change to `curchg-idle-cursor-type'.

To turn on or off automatically changing the cursor type when idle,
use `\\[toggle-cursor-type-when-idle].

\(fn SECS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cursor-chg-autoloads.el ends here
