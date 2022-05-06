;;; highlight-symbol-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "highlight-symbol" "highlight-symbol.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from highlight-symbol.el

(autoload 'highlight-symbol-mode "highlight-symbol" "\
Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'.

This is a minor mode.  If called interactively, toggle the
`Highlight-Symbol mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `highlight-symbol-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defalias 'highlight-symbol-at-point 'highlight-symbol)

(autoload 'highlight-symbol "highlight-symbol" "\
Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'.

\(fn &optional SYMBOL)" t nil)

(autoload 'highlight-symbol-remove-all "highlight-symbol" "\
Remove symbol highlighting in buffer." t nil)

(autoload 'highlight-symbol-list-all "highlight-symbol" "\
List all symbols highlighted in the buffer." t nil)

(autoload 'highlight-symbol-count "highlight-symbol" "\
Print the number of occurrences of symbol at point.

\(fn &optional SYMBOL MESSAGE-P)" t nil)

(autoload 'highlight-symbol-next "highlight-symbol" "\
Jump to the next location of the symbol at point within the buffer." t nil)

(autoload 'highlight-symbol-prev "highlight-symbol" "\
Jump to the previous location of the symbol at point within the buffer." t nil)

(autoload 'highlight-symbol-next-in-defun "highlight-symbol" "\
Jump to the next location of the symbol at point within the defun." t nil)

(autoload 'highlight-symbol-prev-in-defun "highlight-symbol" "\
Jump to the previous location of the symbol at point within the defun." t nil)

(autoload 'highlight-symbol-nav-mode "highlight-symbol" "\
Navigate occurrences of the symbol at point.

When called interactively, toggle `highlight-symbol-nav-mode'.
With prefix ARG, enable `highlight-symbol-nav-mode' if ARG is
positive, otherwise disable it.

When called from Lisp, enable `highlight-symbol-nav-mode' if ARG
is omitted, nil or positive.  If ARG is `toggle', toggle
`highlight-symbol-nav-mode'.  Otherwise behave as if called
interactively.

In `highlight-symbol-nav-mode' provide the following key bindings
to navigate between occurrences of the symbol at point in the
current buffer.

\\{highlight-symbol-nav-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'highlight-symbol-query-replace "highlight-symbol" "\
Replace the symbol at point with REPLACEMENT.

\(fn REPLACEMENT)" t nil)

(autoload 'highlight-symbol-occur "highlight-symbol" "\
Call `occur' with the symbol at point.
Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.

\(fn &optional NLINES)" t nil)

(register-definition-prefixes "highlight-symbol" '("highlight-symbol"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-symbol-autoloads.el ends here
