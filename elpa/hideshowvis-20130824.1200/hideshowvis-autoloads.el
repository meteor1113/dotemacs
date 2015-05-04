;;; hideshowvis-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "hideshowvis" "hideshowvis.el" (21828 5236
;;;;;;  0 0))
;;; Generated autoloads from hideshowvis.el

(autoload 'hideshowvis-click-fringe "hideshowvis" "\


\(fn EVENT)" t nil)

(autoload 'hideshowvis-minor-mode "hideshowvis" "\
Toggle Hideshowvis minor mode on or off.
With a prefix argument ARG, enable Hideshowvis minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{hideshowvis-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'hideshowvis-enable "hideshowvis" "\
Will enable hideshowvis minor mode

\(fn)" t nil)

(autoload 'hideshowvis-symbols "hideshowvis" "\
Defines the things necessary to get a + symbol in the fringe
and a yellow marker indicating the number of hidden lines at
the end of the line for hidden regions.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hideshowvis-autoloads.el ends here
