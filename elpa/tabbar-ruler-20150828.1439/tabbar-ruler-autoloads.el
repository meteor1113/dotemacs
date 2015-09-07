;;; tabbar-ruler-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "tabbar-ruler" "tabbar-ruler.el" (21997 20709
;;;;;;  0 0))
;;; Generated autoloads from tabbar-ruler.el

(autoload 'tabbar-install-faces "tabbar-ruler" "\
Installs faces for a frame.

\(fn &optional FRAME)" t nil)

(autoload 'tabbar-ruler-up "tabbar-ruler" "\
Tabbar press up key.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-ruler-forward "tabbar-ruler" "\
Forward ruler. Takes into consideration if the home-key was pressed.
This is based on the variable `tabbar--buffer-show-groups'

\(fn &optional ARG)" t nil)

(autoload 'tabbar-ruler-backward "tabbar-ruler" "\
Backward ruler.  Takes into consideration if the home-key was pressed.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-ruler-move "tabbar-ruler" "\
Start the movement for the tabbar

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("tabbar-ruler-pkg.el") (21997 20709 382581
;;;;;;  300000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; tabbar-ruler-autoloads.el ends here
