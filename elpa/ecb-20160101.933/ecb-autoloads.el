;;; ecb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ecb" "ecb.el" (22661 29970 0 0))
;;; Generated autoloads from ecb.el

(autoload 'ecb-activate "ecb" "\
Activates ECB and creates the special buffers for the choosen layout.
For the layout see `ecb-layout-name'. This function raises always the
ECB-frame if called from another frame. This is the same as calling
`ecb-minor-mode' with a positive argument.

\(fn)" t nil)

(autoload 'ecb-minor-mode "ecb" "\
Toggle ECB minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{ecb-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'ecb-byte-compile "ecb" "\
Byte-compiles the ECB package.
This is done for all lisp-files of ECB if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist.

\(fn &optional FORCE-ALL)" t nil)

;;;***

;;;### (autoloads nil "ecb-help" "ecb-help.el" (22661 29970 0 0))
;;; Generated autoloads from ecb-help.el

(autoload 'ecb-show-help "ecb-help" "\
Shows the online help of ECB in Info or HTML-format.
The format depends on the setting in `ecb-show-help-format'. If called with
prefix argument, i.e. if FORMAT is not nil then the user is prompted to choose
the format of the help (Info or Html).

If an error about not finding the needed help-file occurs please take a look
at the options `ecb-help-info-start-file' and `ecb-help-html-start-file'!

Note: If you got ECB as a standard XEmacs-package maybe the
HTML-online-documentation is not included.

\(fn &optional FORMAT)" t nil)

;;;***

;;;### (autoloads nil "ecb-util" "ecb-util.el" (22661 29970 0 0))
;;; Generated autoloads from ecb-util.el

(defconst ecb-running-xemacs (featurep 'xemacs))

;;;***

;;;### (autoloads nil nil ("ecb-advice-test.el" "ecb-analyse.el"
;;;;;;  "ecb-autogen.el" "ecb-buffertab.el" "ecb-cedet-wrapper.el"
;;;;;;  "ecb-common-browser.el" "ecb-compatibility.el" "ecb-compilation.el"
;;;;;;  "ecb-create-layout.el" "ecb-cycle.el" "ecb-eshell.el" "ecb-examples.el"
;;;;;;  "ecb-face.el" "ecb-file-browser.el" "ecb-jde.el" "ecb-layout-defs.el"
;;;;;;  "ecb-layout.el" "ecb-method-browser.el" "ecb-mode-line.el"
;;;;;;  "ecb-multiframe.el" "ecb-navigate.el" "ecb-pkg.el" "ecb-semantic-wrapper.el"
;;;;;;  "ecb-semantic.el" "ecb-speedbar.el" "ecb-symboldef.el" "ecb-tod.el"
;;;;;;  "ecb-upgrade.el" "ecb-winman-support.el" "silentcomp.el"
;;;;;;  "tree-buffer.el") (22661 29971 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ecb-autoloads.el ends here
