;;; ecb-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ecb" "ecb.el" (0 0 0 0))
;;; Generated autoloads from ecb.el

(autoload 'ecb-activate "ecb" "\
Activates ECB and creates the special buffers for the choosen layout.
For the layout see `ecb-layout-name'. This function raises always the
ECB-frame if called from another frame. This is the same as calling
`ecb-minor-mode' with a positive argument." t nil)

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

(register-definition-prefixes "ecb" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-advice-test" "ecb-advice-test.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ecb-advice-test.el

(register-definition-prefixes "ecb-advice-test" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-analyse" "ecb-analyse.el" (0 0 0 0))
;;; Generated autoloads from ecb-analyse.el

(register-definition-prefixes "ecb-analyse" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-autogen" "ecb-autogen.el" (0 0 0 0))
;;; Generated autoloads from ecb-autogen.el

(register-definition-prefixes "ecb-autogen" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-cedet-wrapper" "ecb-cedet-wrapper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ecb-cedet-wrapper.el

(register-definition-prefixes "ecb-cedet-wrapper" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-common-browser" "ecb-common-browser.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ecb-common-browser.el

(register-definition-prefixes "ecb-common-browser" '("defecb-" "ecb-"))

;;;***

;;;### (autoloads nil "ecb-compatibility" "ecb-compatibility.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ecb-compatibility.el

(register-definition-prefixes "ecb-compatibility" '("Electric-pop-up-window" "bs-show" "ecb-" "electric-" "one-window-p" "scroll-all-function-all"))

;;;***

;;;### (autoloads nil "ecb-compilation" "ecb-compilation.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ecb-compilation.el

(register-definition-prefixes "ecb-compilation" '("ecb-compilation-"))

;;;***

;;;### (autoloads nil "ecb-create-layout" "ecb-create-layout.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ecb-create-layout.el

(register-definition-prefixes "ecb-create-layout" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-eshell" "ecb-eshell.el" (0 0 0 0))
;;; Generated autoloads from ecb-eshell.el

(register-definition-prefixes "ecb-eshell" '("ecb-eshell-" "eshell"))

;;;***

;;;### (autoloads nil "ecb-examples" "ecb-examples.el" (0 0 0 0))
;;; Generated autoloads from ecb-examples.el

(register-definition-prefixes "ecb-examples" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-face" "ecb-face.el" (0 0 0 0))
;;; Generated autoloads from ecb-face.el

(register-definition-prefixes "ecb-face" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-file-browser" "ecb-file-browser.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ecb-file-browser.el

(register-definition-prefixes "ecb-file-browser" '("clearcase-sync-from-disk" "ecb-" "vc-checkin"))

;;;***

;;;### (autoloads nil "ecb-help" "ecb-help.el" (0 0 0 0))
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

(register-definition-prefixes "ecb-help" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-jde" "ecb-jde.el" (0 0 0 0))
;;; Generated autoloads from ecb-jde.el

(register-definition-prefixes "ecb-jde" '("ecb-jde-"))

;;;***

;;;### (autoloads nil "ecb-layout" "ecb-layout.el" (0 0 0 0))
;;; Generated autoloads from ecb-layout.el

(register-definition-prefixes "ecb-layout" '("balance-windows" "compilation-set-window-height" "current-window-configuration" "display-buffer" "ecb-" "other-window" "scroll-other-window" "set-window-configuration" "split-window" "switch-to-buffer" "walk-windows"))

;;;***

;;;### (autoloads nil "ecb-layout-defs" "ecb-layout-defs.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ecb-layout-defs.el

(register-definition-prefixes "ecb-layout-defs" '("ecb-buildin-layouts"))

;;;***

;;;### (autoloads nil "ecb-method-browser" "ecb-method-browser.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ecb-method-browser.el

(register-definition-prefixes "ecb-method-browser" '("custom-save-all" "ecb-" "make-indirect-buffer"))

;;;***

;;;### (autoloads nil "ecb-mode-line" "ecb-mode-line.el" (0 0 0 0))
;;; Generated autoloads from ecb-mode-line.el

(register-definition-prefixes "ecb-mode-line" '("ecb-mode-line-"))

;;;***

;;;### (autoloads nil "ecb-multiframe" "ecb-multiframe.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ecb-multiframe.el

(register-definition-prefixes "ecb-multiframe" '("ecb-multiframe-variables"))

;;;***

;;;### (autoloads nil "ecb-navigate" "ecb-navigate.el" (0 0 0 0))
;;; Generated autoloads from ecb-navigate.el

(register-definition-prefixes "ecb-navigate" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-semantic" "ecb-semantic.el" (0 0 0 0))
;;; Generated autoloads from ecb-semantic.el

(register-definition-prefixes "ecb-semantic" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-semantic-wrapper" "ecb-semantic-wrapper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ecb-semantic-wrapper.el

(register-definition-prefixes "ecb-semantic-wrapper" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-speedbar" "ecb-speedbar.el" (0 0 0 0))
;;; Generated autoloads from ecb-speedbar.el

(register-definition-prefixes "ecb-speedbar" '("dframe-" "ecb-" "speedbar-"))

;;;***

;;;### (autoloads nil "ecb-symboldef" "ecb-symboldef.el" (0 0 0 0))
;;; Generated autoloads from ecb-symboldef.el

(register-definition-prefixes "ecb-symboldef" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-tod" "ecb-tod.el" (0 0 0 0))
;;; Generated autoloads from ecb-tod.el

(register-definition-prefixes "ecb-tod" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-upgrade" "ecb-upgrade.el" (0 0 0 0))
;;; Generated autoloads from ecb-upgrade.el

(register-definition-prefixes "ecb-upgrade" '("ecb-"))

;;;***

;;;### (autoloads nil "ecb-util" "ecb-util.el" (0 0 0 0))
;;; Generated autoloads from ecb-util.el

(defconst ecb-running-xemacs (featurep 'xemacs))

(register-definition-prefixes "ecb-util" '("defecb-multicache" "ecb-" "when-ecb-running-"))

;;;***

;;;### (autoloads nil "ecb-winman-support" "ecb-winman-support.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ecb-winman-support.el

(register-definition-prefixes "ecb-winman-support" '("ecb-winman-" "escreen-save-current-screen-configuration" "win"))

;;;***

;;;### (autoloads nil "silentcomp" "silentcomp.el" (0 0 0 0))
;;; Generated autoloads from silentcomp.el

(register-definition-prefixes "silentcomp" '("silentcomp-"))

;;;***

;;;### (autoloads nil "tree-buffer" "tree-buffer.el" (0 0 0 0))
;;; Generated autoloads from tree-buffer.el

(register-definition-prefixes "tree-buffer" '("tree-"))

;;;***

;;;### (autoloads nil nil ("ecb-buffertab.el" "ecb-cycle.el" "ecb-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ecb-autoloads.el ends here
