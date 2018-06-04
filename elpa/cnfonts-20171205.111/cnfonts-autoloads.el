;;; cnfonts-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cnfonts" "cnfonts.el" (0 0 0 0))
;;; Generated autoloads from cnfonts.el

(autoload 'cnfonts-set-font-with-saved-step "cnfonts" "\
设置字体为：当前保存 step 对应的字体.
如果 FRAME 是 non-nil, 设置对应的 FRAME 的字体。

\(fn &optional FRAME)" t nil)

(autoload 'cnfonts-reset-fontsize "cnfonts" "\
使用 `cnfonts-default-step' 对应的 step 来设置字体.

\(fn)" t nil)

(autoload 'cnfonts-decrease-fontsize "cnfonts" "\
Cnfonts 减小字体.

\(fn)" t nil)

(autoload 'cnfonts-increase-fontsize "cnfonts" "\
Cnfonts 增大字体.

\(fn)" t nil)

(autoload 'cnfonts-switch-profile "cnfonts" "\
切换 cnfonts profile.

\(fn)" t nil)

(autoload 'cnfonts-next-profile "cnfonts" "\
选择下一个 profile 中当前 STEP 对应的字体设置.

\(fn &optional STEP)" t nil)

(autoload 'cnfonts-edit-profile "cnfonts" "\
编辑当前 cnfonts profile.

\(fn)" t nil)

(autoload 'cnfonts-edit-profile-without-ui "cnfonts" "\
编辑当前 cnfonts profile, 不使用 ‘cnfonts-ui’ 组件.

\(fn)" t nil)

(autoload 'cnfonts-regenerate-profile "cnfonts" "\
重新生成当前 profile.

\(fn)" t nil)

(autoload 'cnfonts-insert-fonts-configure "cnfonts" "\
在光标处，插入一个 elisp 片断，这个 elisp 片断可以用来配置中文和英文字体.

\(fn)" t nil)

(autoload 'cnfonts-insert-fontname "cnfonts" "\
Select a valid font name, and insert at point.

\(fn)" t nil)

(autoload 'cnfonts-enable "cnfonts" "\
运行这个函数，可以让 Emacs 启动的时候就激活 cnfonts.

\(fn)" t nil)

(autoload 'cnfonts-disable "cnfonts" "\
清除与 cnfonts 相关的 hook 设定.

\(fn)" t nil)

(autoload 'cnfonts-set-spacemacs-fallback-fonts "cnfonts" "\
显示 Spacemace mode-line 上面有一些 Unicode 字符.
这些字符需要专门的字体来显示，spacemacs 将这些字体的名字内置在
`spacemacs/set-default-font' 的代码中。运行这个函数后，cnfonts
将使用同样的字体来显示这些 Unicode 字符。

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cnfonts" '("cnfonts-")))

;;;***

;;;### (autoloads nil "cnfonts-ui" "cnfonts-ui.el" (0 0 0 0))
;;; Generated autoloads from cnfonts-ui.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cnfonts-ui" '("cnfonts-ui")))

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
