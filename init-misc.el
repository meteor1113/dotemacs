;;; -*- mode: emacs-lisp; mode: goto-address; coding: utf-8; -*-
;; Copyright (C) 2008-2015 Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@gmail.com>
;; @date 2008-08-08
;; @URL http://git.oschina.net/meteor1113/dotemacs


;; path
(let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
       (default-directory (expand-file-name "lisp" dir)))
  (when (file-exists-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

;; (unless (fboundp 'define-global-minor-mode) ; for emacs-21
;;   (defmacro define-global-minor-mode (global-mode mode turn-on &rest keys)))
(unless (fboundp 'define-globalized-minor-mode) ; for emacs-22
  (defalias 'define-globalized-minor-mode 'define-global-minor-mode))
(unless (fboundp 'with-no-warnings)     ; for emacs-21
  (defun with-no-warnings (body)
    "Before emacs-21, have not with-no-warnings function."))
(unless (fboundp 'define-fringe-bitmap) ; for emacs-21
  (defun define-fringe-bitmap (var value)
    "Before emacs-21, have not define-fringe-bitmap function."))

;; unicad
(require 'unicad nil 'noerror)

;; cal-china-x
(eval-after-load "calendar"
  '(when (require 'cal-china-x nil 'noerror)
     (setq cal-china-x-priority1-holidays
           (append holiday-local-holidays
                   cal-china-x-chinese-holidays
                   cal-china-x-japanese-holidays))
     (setq calendar-holidays
           (append calendar-holidays
                   cal-china-x-chinese-holidays
                   cal-china-x-japanese-holidays))))

;; cn-weather
;; (setq cn-weather-city "大连")
(autoload 'display-cn-weather-mode "cn-weather"
  "Display weather information in the mode line." t)
(autoload 'cn-weather "cn-weather"
  "Print Now today's and realtime weather in the echo area." t)
(autoload 'cn-weather-forecast "cn-weather"
  "Print future two days' weather info in minibuffer." t)

;; vimpulse
(setq viper-mode nil)
(eval-after-load "viper"
  '(require 'vimpulse nil 'noerror))

;; ascii
(autoload 'ascii-on        "ascii" "Turn on ASCII code display."   t)
(autoload 'ascii-off       "ascii" "Turn off ASCII code display."  t)
(autoload 'ascii-display   "ascii" "Toggle ASCII code display."    t)
(autoload 'ascii-customize "ascii" "Customize ASCII code display." t)

;; recent-jump
(when (require 'recent-jump nil 'noerror)
  (global-set-key (kbd "<M-S-left>") 'recent-jump-jump-backward)
  (global-set-key (kbd "<M-S-right>") 'recent-jump-jump-forward))

;; ace-jump-mode
(autoload 'ace-jump-mode "ace-jump-mode" nil t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(set-face-background 'ace-jump-face-foreground "yellow"))
(eval-after-load "viper-keym"
  '(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode))

;; smart-hl
(when (> emacs-major-version 21)
  (require 'smart-hl nil 'noerror))

;; hl-defined
(autoload 'hdefd-highlight-mode "hl-defined" nil t)
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (when (require 'hl-defined nil 'noerror)
               (hdefd-highlight-mode 1)))
          'APPEND)

;; ifdef
(add-hook 'c-mode-common-hook
          '(lambda ()
             (when (require 'ifdef nil 'noerror)
               (define-key c-mode-base-map [?\C-c ?\C-i] 'mark-ifdef)
               (mark-ifdef))))

;; doc-mode/doxymacs-mode
(unless (locate-library "url")
  (provide 'url))                       ; emacs-21 doesn't have url
(add-hook 'c-mode-common-hook
          '(lambda ()
             (if (and (featurep 'semantic)
                      (require 'doc-mode nil 'noerror))
                 (doc-mode t)
               (when (require 'doxymacs nil 'noerror)
                 (doxymacs-mode t)
                 (doxymacs-font-lock)))))

;; xgtags
(when (executable-find "global")
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (require 'xgtags nil 'noerror)
                (xgtags-mode 1)))))

;; ntcmd
;; (autoload 'ntcmd-mode "ntcmd" "Major mode for editing CMD scripts." t)
;; (add-to-list 'auto-mode-alist '("\\.[bB][aA][tT]\\'" . ntcmd-mode))
;; (add-to-list 'auto-mode-alist '("\\.[cC][mM][dD]\\'" . ntcmd-mode))

;; sourcepair
(setq sourcepair-source-extensions
      '(".cpp" ".cxx" ".c++" ".CC" ".cc" ".C" ".c" ".mm" ".m"))
(setq sourcepair-header-extensions
      '(".hpp" ".hxx" ".h++" ".HH" ".hh" ".H" ".h"))
(setq sourcepair-header-path '("." "include" ".." "../include" "../inc"
                               "../../include" "../../inc" "../*"))
(setq sourcepair-source-path '("." "src" ".." "../src" "../*"))
(setq sourcepair-recurse-ignore '("CVS" ".svn" ".hg" ".git" ".bzr"
                                  "Obj" "Debug" "Release" "bin" "lib"))
(add-hook 'c-mode-common-hook
          '(lambda ()
             (when (require 'sourcepair nil 'noerror)
               (define-key c-mode-base-map (kbd "ESC <f12>") 'sourcepair-load)
               (define-key c-mode-base-map [M-f12] 'sourcepair-load))))

;; code-imports
(autoload 'code-imports-grab-import "code-imports" nil t)
(autoload 'code-imports-add-grabbed-imports "code-imports" nil t)
(autoload 'code-imports-organize-imports "code-imports" nil t)
(setq code-imports-project-directory ".")
(global-set-key (kbd "C-S-o") 'code-imports-organize-imports)
(eval-after-load "code-imports"
  '(defun code-imports--import-in-group-p (import-line
                                           group
                                           &optional self-file)
     "Returns t if IMPORT-LINE is in GROUP.
GROUP is one of the elements of the ordering such as
`code-imports-c++-ordering'. SELF-FILE is the .h file
corresponding to the file being modified (or nil if we're not in
a c mode)."
     (cond ((eq group 'self)
            ;; right now self can only refer to c/c++ mode.
            (string-match (regexp-quote (file-name-sans-extension self-file))
                          import-line))
           ;; Ignore the t group for these kinds of matches, otherwise
           ;; it won't match just things not matched by other groups.
           ;; t-matching will happen elsewhere.
           ((eq group t)
            nil)
           (t (string-match group import-line)))))

;; word-count
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

;; eim
;; (when (<= emacs-major-version 21)
;;   (provide 'help-mode)
;;   (defalias 'locate-file 'locate-library)
;;   (defvar emacs-basic-display nil))
(autoload 'eim-use-package "eim" "The eim input method" t)
(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package "eim-wb" "eim-wb" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package "eim-py" "eim-py" "py.txt")
(setq default-input-method "eim-wb")
;; (setq eim-use-tooltip nil)
(eval-after-load "eim"
  `(when (require 'eim-extra nil 'noerror)
     (global-set-key ";" 'eim-insert-ascii)))

;; (when (require 'winsav nil t)
;;   (winsav-save-mode 1))
(autoload 'winsav-save-configuration "winsav" nil t)
(autoload 'winsav-restore-configuration "winsav" nil t)

(provide 'init-misc)
