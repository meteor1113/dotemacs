;;; -*- mode: emacs-lisp; mode: goto-address; coding: utf-8; -*-
;; Copyright (C) 2008-2011 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2008-08-08
;; @URL http://github.com/meteor1113/dotemacs


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

;; bm
(setq bm-restore-repository-on-load t)
(when (require 'bm nil 'noerror)
  (setq-default bm-buffer-persistence t)
  (setq bm-cycle-all-buffers t)
  (setq bm-highlight-style
        (if (and window-system (> emacs-major-version 21))
            'bm-highlight-only-fringe
          'bm-highlight-only-line))
  ;; (add-hook' after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  ;; (add-hook 'after-save-hook 'bm-buffer-save)
  ;; (add-hook 'after-revert-hook 'bm-buffer-restore)
  (defun bm-next-or-previous (&optional previous)
    (interactive "P")
    (if previous
        (bm-previous)
      (bm-next)))
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key [M-f2] 'bm-toggle)
  (global-set-key (kbd "ESC <f2>") 'bm-toggle) ; putty
  (global-set-key (kbd "<f2>")   'bm-next-or-previous)
  (global-set-key (kbd "<S-f2>") 'bm-previous)
  (global-set-key [f14] 'bm-previous)   ; S-f2
  ;; (global-set-key (kbd "ESC ESC <f2>") 'bm-previous)
  (global-set-key (kbd "<C-S-f2>") 'bm-remove-all-current-buffer)
  ;; (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
  ;; (global-set-key (kbd "<left-fringe> <mouse-2>") 'bm-toggle-mouse)
  ;; (global-set-key (kbd "<left-fringe> <mouse-3>") 'bm-next-mouse)
  ;; (global-set-key [left-margin mouse-1] 'bm-toggle-mouse)
  (global-set-key [left-margin mouse-1] 'bm-toggle-mouse)
  (global-set-key [left-margin mouse-3] 'bm-next-mouse)
  (defadvice bm-next (after pulse-advice activate)
    "After bm-next, pulse the line the cursor lands on."
    (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
               (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice bm-previous (after pulse-advice activate)
    "After bm-previous, pulse the line the cursor lands on."
    (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
               (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice bm-next-or-previous (after pulse-advice activate)
    "After bm-next-or-previous, pulse the line the cursor lands on."
    (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
               (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice bm-next-mouse (after pulse-advice activate)
    "After bm-next-mouse, pulse the line the cursor lands on."
    (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
               (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice bm-previous-mouse (after pulse-advice activate)
    "After bm-previous-mouse, pulse the line the cursor lands on."
    (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
               (interactive-p))
      (pulse-momentary-highlight-one-line (point)))))


;; cursor-chg
(autoload 'change-cursor-mode "cursor-chg" nil t)
;; (when (require 'cursor-chg nil 'noerror)
;;   ;; (toggle-cursor-type-when-idle 1)
;;   (change-cursor-mode 1))

;; cal-china-x
(eval-after-load "calendar"
  '(when (require 'cal-china-x nil 'noerror)
     (setq cal-china-x-priority1-holidays
           (append cal-china-x-chinese-holidays
                   '((holiday-fixed 2 14 "情人节")
                     (holiday-fixed 3 8 "妇女节")
                     (holiday-fixed 3 12 "植树节")
                     (holiday-fixed 5 4 "青年节")
                     (holiday-fixed 6 1 "儿童节")
                     (holiday-fixed 9 10 "教师节")
                     (holiday-lunar 1 15 "元宵节(正月十五)" 0)
                     (holiday-lunar 7 7 "七夕节")
                     (holiday-lunar 9 9 "重阳节(九月初九)"))))
     (setq cal-china-x-priority2-holidays
           '((holiday-chinese 6 23 "李浩斌生日")
             (holiday-fixed 10 16 "李可生日(1981)")
             (holiday-chinese 8 15 "爸妈生日(1955)")
             (holiday-fixed 4 9 "戚生日(1983)")
             (holiday-fixed 10 6 "奇奇阳历生日(2010)")
             (holiday-chinese 8 29 "奇奇阴历生日(2010)")
             (holiday-fixed 12 8 "刘阳历生日(1981)")
             (holiday-chinese 11 13 "刘阴历生日(1981)")))
     (setq calendar-holidays
           (append calendar-holidays
                   cal-china-x-priority1-holidays
                   cal-china-x-priority2-holidays))))

;; calfw
(autoload 'cfw:open-org-calendar "calfw-org" nil t)
(autoload 'cfw:open-diary-calendar "calfw-cal" nil t)
;; (autoload 'cfw:open-calendar-buffer "calfw" nil t)
;; (eval-after-load "calfw"
;;   '(when (require 'calfw-org nil 'noerror)
;;      (cfw:install-org-schedules)))

;; cn-weather
(setq cn-weather-city "大连")
(autoload 'display-cn-weather-mode "cn-weather"
  "Display weather information in the mode line." t)
(autoload 'cn-weather "cn-weather"
  "Print Now today's and realtime weather in the echo area." t)
(autoload 'cn-weather-forecast "cn-weather"
  "Print future two days' weather info in minibuffer." t)

;; smart-compile
(autoload 'smart-compile "smart-compile" nil t)
(global-set-key [C-f7] 'smart-compile)

;; tabbar
(when (require 'tabbar nil 'noerror)
  (tabbar-mode t)
  (define-key tabbar-mode-map [C-prior] 'tabbar-backward)
  (define-key tabbar-mode-map [C-next] 'tabbar-forward)
  (defadvice tabbar-buffer-tab-label (after modified-flag activate)
    (setq ad-return-value
          (if (and (or (not (featurep 'tabbar-ruler))
                       (not window-system))
                   (buffer-modified-p (tabbar-tab-value tab)))
                   ;; (buffer-file-name (tabbar-tab-value tab))
              (concat ad-return-value "*")
            ad-return-value)))
  (defun update-tabbar-modified-state ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))
  (defadvice undo (after update-tabbar-tab-label activate)
    (update-tabbar-modified-state))
  (add-hook 'first-change-hook 'update-tabbar-modified-state)
  (add-hook 'after-save-hook 'update-tabbar-modified-state))
(eval-after-load "tabbar"
  '(when (require 'tabbar-ruler nil 'noerror)
     (defadvice tabbar-popup-menu (after add-menu-item activate)
       "Add customize menu item to tabbar popup menu."
       (setq ad-return-value
             (append ad-return-value
                     '("--"
                       ["Copy Buffer Name" (kill-new
                                            (buffer-name
                                             (tabbar-tab-value
                                              tabbar-last-tab)))]
                       ["Copy File Path" (kill-new
                                          (buffer-file-name
                                           (tabbar-tab-value
                                            tabbar-last-tab)))
                        :active (buffer-file-name
                                 (tabbar-tab-value tabbar-last-tab))]
                       ["Open Dired" dired-jump
                        :active (fboundp 'dired-jump)]
                       ;; ["Open Dired" (dired
                       ;;                (let ((file (buffer-file-name
                       ;;                             (tabbar-tab-value
                       ;;                              tabbar-last-tab))))
                       ;;                  (if file
                       ;;                      (file-name-directory file)
                       ;;                    default-directory)))
                       ;;  :active (buffer-file-name
                       ;;           (tabbar-tab-value tabbar-last-tab))]
                       "--"
                       ["Undo Close Tab" undo-kill-buffer
                        :active (fboundp 'undo-kill-buffer)]))))
     (defadvice tabbar-line-tab (around window-or-terminal activate)
       "Fix tabbar-ruler in window-system and terminal"
       (if window-system
           ad-do-it
         (setq ad-return-value
               (let ((tab (ad-get-arg 0))
                     (tabbar-separator-value "|"))
                 (concat (propertize
                          (if tabbar-tab-label-function
                              (funcall tabbar-tab-label-function tab)
                            tab)
                          'tabbar-tab tab
                          'local-map (tabbar-make-tab-keymap tab)
                          'help-echo 'tabbar-help-on-tab
                          'mouse-face 'tabbar-highlight
                          'face (if (tabbar-selected-p tab
                                                       (tabbar-current-tabset))
                                    'tabbar-selected
                                  'tabbar-unselected)
                          'pointer 'hand)
                         tabbar-separator-value)))))
     ;; (unless (eq system-type 'windows-nt)
     (set-face-attribute 'tabbar-default nil
                         :family (face-attribute 'default :family))
     (add-hook 'after-make-frame-functions
               (lambda (frame)
                 (with-selected-frame frame
                   (set-face-attribute 'tabbar-default frame
                                       :family (face-attribute 'default
                                                               :family)))));; )
     (set-face-attribute 'tabbar-selected nil
                         :foreground "blue")
     (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
     (setq EmacsPortable-excluded-buffers '())))

;; window-numbering
(when (require 'window-numbering nil 'noerror)
  (window-numbering-mode 1))

;; win-switch
;; (autoload 'win-switch-dispatch "win-switch" nil t)
;; (global-set-key "\C-xo" 'win-switch-dispatch)
(global-set-key "\C-xo"
                (lambda ()
                  (interactive)
                  (if (require 'win-switch nil 'noerror)
                      (win-switch-dispatch)
                    (other-window 1))))

;; psvn
(autoload 'svn-status "psvn" nil t)
(eval-after-load "vc-svn"
  '(require 'psvn nil 'noerror))

;; magit
(autoload 'magit-status "magit" nil t)

;; fci-mode
(autoload 'fci-mode "fill-column-indicator" nil t)

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

;; drag-stuff
(autoload 'drag-stuff-global-mode "drag-stuff" "Toggle Drag-Stuff mode." t)
;; (when (and (ignore-errors (require 'drag-stuff nil 'noerror))
;;            (fboundp 'drag-stuff-global-mode))
;;   (drag-stuff-global-mode t))

;; rainbow-mode
(autoload 'rainbow-mode "rainbow-mode"
  "Colorize strings that represent colors." t)

;; color-theme
(require 'color-theme-autoloads nil 'noerror)

;; nyan-mode
(autoload 'nyan-mode "nyan-mode" nil t)
(autoload 'nyan-start-animation "nyan-mode" nil t)
(autoload 'nyan-stop-animation "nyan-mode" nil t)
;; (setq nyan-wavy-trail t)
(setq nyan-bar-length 8)
(defadvice nyan-mode (after animation activate)
  (if nyan-mode
        (nyan-start-animation)
    (nyan-stop-animation)))
;; (ignore-errors (and window-system (nyan-mode t)))

;; volatile-highlights
(autoload 'volatile-highlights-mode "volatile-highlights" nil t)
(ignore-errors (volatile-highlights-mode t))

;; highlight-tail
(autoload 'highlight-tail-mode "highlight-tail"
  "Draw a \"tail\" while you're typing." t)

;; highlight-parentheses
(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
;; (add-hook 'find-file-hooks
;;           (lambda ()
;;             (when (require 'highlight-parentheses nil 'noerror)
;;               (highlight-parentheses-mode t))))

;; highlight-symbol
(when (require 'highlight-symbol nil 'noerror)
  ;; (custom-set-faces
  ;;  '(highlight-symbol-face
  ;;    ((((class color) (background dark)) (:background "magenta"))
  ;;     (((class color) (background light)) (:background "gray83")))))
  (set-face-background 'highlight-symbol-face
                       (if window-system "gray83" "magenta"))
  ;; (if (eq frame-background-mode 'dark) "magenta" "gray83"))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (set-face-background 'highlight-symbol-face
                                         (if window-system "gray83" "magenta")
                                         frame)))))
  (defun highlight-symbol-temp-highlight () ; Hack for emacs-21
    "Highlight the current symbol until a command is executed."
    (when highlight-symbol-mode
      (let ((symbol (highlight-symbol-get-symbol)))
        (unless (or (equal symbol highlight-symbol)
                    (member symbol highlight-symbol-list))
          (highlight-symbol-mode-remove-temp)
          (when symbol
            (setq highlight-symbol symbol)
            (if (< emacs-major-version 22)
                (let ((color `((background-color . ,"grey")
                               (foreground-color . "black"))))
                  (hi-lock-set-pattern `(,symbol (0 (quote ,color) t))))
              (hi-lock-set-pattern symbol 'highlight-symbol-face)))))))
  (defadvice highlight-symbol-mode (after disable activate)
    "Disable highlight-symbol-mode-post-command."
    (remove-hook 'post-command-hook 'highlight-symbol-mode-post-command t))
  (setq highlight-symbol-idle-delay 0.5)
  (mapc (lambda (hook)
          (add-hook hook (lambda () (highlight-symbol-mode 1))))
        '(c-mode-common-hook
          fortran-mode-hook f90-mode-hook ada-mode-hook
          python-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook
          emacs-lisp-mode-hook sh-mode-hook js-mode-hook js2-mode-hook
          nxml-mode-hook sgml-mode-hook sql-mode-hook))
  ;; (defvar disable-hl-s-modes
  ;;   '(erc-mode occur-mode w3m-mode help-mode svn-status-mode
  ;;              org-agenda-mode cfw:calendar-mode)
  ;;   "This modes don't active highlight-symbol-mode.")
  ;; (defvar hl-s-modes
  ;;   '(c-mode cc-mode c++-mode java-mode jde-mode objc-mode csharp-mode
  ;;            python-mode ruby-mode perl-mode cperl-mode php-mode
  ;;            fortran-mode f90-mode ada-mode xml-mode nxml-mode html-mode
  ;;            sql-mode emacs-lisp-mode lisp-interaction-mode
  ;;            sh-mode javascript-mode js-mode js2-mode)
  ;;   "This modes active highlight-symbol-mode.")
  ;; (when (fboundp 'define-global-minor-mode)
  ;;   (define-global-minor-mode global-highlight-symbol-mode
  ;;     highlight-symbol-mode
  ;;     (lambda ()
  ;;       (when (memq major-mode hl-s-modes)
  ;;         (highlight-symbol-mode 1)))))
  ;; (if (fboundp 'global-highlight-symbol-mode)
  ;;     (global-highlight-symbol-mode t)
  ;;   (add-hook 'find-file-hooks
  ;;             (lambda ()
  ;;               (when (memq major-mode hl-s-modes)
  ;;                 (highlight-symbol-mode 1)))))
  (defun highlight-symbol-next-or-prev (&optional prev)
    (interactive "P")
    (if prev
        (highlight-symbol-prev)
      (highlight-symbol-next)))
  (defadvice highlight-symbol-next (after pulse-advice activate)
    "After highlight-symbol-next, pulse the line the cursor lands on."
    (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
               (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice highlight-symbol-prev (after pulse-advice activate)
    "After highlight-symbol-prev, pulse the line the cursor lands on."
    (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
               (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice highlight-symbol-next-or-prev (after pulse-advice activate)
    "After highlight-symbol-next-or-prev, pulse the line the cursor lands on."
    (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
               (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (global-set-key [(meta f3)] 'highlight-symbol-at-point)
  (global-set-key (kbd "ESC <f3>") 'highlight-symbol-at-point) ; putty
  (global-set-key [f3] 'highlight-symbol-next-or-prev)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [f15] 'highlight-symbol-prev) ; S-f3
  ;; (global-set-key (kbd "ESC ESC <f3>") 'highlight-symbol-prev)
  (global-set-key [(control f3)] 'highlight-symbol-query-replace))

;; mark-multiple
;; (require 'inline-string-rectangle)
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
;; (require 'mark-more-like-this)
(autoload 'mark-previous-like-this "mark-more-like-this" nil t)
(autoload 'mark-next-like-this "mark-more-like-this" nil t)
(autoload 'mark-more-like-this "mark-more-like-this" nil t)
(autoload 'mark-all-like-this "mark-more-like-this" nil t)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; smart-hl
(when (> emacs-major-version 21)
  (require 'smart-hl nil 'noerror))

;; multi-term
(autoload 'multi-term "multi-term"
  "Managing multiple terminal buffers in Emacs." t)

;; dired+.el
(when window-system
  (eval-after-load "dired"
    '(when (require 'dired+ nil 'noerror)
       (define-key dired-mode-map [mouse-2] 'diredp-mouse-find-file)
       (toggle-dired-find-file-reuse-dir 1))))

;; htmlize
(autoload 'htmlize-buffer "htmlize" nil t)

;; browse-kill-ring
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

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

;; cscope
(when (executable-find "cscope")
  (when (require 'xcscope nil 'noerror)
    (define-key cscope-list-entry-keymap [mouse-1]
      'cscope-mouse-select-entry-other-window)))

;; xgtags
(when (executable-find "global")
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (require 'xgtags nil 'noerror)
                (xgtags-mode 1)))))

;; csv-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; ntcmd
;; (autoload 'ntcmd-mode "ntcmd" "Major mode for editing CMD scripts." t)
;; (add-to-list 'auto-mode-alist '("\\.[bB][aA][tT]\\'" . ntcmd-mode))
;; (add-to-list 'auto-mode-alist '("\\.[cC][mM][dD]\\'" . ntcmd-mode))

;; csharp-mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(add-hook 'csharp-mode-hook
          (lambda ()
            (c-set-style "c#")
            (setq imenu-generic-expression cc-imenu-java-generic-expression)
            (imenu-add-menubar-index)))

;; php-mode
(autoload 'php-mode "php-mode" nil t)
(add-to-list 'auto-mode-alist
             '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

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

;; sql-indent
(unless (functionp 'syntax-ppss)
  (defun syntax-ppss (&optional pos)
    (parse-partial-sexp (point-min) (or pos (point)))))
(eval-after-load "sql"
  '(require 'sql-indent nil 'noerror))

;; plsql
(autoload 'plsql-mode "plsql" nil t)
(setq auto-mode-alist
      (append
       '(("\\.\\(p\\(?:k[bg]\\|ls\\)\\|[sS][qQ][lL]\\|[pP][rR][cC]\\)\\'"
          . plsql-mode))
       auto-mode-alist))

;; sqlplus
(autoload 'sqlplus "sqlplus" nil t)
(eval-after-load "sqlplus"
  '(progn
     (define-key plsql-mode-map [C-down-mouse-1] nil)
     (define-key plsql-mode-map [C-mouse-1] nil)
     (define-key plsql-mode-map [down-mouse-2] 'sqlplus-mouse-select-identifier)
     (define-key plsql-mode-map [mouse-2] 'sqlplus-file-get-source-mouse)
     (define-key sqlplus-mode-map [C-down-mouse-1] nil)
     (define-key sqlplus-mode-map [C-mouse-1] nil)
     (define-key sqlplus-mode-map [down-mouse-2]
       'sqlplus-mouse-select-identifier)
     (define-key sqlplus-mode-map [mouse-2] 'sqlplus-file-get-source-mouse)))
(if (executable-find "sqlplus")
    (require 'sqlplus nil t))
(setq sqlplus-session-cache-dir "~/.emacs.d/sqlplus")
(add-hook 'sqlplus-mode-hook
          (lambda ()
            ;; (setq minor-mode-overriding-map-alist
            ;;       (list (cons 'cua-mode
            ;;                   (let ((map (make-sparse-keymap)))
            ;;                     (define-key map [C-return]
            ;;                       'sqlplus-send-current)
            ;;                     map))))
            ;; (setq overriding-local-map
            ;;       (let ((map (make-sparse-keymap)))
            ;;         (define-key map [C-return] 'sqlplus-send-current)
            ;;         map))
            (when (string-match "chinese" (or (getenv "NLS_LANG") ""))
              (set-process-coding-system
               (get-process (sqlplus-get-process-name sqlplus-connect-string))
               'gbk
               'gbk))))

;; word-count
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

;; undo-tree
(autoload 'undo-tree-mode "undo-tree" nil t)
(autoload 'global-undo-tree-mode "undo-tree" nil t)

;; yasnippet
(when (and (> emacs-major-version 21)
           (or (require 'yasnippet-bundle nil 'noerror)
               (require 'yasnippet nil 'noerror)))
  (setq yas/wrap-around-region t)
  (unless (featurep 'yasnippet-bundle)
    (yas/initialize))
  (let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
         (snippets-dir (expand-file-name "etc/snippets" dir)))
    (when (file-exists-p snippets-dir)
      (yas/load-directory snippets-dir)))
  (when (require 'org nil 'noerror)
    (add-hook 'org-mode-hook
              (let ((original-command (lookup-key org-mode-map [tab])))
                `(lambda ()
                   (setq yas/fallback-behavior
                         '(apply ,original-command))
                   (local-set-key [tab] 'yas/expand))))))

;; auto-complete
(when (and (> emacs-major-version 21)
           (require 'auto-complete nil 'noerror)
           (require 'auto-complete-config nil 'noerror))
  (setq ac-use-comphist nil)
  (define-key ac-completing-map [return] 'ac-complete)
  (setq ac-modes
        (append ac-modes '(org-mode objc-mode csharp-mode jde-mode sql-mode
                                    plsql-mode sqlplus-mode
                                    inferior-emacs-lisp-mode eshell-mode
                                    change-log-mode text-mode
                                    xml-mode nxml-mode html-mode
                                    tex-mode latex-mode plain-tex-mode
                                    conf-unix-mode conf-windows-mode
                                    conf-colon-mode conf-space-mode
                                    conf-javaprop-mode
                                    inetd-conf-generic-mode
                                    etc-services-generic-mode
                                    etc-passwd-generic-mode
                                    etc-fstab-generic-mode
                                    etc-sudoers-generic-mode
                                    resolve-conf-generic-mode
                                    etc-modules-conf-generic-mode
                                    apache-conf-generic-mode
                                    apache-log-generic-mode
                                    samba-generic-mode reg-generic-mode
                                    fvwm-generic-mode ini-generic-mode
                                    x-resource-generic-mode
                                    hosts-generic-mode inf-generic-mode
                                    bat-generic-mode javascript-generic-mode
                                    vrml-generic-mode java-manifest-generic-mode
                                    java-properties-generic-mode
                                    alias-generic-mode rc-generic-mode
                                    makefile-gmake-mode makefile-bsdmake-mode
                                    autoconf-mode makefile-automake-mode)))
  (let ((ac-path (locate-library "auto-complete")))
    (unless (null ac-path)
      (let ((dict-dir (expand-file-name "dict" (file-name-directory ac-path))))
        (add-to-list 'ac-dictionary-directories dict-dir))))
  (defadvice ac-update-word-index-1 (around exclude-hidden-buffer activate)
    "Exclude hidden buffer, hack for eim."
    (unless (string= (substring (buffer-name) 0 1) " ")
      ad-do-it))
  (ac-config-default)
  ;; (global-set-key (kbd "M-n") 'auto-complete)
  (setq ac-disable-faces nil)
  (add-hook 'ielm-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'eshell-mode-hook 'ac-emacs-lisp-mode-setup)
  (defun ac-semantic-setup ()
    ;; (setq ac-sources (append '(ac-source-semantic) ac-sources))
    (local-set-key (kbd "M-n") 'ac-complete-semantic))
  (add-hook 'c-mode-common-hook 'ac-semantic-setup)
  (when (require 'auto-complete-clang nil 'noerror)
    (setq ac-clang-flags
          '("-I.." "-I../include" "-I../inc" "-I../common" "-I../public"
            "-I../.." "-I../../include" "-I../../inc" "-I../../common"
            "-I../../public"))
    (when (fboundp 'semantic-gcc-get-include-paths)
      (let ((dirs (semantic-gcc-get-include-paths "c++")))
        (dolist (dir dirs)
          (add-to-list 'ac-clang-flags (concat "-I" dir)))))
    (defun ac-clang-setup ()
      (local-set-key (kbd "M-p") 'ac-complete-clang))
    (add-hook 'c-mode-common-hook 'ac-clang-setup))
  (setq ac-source-ropemacs              ; Redefine ac-source-ropemacs
        '((candidates . (lambda ()
                          (setq ac-ropemacs-completions-cache
                                (mapcar
                                 (lambda (completion)
                                   (concat ac-prefix completion))
                                 (ignore-errors
                                   (rope-completions))))))
          (prefix . c-dot)
          (requires . 0)))
  (defun ac-complete-ropemacs ()
    (interactive)
    (auto-complete '(ac-source-ropemacs)))
  (defun ac-ropemacs-setup ()
    (when (locate-library "pymacs")
      (ac-ropemacs-require)
      ;; (setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
      (local-set-key (kbd "M-n") 'ac-complete-ropemacs)))
  (ac-ropemacs-initialize)
  (defun ac-yasnippet-setup ()
    (add-to-list 'ac-sources 'ac-source-yasnippet))
  (add-hook 'auto-complete-mode-hook 'ac-yasnippet-setup))

;; company
(setq company--disabled-backends '(company-pysmell))
(autoload 'company-mode "company" nil t)
(autoload 'global-company-mode "company" nil t)
(eval-after-load "company"
  '(progn
     (setq company-idle-delay nil)
     ;; (setq company-idle-delay t
     ;;       company-minimum-prefix-length 1
     ;;       company-begin-commands '(self-insert-command c-electric-lt-gt))
     (define-key company-mode-map (kbd "M-n") 'company-select-next)
     (define-key company-mode-map (kbd "M-p") 'company-select-previous)))

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

;; emms
(autoload 'emms "emms-playlist-mode" nil t)
(eval-after-load "emms-playlist-mode"
  '(progn
     (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
     (define-key emms-playlist-mode-map [double-mouse-1]
       'emms-playlist-mode-play-current-track)))
(defun init-emms ()
  "Initial emms."
  (or (featurep 'emms-setup)
      (when (and (require 'emms-setup nil t)
                 (require 'emms-mode-line nil t)
                 (require 'emms-playing-time nil t))
        (emms-standard)
        (emms-default-players)
        (progn
          (define-emms-simple-player mpg123 '(file url)
            (emms-player-simple-regexp "mp3" "mp2") "mpg123")
          (add-to-list 'emms-player-list 'emms-player-mpg123 'append))
        (setq emms-repeat-playlist t)
        (setq emms-mode-line-format "[%s]")
        (defun emms-mode-line-playlist-current-nondirectory ()
          "Format the currently playing song."
          (format emms-mode-line-format
                  (file-name-nondirectory
                   (emms-track-description
                    (emms-playlist-current-selected-track)))))
        (setq emms-mode-line-mode-line-function
              'emms-mode-line-playlist-current-nondirectory)
        (setq emms-mode-line-titlebar-function
              'emms-mode-line-playlist-current-nondirectory)
        (emms-mode-line 1)
        (emms-mode-line-blank)
        ;; (setq emms-playing-time-style 'bar)
        ;; (emms-playing-time 1)
        t)))
(defadvice emms (before init-emms activate)
  "Initial emms first."
  (init-emms))
(defun emms-dir-tree ()
  "Query for a directory tree, or switch to the current emms-playlist buffer."
  (interactive)
  (if (init-emms)
      (if (or (null emms-playlist-buffer)
              (not (buffer-live-p emms-playlist-buffer)))
          (call-interactively 'emms-play-directory-tree)
        (emms-playlist-mode-go))
    (message "Initial emms failed.")))
(when (and window-system
           (require 'emms-history nil t)
           (file-exists-p emms-history-file)
           (init-emms))
  (setq emms-history-start-playing t)
  (emms-history-load))
(when (featurep 'emms-history)
  (defadvice emms-history-save (around delete-empty-history activate)
    "If have not emms playlist, delete emms-history-file."
    (let (have-playlist)
      (dolist (buf (emms-playlist-buffer-list))
        (when (> (buffer-size buf) 0)
          (setq have-playlist t)))
      (if (not have-playlist)
          (when (file-exists-p emms-history-file)
            (delete-file emms-history-file))
        (ignore-errors (make-directory (file-name-directory emms-history-file)))
        ad-do-it))))

;; anything
(autoload 'anything "anything" nil t)
(setq anything-command-map-prefix-key "")
;; (eval-after-load "anything"
;;   '(require 'anything-config nil 'noerror))

;; (when (require 'winsav nil t)
;;   (winsav-save-mode 1))
(autoload 'winsav-save-configuration "winsav" nil t)
(autoload 'winsav-restore-configuration "winsav" nil t)

(provide 'init-misc)
