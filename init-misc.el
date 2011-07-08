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
(when (require 'cursor-chg nil 'noerror)
  ;; (toggle-cursor-type-when-idle 1)
  (change-cursor-mode 1))

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
                       ["Open Dired" (dired
                                      (let ((file (buffer-file-name
                                                   (tabbar-tab-value
                                                    tabbar-last-tab))))
                                        (if file
                                            (file-name-directory file)
                                          default-directory)))
                        :active (buffer-file-name
                                 (tabbar-tab-value tabbar-last-tab))]
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
                          'face (if (tabbar-selected-p tab (tabbar-current-tabset))
                                    'tabbar-selected
                                  'tabbar-unselected)
                          'pointer 'hand)
                         tabbar-separator-value)))))
     (unless (eq system-type 'windows-nt)
       (set-face-attribute 'tabbar-default nil
                           :family (face-attribute 'default :family))
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     (set-face-attribute 'tabbar-default frame
                                         :family (face-attribute 'default
                                                                 :family))))))
     (set-face-attribute 'tabbar-selected nil
                         :foreground "blue")
     (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
     (setq EmacsPortable-excluded-buffers '())))

;; window-numbering
(when (require 'window-numbering nil 'noerror)
  (window-numbering-mode 1))

;; psvn
(autoload 'svn-status "psvn" nil t)
(eval-after-load "vc-svn"
  '(require 'psvn nil 'noerror))

;; vimpulse
;; (eval-after-load "viper"
;;   '(require 'vimpulse nil 'noerror))

;; ascii
(autoload 'ascii-on        "ascii" "Turn on ASCII code display."   t)
(autoload 'ascii-off       "ascii" "Turn off ASCII code display."  t)
(autoload 'ascii-display   "ascii" "Toggle ASCII code display."    t)
(autoload 'ascii-customize "ascii" "Customize ASCII code display." t)

;; recent-jump
(when (require 'recent-jump nil 'noerror)
  (global-set-key (kbd "<M-S-left>") 'recent-jump-jump-backward)
  (global-set-key (kbd "<M-S-right>") 'recent-jump-jump-forward))

;; drag-stuff
(autoload 'drag-stuff-global-mode "drag-stuff" "Toggle Drag-Stuff mode." t)
;; (when (and (ignore-errors (require 'drag-stuff nil 'noerror))
;;            (fboundp 'drag-stuff-global-mode))
;;   (drag-stuff-global-mode t))

;; rainbow-mode
(autoload 'rainbow-mode "rainbow-mode"
  "Colorize strings that represent colors." t)

;; highlight-tail
(autoload 'highlight-tail-mode "highlight-tail"
  "Draw a \"tail\" while you're typing." t)

;; highlight-parentheses
;; (add-hook 'find-file-hooks
;;           (lambda ()
;;             (when (require 'highlight-parentheses nil 'noerror)
;;               (highlight-parentheses-mode t))))

;; highlight-symbol
(when (require 'highlight-symbol nil 'noerror)
  ;; (custom-set-faces
  ;;  '(highlight-symbol-face
  ;;    ((((class color) (background dark)) (:background "blue"))
  ;;     (((class color) (background light)) (:background "gray83")))))
  (set-face-background 'highlight-symbol-face
                       (if window-system "gray83" "blue"))
  ;; (if (eq frame-background-mode 'dark) "blue" "gray83"))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (set-face-background 'highlight-symbol-face
                                         (if window-system "gray83" "blue")
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
  (defvar disable-hl-s-modes
    '(erc-mode occur-mode w3m-mode help-mode svn-status-mode
               org-agenda-mode )
    "This buffers don't active highlight-symbol-mode.")
  (when (fboundp 'define-global-minor-mode)
    (define-global-minor-mode global-highlight-symbol-mode
      highlight-symbol-mode
      (lambda ()
        (unless (memq major-mode disable-hl-s-modes)
          (highlight-symbol-mode 1)))))
  ;; (defadvice highlight-symbol-mode-post-command
  ;;   (around gud-tooltip-advice activate)
  ;;   "Hack for gud-tooltip-mode."
  ;;   (unless (eq this-command 'gud-tooltip-mouse-motion)
  ;;     (let ((symbol (highlight-symbol-get-symbol)))
  ;;       (unless (or (equal symbol highlight-symbol)
  ;;                   (member symbol highlight-symbol-list))
  ;;         ad-do-it))))
  (defadvice highlight-symbol-mode (after disable activate)
    "Disable highlight-symbol-mode-post-command."
    (remove-hook 'post-command-hook 'highlight-symbol-mode-post-command t))
  (if (fboundp 'global-highlight-symbol-mode)
      (global-highlight-symbol-mode t)
    (add-hook 'find-file-hooks
              (lambda ()
                (unless (memq major-mode disable-hl-s-modes)
                  (highlight-symbol-mode 1)))))
  (setq highlight-symbol-idle-delay 0.5)
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

;; smart-hl
(when (> emacs-major-version 21)
  (require 'smart-hl nil 'noerror))

;; multi-term
(autoload 'multi-term "multi-term"
  "Managing multiple terminal buffers in Emacs." t)

;; dired+.el
(when window-system
  (eval-after-load "dired" '(require 'dired+ nil 'noerror)))

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
;; (add-to-list 'auto-mode-alist '("\\.bat$" . ntcmd-mode))
;; (add-to-list 'auto-mode-alist '("\\.cmd$" . ntcmd-mode))

;; csharp-mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(add-hook 'csharp-mode-hook
          (lambda ()
            (setq imenu-generic-expression cc-imenu-java-generic-expression)
            (imenu-add-menubar-index)))

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

;; sql-indent
(unless (functionp 'syntax-ppss)
  (defun syntax-ppss (&optional pos)
    (parse-partial-sexp (point-min) (or pos (point)))))
(eval-after-load "sql"
  '(require 'sql-indent nil 'noerror))

;; word-count
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

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
  (setq ac-modes
        (append ac-modes '(org-mode objc-mode jde-mode sql-mode
                                    change-log-mode text-mode
                                    tex-mode latex-mode plain-tex-mode
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
        (setq emms-repeat-playlist t)
        (emms-mode-line 1)
        (emms-mode-line-blank)
        (emms-playing-time 1)
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
