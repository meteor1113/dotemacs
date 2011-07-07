;;; -*- mode: emacs-lisp; mode: goto-address; coding: gbk; -*-
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
(let ((default-directory (expand-file-name "~/.emacs.d")))
  (when (file-exists-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))
(let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
       (default-directory (expand-file-name "site-lisp" dir)))
  (when (file-exists-p default-directory)
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(unless (fboundp 'custom-autoload)      ; for emacs-21
  (defun custom-autoload (symbol load &optional noset)
    "Mark SYMBOL as autoloaded custom variable and add dependency LOAD.
If NOSET is non-nil, don't bother autoloading LOAD when setting the variable."
    (put symbol 'custom-autoload (if noset 'noset t))
    (custom-add-load symbol load)))

;; offical cedet
;; (semantic-gcc-setup) will add mingw dirs if found gcc.
(defvar user-include-dirs
  '(".." "../include" "../inc" "../common" "../public"
    "../.." "../../include" "../../inc" "../../common" "../../public"
    "C:/MinGW/include"
    "C:/MinGW/include/c++/3.4.5"
    "C:/MinGW/include/c++/3.4.5/mingw32"
    "C:/MinGW/include/c++/3.4.5/backward"
    "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
    "C:/Program Files/Microsoft Visual Studio/VC98/Include"
    "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"
    ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
    )
  "User include dirs for c/c++ mode")
(defvar c-preprocessor-symbol-files
  '("C:/MinGW/include/c++/3.4.5/mingw32/bits/c++config.h"
    "C:/Program Files/Microsoft Visual Studio/VC98/Include/xstddef"
    ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include/yvals.h"
    ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include/crtdefs.h"
    )
  "Preprocessor symbol files for cedet")

(when (and (or (not (boundp 'semantic-mode))
               (and (boundp 'semantic-mode) (null semantic-mode)))
           (locate-library "semantic-ctxt") ; offical cedet
           (require 'cedet nil 'noerror))
  ;; (semantic-load-enable-minimum-features)
  (semantic-load-enable-code-helpers)
  ;; (semantic-load-enable-gaudy-code-helpers)
  ;; (semantic-load-enable-excessive-code-helpers)
  (global-semantic-decoration-mode 1)
  (require 'semantic-decorate-include nil 'noerror)
  (semantic-toggle-decoration-style "semantic-tag-boundary" -1)
  (if window-system
      (semantic-load-enable-semantic-debugging-helpers)
    (progn (global-semantic-show-unmatched-syntax-mode 1)
           (global-semantic-show-parser-state-mode 1)))
  (ignore-errors (semantic-load-enable-primary-exuberent-ctags-support))
  ;; (global-srecode-minor-mode 1)
  (global-ede-mode 1)
  (when (executable-find "global")
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)
    (setq ede-locate-setup-options '(ede-locate-global ede-locate-base)))
  ;; (setq semantic-c-obey-conditional-section-parsing-flag nil) ; ignore #if

  ;; (defun my-semantic-inhibit-func ()
  ;;   (cond
  ;;    ((member major-mode '(javascript-mode html-helper-mode html-mode))
  ;;     ;; to disable semantic, return non-nil.
  ;;     t)
  ;;    (t nil)))
  ;; (add-to-list 'semantic-inhibit-functions 'my-semantic-inhibit-func)

  (when (executable-find "gcc")
    (require 'semantic-c nil 'noerror)
    (and (eq system-type 'windows-nt)
         (semantic-gcc-setup)))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        user-include-dirs)
  (dolist (file c-preprocessor-symbol-files)
    (when (file-exists-p file)
      (setq semantic-lex-c-preprocessor-symbol-file
            (append semantic-lex-c-preprocessor-symbol-file (list file)))))

  ;; (global-set-key (kbd "C-c , TAB") 'senator-complete-symbol)
  ;; (global-set-key (kbd "C-c , SPC") 'senator-completion-menu-popup)
  ;; (global-set-key (kbd "C-c TAB") 'semantic-ia-complete-symbol)
  ;; (global-set-key (kbd "C-c SPC") 'semantic-ia-complete-symbol-menu)
  (if window-system
      (define-key c-mode-base-map "\C-c " 'semantic-ia-complete-symbol-menu)
    (define-key c-mode-base-map "\C-c " 'semantic-ia-complete-symbol))
  (define-key c-mode-base-map (kbd "M-n") 'semantic-ia-complete-symbol-menu)

  (defun semantic-ia-fast-jump-back ()
    (interactive)
    (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
        (error "Semantic Bookmark ring is currently empty"))
    (let* ((ring (oref semantic-mru-bookmark-ring ring))
           (alist (semantic-mrub-ring-to-assoc-list ring))
           (first (cdr (car alist))))
      ;; (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
      ;;     (setq first (cdr (car (cdr alist)))))
      (semantic-mrub-visit first)
      (ring-remove ring 0)))
  (defun semantic-ia-fast-jump-or-back (&optional back)
    (interactive "P")
    (if back
        (semantic-ia-fast-jump-back)
      (semantic-ia-fast-jump (point))))
  (defun semantic-ia-fast-jump-mouse (ev)
    "semantic-ia-fast-jump with a mouse click. EV is the mouse event."
    (interactive "e")
    (mouse-set-point ev)
    (semantic-ia-fast-jump (point)))
  (global-set-key [f12] 'semantic-ia-fast-jump-or-back)
  (global-set-key [C-f12] 'semantic-ia-fast-jump-or-back)
  (global-set-key [S-f12] 'semantic-ia-fast-jump-back)
  ;; (global-set-key (kbd "ESC ESC <f12>") 'semantic-ia-fast-jump-back)
  ;; (global-set-key [S-f12] 'pop-global-mark)
  (define-key c-mode-base-map [M-S-f12] 'semantic-analyze-proto-impl-toggle)
  (define-key c-mode-base-map [mouse-2] 'semantic-ia-fast-jump-mouse)
  (define-key c-mode-base-map [S-mouse-2] 'semantic-ia-fast-jump-back)
  (define-key senator-mode-map [mouse-2] 'semantic-ia-fast-jump-mouse)
  (define-key senator-mode-map [S-mouse-2] 'semantic-ia-fast-jump-back)
  (define-key senator-mode-map [double-mouse-2] 'semantic-ia-fast-jump-back)

  (enable-visual-studio-bookmarks)
  (defun viss-bookmark-next-buffer-or-prev (&optional prev)
    (interactive "P")
    (if prev
        (viss-bookmark-prev-buffer)
      (viss-bookmark-next-buffer)))
  (defun viss-bookmark-toggle-mouse (ev)
    (interactive "e")
    (mouse-set-point ev)
    (viss-bookmark-toggle))
  (define-key global-map [(control f2)] 'viss-bookmark-toggle)
  (define-key global-map [M-f2] 'viss-bookmark-toggle)
  (define-key global-map (kbd "ESC <f2>") 'viss-bookmark-toggle) ; putty
  (define-key global-map [(f2)] 'viss-bookmark-next-buffer-or-prev)
  (define-key global-map [(shift f2)] 'viss-bookmark-prev-buffer)
  (define-key global-map [f14] 'viss-bookmark-prev-buffer) ; linux console
  ;; (define-key global-map (kbd "ESC ESC <f2>") 'viss-bookmark-prev-buffer)
  (define-key global-map [(control shift f2)] 'viss-bookmark-clear-all-buffer)
  (global-set-key [left-margin mouse-1] 'viss-bookmark-toggle-mouse)
  (global-set-key [left-margin mouse-3] 'viss-bookmark-next-buffer)

  (pulse-toggle-integration-advice 1)   ; (if window-system 1 -1)
  (defadvice cua-exchange-point-and-mark (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p)
               (> (abs (- (point) (mark))) 400))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice switch-to-buffer (after pulse-advice activate)
    "After switch-to-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice previous-buffer (after pulse-advice activate)
    "After previous-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice next-buffer (after pulse-advice activate)
    "After next-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice ido-switch-buffer (after pulse-advice activate)
    "After ido-switch-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice beginning-of-buffer (after pulse-advice activate)
    "After beginning-of-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice viss-bookmark-next-buffer (after pulse-advice activate)
    "After viss-bookmark-next-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice viss-bookmark-prev-buffer (after pulse-advice activate)
    "After viss-bookmark-prev-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice viss-bookmark-next-buffer-or-prev (after pulse-advice activate)
    "After viss-bookmark-next-buffer-or-prev,
 pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))

  (when (and window-system
             (> emacs-major-version 21)
             (require 'semantic-tag-folding nil 'noerror))
    (global-semantic-tag-folding-mode 1)
    (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode)
    (define-key semantic-tag-folding-mode-map
      (kbd "C-c , -") 'semantic-tag-folding-fold-block)
    (define-key semantic-tag-folding-mode-map
      (kbd "C-c , +") 'semantic-tag-folding-show-block)
    (define-key semantic-tag-folding-mode-map
      (kbd "C-_") 'semantic-tag-folding-fold-all)
    (define-key semantic-tag-folding-mode-map
      (kbd "C-+") 'semantic-tag-folding-show-all))

  (when (require 'eassist nil 'noerror)
    (setq eassist-header-switches
          '(("h" . ("cpp" "cxx" "c++" "CC" "cc" "C" "c" "mm" "m"))
            ("hh" . ("cc" "CC" "cpp" "cxx" "c++" "C"))
            ("hpp" . ("cpp" "cxx" "c++" "cc" "CC" "C"))
            ("hxx" . ("cxx" "cpp" "c++" "cc" "CC" "C"))
            ("h++" . ("c++" "cpp" "cxx" "cc" "CC" "C"))
            ("H" . ("C" "CC" "cc" "cpp" "cxx" "c++" "mm" "m"))
            ("HH" . ("CC" "cc" "C" "cpp" "cxx" "c++"))
            ("cpp" . ("hpp" "hxx" "h++" "HH" "hh" "H" "h"))
            ("cxx" . ("hxx" "hpp" "h++" "HH" "hh" "H" "h"))
            ("c++" . ("h++" "hpp" "hxx" "HH" "hh" "H" "h"))
            ("CC" . ("HH" "hh" "hpp" "hxx" "h++" "H" "h"))
            ("cc" . ("hh" "HH" "hpp" "hxx" "h++" "H" "h"))
            ("C" . ("hpp" "hxx" "h++" "HH" "hh" "H" "h"))
            ("c" . ("h"))
            ("m" . ("h"))
            ("mm" . ("h"))))
    (define-key c-mode-base-map [M-f12] 'eassist-switch-h-cpp)
    (define-key c-mode-base-map (kbd "ESC <f12>") 'eassist-switch-h-cpp)))

;; ecb
(require 'ecb-autoloads nil 'noerror)
(when (fboundp 'ecb-minor-mode)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
        ecb-source-path '("/")
        ecb-layout-name 'left3
        ecb-toggle-layout-sequence '("left3"
                                     "left8"
                                     "left-analyse"
                                     "left-symboldef")
        ecb-compile-window-height 0.2
        ecb-compile-window-width 'edit-window
        ecb-compile-window-temporally-enlarge 'after-selection
        ;; ecb-enlarged-compilation-window-max-height 0.8
        ecb-tip-of-the-day nil
        ecb-auto-compatibility-check nil))
(eval-after-load "ecb"
  '(progn
     (setq ecb-compilation-buffer-names
           (append ecb-compilation-buffer-names '(("*Process List*")
                                                  ("*Proced*")
                                                  (".notes")
                                                  ("*appt-buf*")
                                                  ("*Compile-Log*")
                                                  ("*etags tmp*")
                                                  (" *svn-process*")
                                                  ("*svn-info-output*")
                                                  ("*Python Output*")
                                                  ("*Org Agenda*")
                                                  (" *EMMS Playlist*")
                                                  ("*Directory"))))
     (setq ecb-compilation-major-modes
           (append ecb-compilation-major-modes '(change-log-mode
                                                 calendar-mode
                                                 diary-mode
                                                 diary-fancy-display-mode
                                                 xgtags-select-mode
                                                 svn-status-mode
                                                 svn-info-mode
                                                 svn-status-diff-mode
                                                 svn-log-view-mode
                                                 svn-log-edit-mode
                                                 erc-mode
                                                 gud-mode)))))

;; jde
(add-hook 'java-mode-hook
          '(lambda ()
             (when (require 'jde nil 'noerror)
               (setq jde-enable-abbrev-mode t))))

;; ibus
(when (require 'ibus nil 'noerror)
  ;; (dolist (key '((f6) (f7) (f8) (shift f8) (f9) (f10) (f11) (f12)))
  ;;   (setq ibus-common-function-key-list
  ;;         (delete key ibus-common-function-key-list)))
  (setq ibus-common-function-key-list '((control " ")))
  (add-hook 'after-init-hook 'ibus-mode-on))

;; scim
(when (require 'scim-bridge nil 'noerror)
  (setq scim-common-function-key-list '((control " ")))
  (add-hook 'after-init-hook 'scim-mode-on))

(provide 'init-site)
