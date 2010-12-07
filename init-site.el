;;; -*- coding: gbk -*-
;; Copyright (C) 2008, 2009, 2010 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2008-08-08
;; @URL http://github.com/meteor1113/dotemacs


;; load-path
(let ((default-directory (expand-file-name "~/.emacs.d")))
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
(defvar user-include-dirs
  '(".." "../include" "../inc" "../common" "../public"
    "../.." "../../include" "../../inc" "../../common" "../../public"
    ;; "C:/MinGW/include"
    ;; "C:/MinGW/include/c++/3.4.5"
    ;; "C:/MinGW/include/c++/3.4.5/mingw32"
    ;; "C:/MinGW/include/c++/3.4.5/backward"
    ;; "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
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
  (when (executable-find "global")
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))
  (global-ede-mode 1)
  ;; (global-srecode-minor-mode 1)

  ;; (setq semantic-c-obey-conditional-section-parsing-flag nil) ; ignore #if
  (setq ede-locate-setup-options
        '(ede-locate-global
          ede-locate-base))

  (enable-visual-studio-bookmarks)
  (define-key global-map [(control f2)] 'viss-bookmark-toggle)
  (define-key global-map [M-f2] 'viss-bookmark-toggle)
  (define-key global-map (kbd "ESC <f2>") 'viss-bookmark-toggle) ; putty
  (define-key global-map [(f2)] 'viss-bookmark-next-buffer)
  (define-key global-map [(shift f2)] 'viss-bookmark-prev-buffer)
  (define-key global-map [(control shift f2)] 'viss-bookmark-clear-all-buffer)

  (require 'semantic-c nil 'noerror)
  (when (executable-find "gcc")
    (semantic-gcc-setup))
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
  (global-set-key [f12] 'semantic-ia-fast-jump-or-back)
  (global-set-key [C-f12] 'semantic-ia-fast-jump-or-back)
  (global-set-key [S-f12] 'semantic-ia-fast-jump-back)
  ;; (global-set-key [S-f12] 'pop-global-mark)
  (define-key c-mode-base-map [M-S-f12] 'semantic-analyze-proto-impl-toggle)

  (pulse-toggle-integration-advice (if window-system 1 -1))
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
;; (when (require 'ecb nil 'noerror)
(autoload 'ecb-activate "ecb" nil t)
(setq ecb-tip-of-the-day nil)
(setq ecb-auto-compatibility-check nil)
(setq ecb-source-path (quote ("/")))
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)

;; jde
(add-hook 'java-mode-hook
          '(lambda ()
             (when (require 'jde nil 'noerror)
               (setq jde-enable-abbrev-mode t))))

;; scim
(when (require 'scim-bridge nil 'noerror)
  (add-hook 'after-init-hook 'scim-mode-on))

;; ibus
(when (require 'ibus nil 'noerror)
  (add-hook 'after-init-hook 'ibus-mode-on))


(provide 'init-site)
