;;;
;; Copyright (C) 2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2008-08-08


;; load-path
(let ((dir "~/.emacs.d"))
  (when (file-exists-p dir)
    (progn (add-to-list 'load-path dir)
           (let ((old-dir default-directory))
             (cd dir)
             (normal-top-level-add-subdirs-to-load-path)
             (cd old-dir)))))

;; cedet
(when (require 'cedet nil 'noerror)
  ;; (semantic-load-enable-minimum-features)
  ;; (semantic-load-enable-all-exuberent-ctags-support)
  (semantic-load-enable-code-helpers)
  ;; (semantic-load-enable-guady-code-helpers)
  ;; (semantic-load-enable-excessive-code-helpers)
  (if window-system
      (semantic-load-enable-semantic-debugging-helpers)
    (progn (global-semantic-show-unmatched-syntax-mode 1)
           (global-semantic-show-parser-state-mode 1)))
  (enable-visual-studio-bookmarks)
  (global-ede-mode 1)
  ;; (global-srecode-minor-mode 1)
  ;; (semantic-load-enable-primary-exuberent-ctags-support)

  ;; (setq semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
  ;; (setq semanticdb-project-roots (list (expand-file-name "/")))

  ;; (global-set-key [(control tab)] 'senator-complete-symbol)
  ;; (global-set-key [(control tab)] 'senator-completion-menu-popup)
  ;; (global-set-key [(control tab)] 'semantic-ia-complete-symbol)
  ;; (global-set-key [(control tab)] 'semantic-ia-complete-symbol-menu)
  (if window-system
      (define-key c-mode-base-map "\C-c " 'semantic-ia-complete-symbol-menu)
    (define-key c-mode-base-map "\C-c " 'semantic-ia-complete-symbol))
  (define-key c-mode-base-map (kbd "M-n") 'semantic-ia-complete-symbol-menu)
  (define-key c-mode-base-map [M-S-f12] 'semantic-analyze-proto-impl-toggle)
  (global-set-key [f12] 'semantic-ia-fast-jump)
  (global-set-key [C-f12] 'semantic-ia-fast-jump)
  (global-set-key [S-f12]
                  (lambda ()
                    (interactive)
                    (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
                        (error "Semantic Bookmark ring is currently empty"))
                    (let* ((ring (oref semantic-mru-bookmark-ring ring))
                           (alist (semantic-mrub-ring-to-assoc-list ring))
                           (first (cdr (car alist))))
                      (if (semantic-equivalent-tag-p (oref first tag)
                                                     (semantic-current-tag))
                          (setq first (cdr (car (cdr alist)))))
                      (semantic-mrub-switch-tags first))))

  (defconst cedet-user-include-dirs
    (list ".." "../include" "../inc" "../common" "../public"
          "../.." "../../include" "../../inc" "../../common" "../../public"))
  (defconst cedet-win32-include-dirs
    (list "C:/MinGW/include"
          "C:/MinGW/include/c++/3.4.5"
          "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
  (require 'semantic-c nil 'noerror)
  (let ((include-dirs cedet-user-include-dirs))
    (when (eq system-type 'windows-nt)
      (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
    (mapc (lambda (dir)
            (semantic-add-system-include dir 'c++-mode)
            (semantic-add-system-include dir 'c-mode))
          include-dirs))

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
  (defadvice ido-switch-buffer (after pulse-advice activate)
    "After ido-switch-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice beginning-of-buffer (after pulse-advice activate)
    "After beginning-of-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))

  (when (require 'semantic-tag-folding nil 'noerror)
    (global-semantic-tag-folding-mode 1)
    (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode)
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
    (define-key c-mode-base-map [M-f12] 'eassist-switch-h-cpp)))

;; ecb
(when (require 'ecb nil 'noerror)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-auto-compatibility-check nil)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1))

;; cscope
(require 'xcscope nil 'noerror)

;; jde
(add-hook 'java-mode-hook
          '(lambda ()
             (when (require 'jde nil 'noerror)
               (setq jde-enable-abbrev-mode t))))


(provide 'init-site)
