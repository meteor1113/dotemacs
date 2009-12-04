;;;
;; Copyright (C) 2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2008-08-08


;;; load-path setting
(let ((dir "~/.emacs.d"))
  (when (file-exists-p dir)
    (progn (add-to-list 'load-path dir)
           (let ((old-dir default-directory))
             (cd dir)
             (normal-top-level-add-subdirs-to-load-path)
             (cd old-dir)))))


;;; cedet setting
(when (require 'cedet nil t)
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

  ;; (setq semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
  ;; (setq semanticdb-project-roots (list (expand-file-name "/")))

  ;; (global-set-key [(control tab)] 'senator-complete-symbol)
  ;; (global-set-key [(control tab)] 'senator-completion-menu-popup)
  ;; (global-set-key [(control tab)] 'semantic-ia-complete-symbol)
  ;; (global-set-key [(control tab)] 'semantic-ia-complete-symbol-menu)
  (global-set-key [f12] 'semantic-ia-fast-jump)
  (global-set-key [C-f12] 'semantic-ia-fast-jump)
  (global-set-key [S-f12] 'semantic-mrub-switch-tags)

  (defconst cedet-user-include-dirs
    (list "../" "../include/" "../common/"
          "../../" "../../include" "../../common"))
  (defconst cedet-win32-include-dirs
    (list "C:/MinGW/include"
          "C:/MinGW/include/c++/3.4.5"
          "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
  (require 'semantic-c nil t)
  (let ((include-dirs cedet-user-include-dirs))
    (when (eq system-type 'windows-nt)
      (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
    (mapc (lambda (dir)
            (semantic-add-system-include dir 'c++-mode)
            (semantic-add-system-include dir 'c-mode))
          include-dirs))

  (when (require 'eassist nil t)
    (global-set-key [M-f12] 'eassist-switch-h-cpp)))


;;; ecb setting
(when (require 'ecb nil t)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-auto-compatibility-check nil)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1))


;;; cscope setting
(require 'xcscope nil t)


;;; jde setting
(add-hook 'java-mode-hook
          '(lambda ()
             (when (and (not (featurep 'jde))
                        (require 'jde nil t))
               (setq jde-enable-abbrev-mode t))))


(provide 'init-site)
