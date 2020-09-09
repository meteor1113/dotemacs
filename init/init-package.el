;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @URL http://git.oschina.net/meteor1113/dotemacs

;;; Commentary:

;;; Code:

(let ((root-dir (if (boundp 'dotemacs-root-dir)
                    dotemacs-root-dir
                  (file-name-directory
                   (directory-file-name
                    (file-name-directory
                     (or load-file-name buffer-file-name)))))))
  (setq package-user-dir (expand-file-name "elpa" root-dir)))

(setq package--init-file-ensured t)     ; Prevent package--ensure-init-file

(eval-after-load "package"
  '(progn
     ;; (add-to-list 'package-archives
     ;;              '("marmalade" . "https://marmalade-repo.org/packages/"))
     (add-to-list 'package-archives
                  '("melpa-stable" . "http://stable.melpa.org/packages/"))
     (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))))

(package-initialize)

(defun compile-all-packages ()
  "Byte-compile all installed packages."
  (interactive)
  (dolist (elt package-alist)
    (package--compile (car (cdr elt)))))

;; ace-jump-mode
;; (autoload 'ace-jump-mode "ace-jump-mode" nil t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(set-face-background 'ace-jump-face-foreground "yellow"))

;; aggressive-indent
;; (add-hook 'after-init-hook
;;           '(lambda ()
;;              (ignore-errors (global-aggressive-indent-mode 1))))

;; anzu
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => "))
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (global-anzu-mode +1))))
;; (run-with-idle-timer 3 nil #'global-anzu-mode +1)
(eval-after-load "anzu"
  '(progn
     (global-set-key (kbd "M-%") 'anzu-query-replace)
     (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)))

;; ascii

;; color-theme

;; drag-stuff
;; (autoload 'drag-stuff-global-mode "drag-stuff" "Toggle Drag-Stuff mode." t)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (drag-stuff-global-mode t))))
;; (run-with-idle-timer 3 nil #'drag-stuff-global-mode t)

;; editorconfig
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (editorconfig-mode 1))))

;; mark-multiple
;; (require 'inline-string-rectangle)
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
;; (require 'mark-more-like-this)
;; (autoload 'mark-previous-like-this "mark-more-like-this" nil t)
;; (autoload 'mark-next-like-this "mark-more-like-this" nil t)
;; (autoload 'mark-more-like-this "mark-more-like-this" nil t)
;; (autoload 'mark-all-like-this "mark-more-like-this" nil t)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; minimap
(setq minimap-window-location 'right)

;; multi-term
;; (autoload 'multi-term "multi-term" nil t)

;; nyan-mode
;; (autoload 'nyan-mode "nyan-mode" nil t)
(autoload 'nyan-start-animation "nyan-mode" nil t)
(autoload 'nyan-stop-animation "nyan-mode" nil t)
;; (setq nyan-wavy-trail t)
(setq nyan-bar-length 8)
(defadvice nyan-mode (after animation activate)
  (if nyan-mode
      (nyan-start-animation)
    (nyan-stop-animation)))
;; (ignore-errors (and window-system (nyan-mode t)))

;; projectile
;; (autoload 'projectile-mode "projectile" nil t)
;; (autoload 'projectile-global-mode "projectile" nil t)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (projectile-global-mode 1))))
;; (run-with-idle-timer 3 nil #'projectile-global-mode 1)

;; rainbow-mode
;; (autoload 'rainbow-mode "rainbow-mode" nil t)

;; smart-compile
(global-set-key [C-f7] 'smart-compile)

;; smex
(add-hook 'after-init-hook
          ;; (run-with-idle-timer 2 nil
          #'(lambda ()
              (when (ignore-errors (smex-initialize))
                (global-set-key (kbd "M-x") 'smex)
                (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                ;; This is your old M-x.
                (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))))

;; symon
;; (add-hook 'after-init-hook
;;           '(lambda ()
;;              (ignore-errors (symon-mode 1))))

;; undo-tree
;; (autoload 'undo-tree-mode "undo-tree" nil t)
;; (autoload 'global-undo-tree-mode "undo-tree" nil t)

;; vlf
;; (autoload 'vlf "vlf" "View a Large File in Emacs." t)

;; window-numbering
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (window-numbering-mode 1))))
;; (run-with-idle-timer 3 nil #'window-numbering-mode 1)

;; win-switch
;; (autoload 'win-switch-dispatch "win-switch" nil t)
;; (global-set-key "\C-xo" 'win-switch-dispatch)
(global-set-key "\C-xo"
                (lambda ()
                  (interactive)
                  (if (require 'win-switch nil 'noerror)
                      (win-switch-dispatch)
                    (other-window 1))))

(provide 'init-package)

;;; init-package.el ends here
