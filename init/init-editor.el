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

;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default comment-column 40)        ; [C-x ;] (set-comment-column)
(setq ring-bell-function 'ignore)
(setq-default major-mode 'text-mode)
;; (setq enable-recursive-minibuffers t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq buffers-menu-max-size 30)
;; (setq require-final-newline 'ask)
(setq mode-require-final-newline nil)
;; (add-hook 'text-mode-hook (lambda () (setq require-final-newline nil)))

(ignore-errors (tool-bar-mode t))
(column-number-mode t)
(size-indication-mode 1)
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (display-time-mode t)
(setq-default indicate-buffer-boundaries (quote left))
(ignore-errors (set-scroll-bar-mode 'right))
(setq scroll-step 1)
;; (setq scroll-margin 3)
;; (setq scroll-conservatively 10000)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq frame-title-format
      '("[" (:eval (ignore-errors (projectile-project-name))) "] "
        (:eval (or buffer-file-name (buffer-name)))
        " [" (:eval (format "%s" buffer-file-coding-system)) "]"
        (:eval (if (buffer-modified-p) " * " " - "))
        "GNU Emacs " emacs-version " (" system-configuration ") @" system-name))

(when window-system (set-background-color "honeydew")) ; #f0fff0
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (and (framep frame)
                       (not (eq (framep frame) t)))
              (with-selected-frame frame
                ;; (when window-system
                (set-background-color "honeydew")))))
;; (unless window-system
;;   (setq frame-background-mode 'dark))
;; (set-frame-parameter (selected-frame) 'alpha (list 85 50)) ; Transparent
;; (add-to-list 'default-frame-alist (cons 'alpha (list 85 50)))

;; (setq linum-eager nil)
;; (when (fboundp 'global-linum-mode)
;;   (global-linum-mode 1))

(set-language-environment "UTF-8")
;; (setq system-time-locale "C")
(setq erc-server-coding-system '(utf-8 . utf-8))

(when (eq system-type 'windows-nt)
  (let ((code (or file-name-coding-system default-file-name-coding-system)))
    (setq default-process-coding-system (cons code code))))

;; (when (daemonp)
;;   (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;;               (with-selected-frame frame
;;                 (set-locale-environment (getenv "LANG"))))))

(setq kmacro-call-mouse-event nil)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region nil)
;; (setq mouse-yank-at-point t)
;; (mouse-avoidance-mode 'animate)
;; (setq mouse-autoselect-window t)

(unless window-system
  (xterm-mouse-mode 1)               ; (if window-system -1 1)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (xterm-mouse-mode 1)))))

;; (setq-default cursor-type 'bar)
;; (blink-cursor-mode -1)
(setq x-stretch-cursor t)

(ignore-errors (transient-mark-mode t))

(if (fboundp 'cua-mode)
    (progn
      (setq cua-rectangle-mark-key [C-M-return])
      (cua-mode t)
      (setq cua-keep-region-after-copy t))
  (when (fboundp 'pc-selection-mode)
    (setq pc-select-selection-keys-only t)
    (pc-selection-mode)))

(icomplete-mode t)

(setq ido-use-filename-at-point 'guess
      ido-use-url-at-point t)
(ido-mode t)
;; (ido-everywhere t)                    ; For GUI

;; (setq ffap-require-prefix t
;;       dired-at-point-require-prefix t)
;; (ffap-bindings)                         ; Use ido to call ffap

(show-paren-mode t)
;; (setq show-paren-style 'expression)
;; (custom-set-faces
;;  '(show-paren-match
;;    ((((class color) (background light)) (:background "azure2")))))

;; (global-set-key "<" 'skeleton-pair-insert-maybe)
;; (global-set-key "(" 'skeleton-pair-insert-maybe)
;; (global-set-key "[" 'skeleton-pair-insert-maybe)
;; (global-set-key "{" 'skeleton-pair-insert-maybe)
;; (setq skeleton-pair t)

(ignore-errors (global-font-lock-mode t))
;; (setq jit-lock-defer-time 0.05)         ; Make c mode faster
;; (setq hl-line-face 'underline)          ; for highlight-symbol
;; (global-hl-line-mode 1)                 ; (if window-system 1 -1)
;; (global-highlight-changes-mode t)       ; use cedet instead

(setq whitespace-line-column 120)
(setq whitespace-style '(face trailing newline empty lines-tail))
(when window-system
  (setq whitespace-style (append whitespace-style '(tabs tab-mark newline-mark))))
;; (global-whitespace-mode t)
(eval-after-load "whitespace"
  `(defun whitespace-post-command-hook ()
     "Hack whitespace, it's very slow in c++-mode."))

(which-function-mode t)
;; (global-cwarn-mode 1)
(global-auto-revert-mode t)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)

(setq generic-define-mswindows-modes t)
(setq generic-define-unix-modes t)
;; (require 'generic-x nil 'noerror)

(auto-image-file-mode t)
(winner-mode 1)

(setq-default save-place t)
(require 'saveplace)
(ignore-errors (savehist-mode t))
(setq bookmark-save-flag 1)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %U")
(add-hook 'write-file-hooks 'time-stamp)

(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; (setq backup-by-copying t)
;; (setq delete-old-versions t)
;; (setq kept-old-versions 2)
;; (setq kept-new-versions 5)
;; (setq version-control t)

;; (setq desktop-load-locked-desktop nil)  ; 'ask
(setq desktop-not-loaded-hook (quote (desktop-save-mode-off)))
(and (fboundp 'desktop-save-mode)
     (not (daemonp))
     (desktop-save-mode (if window-system 1 -1)))

(eval-after-load "filecache"
  '(progn (file-cache-add-directory-list load-path)))

(setq recentf-menu-open-all-flag t
      recentf-max-saved-items 100
      recentf-max-menu-items 30)
(recentf-mode t)
(defadvice recentf-track-closed-file (after push-beginning activate)
  "Move current buffer to the beginning of the recent list after killed."
  (recentf-track-opened-file))

(setq filesets-data
      '(("temp"
         (:files))
        ("linux"
         (:files "/etc/hosts" "/etc/fstab" "/etc/passwd" "/etc/group" "/boot/grub2/grub.cfg"))
        ("windows"
         (:files "C:/WINDOWS/system32/drivers/etc/hosts"))
        ("~/.emacs.d/"
         (:tree "~/.emacs.d/" "^.+\\.*$"))
        ("~/"
         (:files "~/.emacs" "~/.profile" "~/.bash_profile" "~/.bashrc"))))
(add-to-list 'filesets-data
             (list "dotemacs/"
                   (list :tree
                         (if (boundp 'dotemacs-root-dir)
                             dotemacs-root-dir
                           (file-name-directory
                            (directory-file-name
                             (file-name-directory
                              (or load-file-name buffer-file-name)))))
                         "^.+\\.*$")))
(filesets-init)

(setq inhibit-startup-message t)        ; for no desktop
(setq inhibit-default-init t)           ; for frame-title-format

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; (setq message-log-max t)
;; (setq max-specpdl-size 4000)
;; (setq max-lisp-eval-depth 4000)
;; (setq debug-on-error t)

(autoload 'zone-when-idle "zone" nil t)
(zone-when-idle (* 60  30))
;; zone-pgm-stress will destroy the clipboard
(setq zone-programs (append zone-programs nil))
(setq zone-programs (remq 'zone-pgm-stress zone-programs))
(setq zone-programs (remq 'zone-pgm-stress-destress zone-programs))

(add-hook 'after-init-hook
          (lambda ()
            (message "emacs-init-time: %s" (emacs-init-time))))

;; unicad
(require 'unicad nil 'noerror)

(provide 'init-editor)

;;; init-editor.el ends here
