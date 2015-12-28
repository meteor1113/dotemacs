;;; -*- mode: emacs-lisp; coding: utf-8; -*-
;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @date 2015-12-26
;; @URL http://git.oschina.net/meteor1113/dotemacs

(setq inhibit-startup-message t)        ; for no desktop
(setq inhibit-default-init t)           ; for frame-title-format

(add-hook 'after-init-hook
          (lambda ()
            (message "emacs-init-time: %s" (emacs-init-time))))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; (setq message-log-max t)
;; (setq max-specpdl-size 4000)
;; (setq max-lisp-eval-depth 4000)
;; (setq debug-on-error t)

;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default major-mode 'text-mode)

(setq-default comment-column 40)        ; [C-x ;] (set-comment-column)

;; (setq enable-recursive-minibuffers t)

(setq bookmark-save-flag 1)

(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %U")
(add-hook 'write-file-hooks 'time-stamp)

(set-language-environment "UTF-8")
;; (setq system-time-locale "C")

(when (eq system-type 'windows-nt)
  (let ((code (or file-name-coding-system default-file-name-coding-system)))
    (setq default-process-coding-system (cons code code))))

;; (when (daemonp)
;;   (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;;               (with-selected-frame frame
;;                 (set-locale-environment (getenv "LANG"))))))

(setq erc-server-coding-system '(utf-8 . utf-8))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq kmacro-call-mouse-event nil)

(if (fboundp 'cua-mode)
    (progn
      (setq cua-rectangle-mark-key [C-M-return])
      (cua-mode t)
      (setq cua-keep-region-after-copy t))
  (when (fboundp 'pc-selection-mode)
    (setq pc-select-selection-keys-only t)
    (pc-selection-mode)))

(setq x-select-enable-clipboard t)

(setq mouse-drag-copy-region nil)
;; (setq mouse-yank-at-point t)

;; (setq-default cursor-type 'bar)
;; (blink-cursor-mode -1)
(setq x-stretch-cursor t)

(xterm-mouse-mode 1)               ; (if window-system -1 1)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (xterm-mouse-mode 1))))
;; (mouse-avoidance-mode 'animate)
;; (setq mouse-autoselect-window t)

(icomplete-mode t)

(setq ido-use-filename-at-point 'guess
      ido-use-url-at-point t)
(ignore-errors (ido-mode t))
;; (ido-everywhere t)                    ; For GUI

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

(setq whitespace-style '(face trailing lines-tail newline empty tab-mark))
(when window-system
  (setq whitespace-style (append whitespace-style '(tabs newline-mark))))
;; (global-whitespace-mode t)
(eval-after-load "whitespace"
  `(defun whitespace-post-command-hook ()
     "Hack whitespace, it's very slow in c++-mode."))

;; (setq require-final-newline 'ask)
(setq mode-require-final-newline nil)
;; (add-hook 'text-mode-hook (lambda () (setq require-final-newline nil)))

(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; (setq backup-by-copying t)
;; (setq delete-old-versions t)
;; (setq kept-old-versions 2)
;; (setq kept-new-versions 5)
;; (setq version-control t)

(global-auto-revert-mode t)

(require 'saveplace)
(setq-default save-place t)

(ignore-errors (savehist-mode t))

;; (setq ffap-require-prefix t
;;       dired-at-point-require-prefix t)
;; (ffap-bindings)                         ; Use ido to call ffap

(eval-after-load "filecache"
  '(progn (file-cache-add-directory-list load-path)))

(setq recentf-menu-open-all-flag t
      recentf-max-saved-items 100
      recentf-max-menu-items 30)
(recentf-mode t)
(defadvice recentf-track-closed-file (after push-beginning activate)
  "Move current buffer to the beginning of the recent list after killed."
  (recentf-track-opened-file))

(filesets-init)
(add-to-list 'filesets-data
             (list "~/"
                   (list :files
                         "~/.emacs"
                         "~/.emacs.desktop"
                         "~/.emacs-places"
                         "~/.notes"
                         "~/.recentf"
                         "~/.profile"
                         "~/.bash_profile"
                         "~/.bashrc")))
(add-to-list 'filesets-data
             '("~/.emacs.d/" (:tree "~/.emacs.d/" "^.+\\.*$")))
(add-to-list 'filesets-data
             (list "dotemacs/"
                   (list :tree
                         (file-name-directory (or load-file-name
                                                  (buffer-file-name)))
                         "^.+\\.*$")))
(add-to-list 'filesets-data
             (list "windows"
                   (list :files
                         "C:/WINDOWS/system32/drivers/etc/hosts"
                         "C:/boot.ini")))
(add-to-list 'filesets-data
             (list "linux"
                   (list :files
                         "/etc/hosts"
                         "/etc/fstab"
                         "/etc/passwd"
                         "/etc/group"
                         "/boot/grub2/grub.cfg")))
(add-to-list 'filesets-data (list "temp" (list :files)))

(setq desktop-load-locked-desktop nil
      desktop-not-loaded-hook (quote (desktop-save-mode-off)))
(and (fboundp 'desktop-save-mode)
     (not (daemonp))
     (desktop-save-mode (if window-system 1 -1)))

(which-function-mode t)

(global-cwarn-mode 1)

(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)

(setq generic-define-mswindows-modes t)
(setq generic-define-unix-modes t)
(require 'generic-x nil 'noerror)

(ignore-errors (tool-bar-mode t))

(ignore-errors (transient-mark-mode t))
(column-number-mode t)
(size-indication-mode 1)

;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (display-time-mode t)

(setq buffers-menu-max-size 30)

(ignore-errors (set-scroll-bar-mode 'right))
(setq scroll-step 1)
;; (setq scroll-margin 3)
;; (setq scroll-conservatively 10000)

(setq-default indicate-buffer-boundaries (quote left))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq frame-title-format
      '("[" (:eval (ignore-errors (projectile-project-name))) "]"
        (:eval (or buffer-file-name (buffer-name)))
        "[" (:eval (format "%s" buffer-file-coding-system)) "]"
        (:eval (if (buffer-modified-p) " * " " - "))
        invocation-name "@" system-name))

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

(ignore-errors (winner-mode 1))

(setq ring-bell-function 'ignore)

(auto-image-file-mode t)

(autoload 'zone-when-idle "zone" nil t)
(zone-when-idle (* 60  30))
;; zone-pgm-stress will destroy the clipboard
(setq zone-programs (append zone-programs nil))
(setq zone-programs (remq 'zone-pgm-stress zone-programs))
(setq zone-programs (remq 'zone-pgm-stress-destress zone-programs))

;; ace-jump-mode
;; (autoload 'ace-jump-mode "ace-jump-mode" nil t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(set-face-background 'ace-jump-face-foreground "yellow"))

;; ascii

;; drag-stuff
;; (autoload 'drag-stuff-global-mode "drag-stuff" "Toggle Drag-Stuff mode." t)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (drag-stuff-global-mode t))))

;; fill-column-indicator
;; (autoload 'fci-mode "fill-column-indicator" nil t)

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

;; rainbow-mode
;; (autoload 'rainbow-mode "rainbow-mode" nil t)

;; smart-compile
(global-set-key [C-f7] 'smart-compile)

;; undo-tree
;; (autoload 'undo-tree-mode "undo-tree" nil t)
;; (autoload 'global-undo-tree-mode "undo-tree" nil t)

;; vlf
;; (autoload 'vlf "vlf" "View a Large File in Emacs." t)

;; window-numbering
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (window-numbering-mode 1))))

;; win-switch
;; (autoload 'win-switch-dispatch "win-switch" nil t)
;; (global-set-key "\C-xo" 'win-switch-dispatch)
(global-set-key "\C-xo"
                (lambda ()
                  (interactive)
                  (if (require 'win-switch nil 'noerror)
                      (win-switch-dispatch)
                    (other-window 1))))

;; unicad
(require 'unicad nil 'noerror)

(provide 'init-editor)
