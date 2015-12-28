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

;; edit
(setq-default tab-width 4)
(setq-default comment-column 40)        ; [C-x ;] (set-comment-column)
(setq-default major-mode 'text-mode)    ; (setq default-major-mode 'text-mode)
(setq bookmark-save-flag 1)
(global-auto-revert-mode t)
;; (setq require-final-newline 'ask)
(setq mode-require-final-newline nil)
;; (add-hook 'text-mode-hook (lambda () (setq require-final-newline nil)))
(find-function-setup-keys)
(when (fboundp 'ido-mode)
  (ido-mode t)
  ;; (ido-everywhere t)                    ; For GUI
  (setq ido-use-filename-at-point 'guess
        ido-use-url-at-point t))
(icomplete-mode t)
(show-paren-mode t)
;; (setq show-paren-style 'expression)
;; (custom-set-faces
;;  '(show-paren-match
;;    ((((class color) (background light)) (:background "azure2")))))
(global-cwarn-mode 1)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %U")
(add-hook 'write-file-hooks 'time-stamp)
;; (global-set-key "<" 'skeleton-pair-insert-maybe)
;; (global-set-key "(" 'skeleton-pair-insert-maybe)
;; (global-set-key "[" 'skeleton-pair-insert-maybe)
;; (global-set-key "{" 'skeleton-pair-insert-maybe)
;; (setq skeleton-pair t)
;; (setq enable-recursive-minibuffers t)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; input
(if (fboundp 'cua-mode)
    (progn
      (setq cua-rectangle-mark-key [C-M-return])
      (cua-mode t)
      (setq cua-keep-region-after-copy t))
  (when (fboundp 'pc-selection-mode)
    (setq pc-select-selection-keys-only t)
    (pc-selection-mode)))
(setq mouse-drag-copy-region nil)
(setq x-select-enable-clipboard t)
;; (setq mouse-yank-at-point t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; coding
(set-language-environment "UTF-8")
;; (setq system-time-locale "C")
(when (eq system-type 'windows-nt)
  (let ((code (or file-name-coding-system default-file-name-coding-system)))
    (setq default-process-coding-system (cons code code))))
(setq erc-server-coding-system '(utf-8 . utf-8))
;; (when (daemonp)
;;   (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;;               (with-selected-frame frame
;;                 (set-locale-environment (getenv "LANG"))))))

;; session
(require 'saveplace)
(setq-default save-place t)
(ignore-errors (savehist-mode t))
(setq recentf-menu-open-all-flag t
      recentf-max-saved-items 100
      recentf-max-menu-items 30)
(recentf-mode t)
(defadvice recentf-track-closed-file (after push-beginning activate)
  "Move current buffer to the beginning of the recent list after killed."
  (recentf-track-opened-file))
(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf))))
                           (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delete buf-file recently-killed-list)))
     buffer-files-list)
    (find-file (nth (- arg 1) recently-killed-list))))
(setq desktop-load-locked-desktop nil
      desktop-not-loaded-hook (quote (desktop-save-mode-off)))
(and (fboundp 'desktop-save-mode)
     (not (daemonp))
     (desktop-save-mode (if window-system 1 -1)))

;; backup
(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; (setq backup-by-copying t)
;; (setq delete-old-versions t)
;; (setq kept-old-versions 2)
;; (setq kept-new-versions 5)
;; (setq version-control t)

;; highlight
(ignore-errors (global-font-lock-mode t))
;; (setq jit-lock-defer-time 0.05)         ; Make c mode faster
(ignore-errors (transient-mark-mode t))
;; (setq hl-line-face 'underline)          ; for highlight-symbol
;; (global-hl-line-mode 1)                 ; (if window-system 1 -1)
;; (global-highlight-changes-mode t)       ; use cedet instead
;; (setq-default show-trailing-whitespace t) ; use whitespace-mode instead
(setq whitespace-style '(face trailing lines-tail newline empty tab-mark))
(when window-system
  (setq whitespace-style (append whitespace-style '(tabs newline-mark))))
;; (global-whitespace-mode t)
(eval-after-load "whitespace"
  `(defun whitespace-post-command-hook ()
     "Hack whitespace, it's very slow in c++-mode."))

;; file
;; (setq ffap-require-prefix t
;;       dired-at-point-require-prefix t)
;; (ffap-bindings)                         ; Use ido to call ffap
(autoload 'dired-jump "dired-x" nil t)
(setq kmacro-call-mouse-event nil)

(defvar user-include-dirs
  '("." "./include" "./inc" "./common" "./public"
    ".." "../include" "../inc" "../common" "../public"
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

(eval-after-load "ffap"
  '(setq ffap-c-path (append ffap-c-path user-include-dirs)))

(eval-after-load "filecache"
  '(progn (file-cache-add-directory-list load-path)
          (file-cache-add-directory-list user-include-dirs)
          (file-cache-add-directory "/usr/include")
          (file-cache-add-directory-recursively "/usr/include/c++")
          (file-cache-add-directory-recursively "/usr/local/include")))

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

;; misc
(setq inhibit-startup-message t)        ; for no desktop
(setq inhibit-default-init t)           ; for frame-title-format
(setq generic-define-mswindows-modes t)
(setq generic-define-unix-modes t)
(require 'generic-x nil 'noerror)
(setq ring-bell-function 'ignore)
(auto-image-file-mode t)
;; (setq message-log-max t)
;; (add-hook 'find-file-hook 'goto-address-mode)
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

(when (executable-find "chmod")
  (add-hook 'after-save-hook
            '(lambda ()
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
                    (not (file-executable-p buffer-file-name))
                    (shell-command (format "chmod +x '%s'" buffer-file-name))
                    (kill-buffer "*Shell Command Output*")))))

;; unicad
(require 'unicad nil 'noerror)

;; ace-jump-mode
;; (autoload 'ace-jump-mode "ace-jump-mode" nil t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(set-face-background 'ace-jump-face-foreground "yellow"))
(eval-after-load "viper-keym"
  '(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode))

;; ascii
;; (autoload 'ascii-on        "ascii" "Turn on ASCII code display."   t)
;; (autoload 'ascii-off       "ascii" "Turn off ASCII code display."  t)
;; (autoload 'ascii-display   "ascii" "Toggle ASCII code display."    t)
;; (autoload 'ascii-customize "ascii" "Customize ASCII code display." t)

;; browse-kill-ring
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (browse-kill-ring-default-keybindings))))

;; drag-stuff
;; (autoload 'drag-stuff-global-mode "drag-stuff" "Toggle Drag-Stuff mode." t)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (drag-stuff-global-mode t))))

;; fill-column-indicator
;; (autoload 'fci-mode "fill-column-indicator" nil t)

;; ggtags
(when (executable-find "global")
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (ggtags-mode 1))))

;; smart-compile
;; (autoload 'smart-compile "smart-compile" nil t)
(global-set-key [C-f7] 'smart-compile)

;; xcscope
(add-hook 'after-init-hook
          '(lambda ()
             (when (executable-find "cscope")
               (when (require 'xcscope nil 'noerror)
                 (define-key cscope-list-entry-keymap [mouse-1]
                   'cscope-mouse-select-entry-other-window)))))

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

;; projectile
;; (autoload 'projectile-mode "projectile" nil t)
;; (autoload 'projectile-global-mode "projectile" nil t)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (projectile-global-mode 1))))

;; undo-tree
;; (autoload 'undo-tree-mode "undo-tree" nil t)
;; (autoload 'global-undo-tree-mode "undo-tree" nil t)

;; vlf
;; (autoload 'vlf "vlf" "View a Large File in Emacs." t)

(provide 'init-editor)
