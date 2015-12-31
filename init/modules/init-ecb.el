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

;; ecb
;; (require 'ecb-autoloads nil 'noerror)
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
      ecb-source-path (quote ("/" ("c:" "c:")))
      ecb-layout-name 'left3
      ecb-toggle-layout-sequence '("left3"
                                   "left8"
                                   "left-analyse"
                                   "left-symboldef")
      ecb-windows-width 0.25
      ecb-compile-window-height 0.1
      ecb-compile-window-width 'edit-window
      ecb-compile-window-temporally-enlarge 'after-selection
      ;; ecb-enlarged-compilation-window-max-height 0.8
      ;; ecb-cedet-required-version-max '(2 0 4 9)
      ecb-tip-of-the-day nil
      ecb-auto-compatibility-check nil)

(unless (boundp 'stack-trace-on-error)
  (defvar stack-trace-on-error nil))

(when (fboundp 'ecb-minor-mode)
  (defvar ecb-minor-mode nil))

(defadvice ecb-symboldef-find-tag-by-etags (around no-prompt-etags activate)
  "Disable etags's 'Visit tags table' dialog."
  (when tags-file-name
    ad-do-it))

(eval-after-load "ecb-compilation"
  '(progn
     (setq ecb-compilation-buffer-names
           (append ecb-compilation-buffer-names '(("*Process List*")
                                                  ("*Proced*")
                                                  (".notes")
                                                  ("notes")
                                                  ("*appt-buf*")
                                                  ("*Compile-Log*")
                                                  ("*etags tmp*")
                                                  (" *svn-process*")
                                                  ("*svn-info-output*")
                                                  ("*Python Output*")
                                                  ("*Org Agenda*")
                                                  (" *EMMS Playlist*")
                                                  ("*Moccur*")
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
                                                 flycheck-error-list-mode
                                                 gud-mode)))))

(provide 'init-ecb)
