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

;; bm
(setq bm-restore-repository-on-load t)
(setq-default bm-buffer-persistence t)
(setq bm-cycle-all-buffers t)
(setq bm-highlight-style
      (if (and window-system (> emacs-major-version 21))
          'bm-highlight-only-fringe
        'bm-highlight-only-line))
(add-hook 'after-init-hook
          '(lambda ()
             (require 'bm nil 'noerror)))

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key [M-f2] 'bm-toggle)
(global-set-key (kbd "ESC <f2>") 'bm-toggle) ; putty
(global-set-key (kbd "<f2>")
                '(lambda (&optional previous)
                   (interactive "P")
                   (call-interactively (if previous 'bm-previous 'bm-next))))
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

(eval-after-load "bm"
  '(progn
     ;; (add-hook' after-init-hook 'bm-repository-load)
     ;; (add-hook 'after-save-hook 'bm-buffer-save)
     ;; (add-hook 'after-revert-hook 'bm-buffer-restore)
     (add-hook 'find-file-hooks 'bm-buffer-restore)
     (add-hook 'kill-buffer-hook 'bm-buffer-save)
     (add-hook 'kill-emacs-hook '(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))

     (eval-after-load "pulse"
	   '(progn
          (defadvice bm-next (after pulse-advice activate)
            (pulse-line-hook-function))
          (defadvice bm-previous (after pulse-advice activate)
            (pulse-line-hook-function))
          (defadvice bm-next-mouse (after pulse-advice activate)
            (pulse-line-hook-function))
          (defadvice bm-previous-mouse (after pulse-advice activate)
            (pulse-line-hook-function))))))

(provide 'init-bm)

;;; init-bm.el ends here
