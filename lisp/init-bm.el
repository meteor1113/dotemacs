;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @URL https://github.com/meteor1113/dotemacs

;;; Commentary:

;; https://github.com/joodland/bm

;;; Code:

;; bm
(use-package bm
  ;; :ensure t
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  :config
  (setq bm-cycle-all-buffers t)
  (setq-default bm-buffer-persistence t)
  (setq bm-highlight-style
        (if (and window-system (> emacs-major-version 21))
            'bm-highlight-only-fringe
          'bm-highlight-only-line))

  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  (eval-after-load "pulse"
    '(progn
       (advice-add 'bm-next :after #'pulse-line-hook-function)
       (advice-add 'bm-previous :after #'pulse-line-hook-function)
       (advice-add 'bm-next-mouse :after #'pulse-line-hook-function)
       (advice-add 'bm-previous-mouse :after #'pulse-line-hook-function)))

  :bind
  (("C-<f2>" . bm-toggle)
   ("M-<f2>" . bm-toggle)
   ("ESC <f2>" . bm-toggle)             ; putty
   ("<f2>" . (lambda (&optional previous)
               (interactive "P")
               (call-interactively (if previous 'bm-previous 'bm-next))))
   ("<S-f2>" . bm-previous)
   ;; (global-set-key [f14] 'bm-previous)   ; S-f2
   ;; (global-set-key (kbd "ESC ESC <f2>") 'bm-previous)
   ("C-<S-f2>" . bm-remove-all-current-buffer)
   ([left-fringe mouse-1] . bm-toggle-mouse)
   ([left-fringe mouse-3] . bm-next-mouse))
  )

(provide 'init-bm)

;;; init-bm.el ends here
