;;; -*- mode: emacs-lisp; mode: goto-address; coding: utf-8; -*-
;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @date 2009-08-08
;; @URL http://git.oschina.net/meteor1113/dotemacs

;; global key bindings
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(define-key cua-global-keymap (kbd "M-SPC") 'cua-set-mark)
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key (kbd "<find>") 'move-beginning-of-line) ; putty
(global-set-key (kbd "<select>") 'move-end-of-line) ; putty
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-decrease)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-increase)
(unless (key-binding [mouse-4])
  (global-set-key [mouse-4] 'mwheel-scroll)) ; putty
(unless (key-binding [mouse-5])
  (global-set-key [mouse-5] 'mwheel-scroll)) ; putty
(global-set-key (kbd "C-=") 'align)
(global-set-key (kbd "C-S-u") 'upcase-region)
(global-set-key (kbd "C-S-l") 'downcase-region)
;; (global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
;; (global-set-key (kbd "ESC M-;") 'comment-or-uncomment-region) ; putty
(global-set-key [M-f8] 'format-region)
(global-set-key (kbd "ESC <f8>") 'format-region) ; putty
(global-set-key (kbd "C-S-f") 'format-region)
;; (global-set-key (kbd "M-P") 'previous-buffer)
;; (global-set-key (kbd "M-N") 'next-buffer)
(global-set-key [C-prior] 'previous-buffer)
(global-set-key [C-next] 'next-buffer)
(global-set-key [(control tab)] 'switch-to-other-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(define-key global-map "\C-x\C-j" 'dired-jump)
;; (global-set-key [f4] 'next-error)
(global-set-key [f4] (lambda (&optional previous)
                       (interactive "P")
                       (if previous
                           (previous-error)
                         (next-error))))
(global-set-key [S-f4] 'previous-error)
(global-set-key [f16] 'previous-error)  ; S-f4
(global-set-key [C-f4] 'kill-this-buffer)
(global-set-key (kbd "ESC <f4>") 'kill-this-buffer) ; putty
(global-set-key [(control ?.)] 'repeat)
(global-set-key [f6] 'grep-current-dir)
(global-set-key [C-f6] 'moccur-all-buffers)
(global-set-key [M-f6] 'grep-todo-current-dir)
;; (lambda () (interactive) (grep-current-dir nil "TODO|FIXME")))
(global-set-key (kbd "ESC <f6>") (key-binding [M-f6]))
(global-set-key [C-M-f6] 'moccur-todo-all-buffers)
;; '(lambda ()
;;    (interactive)
;;    (moccur-word-all-buffers
;;     "\\<\\([Tt][Oo][Dd][Oo]\\|[Ff][Ii][Xx][Mm][Ee]\\)\\>")))
(global-set-key (kbd "ESC <C-f6>") (key-binding [C-M-f6]))
(global-set-key [f7] '(lambda () (interactive) (compile compile-command)))
(unless (key-binding [f11])
  (global-set-key [f11] 'toggle-frame-fullscreen))
;; (global-set-key [header-line double-mouse-1] 'kill-this-buffer)
(global-set-key [header-line double-mouse-1]
                '(lambda ()
                   (interactive)
                   (let* ((i 1)
                          (name (format "new %d" i)))
                     (while (get-buffer name)
                       (setq i (1+ i))
                       (setq name (format "new %d" i)))
                     (switch-to-buffer name))))
;; (global-set-key [header-line double-mouse-1]
;;                 '(lambda () (interactive) (switch-to-buffer "new")))
(global-set-key [header-line mouse-3] 'kill-this-buffer)
(global-set-key [mouse-2] nil)
(global-set-key [left-fringe mouse-2] nil)
(global-set-key [left-margin mouse-2] nil)
(global-set-key [mouse-3] menu-bar-edit-menu)
(global-set-key (kbd "<left-margin> <mouse-2>") 'mark-current-line-mouse)
(global-set-key (kbd "C-S-t") 'undo-kill-buffer)
(global-set-key (kbd "C-c C-v") 'view-mode)
(global-set-key [(control %)] 'goto-match-paren)
(when (eq system-type 'aix)
  (global-set-key (kbd "C-d") 'backward-delete-char-untabify)
  (eval-after-load "cc-mode"
    '(progn
       (define-key c-mode-base-map "\C-d" 'c-electric-backspace)))
  (eval-after-load "comint"
    '(progn
       (define-key comint-mode-map "\C-d" 'delete-backward-char))))
(global-set-key [(meta f1)] 'highlight-text-at-point)

(provide 'init-global-keybindings)
