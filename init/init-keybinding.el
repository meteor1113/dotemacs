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

(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(define-key cua-global-keymap (kbd "M-SPC") 'cua-set-mark)
(global-set-key (kbd "<find>") 'move-beginning-of-line) ; putty
(global-set-key (kbd "<select>") 'move-end-of-line) ; putty

(define-key global-map "\C-x\C-j" 'dired-jump)
(global-set-key [(control %)]
                '(lambda (arg)
                   "Go to the matching  if on (){}[], similar to vi style of % "
                   (interactive "p")
                   ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
                   (cond ((looking-at "[\[\(\{]") (forward-sexp))
                         ((looking-back "[\]\)\}]" 1) (backward-sexp))
                         ;; now, try to succeed from inside of a bracket
                         ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
                         ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
                         (t nil))))

(global-set-key [C-prior] 'previous-buffer)
(global-set-key [C-next] 'next-buffer)
(global-set-key [(control tab)]
                '(lambda ()
                   (interactive)
                   (call-interactively (switch-to-buffer (other-buffer)))))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-=") 'align)
(global-set-key (kbd "C-S-u") 'upcase-region)
(global-set-key (kbd "C-S-l") 'downcase-region)
;; (global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
;; (global-set-key (kbd "ESC M-;") 'comment-or-uncomment-region) ; putty
(global-set-key [(control ?.)] 'repeat)

(global-set-key [M-f8] 'format-region)
(global-set-key (kbd "ESC <f8>") 'format-region) ; putty
(global-set-key (kbd "C-S-f") 'format-region)

(global-set-key [f4] (lambda (&optional previous)
                       (interactive "P")
                       (if previous
                           (previous-error)
                         (next-error))))
(global-set-key [S-f4] 'previous-error)
(global-set-key [f16] 'previous-error)  ; S-f4

(global-set-key [C-f4] 'kill-this-buffer)
(global-set-key (kbd "ESC <f4>") 'kill-this-buffer) ; putty
(global-set-key (kbd "C-S-t") 'undo-kill-buffer)

(global-set-key [f6] 'grep-current-dir)
(global-set-key [C-f6] 'moccur-all-buffers)
(global-set-key [M-f6] 'grep-todo-current-dir)
;; (lambda () (interactive) (grep-current-dir nil "TODO|FIXME")))
(global-set-key (kbd "ESC <f6>") (key-binding [M-f6]))
(global-set-key [C-M-f6] 'moccur-todo-all-buffers)
(global-set-key (kbd "ESC <C-f6>") (key-binding [C-M-f6]))

(global-set-key [f7] 'compile)

(unless (key-binding [f11])
  (global-set-key [f11] 'toggle-frame-fullscreen))

(global-set-key [mouse-2] nil)
(global-set-key [mouse-3] menu-bar-edit-menu)

(global-set-key [S-mouse-3] 'ffap-at-mouse)
(global-set-key [C-S-mouse-3] 'ffap-menu)

(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-decrease)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-increase)
(unless (key-binding [mouse-4])
  (global-set-key [mouse-4] 'mwheel-scroll)) ; putty
(unless (key-binding [mouse-5])
  (global-set-key [mouse-5] 'mwheel-scroll)) ; putty

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
;; (global-set-key [header-line double-mouse-1] 'kill-this-buffer)

(global-set-key [left-fringe mouse-2] nil)
(global-set-key [left-margin mouse-2] nil)
(global-set-key (kbd "<left-margin> <mouse-2>")
                '(lambda (ev)
                   "Mark current line with a mouse click."
                   (interactive "e")
                   (mouse-set-point ev)
                   (move-beginning-of-line 1)
                   (set-mark (point))
                   (move-end-of-line 1)))

(when (eq system-type 'aix)
  (global-set-key (kbd "C-d") 'backward-delete-char-untabify)
  (eval-after-load "cc-mode"
    '(progn
       (define-key c-mode-base-map "\C-d" 'c-electric-backspace)))
  (eval-after-load "comint"
    '(progn
       (define-key comint-mode-map "\C-d" 'delete-backward-char))))

(autoload 'grep-tag-default "grep")
(autoload 'grep-apply-setting "grep")

(defvar grep-dir-format
  (cond ((eq system-type 'aix)
         "grep -inrH '%s' . | grep -vE \"\.svn/|\.git/|\.hg/|\.bzr/|CVS/\"")
        ;; ((eq system-type 'gnu/linux)
        ;;  "grep -inrHI '%s' . | grep -vE \"\.svn/|\.git/|\.hg/|\.bzr/|CVS/\"")
        ;; ((eq system-type 'windows-nt)
        ;;  "grep --exclude-dir=.svn --exclude-dir=.git --exclude-dir=.hg \
        ;;   --exclude-dir=.bzr --exclude-dir=CVS -inrHI \"%s\" .")
        (t
         "grep --exclude-dir=.svn --exclude-dir=.git --exclude-dir=.hg \
--exclude-dir=.bzr --exclude-dir=CVS -inrHI '%s' .")))

(defun grep-current-dir (&optional prompt wd)
  "Run `grep' to find current word in current directory."
  (interactive "P")
  (let* ((word (or wd
                   (and (fboundp 'region-active-p)
                        (region-active-p)
                        (buffer-substring-no-properties (region-beginning)
                                                        (region-end)))
                   (grep-tag-default)))
         (cmd (format grep-dir-format word)))
    (grep-apply-setting 'grep-use-null-device nil)
    (if (or prompt (= (length word) 0))
        (grep (read-shell-command
               "Run grep (like this): " cmd 'grep-history))
      (if (= 0 (length word))
          (message "Word is blank.")
        (grep cmd)))))

(defun grep-todo-current-dir ()
  "Run `grep' to find 'TODO' in current directory."
  (interactive)
  (grep-current-dir nil "TODO|FIXME"))

(defun moccur-word-all-buffers (regexp)
  "Run `multi-occur' to find regexp in all buffers."
  (if (= 0 (length regexp))
      (message "Regexp is blank.")
    (let ((buffers (buffer-list)))
      (dolist (buffer buffers)
        (let ((pos (string-match " *\\*" (buffer-name buffer))))
          (when (and pos (= 0 pos))
            (setq buffers (remq buffer buffers)))))
      (multi-occur buffers regexp))))

(defun moccur-all-buffers (&optional prompt)
  "Run `multi-occur' to find current word in all buffers."
  (interactive "P")
  (let ((word (grep-tag-default)))
    (when (or prompt (= (length word) 0))
      (setq word (read-regexp "List lines matching regexp" word)))
    (moccur-word-all-buffers word)))

(defun moccur-todo-all-buffers ()
  "Run `multi-occur' to find 'TODO' in all buffers."
  (interactive)
  (moccur-word-all-buffers
   "\\<\\([Tt][Oo][Dd][Oo]\\|[Ff][Ii][Xx][Mm][Ee]\\)\\>"))

(defun format-region ()
  "Format region, if no region actived, format current buffer.
Like eclipse's Ctrl+Alt+F."
  (interactive)
  (let ((start (point-min))
        (end (point-max)))
    (if (and (fboundp 'region-active-p) (region-active-p))
        (progn (setq start (region-beginning))
               (setq end (region-end)))
      (progn (when (fboundp 'whitespace-cleanup)
               (whitespace-cleanup))
             (setq end (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) end)
        (push-mark (point))
        (push-mark (point-max) nil t)
        (goto-char start)
        (when (fboundp 'whitespace-cleanup)
          (whitespace-cleanup))
        (untabify start (point-max))
        (indent-region start (point-max) nil)))))

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

(defun find-dotemacs-file ()
  "Open .emacs file"
  (interactive)
  (let* ((paths '("~/.emacs" "~/.emacs.el" "~/.emacs.d/init.el" "~/_emacs"))
         (dotemacs-path))
    (dolist (path paths)
      (and (not dotemacs-path)
           (file-exists-p path)
           (setq dotemacs-path path)))
    (find-file (or dotemacs-path
                   (locate-file "site-start.el" load-path)
                   "~/.emacs"))))

(provide 'init-keybinding)
