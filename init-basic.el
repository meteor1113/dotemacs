;;;
;; Copyright (C) 2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2009-08-08


;;; global setting

;; user information
(setq user-full-name "Meteor Liu")
(setq user-mail-address "meteor1113@gmail.com")

;; tool-bar
(tool-bar-mode t)

;; scroll-bar
(set-scroll-bar-mode 'right)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; cua
(cua-mode t)
(setq cua-keep-region-after-copy t)
(setq mouse-drag-copy-region nil)
(setq x-select-enable-clipboard t)

;; mode-line
(column-number-mode t)
;; (size-indication-mode 1)
(display-time-mode t)
(which-function-mode t)

;; save information
(require 'saveplace)
(setq-default save-place t)
(savehist-mode t)
(recentf-mode t)
(desktop-save-mode t)

;; whitespace
(setq-default show-trailing-whitespace t)
(setq whitespace-style
      '(tabs trailing lines-tail space-before-tab newline
             indentation empty space-after-tab tab-mark newline-mark))
;; (global-whitespace-mode t)

;; bookmark
(setq bookmark-save-flag 1)

;; compile
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)

;; complete
(ido-mode t)
(icomplete-mode t)

;; cursor
;; (setq-default cursor-type 'bar)
;; (blink-cursor-mode -1)
(setq x-stretch-cursor t)

;; erc
(setq erc-server-coding-system '(utf-8 . utf-8))

;; program
(show-paren-mode t)
;; (setq show-paren-style 'mixed)
(global-cwarn-mode 1)

;; misc
(require 'generic-x nil 'noerror)
(setq ring-bell-function 'ignore)
(global-auto-revert-mode t)
(global-hl-line-mode (if window-system 1 -1))
;; (global-highlight-changes-mode t)       ; use cedet instead
;; (global-linum-mode 1)                   ; conflict with company-mode

;; ffap
(defconst user-include-dirs
  (list "../" "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))
(defconst win32-include-dirs
  (list "C:/MinGW/include"
        "C:/MinGW/include/c++/3.4.5"
        "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
(ffap-bindings)
(when (boundp 'ffap-c-path)
  (setq ffap-c-path (append ffap-c-path user-include-dirs))
  (when (eq system-type 'windows-nt)
    (setq ffap-c-path (append ffap-c-path win32-include-dirs))))

(defadvice find-tag (before tags-file-name-advice activate)
  "Find TAGS file in ./ or ../ or ../../ dirs"
  (let ((list (mapcar 'expand-file-name '("./TAGS" "../TAGS" "../../TAGS"))))
    (while list
      (if (file-exists-p (car list))
          (progn
            (setq tags-file-name (car list))
            (setq list nil))
        (setq list (cdr list))))))

(defun find-dotemacs-file ()
  "Open .emacs file"
  (interactive)
  (let* ((homedir (getenv "HOME"))
         (path1 (expand-file-name ".emacs" homedir))
         (path2 (expand-file-name "_emacs" homedir))
         (dotemacs-path path1))
    (when (file-exists-p path2)
      (setq dotemacs-path path2))
    (when (file-exists-p path1)
      (setq dotemacs-path path1))
    (find-file dotemacs-path)))

(defun move-line-up (p)
  "Move current line up, copy from crazycool@smth"
  (interactive "*p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (previous-line p)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defun move-line-down (p)
  "Move current line down, copy from crazycool@smth"
  (interactive "*p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (next-line p)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defun format-region ()
  "Format region, if no region actived, format current buffer.
Like eclipse's Ctrl+Alt+F."
  (interactive)
  (let ((start (point-min))
        (end (point-max)))
    (if (region-active-p)
        (progn (setq start (region-beginning))
               (setq end (region-end)))
      (progn (whitespace-cleanup)
             (setq end (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) end)
        (push-mark (point))
        (push-mark (point-max) nil t)
        (goto-char start)
        (whitespace-cleanup)
        (untabify start (point-max))
        (indent-region start (point-max))))))

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

(defun moccur-all-buffers (&optional is-prompt)
  "Run `multi-occur' to find current word in all buffers."
  (interactive "P")
  (let ((word (grep-tag-default)))
    (when is-prompt
      (setq word (read-regexp "List lines matching regexp" word)))
    (moccur-word-all-buffers word)))

(require 'grep)
(defun grep-current-dir (&optional is-prompt)
  "Run `grep' to find current word in current directory."
  (interactive "P")
  (let* ((word (grep-tag-default))
         (commands (concat "grep -inr '" word "' .")))
    (if is-prompt
        (grep (read-shell-command
               "Run grep (like this): " commands 'grep-history))
      (if (= 0 (length word))
          (message "Word is blank.")
        (grep commands)))))

;; global key bindings
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key [M-f8] 'format-region)
(global-set-key (kbd "C-S-f") 'format-region)
(global-set-key (kbd "C-=") 'align)
(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key [(control tab)]
                (lambda ()
                  (interactive)
                  (call-interactively (switch-to-buffer (other-buffer)))))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key [f4] 'next-error)
(global-set-key [S-f4] 'previous-error)
(global-set-key [C-f4] 'kill-this-buffer)
(global-set-key [f6] 'moccur-all-buffers)
(global-set-key [C-f6] 'grep-current-dir)
(global-set-key [M-f6]
                '(lambda () (interactive) (moccur-word-all-buffers "TODO")))
(global-set-key [C-M-f6] (lambda () (interactive) (grep "grep -inr TODO .")))


;;; special mode setting

(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

(setq org-log-done 'time)
(add-hook 'org-mode-hook
          (lambda ()
            (imenu-add-menubar-index)
            (setq comment-start nil)
            (auto-fill-mode t)))

(when (require 'nxml-mode nil 'noerror)
  (add-to-list 'auto-mode-alist
               '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (add-hook 'nxml-mode-hook
            '(lambda ()
               (require 'sgml-mode)
               (set-syntax-table sgml-mode-syntax-table))))

(defun program-common-function ()
  (setq indent-tabs-mode nil)
  ;; (local-set-key (kbd "<return>") 'newline-and-indent)
  (whitespace-mode t)
  ;; (hs-minor-mode t)
  (imenu-add-menubar-index))

(add-hook 'c-mode-common-hook
          (lambda ()
            (program-common-function)
            (turn-on-auto-fill)))

(add-to-list 'auto-mode-alist '("\\.[ch]\\'" . c++-mode))
(add-hook 'c-mode-hook (lambda () (c-set-style "stroustrup")))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "stroustrup")
            (c-set-offset 'innamespace 0)))

(add-hook 'java-mode-hook (lambda () (c-set-style "java")))

(add-hook 'objc-mode-hook (lambda () (c-set-style "stroustrup")))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (program-common-function)
            (turn-on-eldoc-mode)))

(add-hook 'python-mode-hook 'program-common-function)

(add-hook 'sh-mode-hook 'program-common-function)

(add-hook 'makefile-mode-hook (lambda () (whitespace-mode 1)))

(add-hook 'autoconf-mode-hook (lambda () (whitespace-mode 1)))

(add-hook 'perl-mode-hook 'program-common-function)
(add-to-list 'auto-mode-alist
             '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-hook 'cperl-mode-hook
          '(lambda ()
             (program-common-function)
             (cperl-set-style "PerlStyle")
             (setq cperl-continued-brace-offset -4)
             (abbrev-mode t)))

;; gdb setting
(require 'gdb-ui nil 'noerror)
(require 'gdb-mi nil 'noerror)

(defun gdb-or-gud-go ()
  "If gdb isn't running; run gdb, else call gud-go."
  (interactive)
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer)
           (with-current-buffer gud-comint-buffer
             (or (eq gud-minor-mode 'gdba) (eq gud-minor-mode 'gdbmi))))
      (gud-go nil)
    (gdb (gud-query-cmdline 'gdb))))

(defun gud-break-or-remove ()
  "Set/clear breakpoin."
  (interactive)
  (save-excursion
    (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
        (gud-remove nil)
      (gud-break nil))))

(defun gud-enable-or-disable ()
  "Enable/disable breakpoin."
  (interactive)
  (let ((pos))
    (save-excursion
      (move-beginning-of-line nil)
      (dolist (overlay (overlays-in (point) (point)))
        (when (overlay-get overlay 'put-break)
          (setq obj (overlay-get overlay 'before-string))))
      (when (and (stringp obj) (featurep 'gdb-ui))
        (let* ((bptno (get-text-property 0 'gdb-bptno obj)))
          (string-match "\\([0-9+]\\)*" bptno)
          (gdb-enqueue-input
           (list
            (concat gdb-server-prefix
                    (if (get-text-property 0 'gdb-enabled obj)
                        "disable "
                      "enable ")
                    (match-string 1 bptno) "\n")
            'ignore))))
      (when (and (stringp obj) (featurep 'gdb-mi))
        (gud-basic-call
         (concat
          (if (get-text-property 0 'gdb-enabled obj)
              "-break-disable "
            "-break-enable ")
          (get-text-property 0 'gdb-bptno obj)))))))

(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (set-process-query-on-exit-flag (get-buffer-process gud-comint-buffer) nil)
  (kill-buffer gud-comint-buffer))

(setq gdb-many-windows t)
(gud-tooltip-mode t)
;; (global-set-key [f5] 'gdb-or-gud-go)
(define-key c-mode-base-map [f5] 'gdb)
(define-key gud-minor-mode-map [f5] 'gud-go)
(define-key gud-minor-mode-map [S-f5] 'gud-kill)
(define-key c-mode-base-map [f7]
  '(lambda () (interactive) (compile compile-command)))
(define-key gud-minor-mode-map [f8] 'gud-print)
(define-key gud-minor-mode-map [C-f8] 'gud-pstar)
(define-key gud-minor-mode-map [f9] 'gud-break-or-remove)
(define-key gud-minor-mode-map [C-f9] 'gud-enable-or-disable)
(define-key gud-minor-mode-map [S-f9] 'gud-watch)
(define-key gud-minor-mode-map [f10] 'gud-next)
(define-key gud-minor-mode-map [C-f10] 'gud-until)
(define-key gud-minor-mode-map [C-S-f10] 'gud-jump)
(define-key gud-minor-mode-map [f11] 'gud-step)
(define-key gud-minor-mode-map [C-f11] 'gud-finish)


(provide 'init-basic)
