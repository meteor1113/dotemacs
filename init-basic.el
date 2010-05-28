;;;
;; Copyright (C) 2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2009-08-08
;; @URL http://github.com/meteor1113/dotemacs


;;; global setting

;; user information
(setq user-full-name "Meteor Liu")
(setq user-mail-address "meteor1113@gmail.com")

;; c/c++ include dir
(defvar user-include-dirs
  '(".." "../include" "../inc" "../common" "../public"
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
(defvar c-preprocessor-symbol-files
  '("C:/MinGW/include/c++/3.4.5/mingw32/bits/c++config.h"
    "C:/Program Files/Microsoft Visual Studio/VC98/Include/xstddef"
    ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include/yvals.h"
    ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include/crtdefs.h"
    )
  "Preprocessor symbol files for cedet")

;; tool-bar
(tool-bar-mode t)

;; scroll-bar
(set-scroll-bar-mode 'right)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; edit
(setq-default tab-width 4)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; cua
(cua-mode t)
(define-key cua-global-keymap (kbd "<M-RET>") 'cua-set-rectangle-mark)
(setq cua-keep-region-after-copy (if window-system t nil))
(setq mouse-drag-copy-region nil)
(setq x-select-enable-clipboard t)

;; mode-line
(column-number-mode t)
;; (size-indication-mode 1)
(display-time-mode t)
(which-function-mode t)
(setq frame-title-format
      '("emacs@"
        (:eval (system-name))
        " - "
        (:eval (or (buffer-file-name) (buffer-name)))))

;; save information
(require 'saveplace)
(setq-default save-place t)
(savehist-mode t)
(recentf-mode t)
(desktop-save-mode (if window-system 1 -1))

;; whitespace
;; (setq-default show-trailing-whitespace t) ; use whitespace-mode instead
(setq whitespace-style '(trailing lines-tail newline empty tab-mark))
(when window-system
  (setq whitespace-style (append whitespace-style '(tabs))))
;; (global-whitespace-mode t)
(eval-after-load "whitespace"
  `(defun whitespace-post-command-hook ()
     "Hack for emacs-23.2's whitespace, it's very slow in c++-mode."
     ))

;; bookmark
(setq bookmark-save-flag 1)

;; backup
;; (setq make-backup-files nil)
;; (setq backup-by-copying t)
;; (setq backup-directory-alist '(("." . "~/.backups")))
;; (setq delete-old-versions t)
;; (setq kept-old-versions 2)
;; (setq kept-new-versions 5)
;; (setq version-control t)

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
;; (mouse-avoidance-mode 'animate)

;; erc
(setq erc-server-coding-system '(utf-8 . utf-8))

;; program
(show-paren-mode t)
;; (setq show-paren-style 'mixed)
(global-cwarn-mode 1)

;; highlight
;; (global-hl-line-mode (if window-system 1 -1))
;; (global-highlight-changes-mode t)       ; use cedet instead
(dolist (mode '(c-mode c++-mode objc-mode java-mode jde-mode
                       perl-mode cperl-mode python-mode ruby-mode
                       lisp-mode emacs-lisp-mode
                       lisp-interaction-mode sh-mode sgml-mode))
  (font-lock-add-keywords
   mode
   '(("\\<\\(FIXME\\|TODO\\|Todo\\)\\>" 1 font-lock-warning-face prepend)
     ("\\<\\(FIXME\\|TODO\\|Todo\\):" 1 font-lock-warning-face prepend))))

;; ffap
(ffap-bindings)
(when (boundp 'ffap-c-path)
  (setq ffap-c-path (append ffap-c-path user-include-dirs)))

;; misc
(setq inhibit-startup-message t)
(require 'generic-x nil 'noerror)
(setq ring-bell-function 'ignore)
(auto-image-file-mode t)
(global-auto-revert-mode t)
;; (setq require-final-newline 'ask)
(setq-default indicate-buffer-boundaries (quote left))
(when (fboundp 'global-linum-mode)
  (global-linum-mode 1))

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

;; (defun move-line-up (p)
;;   "Move current line up, copy from crazycool@smth"
;;   (interactive "*p")
;;   (let ((c (current-column)))
;;     (beginning-of-line)
;;     (kill-line 1)
;;     (previous-line p)
;;     (beginning-of-line)
;;     (yank)
;;     (previous-line 1)
;;     (move-to-column c)))

;; (defun move-line-down (p)
;;   "Move current line down, copy from crazycool@smth"
;;   (interactive "*p")
;;   (let ((c (current-column)))
;;     (beginning-of-line)
;;     (kill-line 1)
;;     (next-line p)
;;     (beginning-of-line)
;;     (yank)
;;     (previous-line 1)
;;     (move-to-column c)))

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
    (when (or is-prompt (= (length word) 0))
      (setq word (read-regexp "List lines matching regexp" word)))
    (moccur-word-all-buffers word)))

(autoload 'grep-tag-default "grep")
(defun grep-current-dir (&optional is-prompt wd)
  "Run `grep' to find current word in current directory."
  (interactive "P")
  (let* ((word (or wd (grep-tag-default)))
         (cmd (concat "grep -inrI '" word "' ."
                      (if (eq system-type 'windows-nt)
                          nil
                        " | grep -vE '\.svn/|\.git/|\.hg/|\.bzr/|CVS/'"))))
    (if (or is-prompt (= (length word) 0))
        (grep (read-shell-command
               "Run grep (like this): " cmd 'grep-history))
      (if (= 0 (length word))
          (message "Word is blank.")
        (grep cmd)))))

(defun switch-to-other-buffer ()
  "Switch to (other-buffer)."
  (interactive)
  (switch-to-buffer (other-buffer)))
(defadvice switch-to-other-buffer (after pulse-advice activate)
  "After switch-to-other-buffer, pulse the line the cursor lands on."
  (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
             (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

;; global key bindings
;; (global-set-key (kbd "<M-up>") 'move-line-up)
;; (global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key (kbd "<select>") 'move-end-of-line) ; for putty
(global-set-key (kbd "C-=") 'align)
(global-set-key (kbd "C-S-u") 'upcase-region)
(global-set-key (kbd "C-S-l") 'downcase-region)
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "ESC M-;") 'comment-or-uncomment-region) ; putty
(global-set-key [M-f8] 'format-region)
(global-set-key (kbd "ESC <f8>") 'format-region) ; putty
(global-set-key (kbd "C-S-f") 'format-region)
(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key [(control tab)] 'switch-to-other-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key [f4] 'next-error)
(global-set-key [S-f4] 'previous-error)
(global-set-key [C-f4] 'kill-this-buffer)
(global-set-key (kbd "ESC <f4>") 'kill-this-buffer) ; putty
(global-set-key [f6] 'grep-current-dir)
(global-set-key [C-f6] 'moccur-all-buffers)
(global-set-key [M-f6]
                '(lambda () (interactive) (grep-current-dir nil "TODO")))
(global-set-key (kbd "ESC <f6>") (key-binding [M-f6]))
(global-set-key [C-M-f6]
                '(lambda () (interactive) (moccur-word-all-buffers "TODO")))
(global-set-key (kbd "ESC <C-f6>") (key-binding [C-M-f6]))
(global-set-key [f7] '(lambda () (interactive) (compile compile-command)))


;;; special mode setting

(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

(setq org-log-done 'time)
(add-hook 'org-mode-hook
          (lambda ()
            (imenu-add-menubar-index)
            (setq comment-start nil)
            (auto-fill-mode t)))
(eval-after-load "org"
  `(progn
     (define-key org-mode-map [(control tab)] nil)
     (define-key org-mode-map (kbd "<C-S-tab>") 'org-force-cycle-archived)))

(when (fboundp 'nxml-mode)
  (add-to-list 'auto-mode-alist
               '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (add-hook 'nxml-mode-hook
            '(lambda ()
               (require 'sgml-mode)
               (set-syntax-table sgml-mode-syntax-table))))

(defadvice artist-coord-win-to-buf (before tabbar-mode activate compile)
  (if tabbar-mode (setq coord (cons (car coord) (1- (cdr coord))))))

(defun program-common-function ()
  (setq indent-tabs-mode nil)
  ;; (local-set-key (kbd "<return>") 'newline-and-indent)
  (when (fboundp 'whitespace-mode)
    (whitespace-mode t))
  ;; (hs-minor-mode t)
  (imenu-add-menubar-index))

(add-hook 'c-mode-common-hook 'program-common-function)

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

(add-hook 'makefile-mode-hook 'imenu-add-menubar-index)

(when (fboundp 'whitespace-mode)
  (add-hook 'makefile-mode-hook (lambda () (whitespace-mode 1)))
  (add-hook 'autoconf-mode-hook (lambda () (whitespace-mode 1))))

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

(defun gud-break-or-remove (&optional force-remove)
  "Set/clear breakpoin."
  (interactive "P")
  (save-excursion
    (if (or force-remove
            (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint))
        (gud-remove nil)
      (gud-break nil))))

(defun gud-enable-or-disable ()
  "Enable/disable breakpoint."
  (interactive)
  (let ((obj))
    (save-excursion
      (move-beginning-of-line nil)
      (dolist (overlay (overlays-in (point) (point)))
        (when (overlay-get overlay 'put-break)
          (setq obj (overlay-get overlay 'before-string))))
      (if  (and obj (stringp obj))
          (cond ((featurep 'gdb-ui)
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
                ((featurep 'gdb-mi)
                 (gud-basic-call
                  (concat
                   (if (get-text-property 0 'gdb-enabled obj)
                       "-break-disable "
                     "-break-enable ")
                   (get-text-property 0 'gdb-bptno obj))))
                (t (error "No gud-ui or gui-mi?")))
        (message "May be there isn't have a breakpoint.")))))

(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  ;; (set-process-query-on-exit-flag (get-buffer-process gud-comint-buffer) nil)
  ;; (kill-buffer gud-comint-buffer))
  (dolist (buffer '(gdba gdb-stack-buffer gdb-breakpoints-buffer
                         gdb-threads-buffer gdb-inferior-io
                         gdb-registers-buffer gdb-memory-buffer
                         gdb-locals-buffer gdb-assembler-buffer))
    (when (gdb-get-buffer buffer)
      (let ((proc (get-buffer-process (gdb-get-buffer buffer))))
        (when proc (set-process-query-on-exit-flag proc nil)))
      (kill-buffer (gdb-get-buffer buffer)))))

(defadvice gdb (before ecb-deactivate activate)
  "if ecb activated, deactivate it."
  (when (and (boundp 'ecb-minor-mode) (ecb-minor-mode))
    (ecb-deactivate)))

;; (defun gdb-tooltip-hook ()
;;   (gud-tooltip-mode 1)
;;   (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
;;     (when process
;;       (set-process-sentinel process
;;                             (lambda (proc change)
;;                               (let ((status (process-status proc)))
;;                                 (when (or (eq status 'exit)
;;                                           (eq status 'signal))
;;                                   (gud-tooltip-mode -1))))))))
;; (add-hook 'gdb-mode-hook 'gdb-tooltip-hook)
(add-hook 'gdb-mode-hook (lambda () (gud-tooltip-mode 1)))
(defadvice gud-kill-buffer-hook (after gud-tooltip-mode activate)
  "After gdb killed, disable gud-tooltip-mode."
  (gud-tooltip-mode -1))

(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
;; (gud-tooltip-mode t)
(define-key c-mode-base-map [f5] 'gdb)
(define-key gud-minor-mode-map [f5] 'gud-go)
(define-key gud-minor-mode-map [S-f5] 'gud-kill)
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

;; buildin cedet
(when (and (fboundp 'semantic-mode)
           (not (locate-library "semantic-ctxt"))) ; can't found offical cedet
  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    global-semantic-idle-summary-mode
                                    global-semantic-mru-bookmark-mode))
  (semantic-mode 1)
  (global-semantic-highlight-edits-mode (if window-system 1 -1))
  (global-semantic-show-unmatched-syntax-mode 1)
  (global-semantic-show-parser-state-mode 1)
  (global-ede-mode 1)

  (require 'semantic/bovine/c nil 'noerror)
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        user-include-dirs)
  (dolist (file c-preprocessor-symbol-files)
    (when (file-exists-p file)
      (setq semantic-lex-c-preprocessor-symbol-file
            (append semantic-lex-c-preprocessor-symbol-file (list file)))))

  (require 'semantic/analyze/refs)      ; for semantic-ia-fast-jump
  (defadvice push-mark (around semantic-mru-bookmark activate)
    "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
    (semantic-mrub-push semantic-mru-bookmark-ring
                        (point)
                        'mark)
    ad-do-it)
  (defun semantic-ia-fast-jump-back ()
    (interactive)
    (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
        (error "Semantic Bookmark ring is currently empty"))
    (let* ((ring (oref semantic-mru-bookmark-ring ring))
           (alist (semantic-mrub-ring-to-assoc-list ring))
           (first (cdr (car alist))))
      (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
          (setq first (cdr (car (cdr alist)))))
      (semantic-mrub-switch-tags first)))
  (defun semantic-ia-fast-jump-or-back (&optional back)
    (interactive "P")
    (if back
        (semantic-ia-fast-jump-back)
      (semantic-ia-fast-jump (point))))

  (define-key semantic-mode-map [f12] 'semantic-ia-fast-jump-or-back)
  (define-key semantic-mode-map [C-f12] 'semantic-ia-fast-jump-or-back)
  (define-key semantic-mode-map [S-f12] 'semantic-ia-fast-jump-back)
  ;; (define-key semantic-mode-map [S-f12] 'pop-global-mark)
  (define-key semantic-mode-map [M-S-f12] 'semantic-analyze-proto-impl-toggle)
  (define-key semantic-mode-map (kbd "C-c , ,") 'semantic-force-refresh)

  (autoload 'pulse-momentary-highlight-one-line "pulse" "" nil)
  (autoload 'pulse-line-hook-function "pulse" "" nil)
  (setq pulse-command-advice-flag (if window-system 1 nil))
  (defadvice goto-line (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice exchange-point-and-mark (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p)
               (> (abs (- (point) (mark))) 400))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice find-tag (after pulse-advice activate)
    "After going to a tag, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice tags-search (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice tags-loop-continue (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice pop-tag-mark (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice imenu-default-goto-function (after pulse-advice activate)
    "After going to a tag, pulse the line the cursor lands on."
    (when pulse-command-advice-flag
      (pulse-momentary-highlight-one-line (point))))
  (defadvice cua-exchange-point-and-mark (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p)
               (> (abs (- (point) (mark))) 400))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice switch-to-buffer (after pulse-advice activate)
    "After switch-to-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice previous-buffer (after pulse-advice activate)
    "After previous-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice next-buffer (after pulse-advice activate)
    "After next-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice ido-switch-buffer (after pulse-advice activate)
    "After ido-switch-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice beginning-of-buffer (after pulse-advice activate)
    "After beginning-of-buffer, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (add-hook 'next-error-hook 'pulse-line-hook-function))

(provide 'init-basic)
