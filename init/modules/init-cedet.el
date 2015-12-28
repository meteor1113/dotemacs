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

;; cedet
(dolist (dir (mapcar 'expand-file-name '("~/.emacs.d/cedet-1.0pre6"
                                         "~/.emacs.d/cedet-1.0pre7"
                                         "~/.emacs.d/cedet-1.0"
                                         "~/.emacs.d/cedet-1.1")))
  (add-to-list 'load-path dir)
  (let ((default-directory dir))
    (ignore-errors (normal-top-level-add-subdirs-to-load-path))))

;; buildin
(when (and (fboundp 'semantic-mode)
           (not (locate-library "semantic-ctxt")))
  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    global-semantic-idle-summary-mode
                                    global-semantic-mru-bookmark-mode))
  (semantic-mode 1)
  (require 'semantic/decorate/include nil 'noerror)
  (require 'semantic/bovine/el nil 'noerror)
  (require 'semantic/analyze/refs))

;; standalone
(when (and (or (not (boundp 'semantic-mode))
               (and (boundp 'semantic-mode) (null semantic-mode)))
           (locate-library "semantic-ctxt")
           (require 'cedet nil 'noerror))
  ;; (semantic-load-enable-minimum-features)
  (semantic-load-enable-code-helpers)
  ;; (semantic-load-enable-gaudy-code-helpers)
  ;; (semantic-load-enable-excessive-code-helpers)
  ;; (semantic-load-enable-semantic-debugging-helpers)
  ;; (global-srecode-minor-mode 1)
  (require 'semantic-decorate-include nil 'noerror)
  (ignore-errors (semantic-load-enable-primary-exuberent-ctags-support)))

(when (featurep 'cedet)
  (global-semantic-decoration-mode 1)
  (semantic-toggle-decoration-style "semantic-tag-boundary" -1)
  ;; (global-semantic-highlight-edits-mode (if window-system 1 -1))
  (global-semantic-show-unmatched-syntax-mode 1)
  (global-semantic-show-parser-state-mode 1)
  (global-ede-mode 1)

  (when (executable-find "global")
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)
    (setq ede-locate-setup-options '(ede-locate-global ede-locate-base)))

  (when (and (eq system-type 'windows-nt)
             (executable-find "gcc"))
    (if (and (boundp 'semantic-mode) semantic-mode)
        (require 'semantic/bovine/c nil 'noerror)
      (require 'semantic-c nil 'noerror))
    (ignore-errors (semantic-gcc-setup)))

  ;; (defvar semantic-dependency-include-paths
  ;;   '("." "./include" "./inc" "./common" "./public"
  ;;     ".." "../include" "../inc" "../common" "../public"
  ;;     "../.." "../../include" "../../inc" "../../common" "../../public"
  ;;     "C:/MinGW/include"
  ;;     "C:/MinGW/include/c++/3.4.5"
  ;;     "C:/MinGW/include/c++/3.4.5/mingw32"
  ;;     "C:/MinGW/include/c++/3.4.5/backward"
  ;;     "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
  ;;     "C:/Program Files/Microsoft Visual Studio/VC98/Include"
  ;;     "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"
  ;;     ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
  ;;     )
  ;;   "User include dirs for c/c++ mode")
  ;; (mapc (lambda (dir)
  ;;         (semantic-add-system-include dir 'c++-mode)
  ;;         (semantic-add-system-include dir 'c-mode))
  ;;       semantic-dependency-include-paths)

  ;; (defvar c-preprocessor-symbol-files
  ;;   '("C:/MinGW/include/c++/3.4.5/mingw32/bits/c++config.h"
  ;;     "C:/Program Files/Microsoft Visual Studio/VC98/Include/xstddef"
  ;;     ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include/yvals.h"
  ;;     ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include/crtdefs.h"
  ;;     )
  ;;   "Preprocessor symbol files for cedet")
  ;; (dolist (file c-preprocessor-symbol-files)
  ;;   (when (file-exists-p file)
  ;;     (setq semantic-lex-c-preprocessor-symbol-file
  ;;           (append semantic-lex-c-preprocessor-symbol-file (list file)))))

  (defun semantic-ia-fast-jump-back ()
    (interactive)
    (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
        (error "Semantic Bookmark ring is currently empty"))
    (let* ((ring (oref semantic-mru-bookmark-ring ring))
           (alist (semantic-mrub-ring-to-assoc-list ring))
           (first (cdr (car alist))))
      ;; (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
      ;;     (setq first (cdr (car (cdr alist)))))
      (semantic-mrub-visit first)
      (ring-remove ring 0)))

  (defun semantic-ia-fast-jump-or-back (&optional back)
    (interactive "P")
    (if back
        (semantic-ia-fast-jump-back)
      (semantic-ia-fast-jump (point))))

  (global-set-key [f12] 'semantic-ia-fast-jump-or-back)
  (global-set-key [C-f12] 'semantic-ia-fast-jump-or-back)
  (global-set-key [S-f12] 'semantic-ia-fast-jump-back)
  (global-set-key [mouse-2] 'semantic-ia-fast-mouse-jump)
  (global-set-key [S-mouse-2] 'semantic-ia-fast-jump-back)
  (global-set-key [double-mouse-2] 'semantic-ia-fast-jump-back)
  (global-set-key [M-S-f12] 'semantic-analyze-proto-impl-toggle)

  (setq pulse-command-advice-flag t)    ; (if window-system 1 nil)
  (require 'pulse)
  (defadvice cua-exchange-point-and-mark (after pulse-advice activate)
    (when (and (called-interactively-p) (> (abs (- (point) (mark))) 400))
      (pulse-line-hook-function)))
  (defadvice switch-to-buffer (after pulse-advice activate)
    (pulse-line-hook-function))
  (defadvice previous-buffer (after pulse-advice activate)
    (pulse-line-hook-function))
  (defadvice next-buffer (after pulse-advice activate)
    (pulse-line-hook-function))
  (defadvice ido-switch-buffer (after pulse-advice activate)
    (pulse-line-hook-function))
  (defadvice beginning-of-buffer (after pulse-advice activate)
    (when (called-interactively-p) (pulse-line-hook-function)))
;;   (defmacro cedet-advise-commands (class advice-name commands &rest body)
;;     "Apply advice named ADVICE-NAME to multiple COMMANDS.
;; The body of the advice is in BODY."
;;     `(progn
;;        ,@(mapcar (lambda (command)
;;                    `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
;;                       ,@body))
;;                  commands)))

;;   (cedet-advise-commands after
;;                          "pulse-advice"
;;                          (cua-exchange-point-and-mark
;;                           switch-to-buffer
;;                           previous-buffer
;;                           next-buffer
;;                           ido-switch-buffer
;;                           beginning-of-buffer)
;;                          (pulse-line-hook-function))

  (when (and (boundp 'semantic-mode) semantic-mode)
    (unless (executable-find "python")
      (remove-hook 'python-mode-hook 'wisent-python-default-setup)
      (setq semantic-new-buffer-setup-functions
            (delete (assq 'python-mode semantic-new-buffer-setup-functions)
                    semantic-new-buffer-setup-functions)))

    (defadvice push-mark (around semantic-mru-bookmark activate)
      "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
      (semantic-mrub-push semantic-mru-bookmark-ring
                          (point)
                          'mark)
      ad-do-it)

    (defadvice goto-line (after pulse-advice activate)
      (when (called-interactively-p) (pulse-line-hook-function)))
    (defadvice find-tag (after pulse-advice activate)
      (when (called-interactively-p) (pulse-line-hook-function)))
    (defadvice tags-search (after pulse-advice activate)
      (when (called-interactively-p) (pulse-line-hook-function)))
    (defadvice tags-loop-continue (after pulse-advice activate)
      (when (called-interactively-p) (pulse-line-hook-function)))
    (defadvice pop-tag-mark (after pulse-advice activate)
      (when (called-interactively-p) (pulse-line-hook-function)))
    (defadvice imenu-default-goto-function (after pulse-advice activate)
      (when (called-interactively-p) (pulse-line-hook-function)))
    (defadvice exchange-point-and-mark (after pulse-advice activate)
      (when (and (called-interactively-p) (> (abs (- (point) (mark))) 400))
        (pulse-line-hook-function)))
    (add-hook 'next-error-hook 'pulse-line-hook-function)))

(provide 'init-cedet)
