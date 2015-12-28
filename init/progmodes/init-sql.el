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

;; sql-mode
(add-to-list 'auto-mode-alist '("\\.[pP][rR][cC]\\'" . sql-mode))
(add-hook 'sql-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (ignore-errors (whitespace-mode t))
             (linum-mode 1)
             ;; (or (ignore-errors (hideshowvis-minor-mode t)) (hs-minor-mode t))
             (hs-minor-mode t)
             (ignore-errors (imenu-add-menubar-index))))

;; plsql
(autoload 'plsql-mode "plsql" nil t)
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.\\(p\\(?:k[bg]\\|ls\\)\\|[sS][qQ][lL]\\|[pP][rR][cC]\\)\\'"
;;           . plsql-mode))
;;        auto-mode-alist))

;; sql-indent
(unless (functionp 'syntax-ppss)
  (defun syntax-ppss (&optional pos)
    (parse-partial-sexp (point-min) (or pos (point)))))
(eval-after-load "sql"
  '(require 'sql-indent nil 'noerror))

;; sqlplus
;; (autoload 'sqlplus "sqlplus" nil t)
(setq sqlplus-session-cache-dir "~/.emacs.d/sqlplus")
(add-hook 'after-init-hook
          '(lambda ()
             (if (executable-find "sqlplus")
                 (require 'sqlplus nil t))))
(eval-after-load "sqlplus"
  '(progn
     (define-key plsql-mode-map [C-down-mouse-1] nil)
     (define-key plsql-mode-map [C-mouse-1] nil)
     (define-key plsql-mode-map [down-mouse-2] 'sqlplus-mouse-select-identifier)
     (define-key plsql-mode-map [mouse-2] 'sqlplus-file-get-source-mouse)
     (define-key sqlplus-mode-map [C-down-mouse-1] nil)
     (define-key sqlplus-mode-map [C-mouse-1] nil)
     (define-key sqlplus-mode-map [down-mouse-2]
       'sqlplus-mouse-select-identifier)
     (define-key sqlplus-mode-map [mouse-2] 'sqlplus-file-get-source-mouse)))
(add-hook 'sqlplus-mode-hook
          (lambda ()
            ;; (setq minor-mode-overriding-map-alist
            ;;       (list (cons 'cua-mode
            ;;                   (let ((map (make-sparse-keymap)))
            ;;                     (define-key map [C-return]
            ;;                       'sqlplus-send-current)
            ;;                     map))))
            ;; (setq overriding-local-map
            ;;       (let ((map (make-sparse-keymap)))
            ;;         (define-key map [C-return] 'sqlplus-send-current)
            ;;         map))
            (when (string-match "chinese" (or (getenv "NLS_LANG") ""))
              (set-process-coding-system
               (get-process (sqlplus-get-process-name sqlplus-connect-string))
               'gbk
               'gbk))))

(provide 'init-sql)
