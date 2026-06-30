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

;;; Code:

(let ((root-dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; load-path
  (let ((lisp-dirs (list (expand-file-name "lisp" root-dir)
                         (expand-file-name "site-lisp" root-dir))))
    (mapc (lambda (dir)
            (add-to-list 'load-path dir)
            (let ((default-directory dir))
              (ignore-errors (normal-top-level-add-subdirs-to-load-path))))
          (mapcar 'expand-file-name lisp-dirs)))

  ;; exec-path
  (let ((path-eparator (if (eq system-type 'windows-nt) ";" ":"))
        (bin-dirs (list (expand-file-name "bin" root-dir))))
    (dolist (dir (mapcar 'expand-file-name bin-dirs))
      (setenv "PATH" (concat dir path-eparator (getenv "PATH")))
      (add-to-list 'exec-path dir 'append)))
  )

;; (require 'init-custom nil 'noerror)
;; (require 'init-path nil 'noerror)
(require 'init-editor nil 'noerror)
(require 'init-keybinding nil 'noerror)
(require 'init-package nil 'noerror)

(require 'init-auto-complete nil 'noerror)
(require 'init-auto-insert nil 'noerror)
(require 'init-bm nil 'noerror)
(require 'init-calendar nil 'noerror)
;; (require 'init-cedet nil 'noerror)
(require 'init-company nil 'noerror)
(require 'init-dired nil 'noerror)
(require 'init-ecb nil 'noerror)
(require 'init-emms nil 'noerror)
(require 'init-fci nil 'noerror)
(require 'init-flycheck nil 'noerror)
(require 'init-font nil 'noerror)
(require 'init-helm nil 'noerror)
(require 'init-hideshow nil 'noerror)
(require 'init-highlight nil 'noerror)
(require 'init-leim nil 'noerror)
;; (require 'init-server nil 'noerror)
(require 'init-tabbar nil 'noerror)
(require 'init-toolbar nil 'noerror)
(require 'init-vc nil 'noerror)
(require 'init-yasnippet nil 'noerror)

;; progmodes
(require 'init-prog nil 'noerror)
(require 'init-cc nil 'noerror)
(require 'init-csharp nil 'noerror)
(require 'init-emacs-lisp nil 'noerror)
(require 'init-gdb nil 'noerror)
(require 'init-go nil 'noerror)
(require 'init-haskell nil 'noerror)
(require 'init-java nil 'noerror)
(require 'init-javascript nil 'noerror)
(require 'init-perl nil 'noerror)
(require 'init-php nil 'noerror)
(require 'init-python nil 'noerror)
(require 'init-rust nil 'noerror)
(require 'init-sh nil 'noerror)
(require 'init-sql nil 'noerror)

;; textmodes
(require 'init-artist nil 'noerror)
(require 'init-org nil 'noerror)
(require 'init-text nil 'noerror)
(require 'init-xml nil 'noerror)

(setq custom-file (locate-user-emacs-file "emacs-custom.el"))
(load custom-file 'noerror)

(provide 'init-emacs)

;;; init-emacs.el ends here
