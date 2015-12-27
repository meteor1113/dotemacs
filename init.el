;;; -*- mode: emacs-lisp; coding: utf-8; -*-
;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @date 2015-04-28
;; @URL http://git.oschina.net/meteor1113/dotemacs

(add-to-list 'load-path
             (expand-file-name "init"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))

(require 'init-path)
(require 'init-package)
(require 'init-custom)
(require 'init-editor)
(require 'init-func)
(require 'init-global-keybindings)
(require 'init-ui)
(require 'init-ui-toolbar)

;; modules
(require 'init-auto-complete)
(require 'init-auto-insert)
(require 'init-bm)
(require 'init-calendar)
(require 'init-cedet)
(require 'init-company)
(require 'init-dired)
(require 'init-ecb)
(require 'init-emms)
(require 'init-flycheck)
(require 'init-flymake)
(require 'init-hideshow)
(require 'init-leim)
(require 'init-org)
(require 'init-tabbar)
(require 'init-vc)
(require 'init-yasnippet)

;; textmodes
(require 'init-artist)
(require 'init-csv)
(require 'init-hexl)
(require 'init-markdown)
(require 'init-text)
(require 'init-xml)

;; progmodes
(require 'init-prog-mode)
(require 'init-autoconf)
;; (require 'init-c)
(require 'init-csharp)
;; (require 'init-emacs-lisp)
(require 'init-go)
(require 'init-java)
(require 'init-javascript)
(require 'init-makefile)
;; (require 'init-perl)
(require 'init-php)
;; (require 'init-python)
(require 'init-rust)
;; (require 'init-sh)
(require 'init-shell)
;; (require 'init-sql)

(provide 'init)
