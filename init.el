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

(add-to-list 'load-path
             (expand-file-name "init"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))

(require 'init-custom)
(require 'init-path)
(require 'init-package)
(require 'init-editor)
(require 'init-keybinding)

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
(require 'init-helm)
(require 'init-hideshow)
(require 'init-highlight)
(require 'init-leim)
(require 'init-server)
(require 'init-tabbar)
(require 'init-toolbar)
(require 'init-vc)
(require 'init-yasnippet)

;; progmodes
(require 'init-prog)
(require 'init-cc)
(require 'init-csharp)
(require 'init-emacs-lisp)
(require 'init-go)
(require 'init-java)
(require 'init-javascript)
(require 'init-perl)
(require 'init-php)
(require 'init-python)
(require 'init-rust)
(require 'init-sh)
(require 'init-sql)

;; textmodes
(require 'init-artist)
(require 'init-org)
(require 'init-text)
(require 'init-xml)

(provide 'init)
