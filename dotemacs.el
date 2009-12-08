;;;
;; Copyright (C) 2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2009-12-06


(let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
       (pdir (file-name-directory (directory-file-name dir)))
       (note-dir (expand-file-name "note" pdir))
       (dirs (list "~/.emacs.d/dotemacs"
                   "~/common/dotemacs"
                   "~/project/common/dotemacs"
                   "~/common/note"
                   "~/project/common/note"
                   dir
                   note-dir)))
  (when (eq system-type 'windows-nt)
    (setq dirs (append dirs '("c:/common/dotemacs"
                              "c:/project/common/dotemacs"
                              "c:/common/note"
                              "c:/project/common/note"
                              "d:/common/dotemacs"
                              "d:/project/common/dotemacs"
                              "d:/common/note"
                              "d:/project/common/note"
                              "e:/common/dotemacs"
                              "e:/project/common/dotemacs"
                              "e:/common/note"
                              "e:/project/common/note"
                              "f:/common/dotemacs"
                              "f:/project/common/dotemacs"
                              "f:/common/note"
                              "f:/project/common/note"))))
  (dolist (path dirs)
    (when (file-exists-p path)
      (add-to-list 'load-path path))))

(require 'init-basic nil t)
(require 'init-site nil t)
(require 'init-misc nil t)
(require 'init-note nil t)


(provide 'dotemacs)
