;;; ecb-autogen.el --- Auto load statement generator

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2003

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id$

;;; Commentary:
;;
;; Automatically generate autoloads for ECB
;;
;; This code is based onto semantic-autogen.el, the autoload generator of
;; semantic.
;;

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code
;;

(require 'loaddefs-gen)
(require 'ecb-util)

(eval-when-compile
  (require 'silentcomp))


(when (ecb-noninteractive)
  ;; If the user is doing this non-interactively, we need to set up
  ;; these conveniences.
  (add-to-list 'load-path nil)
  (set (if (boundp 'find-file-hook)
           'find-file-hook
         'find-file-hooks) nil)
  (setq find-file-suppress-same-file-warnings t)
  )


(defconst ecb-autogen-header
  "Auto-generated ecb autoloads"
  "Header of the auto-generated autoloads file.")

(defconst ecb-autogen-file "ecb-autoloads.el"
  "Name of the auto-generated autoloads file.")

(defconst ecb-autoload-feature "ecb-autoloads"
  "Feature-name of the autoloads")

(defvar ecb-autogen-subdirs nil
  "Sub-directories to scan for autoloads.")

(defun ecb-update-autoloads ()
  "Update ecb autoloads from sources.
Autoloads file name is defined in variable `ecb-autogen-file'."
  (interactive)

  (if (file-exists-p (expand-file-name ecb-autogen-file))
      (delete-file (expand-file-name ecb-autogen-file)))
    (loaddefs-generate "." ecb-autogen-file))


(silentcomp-provide 'ecb-autogen)

;;; ecb-autogen.el ends here
