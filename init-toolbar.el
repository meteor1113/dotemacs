;;; -*- coding: gbk -*-
;; Copyright (C) 2010 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2010-05-29
;; @URL http://github.com/meteor1113/dotemacs


;; Remove the some buttons in the tool bar.
(when (boundp 'tool-bar-map)
  (defvar need-delete-toolbar-buttons '(print-buffer))
  (let ((need-delete-btns))
    (dolist (button tool-bar-map)
      (when (and (consp button)
                 (memq (car button) need-delete-toolbar-buttons))
        (add-to-list 'need-delete-btns button)))
    (dolist (button need-delete-btns)
      (delq button tool-bar-map))))

;; image-load-path
(let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
       (images-dir (expand-file-name "images" dir)))
  (if (boundp 'image-load-path)         ; emacs-21 isn't have image-load-path
      (add-to-list 'image-load-path images-dir)
    (add-to-list 'load-path images-dir)))

(setq tool-bar-button-margin 0)
;; (setq auto-resize-tool-bars nil)

(defgroup toolbarshow nil
  "Custom toolbar"
  :group 'environment)

;; (defvar toolbarshow-edit t
;;   "If show edit toolbar.")
(defcustom toolbarshow-edit t
  "If show edit toolbar."
  :type 'boolean
  :group 'toolbarshow)
(defun toolbarshow-toggle-edit ()
  "Turn edit toolbar on/off."
  (interactive)
  (setq toolbarshow-edit (if toolbarshow-edit nil t))
  (force-window-update)
  (customize-mark-to-save 'toolbarshow-edit)
  (custom-save-all))

(defcustom toolbarshow-search nil
  "If show search toolbar."
  :type 'boolean
  :group 'toolbarshow)
(defun toolbarshow-toggle-search ()
  "Turn search toolbar on/off."
  (interactive)
  (setq toolbarshow-search (if toolbarshow-search nil t))
  (force-window-update)
  (customize-mark-to-save 'toolbarshow-search)
  (custom-save-all))

(defcustom toolbarshow-bookmark nil
  "If show bookmark toolbar."
  :type 'boolean
  :group 'toolbarshow)
(defun toolbarshow-toggle-bookmark ()
  "Turn bookmark toolbar on/off."
  (interactive)
  (setq toolbarshow-bookmark (if toolbarshow-bookmark nil t))
  (force-window-update)
  (customize-mark-to-save 'toolbarshow-bookmark)
  (custom-save-all))

(defcustom toolbarshow-view t
  "If show view toolbar."
  :type 'boolean
  :group 'toolbarshow)
(defun toolbarshow-toggle-view ()
  "Turn view toolbar on/off."
  (interactive)
  (setq toolbarshow-view (if toolbarshow-view nil t))
  (force-window-update)
  (customize-mark-to-save 'toolbarshow-view)
  (custom-save-all))

(defcustom toolbarshow-program t
  "If show program toolbar."
  :type 'boolean
  :group 'toolbarshow)
(defun toolbarshow-toggle-program ()
  "Turn program toolbar on/off."
  (interactive)
  (setq toolbarshow-program (if toolbarshow-program nil t))
  (force-window-update)
  (customize-mark-to-save 'toolbarshow-program)
  (custom-save-all))

(defcustom toolbarshow-remember nil
  "If show remember toolbar."
  :type 'boolean
  :group 'toolbarshow)
(defun toolbarshow-toggle-remember ()
  "Turn remember toolbar on/off."
  (interactive)
  (setq toolbarshow-remember (if toolbarshow-remember nil t))
  (force-window-update)
  (customize-mark-to-save 'toolbarshow-remember)
  (custom-save-all))

(defcustom toolbarshow-emms nil
  "If show emms toolbar."
  :type 'boolean
  :group 'toolbarshow)
(defun toolbarshow-toggle-emms ()
  "Turn emms toolbar on/off."
  (interactive)
  (setq toolbarshow-emms (if toolbarshow-emms nil t))
  (force-window-update)
  (customize-mark-to-save 'toolbarshow-emms)
  (custom-save-all))

;; toggle toolbar menu
(defvar toggle-toolbar-menu (make-sparse-keymap "Toolbar"))
(define-key toggle-toolbar-menu [find-dotemacs-file]
  '(menu-item "Open .emacs" find-dotemacs-file
              :enable (fboundp 'find-dotemacs-file)
              :help "Open .emacs file"))
(define-key toggle-toolbar-menu [emms]
  '(menu-item "Emms"
              (lambda ()
                (interactive)
                (if (fboundp 'emms-dir-tree)
                    (emms-dir-tree)
                  (emms)))
              :enable (or (fboundp 'emms-dir-tree) (fboundp 'emms))
              :help "Emms"))
(define-key toggle-toolbar-menu [separatore-dotemacs-file]
  '(menu-item "--"))
(define-key toggle-toolbar-menu [xml-mode]
  '(menu-item "XML" nxml-mode
              :visible (fboundp 'nxml-mode)
              :button (:toggle . (eq major-mode 'nxml-mode))))
(define-key toggle-toolbar-menu [text-mode]
  '(menu-item "Text" text-mode
              :visible (fboundp 'text-mode)
              :button (:toggle . (eq major-mode 'text-mode))))
(define-key toggle-toolbar-menu [tcl-mode]
  '(menu-item "Tcl" tcl-mode
              :visible (fboundp 'tcl-mode)
              :button (:toggle . (eq major-mode 'tcl-mode))))
(define-key toggle-toolbar-menu [sql-mode]
  '(menu-item "SQL" sql-mode
              :visible (fboundp 'sql-mode)
              :button (:toggle . (eq major-mode 'sql-mode))))
(define-key toggle-toolbar-menu [sh-mode]
  '(menu-item "Shell" sh-mode
              :visible (fboundp 'sh-mode)
              :button (:toggle . (eq major-mode 'sh-mode))))
(define-key toggle-toolbar-menu [scheme-mode]
  '(menu-item "Scheme" scheme-mode
              :visible (fboundp 'scheme-mode)
              :button (:toggle . (eq major-mode 'scheme-mode))))
(define-key toggle-toolbar-menu [ruby-mode]
  '(menu-item "Ruby" ruby-mode
              :visible (fboundp 'ruby-mode)
              :button (:toggle . (eq major-mode 'ruby-mode))))
(define-key toggle-toolbar-menu [rst-mode]
  '(menu-item "ReST" rst-mode
              :visible (fboundp 'rst-mode)
              :button (:toggle . (eq major-mode 'rst-mode))))
(define-key toggle-toolbar-menu [python-mode]
  '(menu-item "Python" python-mode
              :visible (fboundp 'python-mode)
              :button (:toggle . (eq major-mode 'python-mode))))
(define-key toggle-toolbar-menu [php-mode]
  '(menu-item "PHP" php-mode
              :visible (fboundp 'php-mode)
              :button (:toggle . (eq major-mode 'php-mode))))
(defvar perl-sub-mode-menu (make-sparse-keymap "Perl"))
(define-key perl-sub-mode-menu [cperl-mode]
  '(menu-item "CPerl" cperl-mode
              :visible (fboundp 'cperl-mode)
              :button (:toggle . (eq major-mode 'cperl-mode))))
(define-key perl-sub-mode-menu [perl-mode]
  '(menu-item "Perl" perl-mode
              :visible (fboundp 'perl-mode)
              :button (:toggle . (eq major-mode 'perl-mode))))
(define-key toggle-toolbar-menu [perl]
  (list 'menu-item "Perl" perl-sub-mode-menu))
(define-key toggle-toolbar-menu [pascal-mode]
  '(menu-item "Pascal" pascal-mode
              :visible (fboundp 'pascal-mode)
              :button (:toggle . (eq major-mode 'pascal-mode))))
(define-key toggle-toolbar-menu [org-mode]
  '(menu-item "Org" org-mode
              :visible (fboundp 'org-mode)
              :button (:toggle . (eq major-mode 'org-mode))))
(define-key toggle-toolbar-menu [objc-mode]
  '(menu-item "ObjC" objc-mode
              :visible (fboundp 'objc-mode)
              :button (:toggle . (eq major-mode 'objc-mode))))
(defvar makefile-sub-mode-menu (make-sparse-keymap "Makefile"))
(define-key makefile-sub-mode-menu [makefile-makepp-mode]
  '(menu-item "Makepp" makefile-makepp-mode
              :visible (fboundp 'makefile-makepp-mode)
              :button (:toggle . (eq major-mode 'makefile-makepp-mode))))
(define-key makefile-sub-mode-menu [makefile-imake-mode]
  '(menu-item "Imake" makefile-imake-mode
              :visible (fboundp 'makefile-imake-mode)
              :button (:toggle . (eq major-mode 'makefile-imake-mode))))
(define-key makefile-sub-mode-menu [makefile-mode]
  '(menu-item "Classic" makefile-mode
              :visible (fboundp 'makefile-mode)
              :button (:toggle . (eq major-mode 'makefile-mode))))
(define-key makefile-sub-mode-menu [makefile-bsdmake-mode]
  '(menu-item "BSD" makefile-bsdmake-mode
              :visible (fboundp 'makefile-bsdmake-mode)
              :button (:toggle . (eq major-mode 'makefile-bsdmake-mode))))
(define-key makefile-sub-mode-menu [makefile-automake-mode]
  '(menu-item "Automake" makefile-automake-mode
              :visible (fboundp 'makefile-automake-mode)
              :button (:toggle . (eq major-mode 'makefile-automake-mode))))
(define-key makefile-sub-mode-menu [makefile-gmake-mode]
  '(menu-item "GNU make" makefile-gmake-mode
              :visible (fboundp 'makefile-gmake-mode)
              :button (:toggle . (eq major-mode 'makefile-gmake-mode))))
(define-key toggle-toolbar-menu [makefile]
  (list 'menu-item "Makefile" makefile-sub-mode-menu))
(define-key toggle-toolbar-menu [lisp-mode]
  '(menu-item "Lisp" lisp-mode
              :visible (fboundp 'lisp-mode)
              :button (:toggle . (eq major-mode 'lisp-mode))))
(define-key toggle-toolbar-menu [latex-mode]
  '(menu-item "LaTeX" latex-mode
              :visible (fboundp 'latex-mode)
              :button (:toggle . (eq major-mode 'latex-mode))))
(define-key toggle-toolbar-menu [js-mode]
  '(menu-item "Javascript" js-mode
              :visible (fboundp 'js-mode)
              :button (:toggle . (eq major-mode 'js-mode))))
(define-key toggle-toolbar-menu [java-mode]
  '(menu-item "Java" java-mode
              :visible (fboundp 'java-mode)
              :button (:toggle . (eq major-mode 'java-mode))))
(define-key toggle-toolbar-menu [html-mode]
  '(menu-item "HTML" html-mode
              :visible (fboundp 'html-mode)
              :button (:toggle . (eq major-mode 'html-mode))))
(defvar fortran-sub-mode-menu (make-sparse-keymap "Fortran"))
(define-key fortran-sub-mode-menu [f90-mode]
  '(menu-item "F90" f90-mode
              :visible (fboundp 'f90-mode)
              :button (:toggle . (eq major-mode 'f90-mode))))
(define-key fortran-sub-mode-menu [fortran-mode]
  '(menu-item "Fortran" fortran-mode
              :visible (fboundp 'fortran-mode)
              :button (:toggle . (eq major-mode 'fortran-mode))))
(define-key toggle-toolbar-menu [fortran]
  (list 'menu-item "Fortran" fortran-sub-mode-menu))
(define-key toggle-toolbar-menu [emacs-lisp-mode]
  '(menu-item "Emacs-Lisp" emacs-lisp-mode
              :visible (fboundp 'emacs-lisp-mode)
              :button (:toggle . (eq major-mode 'emacs-lisp-mode))))
(define-key toggle-toolbar-menu [delphi-mode]
  '(menu-item "Delphi" delphi-mode
              :visible (fboundp 'delphi-mode)
              :button (:toggle . (eq major-mode 'delphi-mode))))
(define-key toggle-toolbar-menu [css-mode]
  '(menu-item "CSS" css-mode
              :visible (fboundp 'css-mode)
              :button (:toggle . (eq major-mode 'css-mode))))
(define-key toggle-toolbar-menu [csv-mode]
  '(menu-item "CSV" csv-mode
              :visible (fboundp 'csv-mode)
              :button (:toggle . (eq major-mode 'csv-mode))))
(defvar conf-sub-mode-menu (make-sparse-keymap "Conf"))
(define-key conf-sub-mode-menu [conf-windows-mode]
  '(menu-item "Windows" conf-windows-mode
              :visible (fboundp 'conf-windows-mode)
              :button (:toggle . (eq major-mode 'conf-windows-mode))))
(define-key conf-sub-mode-menu [conf-javaprop-mode]
  '(menu-item "Java properties" conf-javaprop-mode
              :visible (fboundp 'conf-javaprop-mode)
              :button (:toggle . (eq major-mode 'conf-javaprop-mode))))
(define-key conf-sub-mode-menu [conf-space-mode]
  '(menu-item "Space keywords" conf-space-mode
              :visible (fboundp 'conf-space-mode)
              :button (:toggle . (eq major-mode 'conf-space-mode))))
(define-key conf-sub-mode-menu [conf-ppd-mode]
  '(menu-item "PPD" conf-ppd-mode
              :visible (fboundp 'conf-ppd-mode)
              :button (:toggle . (eq major-mode 'conf-ppd-mode))))
(define-key conf-sub-mode-menu [conf-colon-mode]
  '(menu-item "Colon" conf-colon-mode
              :visible (fboundp 'conf-colon-mode)
              :button (:toggle . (eq major-mode 'conf-colon-mode))))
(define-key conf-sub-mode-menu [conf-unix-mode]
  '(menu-item "Unix" conf-unix-mode
              :visible (fboundp 'conf-unix-mode)
              :button (:toggle . (eq major-mode 'conf-unix-mode))))
(define-key conf-sub-mode-menu [conf-xdefaults-mode]
  '(menu-item "Xdefaults" conf-xdefaults-mode
              :visible (fboundp 'conf-xdefaults-mode)
              :button (:toggle . (eq major-mode 'conf-xdefaults-mode))))
(define-key conf-sub-mode-menu [conf-mode]
  '(menu-item "Auto detect..." conf-mode
              :visible (fboundp 'conf-mode)))
(define-key toggle-toolbar-menu [conf]
  (list 'menu-item "Conf" conf-sub-mode-menu))
(define-key toggle-toolbar-menu [csharp-mode]
  '(menu-item "C#" csharp-mode
              :visible (fboundp 'csharp-mode)
              :button (:toggle . (eq major-mode 'csharp-mode))))
(define-key toggle-toolbar-menu [c++-mode]
  '(menu-item "C++" c++-mode
              :visible (fboundp 'c++-mode)
              :button (:toggle . (eq major-mode 'c++-mode))))
(define-key toggle-toolbar-menu [c-mode]
  '(menu-item "C" c-mode
              :visible (fboundp 'c-mode)
              :button (:toggle . (eq major-mode 'c-mode))))
(define-key toggle-toolbar-menu [autoconf-mode]
  '(menu-item "Autoconf" autoconf-mode
              :visible (fboundp 'autoconf-mode)
              :button (:toggle . (eq major-mode 'autoconf-mode))))
(define-key toggle-toolbar-menu [asm-mode]
  '(menu-item "Assembler" asm-mode
              :visible (fboundp 'asm-mode)
              :button (:toggle . (eq major-mode 'asm-mode))))
(define-key toggle-toolbar-menu [ada-mode]
  '(menu-item "Ada" ada-mode
              :visible (fboundp 'ada-mode)
              :button (:toggle . (eq major-mode 'ada-mode))))
(define-key toggle-toolbar-menu [separatore-major-mode]
  '(menu-item "--"))
(define-key toggle-toolbar-menu [toolbarshow-toggle-emms]
  '(menu-item "Emms toolbar" toolbarshow-toggle-emms
              :help "Turn emms toolbar on/off"
              :button (:toggle . toolbarshow-emms)))
(define-key toggle-toolbar-menu [toolbarshow-toggle-remember]
  '(menu-item "Remember toolbar" toolbarshow-toggle-remember
              :help "Turn remember toolbar on/off"
              :button (:toggle . toolbarshow-remember)))
(define-key toggle-toolbar-menu [toolbarshow-toggle-program]
  '(menu-item "Program toolbar" toolbarshow-toggle-program
              :help "Turn program toolbar on/off"
              :button (:toggle . toolbarshow-program)))
(define-key toggle-toolbar-menu [toolbarshow-toggle-view]
  '(menu-item "View toolbar" toolbarshow-toggle-view
              :help "Turn view toolbar on/off"
              :button (:toggle . toolbarshow-view)))
(define-key toggle-toolbar-menu [toolbarshow-toggle-bookmark]
  '(menu-item "Bookmark toolbar" toolbarshow-toggle-bookmark
              :help "Turn bookmark toolbar on/off"
              :button (:toggle . toolbarshow-bookmark)))
(define-key toggle-toolbar-menu [toolbarshow-toggle-search]
  '(menu-item "Search toolbar" toolbarshow-toggle-search
              :help "Turn search toolbar on/off"
              :button (:toggle . toolbarshow-search)))
(define-key toggle-toolbar-menu [toolbarshow-toggle-edit]
  '(menu-item "Edit toolbar" toolbarshow-toggle-edit
              :help "Turn edit toolbar on/off"
              :button (:toggle . toolbarshow-edit)))
;; (global-set-key (kbd "<S-mouse-2>") toggle-toolbar-menu)
;; (define-key-after menu-bar-tools-menu [toggle-toolbar]
;;   (list 'menu-item "Toolbar" toggle-toolbar-menu))
;; (setq tool-bar-map (make-sparse-keymap))
(tool-bar-add-item "pop-menu"
                   (lambda ()
                     (interactive)
                     (popup-menu toggle-toolbar-menu))
                   'toggle-toolbar-menu)

;; edit toolbar
(tool-bar-add-item "separator" nil 'edit-toolbar
                   :visible 'toolbarshow-edit
                   :enable nil)
(tool-bar-add-item "upcase" 'upcase-region 'upcase-region
                   :visible 'toolbarshow-edit
                   :enable '(region-active-p)
                   :help "Convert the region to upper case")
(tool-bar-add-item "downcase" 'downcase-region 'downcase-region
                   :visible 'toolbarshow-edit
                   :enable '(region-active-p)
                   :help "Convert the region to lower case")
(tool-bar-add-item "comment-toggle" 'comment-or-uncomment-region 'comment-toggle
                   :visible 'toolbarshow-edit
                   :enable '(region-active-p)
                   :help "Comment or uncomment region")
(tool-bar-add-item "format-region" 'format-region 'format-region
                   :visible 'toolbarshow-edit
                   :enable '(fboundp 'format-region)
                   :help "Format region or all buffer")

;; search toolbar
(tool-bar-add-item "separator" nil 'search-toolbar
                   :visible 'toolbarshow-search
                   :enable nil)
(tool-bar-add-item "recent-backward"'recent-jump-jump-backward
                   'recent-jump-jump-backward
                   :visible 'toolbarshow-search
                   :enable '(fboundp 'recent-jump-jump-backward)
                   :help "Backward in the history")
(tool-bar-add-item "recent-forward" 'recent-jump-jump-forward
                   'recent-jump-jump-forward
                   :visible 'toolbarshow-search
                   :enable '(fboundp 'recent-jump-jump-forward)
                   :help "Forward in the history")
(tool-bar-add-item "find" 'isearch-forward
                   'isearch-forward
                   :visible 'toolbarshow-search
                   :help "Forward String...")
(tool-bar-add-item "find-next" 'isearch-repeat-forward
                   'isearch-repeat-forward
                   :visible 'toolbarshow-search
                   :help "Repeat Forward String")
(tool-bar-add-item "replace" 'query-replace 'query-replace
                   :visible 'toolbarshow-search
                   :help "Replace String...")

;; bookmark toolbar
(tool-bar-add-item "separator" nil 'bookmark-toolbar
                   :visible 'toolbarshow-bookmark
                   :enable nil)
(tool-bar-add-item "bm-toggle"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-toggle)
                         (bm-toggle)
                       (viss-bookmark-toggle)))
                   'bm-toggle
                   :visible 'toolbarshow-bookmark
                   :help "Toggle bookmark at point")
(tool-bar-add-item "bm-next"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-next)
                         (call-interactively (bm-next))
                       (viss-bookmark-next-buffer)))
                   'bm-next
                   :visible 'toolbarshow-bookmark
                   :help "Goto next bookmark")
(tool-bar-add-item "bm-previous"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-previous)
                         (call-interactively (bm-previous))
                       (viss-bookmark-prev-buffer)))
                   'bm-previous
                   :visible 'toolbarshow-bookmark
                   :help "Goto previous bookmark")
(tool-bar-add-item "bm-clear"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-remove-all-current-buffer)
                         (bm-remove-all-current-buffer)
                       (viss-bookmark-clear-all-buffer)))
                   'bm-remove-all-current-buffer
                   :visible 'toolbarshow-bookmark
                   :help "Delete all visible bookmarks in current buffer")

;; view toolbar
(tool-bar-add-item "separator" nil 'view-toolbar
                   :visible 'toolbarshow-view
                   :enable nil)
(tool-bar-add-item "folding" 'hs-toggle-hiding 'folding
                   :visible 'toolbarshow-view
                   :enable '(and hs-minor-mode (fboundp 'hs-toggle-hiding))
                   :help "Toggle hiding/showing of a block(hs-minor-mode)")
(tool-bar-add-item "linum" 'global-linum-mode
                   'global-linum-mode
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'global-linum-mode)
                   :help "Toggle Global Linum mode")
(tool-bar-add-item "whitespace" 'whitespace-mode
                   'whitespace-mode
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'whitespace-mode)
                   :help "Toggle whitespace minor mode visualization")
(tool-bar-add-item "ecb"
                   (lambda ()
                     (interactive)
                     (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
                         (ecb-deactivate)
                       (ecb-activate)))
                   'ecb
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'ecb-activate)
                   :help "Toggle ECB")

;; program toolbar
(tool-bar-add-item "separator" nil 'program-toolbar
                   :visible 'toolbarshow-program
                   :enable nil)
(tool-bar-add-item "semantic-jump-back" 'semantic-ia-fast-jump-back
                   'semantic-ia-fast-jump-back
                   :visible 'toolbarshow-program
                   :enable (fboundp 'semantic-ia-fast-jump-back)
                   :help "Jump back to previous tag (Semantic)")
(tool-bar-add-item "semantic-jump" 'semantic-ia-fast-jump
                   'semantic-ia-fast-jump
                   :visible 'toolbarshow-program
                   :enable (fboundp 'semantic-ia-fast-jump)
                   :help "Jump to the tag at point (Semantic)")
;; (tool-bar-add-item "semantic-impl-toggle" 'semantic-analyze-proto-impl-toggle
;;                    'semantic-analyze-proto-impl-toggle
;;                    :visible 'toolbarshow-program
;;                    :enable (fboundp 'semantic-analyze-proto-impl-toggle)
;;                    :help "Toggle implementation and prototype (Semantic)")
(tool-bar-add-item "sourcepair"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'sourcepair-load)
                         (sourcepair-load)
                       (eassist-switch-h-cpp)))
                   'sourcepair
                   :visible 'toolbarshow-program
                   :enable '(and (memq major-mode '(c++-mode c-mode objc-mode))
                                 (or (fboundp 'sourcepair-load)
                                     (fboundp 'eassist-switch-h-cpp)))
                   :help "Switch header and body file")
(tool-bar-add-item "compile" 'compile 'compile
                   :visible 'toolbarshow-program
                   :help "Compile...")
(tool-bar-add-item "debug" 'gdb 'gdb
                   :visible 'toolbarshow-program
                   :help "Debugger (GDB)...")

;; remember toolbar
(tool-bar-add-item "separator" nil 'remember-toolbar
                   :visible 'toolbarshow-remember
                   :enable nil)
(tool-bar-add-item "remember" 'remember 'remember
                   :visible '(and toolbarshow-remember
                                  (not (eq major-mode 'remember-mode)))
                   :help "Remember")
(tool-bar-add-item "remember-open"
                   (lambda ()
                     (interactive)
                     (require 'remember nil 'noerror)
                     (find-file remember-data-file))
                   'remember-open
                   :visible '(and toolbarshow-remember
                                  (not (eq major-mode 'remember-mode)))
                   :help "Open remember data file")
(tool-bar-add-item "remember-finalize" 'remember-finalize 'remember-finalize
                   :visible '(and toolbarshow-remember
                                  (eq major-mode 'remember-mode))
                   :help "Remember the contents of the current buffer")
(tool-bar-add-item "remember-destroy" 'remember-destroy 'remember-destroy
                   :visible '(and toolbarshow-remember
                                  (eq major-mode 'remember-mode))
                   :help "Destroy the current *Remember* buffer")

;; emms toolbar
(tool-bar-add-item "separator" nil 'emms-toolbar
                   :visible 'toolbarshow-emms
                   :enable nil)
(tool-bar-add-item "emms"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'emms-dir-tree)
                         (emms-dir-tree)
                       (emms)))
                   'emms
                   :visible 'toolbarshow-emms
                   :help "Emms")
(tool-bar-add-item "emms-previous" 'emms-previous 'emms-previous
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-previous)
                   :help "Previous track")
(tool-bar-add-item "emms-seek-backward" 'emms-seek-backward 'emms-seek-backward
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-seek-backward)
                   :help "Seek backward")
(tool-bar-add-item "emms-stop" 'emms-stop 'emms-stop
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-stop)
                   :help "Stop")
(tool-bar-add-item "emms-start" 'emms-start 'emms-start
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-start)
                   :help "Start")
(tool-bar-add-item "emms-pause" 'emms-pause 'emms-pause
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-pause)
                   :help "Pause")
(tool-bar-add-item "emms-seek-forward" 'emms-seek-forward 'emms-seek-forward
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-seek-forward)
                   :help "Seek forward")
(tool-bar-add-item "emms-next" 'emms-next 'emms-next
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-next)
                   :help "Next track")


(provide 'init-toolbar)
