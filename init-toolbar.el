;;;
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
(defvar need-delete-toolbar-buttons '(print-buffer))
(let ((need-delete-btns))
  (dolist (button tool-bar-map)
    (when (and (consp button)
               (memq (car button) need-delete-toolbar-buttons))
      (add-to-list 'need-delete-btns button)))
  (dolist (button need-delete-btns)
    (delq button tool-bar-map)))

;; image-load-path
(let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
       (images-dir (expand-file-name "images" dir)))
  (add-to-list 'image-load-path images-dir))

(setq tool-bar-button-margin 0)
;; (setq auto-resize-tool-bars nil)

(defvar bookmark-toolbar-show nil
  "If show bookmark toolbar.")
(defun bookmark-toolbar-toggle ()
  "Turn bookmark toolbar on/off."
  (interactive)
  (setq bookmark-toolbar-show (if bookmark-toolbar-show nil t))
  (force-window-update))

(defvar edit-toolbar-show t
  "If show edit toolbar.")
(defun edit-toolbar-toggle ()
  "Turn edit toolbar on/off."
  (interactive)
  (setq edit-toolbar-show (if edit-toolbar-show nil t))
  (force-window-update))

(defvar toggle-toolbar-show t
  "If show toogle toolbar.")
(defun toggle-toolbar-toggle ()
  "Turn toogle toolbar on/off."
  (interactive)
  (setq toggle-toolbar-show (if toggle-toolbar-show nil t))
  (force-window-update))

(defvar program-toolbar-show t
  "If show program toolbar.")
(defun program-toolbar-toggle ()
  "Turn program toolbar on/off."
  (interactive)
  (setq program-toolbar-show (if program-toolbar-show nil t))
  (force-window-update))

;; toggle toolbar menu
(defvar toggle-toolbar-menu (make-sparse-keymap "Toolbar"))
(define-key toggle-toolbar-menu [xml-mode]
  '(menu-item "XML" nxml-mode
              :visible (fboundp 'nxml-mode)
              :button (:radio . (eq major-mode 'nxml-mode))))
(define-key toggle-toolbar-menu [text-mode]
  '(menu-item "Text" text-mode
              :visible (fboundp 'text-mode)
              :button (:radio . (eq major-mode 'text-mode))))
(define-key toggle-toolbar-menu [tcl-mode]
  '(menu-item "Tcl" tcl-mode
              :visible (fboundp 'tcl-mode)
              :button (:radio . (eq major-mode 'tcl-mode))))
(define-key toggle-toolbar-menu [sql-mode]
  '(menu-item "SQL" sql-mode
              :visible (fboundp 'sql-mode)
              :button (:radio . (eq major-mode 'sql-mode))))
(define-key toggle-toolbar-menu [sh-mode]
  '(menu-item "Shell" sh-mode
              :visible (fboundp 'sh-mode)
              :button (:radio . (eq major-mode 'sh-mode))))
(define-key toggle-toolbar-menu [scheme-mode]
  '(menu-item "Scheme" scheme-mode
              :visible (fboundp 'scheme-mode)
              :button (:radio . (eq major-mode 'scheme-mode))))
(define-key toggle-toolbar-menu [ruby-mode]
  '(menu-item "Ruby" ruby-mode
              :visible (fboundp 'ruby-mode)
              :button (:radio . (eq major-mode 'ruby-mode))))
(define-key toggle-toolbar-menu [rst-mode]
  '(menu-item "ReST" rst-mode
              :visible (fboundp 'rst-mode)
              :button (:radio . (eq major-mode 'rst-mode))))
(define-key toggle-toolbar-menu [python-mode]
  '(menu-item "Python" python-mode
              :visible (fboundp 'python-mode)
              :button (:radio . (eq major-mode 'python-mode))))
(define-key toggle-toolbar-menu [php-mode]
  '(menu-item "PHP" php-mode
              :visible (fboundp 'php-mode)
              :button (:radio . (eq major-mode 'php-mode))))
(define-key toggle-toolbar-menu [perl-mode]
  '(menu-item "Perl" cperl-mode
              :visible (fboundp 'cperl-mode)
              :button (:radio . (eq major-mode 'cperl-mode))))
(define-key toggle-toolbar-menu [pascal-mode]
  '(menu-item "Pascal" pascal-mode
              :visible (fboundp 'pascal-mode)
              :button (:radio . (eq major-mode 'pascal-mode))))
(define-key toggle-toolbar-menu [org-mode]
  '(menu-item "Org" org-mode
              :visible (fboundp 'org-mode)
              :button (:radio . (eq major-mode 'org-mode))))
(define-key toggle-toolbar-menu [objc-mode]
  '(menu-item "ObjC" objc-mode
              :visible (fboundp 'objc-mode)
              :button (:radio . (eq major-mode 'objc-mode))))
(define-key toggle-toolbar-menu [makefile-mode]
  '(menu-item "Makefile" makefile-mode
              :visible (fboundp 'makefile-mode)
              :button (:radio . (eq major-mode 'makefile-mode))))
(define-key toggle-toolbar-menu [lisp-mode]
  '(menu-item "Lisp" lisp-mode
              :visible (fboundp 'lisp-mode)
              :button (:radio . (eq major-mode 'lisp-mode))))
(define-key toggle-toolbar-menu [latex-mode]
  '(menu-item "LaTeX" latex-mode
              :visible (fboundp 'latex-mode)
              :button (:radio . (eq major-mode 'latex-mode))))
(define-key toggle-toolbar-menu [js-mode]
  '(menu-item "Javascript" js-mode
              :visible (fboundp 'js-mode)
              :button (:radio . (eq major-mode 'js-mode))))
(define-key toggle-toolbar-menu [java-mode]
  '(menu-item "Java" java-mode
              :visible (fboundp 'java-mode)
              :button (:radio . (eq major-mode 'java-mode))))
(define-key toggle-toolbar-menu [html-mode]
  '(menu-item "HTML" html-mode
              :visible (fboundp 'html-mode)
              :button (:radio . (eq major-mode 'html-mode))))
(define-key toggle-toolbar-menu [fortran-mode]
  '(menu-item "Fortran" fortran-mode
              :visible (fboundp 'fortran-mode)
              :button (:radio . (eq major-mode 'fortran-mode))))
(define-key toggle-toolbar-menu [emacs-lisp-mode]
  '(menu-item "Emacs-Lisp" emacs-lisp-mode
              :visible (fboundp 'emacs-lisp-mode)
              :button (:radio . (eq major-mode 'emacs-lisp-mode))))
(define-key toggle-toolbar-menu [delphi-mode]
  '(menu-item "Delphi" delphi-mode
              :visible (fboundp 'delphi-mode)
              :button (:radio . (eq major-mode 'delphi-mode))))
(define-key toggle-toolbar-menu [css-mode]
  '(menu-item "CSS" css-mode
              :visible (fboundp 'css-mode)
              :button (:radio . (eq major-mode 'css-mode))))
(define-key toggle-toolbar-menu [csharp-mode]
  '(menu-item "C#" csharp-mode
              :visible (fboundp 'csharp-mode)
              :button (:radio . (eq major-mode 'csharp-mode))))
(define-key toggle-toolbar-menu [c++-mode]
  '(menu-item "C++" c++-mode
              :visible (fboundp 'c++-mode)
              :button (:radio . (eq major-mode 'c++-mode))))
(define-key toggle-toolbar-menu [c-mode]
  '(menu-item "C" c-mode
              :visible (fboundp 'c-mode)
              :button (:radio . (eq major-mode 'c-mode))))
(define-key toggle-toolbar-menu [autoconf-mode]
  '(menu-item "Autoconf" autoconf-mode
              :visible (fboundp 'autoconf-mode)
              :button (:radio . (eq major-mode 'autoconf-mode))))
(define-key toggle-toolbar-menu [asm-mode]
  '(menu-item "Assembler" asm-mode
              :visible (fboundp 'asm-mode)
              :button (:radio . (eq major-mode 'asm-mode))))
(define-key toggle-toolbar-menu [ada-mode]
  '(menu-item "Ada" ada-mode
              :visible (fboundp 'ada-mode)
              :button (:radio . (eq major-mode 'ada-mode))))
(define-key toggle-toolbar-menu [separatore-major-mode]
  '(menu-item "--"))
(define-key toggle-toolbar-menu [program-toolbar-toggle]
  '(menu-item "Program toolbar" program-toolbar-toggle
              :help "Turn program toolbar on/off"
              :button (:toggle . program-toolbar-show)))
(define-key toggle-toolbar-menu [toggle-toolbar-toggle]
  '(menu-item "Toggle toolbar" toggle-toolbar-toggle
              :help "Turn toggle toolbar on/off"
              :button (:toggle . toggle-toolbar-show)))
(define-key toggle-toolbar-menu [edit-toolbar-toggle]
  '(menu-item "Edit toolbar" edit-toolbar-toggle
              :help "Turn edit toolbar on/off"
              :button (:toggle . edit-toolbar-show)))
(define-key toggle-toolbar-menu [bookmark-toolbar-toggle]
  '(menu-item "Bookmark toolbar" bookmark-toolbar-toggle
              :help "Turn bookmark toolbar on/off"
              :button (:toggle . bookmark-toolbar-show)))
;; (global-set-key (kbd "<S-mouse-2>") toggle-toolbar-menu)
;; (define-key-after menu-bar-tools-menu [toggle-toolbar]
;;   (list 'menu-item "Toolbar" toggle-toolbar-menu))
;; (setq tool-bar-map (make-sparse-keymap))
(tool-bar-add-item "pop-menu"
                   (lambda ()
                     (interactive)
                     (popup-menu toggle-toolbar-menu))
                   'toggle-toolbar-menu)

;; bookmark toolbar
(tool-bar-add-item "separator" nil 'bookmark-toolbar
                   :visible 'bookmark-toolbar-show)
(tool-bar-add-item "bm-toggle"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-toggle)
                         (bm-toggle)
                       (viss-bookmark-toggle)))
                   'bm-toggle
                   :visible 'bookmark-toolbar-show
                   :help "Toggle bookmark at point")
(tool-bar-add-item "bm-next"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-next)
                         (call-interactively (bm-next))
                       (viss-bookmark-next-buffer)))
                   'bm-next
                   :visible 'bookmark-toolbar-show
                   :help "Goto next bookmark")
(tool-bar-add-item "bm-previous"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-previous)
                         (call-interactively (bm-previous))
                       (viss-bookmark-prev-buffer)))
                   'bm-previous
                   :visible 'bookmark-toolbar-show
                   :help "Goto previous bookmark")
(tool-bar-add-item "bm-clear"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-remove-all-current-buffer)
                         (bm-remove-all-current-buffer)
                       (viss-bookmark-clear-all-buffer)))
                   'bm-remove-all-current-buffer
                   :visible 'bookmark-toolbar-show
                   :help "Delete all visible bookmarks in current buffer")

;; edit toolbar
(tool-bar-add-item "separator" nil 'edit-toolbar
                   :visible 'edit-toolbar-show)
(tool-bar-add-item "recent-backward"'recent-jump-jump-backward
                   'recent-jump-jump-backward
                   :visible 'edit-toolbar-show
                   :enable '(fboundp 'recent-jump-jump-backward)
                   :help "Backward in the history")
(tool-bar-add-item "recent-forward" 'recent-jump-jump-forward
                   'recent-jump-jump-forward
                   :visible 'edit-toolbar-show
                   :enable '(fboundp 'recent-jump-jump-forward)
                   :help "Forward in the history")
(tool-bar-add-item "find" 'isearch-forward
                   'isearch-forward
                   :visible 'edit-toolbar-show
                   :help "Forward String...")
(tool-bar-add-item "find-next" 'isearch-repeat-forward
                   'isearch-repeat-forward
                   :visible 'edit-toolbar-show
                   :help "Repeat Forward String")
(tool-bar-add-item "replace" 'query-replace 'query-replace
                   :visible 'edit-toolbar-show
                   :help "Replace String...")
(tool-bar-add-item "upcase" 'upcase-region 'upcase-region
                   :visible 'edit-toolbar-show
                   :help "Convert the region to upper case")
(tool-bar-add-item "downcase" 'downcase-region 'downcase-region
                   :visible 'edit-toolbar-show
                   :help "Convert the region to lower case")

;; toggle toolbar
(tool-bar-add-item "separator" nil 'toggle-toolbar
                   :visible 'toggle-toolbar-show)
(tool-bar-add-item "linum" 'global-linum-mode
                   'global-linum-mode
                   :visible 'toggle-toolbar-show
                   :enable '(fboundp 'global-linum-mode)
                   :help "Toggle Global Linum mode")
(tool-bar-add-item "whitespace" 'whitespace-mode
                   'whitespace-mode
                   :visible 'toggle-toolbar-show
                   :enable '(fboundp 'whitespace-mode)
                   :help "Toggle whitespace minor mode visualization")
(tool-bar-add-item "ecb"
                   (lambda ()
                     (interactive)
                     (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
                         (ecb-deactivate)
                       (ecb-activate)))
                   'ecb
                   :visible 'toggle-toolbar-show
                   :enable '(fboundp 'ecb-activate)
                   :help "Toggle ECB")

;; program toolbar
(tool-bar-add-item "separator" nil 'program-toolbar
                   :visible 'program-toolbar-show)
(tool-bar-add-item "semantic-jump-back" 'semantic-ia-fast-jump-back
                   'semantic-ia-fast-jump-back
                   :visible 'program-toolbar-show
                   :enable (fboundp 'semantic-ia-fast-jump-back)
                   :help "Jump back to previous tag (Semantic)")
(tool-bar-add-item "semantic-jump" 'semantic-ia-fast-jump
                   'semantic-ia-fast-jump
                   :visible 'program-toolbar-show
                   :enable (fboundp 'semantic-ia-fast-jump)
                   :help "Jump to the tag at point (Semantic)")
(tool-bar-add-item "semantic-impl-toggle" 'semantic-analyze-proto-impl-toggle
                   'semantic-analyze-proto-impl-toggle
                   :visible 'program-toolbar-show
                   :enable (fboundp 'semantic-analyze-proto-impl-toggle)
                   :help "Toggle implementation and prototype (Semantic)")
(tool-bar-add-item "sourcepair"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'sourcepair-load)
                         (sourcepair-load)
                       (eassist-switch-h-cpp)))
                   'sourcepair
                   :visible 'program-toolbar-show
                   :enable '(and (memq major-mode '(c++-mode c-mode objc-mode))
                                 (or (fboundp 'sourcepair-load)
                                     (fboundp 'eassist-switch-h-cpp)))
                   :help "Switch header and body file")
(tool-bar-add-item "compile" 'compile 'compile
                   :visible 'program-toolbar-show
                   :help "Compile...")
(tool-bar-add-item "debug" 'gdb 'gdb
                   :visible 'program-toolbar-show
                   :help "Debugger (GDB)...")


(provide 'init-toolbar)
