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

(setq tool-bar-button-margin 0)
;; (setq auto-resize-tool-bars nil)

;; Remove the some buttons in the tool bar.
(when (boundp 'tool-bar-map)
  (let ((need-delete-btns))
    (dolist (button tool-bar-map)
      (when (and (consp button)
                 (memq (car button) '(print-buffer write-file customize help)))
        (add-to-list 'need-delete-btns button)))
    (dolist (button need-delete-btns)
      (delq button tool-bar-map))))

;; image-load-path
(let* ((root-dir (if (boundp 'dotemacs-root-dir)
                     dotemacs-root-dir
                   (file-name-directory
                    (directory-file-name
                     (file-name-directory
                      (or load-file-name buffer-file-name))))))
       (images-dir (expand-file-name "etc/images" root-dir)))
  (if (boundp 'image-load-path)         ; emacs-21 isn't have image-load-path
      (add-to-list 'image-load-path images-dir)
    (add-to-list 'load-path images-dir)))

(defgroup toolbarshow nil
  "Custom toolbar"
  :group 'environment)

(defcustom toolbarshow-edit t
  "If show edit toolbar."
  :type 'boolean
  :group 'toolbarshow)

(defcustom toolbarshow-search nil
  "If show search toolbar."
  :type 'boolean
  :group 'toolbarshow)

(defcustom toolbarshow-bookmark nil
  "If show bookmark toolbar."
  :type 'boolean
  :group 'toolbarshow)

(defcustom toolbarshow-view t
  "If show view toolbar."
  :type 'boolean
  :group 'toolbarshow)

(defcustom toolbarshow-program t
  "If show program toolbar."
  :type 'boolean
  :group 'toolbarshow)

(defcustom toolbarshow-flymake nil
  "If show flymake toolbar."
  :type 'boolean
  :group 'toolbarshow)

(defcustom toolbarshow-remember t
  "If show remember toolbar."
  :type 'boolean
  :group 'toolbarshow)

(defcustom toolbarshow-emms nil
  "If show emms toolbar."
  :type 'boolean
  :group 'toolbarshow)

(defun toolbarshow-toggle (group)
  "Turn toolbar on/off for grouop."
  (set group (if (symbol-value group) nil t))
  (force-window-update)
  (customize-mark-to-save group)
  (custom-save-all))

(defun key4cmd (cmd &optional optioncmd)
  "Get keys for command."
  (let ((key (mapconcat 'key-description (where-is-internal cmd) ",")))
    (when (and (= 0 (length key)) optioncmd)
      (setq key (mapconcat 'key-description (where-is-internal optioncmd) ",")))
    (if (= 0 (length key))
        (concat "(M-x " (symbol-name cmd) ")")
      (concat "(" key ")"))))

(require 'easymenu)

;; toggle toolbar menu
(easy-menu-define toggle-toolbar-menu nil
  "toolbar  menu"
  '("Toolbar"
    ("Major mode"
     ["Ada" ada-mode :active (fboundp 'ada-mode)
      :style radio :selected (eq major-mode 'ada-mode)]
     ["Assembler" asm-mode :active (fboundp 'asm-mode)
      :style radio :selected (eq major-mode 'asm-mode)]
     ["Autoconf" autoconf-mode :active (fboundp 'autoconf-mode)
      :style radio :selected (eq major-mode 'autoconf-mode)]
     ["C" c-mode :active (fboundp 'c-mode)
      :style radio :selected (eq major-mode 'c-mode)]
     ["C++" c++-mode :active (fboundp 'c++-mode)
      :style radio :selected (eq major-mode 'c++-mode)]
     ["C#" csharp-mode :active (fboundp 'csharp-mode)
      :style radio :selected (eq major-mode 'csharp-mode)]
     ("Conf"
      ["Auto detect..." conf-mode :active (fboundp 'conf-mode)]
      ["Xdefaults" conf-xdefaults-mode :active (fboundp 'conf-xdefaults-mode)
       :style radio :selected (eq major-mode 'conf-xdefaults-mode)]
      ["Unix" conf-unix-mode :active (fboundp 'conf-unix-mode)
       :style radio :selected (eq major-mode 'conf-unix-mode)]
      ["Colon" conf-colon-mode :active (fboundp 'conf-colon-mode)
       :style radio :selected (eq major-mode 'conf-colon-mode)]
      ["PPD" conf-ppd-mode :active (fboundp 'conf-ppd-mode)
       :style radio :selected (eq major-mode 'conf-ppd-mode)]
      ["Space keywords" conf-space-mode :active (fboundp 'conf-space-mode)
       :style radio :selected (eq major-mode 'conf-space-mode)]
      ["Java properties" conf-javaprop-mode :active (fboundp 'conf-javaprop-mode)
       :style radio :selected (eq major-mode 'conf-javaprop-mode)]
      ["Windows" conf-windows-mode :active (fboundp 'conf-windows-mode)
       :style radio :selected (eq major-mode 'conf-windows-mode)])
     ["CSV" csv-mode :active (fboundp 'csv-mode)
      :style radio :selected (eq major-mode 'csv-mode)]
     ["CSS" css-mode :active (fboundp 'css-mode)
      :style radio :selected (eq major-mode 'css-mode)]
     ["Delphi" delphi-mode :active (fboundp 'delphi-mode)
      :style radio :selected (eq major-mode 'delphi-mode)]
     ["Emacs-Lisp" emacs-lisp-mode :active (fboundp 'emacs-lisp-mode)
      :style radio :selected (eq major-mode 'emacs-lisp-mode)]
     ("Fortran"
      ["Fortran" fortran-mode :active (fboundp 'fortran-mode)
       :style radio :selected (eq major-mode 'fortran-mode)]
      ["F90" f90-mode :active (fboundp 'f90-mode)
       :style radio :selected (eq major-mode 'f90-mode)])
     ["Go" go-mode :active (fboundp 'go-mode)
      :style radio :selected (eq major-mode 'go-mode)]
     ["Hex" hexl-mode :active (fboundp 'hexl-mode)
      :style radio :selected (eq major-mode 'hexl-mode)]
     ["HTML" html-mode :active (fboundp 'html-mode)
      :style radio :selected (eq major-mode 'html-mode)]
     ["Java" java-mode :active (fboundp 'java-mode)
      :style radio :selected (eq major-mode 'java-mode)]
     ("Javascript"
      ["Javascript" js-mode :active (fboundp 'js-mode)
       :style radio :selected (eq major-mode 'js-mode)]
      ["Javascript2" js2-mode :active (fboundp 'js2-mode)
       :style radio :selected (eq major-mode 'js2-mode)])
     ["LaTeX" latex-mode :active (fboundp 'latex-mode)
      :style radio :selected (eq major-mode 'latex-mode)]
     ["Lisp" lisp-mode :active (fboundp 'lisp-mode)
      :style radio :selected (eq major-mode 'lisp-mode)]
     ("Makefile"
      ["GNU make" makefile-gmake-mode :active (fboundp 'makefile-gmake-mode)
       :style radio :selected (eq major-mode 'makefile-gmake-mode)]
      ["Automake" makefile-automake-mode :active (fboundp 'makefile-automake-mode)
       :style radio :selected (eq major-mode 'makefile-automake-mode)]
      ["BSD" makefile-bsdmake-mode :active (fboundp 'makefile-bsdmake-mode)
       :style radio :selected (eq major-mode 'makefile-bsdmake-mode)]
      ["Classic" makefile-mode :active (fboundp 'makefile-mode)
       :style radio :selected (eq major-mode 'makefile-mode)]
      ["Imake" makefile-imake-mode :active (fboundp 'makefile-imake-mode)
       :style radio :selected (eq major-mode 'makefile-imake-mode)]
      ["Makepp" makefile-makepp-mode :active (fboundp 'makefile-makepp-mode)
       :style radio :selected (eq major-mode 'makefile-makepp-mode)])
     ["ObjC" objc-mode :active (fboundp 'objc-mode)
      :style radio :selected (eq major-mode 'objc-mode)]
     ["Org" org-mode :active (fboundp 'org-mode)
      :style radio :selected (eq major-mode 'org-mode)]
     ["Outline" outline-mode :active (fboundp 'outline-mode)
      :style radio :selected (eq major-mode 'outline-mode)]
     ["Pascal" pascal-mode :active (fboundp 'pascal-mode)
      :style radio :selected (eq major-mode 'pascal-mode)]
     ("Perl"
      ["Perl" perl-mode :active (fboundp 'perl-mode)
       :style radio :selected (eq major-mode 'perl-mode)]
      ["CPerl" cperl-mode :active (fboundp 'cperl-mode)
       :style radio :selected (eq major-mode 'cperl-mode)])
     ["PHP" php-mode :active (fboundp 'php-mode)
      :style radio :selected (eq major-mode 'php-mode)]
     ["PL/SQL" plsql-mode :active (fboundp 'plsql-mode)
      :style radio :selected (eq major-mode 'plsql-mode)]
     ["Python" python-mode :active (fboundp 'python-mode)
      :style radio :selected (eq major-mode 'python-mode)]
     ["ReST" rst-mode :active (fboundp 'rst-mode)
      :style radio :selected (eq major-mode 'rst-mode)]
     ["Ruby" ruby-mode :active (fboundp 'ruby-mode)
      :style radio :selected (eq major-mode 'ruby-mode)]
     ["Rust" rust-mode :active (fboundp 'rust-mode)
      :style radio :selected (eq major-mode 'rust-mode)]
     ["Scheme" scheme-mode :active (fboundp 'scheme-mode)
      :style radio :selected (eq major-mode 'scheme-mode)]
     ["Shell" sh-mode :active (fboundp 'sh-mode)
      :style radio :selected (eq major-mode 'sh-mode)]
     ["SQL" sql-mode :active (fboundp 'sql-mode)
      :style radio :selected (eq major-mode 'sql-mode)]
     ["Text" text-mode :active (fboundp 'text-mode)
      :style radio :selected (eq major-mode 'text-mode)]
     ["XML" nxml-mode :active (fboundp 'nxml-mode)
      :style radio :selected (eq major-mode 'nxml-mode)])

    ("Minor mode"
     ["artist-mode" artist-mode :active (fboundp 'artist-mode)
      :style toggle :selected artist-mode]
     ["auto-complete-mode (global)" global-auto-complete-mode :active (fboundp 'global-auto-complete-mode)
      :style toggle :selected global-auto-complete-mode]
     ["diff-hl-mode (global)" global-diff-hl-mode :active (fboundp 'global-diff-hl-mode)
      :style toggle :selected global-diff-hl-mode]
     ["drag-stuff-mode (global)" drag-stuff-global-mode :active (fboundp 'drag-stuff-global-mode)
      :style toggle :selected drag-stuff-global-mode]
     ["electric-pair-mode" electric-pair-mode :active (fboundp 'electric-pair-mode)
      :style toggle :selected electric-pair-mode]
     ["flymake-mode" flymake-mode :active (fboundp 'flymake-mode)
      :style toggle :selected flymake-mode]
     ["flymake-find-file-hook"
      (lambda ()
        (interactive)
        (if (memq 'flymake-find-file-hook find-file-hook)
            (remove-hook 'find-file-hook 'flymake-find-file-hook)
          (add-hook 'find-file-hook 'flymake-find-file-hook)))
      :active (fboundp 'flymake-find-file-hook)
      :style toggle :selected (memq 'flymake-find-file-hook find-file-hook)]
     ["goto-address-mode" goto-address-mode :active (fboundp 'goto-address-mode)
      :style toggle :selected goto-address-mode]
     ["hideshowvis-minor-mode" hideshowvis-minor-mode :active (fboundp 'hideshowvis-minor-mode)
      :style toggle :selected hideshowvis-minor-mode]
     ["highlight-parentheses-mode" highlight-parentheses-mode :active (fboundp 'highlight-parentheses-mode)
      :style toggle :selected highlight-parentheses-mode]
     ["highlight-symbol-mode" highlight-symbol-mode :active (fboundp 'highlight-symbol-mode)
      :style toggle :selected highlight-symbol-mode]
     ["highlight-tail-mode" highlight-tail-mode :active (fboundp 'highlight-tail-mode)
      :style toggle :selected highlight-tail-mode]
     ["hl-line-mode (global)" global-hl-line-mode :active (fboundp 'global-hl-line-mode)
      :style toggle :selected global-hl-line-mode]
     ["iimage-mode" iimage-mode :active (fboundp 'iimage-mode)
      :style toggle :selected iimage-mode]
     ["linum-mode" linum-mode :active (fboundp 'linum-mode)
      :style toggle :selected linum-mode]
     ["linum-mode (global)" global-linum-mode :active (fboundp 'global-linum-mode)
      :style toggle :selected global-linum-mode]
     ["nyan-mode (global)" nyan-mode :active (fboundp 'nyan-mode)
      :style toggle :selected nyan-mode]
     ["outline-minor-mode" outline-minor-mode :active (fboundp 'outline-minor-mode)
      :style toggle :selected outline-minor-mode]
     ["projectile-mode" projectile-mode :active (fboundp 'projectile-mode)
      :style toggle :selected projectile-mode]
     ["projectile-mode (global)" projectile-global-mode :active (fboundp 'projectile-global-mode)
      :style toggle :selected projectile-global-mode]
     ["rainbow-mode" rainbow-mode :active (fboundp 'rainbow-mode)
      :style toggle :selected rainbow-mode]
     ["ruler-mode" ruler-mode :active (fboundp 'ruler-mode)
      :style toggle :selected ruler-mode]
     ["tabbar-mode" tabbar-mode :active (fboundp 'tabbar-mode)
      :style toggle :selected tabbar-mode]
     ["undo-tree-mode" undo-tree-mode :active (fboundp 'undo-tree-mode)
      :style toggle :selected undo-tree-mode]
     ["undo-tree-mode (global)" global-undo-tree-mode :active (fboundp 'global-undo-tree-mode)
      :style toggle :selected global-undo-tree-mode]
     ["view-mode" view-mode :active (fboundp 'view-mode)
      :style toggle :selected view-mode]
     ["volatile-highlights-mode" volatile-highlights-mode :active (fboundp 'volatile-highlights-mode)
      :style toggle :selected volatile-highlights-mode]
     ["whitespace-mode" whitespace-mode :active (fboundp 'whitespace-mode)
      :style toggle :selected whitespace-mode])

    "--"

    ("Revert with encoding"
     ["US-ASCII"
      (lambda () (interactive) (revert-buffer-with-coding-system 'us-ascii))]
     ["UTF-8"
      (lambda () (interactive) (revert-buffer-with-coding-system 'utf-8))]
     ["UTF-8(BOM)"
      (lambda () (interactive) (revert-buffer-with-coding-system 'utf-8-with-signature))]
     ["UTF-16"
      (lambda () (interactive) (revert-buffer-with-coding-system 'utf-16))]
     ["UTF-16(BOM)"
      (lambda () (interactive) (revert-buffer-with-coding-system 'utf-16be-with-signature))]
     ["UTF-16-LE"
      (lambda () (interactive) (revert-buffer-with-coding-system 'utf-16-le))]
     ["UTF-16-LE(BOM)"
      (lambda () (interactive) (revert-buffer-with-coding-system 'utf-16le-with-signature))]
     ["GBK"
      (lambda () (interactive) (revert-buffer-with-coding-system 'gbk))]
     ["BIG5"
      (lambda () (interactive) (revert-buffer-with-coding-system 'big5))]
     ["SHIFT_JIS"
      (lambda () (interactive) (revert-buffer-with-coding-system 'sjis))])

    ("Change encoding"
     ["US-ASCII"
      (lambda () (interactive) (set-buffer-file-coding-system 'us-ascii))]
     ["UTF-8"
      (lambda () (interactive) (set-buffer-file-coding-system 'utf-8))]
     ["UTF-8(BOM)"
      (lambda () (interactive) (set-buffer-file-coding-system 'utf-8-with-signature))]
     ["UTF-16"
      (lambda () (interactive) (set-buffer-file-coding-system 'utf-16))]
     ["UTF-16(BOM)"
      (lambda () (interactive) (set-buffer-file-coding-system 'utf-16be-with-signature))]
     ["UTF-16-LE"
      (lambda () (interactive) (set-buffer-file-coding-system 'utf-16-le))]
     ["UTF-16-LE(BOM)"
      (lambda () (interactive) (set-buffer-file-coding-system 'utf-16le-with-signature))]
     ["GBK"
      (lambda () (interactive) (set-buffer-file-coding-system 'gbk))]
     ["BIG5"
      (lambda () (interactive) (set-buffer-file-coding-system 'big5))]
     ["SHIFT_JIS"
      (lambda () (interactive) (set-buffer-file-coding-system 'sjis))])

    "--"

    ("Toolbar"
     ["Edit toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-edit))
      :style toggle :selected toolbarshow-edit]
     ["Search toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-search))
      :style toggle :selected toolbarshow-search]
     ["Bookmark toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-bookmark))
      :style toggle :selected toolbarshow-bookmark]
     ["View toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-view))
      :style toggle :selected toolbarshow-view]
     ["Program toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-program))
      :style toggle :selected toolbarshow-program]
     ["Flymake toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-flymake))
      :style toggle :selected toolbarshow-flymake]
     ["Remember toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-remember))
      :style toggle :selected toolbarshow-remember]
     ["Emms toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-emms))
      :style toggle :selected toolbarshow-emms])

    ("Emms"
     ["Emms" (lambda ()
               (interactive)
               (if (fboundp 'emms-dir-tree)
                   (emms-dir-tree)
                 (emms)))
      :active (or (fboundp 'emms-dir-tree) (fboundp 'emms))]
     ["Previous track" emms-previous (fboundp 'emms-previous)]
     ["Seek backward" emms-seek-backward
      :active (and (fboundp 'emms-seek-backward) emms-player-playing-p)]
     ["Start" emms-start
      :active (and (fboundp 'emms-start) (not emms-player-playing-p))]
     ["Pause" emms-pause
      :active (and (boundp 'emms-player-playing-p) emms-player-playing-p)]
     ["Resume" emms-pause
      :active (and (boundp 'emms-player-paused-p) emms-player-paused-p)]
     ["Stop" emms-stop
      :active (and (fboundp 'emms-stop) emms-player-playing-p)]
     ["Seek forward" emms-seek-forward
      :active (and (fboundp 'emms-seek-forward) emms-player-playing-p)]
     ["Next track" emms-next (fboundp 'emms-next)])

    ("Misc"
     ["find-dotemacs-file" find-dotemacs-file (fboundp 'find-dotemacs-file)]
     ["multi-term" multi-term (fboundp 'multi-term)]
     ["cfw:open-org-calendar" cfw:open-org-calendar (fboundp 'cfw:open-org-calendar)]
     ["format-cxx-directory" format-cxx-directory (fboundp 'format-cxx-directory)]
     ["format-xml-directory" format-xml-directory (fboundp 'format-xml-directory)]
     ["ielm" ielm (fboundp 'ielm)]
     ["proced" proced (fboundp 'proced)]
     ["ascii-display" ascii-display (fboundp 'ascii-display)])))


;; (defvar toggle-toolbar-menu (make-sparse-keymap "Toolbar"))

;; (defvar misc-sub-menu (make-sparse-keymap "Misc"))

;; (define-key misc-sub-menu [ascii-display]
;;   '(menu-item "ascii-display" ascii-display
;;               :enable (fboundp 'ascii-display)
;;               :button (:toggle . ascii-display)))
;; (define-key misc-sub-menu [proced]
;;   '(menu-item "proced" proced
;;               :enable (fboundp 'proced)))
;; (define-key misc-sub-menu [ielm]
;;   '(menu-item "ielm" ielm
;;               :enable (fboundp 'ielm)))
;; (define-key misc-sub-menu [format-xml-directory]
;;   '(menu-item "format-xml-directory" format-xml-directory
;;               :enable (fboundp 'format-xml-directory)))
;; (define-key misc-sub-menu [format-cxx-directory]
;;   '(menu-item "format-cxx-directory" format-cxx-directory
;;               :enable (fboundp 'format-cxx-directory)))
;; (define-key misc-sub-menu [cfw:open-org-calendar]
;;   '(menu-item "cfw:open-org-calendar" cfw:open-org-calendar
;;               :enable (fboundp 'cfw:open-org-calendar)))
;; (define-key misc-sub-menu [multi-term]
;;   '(menu-item "multi-term" multi-term
;;               :enable (fboundp 'multi-term)))
;; (define-key misc-sub-menu [find-dotemacs-file]
;;   '(menu-item "find-dotemacs-file" find-dotemacs-file
;;               :enable (fboundp 'find-dotemacs-file)
;;               :help "Open .emacs file"))
;; (define-key toggle-toolbar-menu [misc]
;;   (list 'menu-item "Misc" misc-sub-menu))

;; (defvar emms-sub-menu (make-sparse-keymap "Emms"))
;; (define-key emms-sub-menu [emms-next]
;;   '(menu-item "Next track" emms-next
;;               :enable (fboundp 'emms-next)))
;; (define-key emms-sub-menu [emms-seek-forward]
;;   '(menu-item "Seek forward" emms-seek-forward
;;               :enable (and (fboundp 'emms-seek-forward)
;;                            emms-player-playing-p)))
;; (define-key emms-sub-menu [emms-pause]
;;   '(menu-item (if (and (boundp 'emms-player-paused-p)
;;                        emms-player-paused-p)
;;                   "Resume"
;;                 "Pause")
;;               emms-pause
;;               :enable (and (fboundp 'emms-pause)
;;                            emms-player-playing-p)))
;; (define-key emms-sub-menu [emms-stop]
;;   '(menu-item "Stop" emms-stop
;;               :enable (fboundp 'emms-stop)
;;               :visible emms-player-playing-p))
;; (define-key emms-sub-menu [emms-start]
;;   '(menu-item "Start" emms-start
;;               :enable (fboundp 'emms-start)
;;               :visible (or (not (boundp 'emms-player-playing-p))
;;                            (not emms-player-playing-p))))
;; (define-key emms-sub-menu [emms-seek-backward]
;;   '(menu-item "Seek backward" emms-seek-backward
;;               :enable (and (fboundp 'emms-seek-backward)
;;                            emms-player-playing-p)))
;; (define-key emms-sub-menu [emms-previous]
;;   '(menu-item "Previous track" emms-previous
;;               :enable (fboundp 'emms-previous)))
;; (define-key emms-sub-menu [emms]
;;   '(menu-item "Emms"
;;               (lambda ()
;;                 (interactive)
;;                 (if (fboundp 'emms-dir-tree)
;;                     (emms-dir-tree)
;;                   (emms)))
;;               :enable (or (fboundp 'emms-dir-tree) (fboundp 'emms))
;;               :help "Emms"))
;; (define-key toggle-toolbar-menu [emms]
;;   (list 'menu-item "Emms" emms-sub-menu))
;; (define-key toggle-toolbar-menu [separatore-misc]
;;   '(menu-item "--"))

;; (defvar change-encoding-sub-menu (make-sparse-keymap "Change encoding"))
;; (define-key change-encoding-sub-menu [encoding-sjis]
;;   '(menu-item "SHIFT_JIS" (lambda () (interactive)
;;                             (set-buffer-file-coding-system 'sjis))))
;; (define-key change-encoding-sub-menu [encoding-big5]
;;   '(menu-item "BIG5" (lambda () (interactive)
;;                                (set-buffer-file-coding-system 'big5))))
;; (define-key change-encoding-sub-menu [encoding-gbk]
;;   '(menu-item "GBK" (lambda () (interactive)
;;                               (set-buffer-file-coding-system 'gbk))))
;; (define-key change-encoding-sub-menu [encoding-utf-16le-sign]
;;   '(menu-item "UTF-16-LE(BOM)"
;;               (lambda () (interactive)
;;                 (set-buffer-file-coding-system 'utf-16le-with-signature))))
;; (define-key change-encoding-sub-menu [encoding-utf-16le]
;;   '(menu-item "UTF-16-LE" (lambda () (interactive)
;;                             (set-buffer-file-coding-system 'utf-16-le))))
;; (define-key change-encoding-sub-menu [encoding-utf-16-sign]
;;   '(menu-item "UTF-16(BOM)"
;;               (lambda () (interactive)
;;                 (set-buffer-file-coding-system 'utf-16be-with-signature))))
;; (define-key change-encoding-sub-menu [encoding-utf-16]
;;   '(menu-item "UTF-16" (lambda () (interactive)
;;                          (set-buffer-file-coding-system 'utf-16))))
;; (define-key change-encoding-sub-menu [encoding-utf-8-sign]
;;   '(menu-item "UTF-8(BOM)"
;;               (lambda () (interactive)
;;                 (set-buffer-file-coding-system 'utf-8-with-signature))))
;; (define-key change-encoding-sub-menu [encoding-utf-8]
;;   '(menu-item "UTF-8" (lambda () (interactive)
;;                         (set-buffer-file-coding-system 'utf-8))))
;; (define-key change-encoding-sub-menu [encoding-ascii]
;;   '(menu-item "US-ASCII" (lambda () (interactive)
;;                            (set-buffer-file-coding-system 'us-ascii))))
;; (define-key toggle-toolbar-menu [change-encoding]
;;   (list 'menu-item "Change encoding" change-encoding-sub-menu))

;; (defvar revert-encoding-sub-menu (make-sparse-keymap "Revert encoding"))
;; (define-key revert-encoding-sub-menu [encoding-sjis]
;;   '(menu-item "SHIFT_JIS" (lambda () (interactive)
;;                             (revert-buffer-with-coding-system 'sjis))))
;; (define-key revert-encoding-sub-menu [encoding-big5]
;;   '(menu-item "BIG5" (lambda () (interactive)
;;                                (revert-buffer-with-coding-system 'big5))))
;; (define-key revert-encoding-sub-menu [encoding-gbk]
;;   '(menu-item "GBK" (lambda () (interactive)
;;                               (revert-buffer-with-coding-system 'gbk))))
;; (define-key revert-encoding-sub-menu [encoding-utf-16le-sign]
;;   '(menu-item "UTF-16-LE(BOM)"
;;               (lambda () (interactive)
;;                 (revert-buffer-with-coding-system 'utf-16le-with-signature))))
;; (define-key revert-encoding-sub-menu [encoding-utf-16le]
;;   '(menu-item "UTF-16-LE" (lambda () (interactive)
;;                             (revert-buffer-with-coding-system 'utf-16-le))))
;; (define-key revert-encoding-sub-menu [encoding-utf-16-sign]
;;   '(menu-item "UTF-16(BOM)"
;;               (lambda () (interactive)
;;                 (revert-buffer-with-coding-system 'utf-16be-with-signature))))
;; (define-key revert-encoding-sub-menu [encoding-utf-16]
;;   '(menu-item "UTF-16" (lambda () (interactive)
;;                          (revert-buffer-with-coding-system 'utf-16))))
;; (define-key revert-encoding-sub-menu [encoding-utf-8-sign]
;;   '(menu-item "UTF-8(BOM)"
;;               (lambda () (interactive)
;;                 (revert-buffer-with-coding-system 'utf-8-with-signature))))
;; (define-key revert-encoding-sub-menu [encoding-utf-8]
;;   '(menu-item "UTF-8" (lambda () (interactive)
;;                         (revert-buffer-with-coding-system 'utf-8))))
;; (define-key revert-encoding-sub-menu [encoding-ascii]
;;   '(menu-item "US-ASCII" (lambda () (interactive)
;;                            (revert-buffer-with-coding-system 'us-ascii))))
;; (define-key toggle-toolbar-menu [revert-encoding]
;;   (list 'menu-item "Revert with encoding" revert-encoding-sub-menu))

;; (defvar minormode-sub-menu (make-sparse-keymap "Minor mode"))
;; (define-key minormode-sub-menu [whitespace-mode]
;;   '(menu-item "whitespace-mode" whitespace-mode
;;               :enable (fboundp 'whitespace-mode)
;;               :button (:toggle . whitespace-mode)))
;; (define-key minormode-sub-menu [volatile-highlights-mode]
;;   '(menu-item "volatile-highlights-mode" volatile-highlights-mode
;;               :enable (fboundp 'volatile-highlights-mode)
;;               :button (:toggle . volatile-highlights-mode)))
;; (define-key minormode-sub-menu [viper-mode]
;;   '(menu-item "viper-mode" toggle-viper-mode
;;               :enable (fboundp 'toggle-viper-mode)
;;               :button (:toggle . viper-mode)))
;; (define-key minormode-sub-menu [view-mode]
;;   '(menu-item "view-mode" view-mode
;;               :enable (fboundp 'view-mode)
;;               :button (:toggle . view-mode)))
;; (define-key minormode-sub-menu [global-undo-tree-mode]
;;   '(menu-item "undo-tree-mode (global)" global-undo-tree-mode
;;               :enable (fboundp 'global-undo-tree-mode)
;;               :button (:toggle . global-undo-tree-mode)))
;; (define-key minormode-sub-menu [undo-tree-mode]
;;   '(menu-item "undo-tree-mode" undo-tree-mode
;;               :enable (fboundp 'undo-tree-mode)
;;               :button (:toggle . undo-tree-mode)))
;; (define-key minormode-sub-menu [tabbar-mode]
;;   '(menu-item "tabbar-mode" tabbar-mode
;;               :enable (fboundp 'tabbar-mode)
;;               :button (:toggle . tabbar-mode)))
;; (define-key minormode-sub-menu [ruler-mode]
;;   '(menu-item "ruler-mode" ruler-mode
;;               :enable (fboundp 'ruler-mode)
;;               :button (:toggle . ruler-mode)))
;; (define-key minormode-sub-menu [rainbow-mode]
;;   '(menu-item "rainbow-mode" rainbow-mode
;;               :enable (fboundp 'rainbow-mode)
;;               :button (:toggle . rainbow-mode)))
;; (define-key minormode-sub-menu [projectile-global-mode]
;;   '(menu-item "projectile-mode (global)" projectile-global-mode
;;               :enable (fboundp 'projectile-global-mode)
;;               :button (:toggle . projectile-global-mode)))
;; (define-key minormode-sub-menu [projectile-mode]
;;   '(menu-item "projectile-mode" projectile-mode
;;               :enable (fboundp 'projectile-mode)
;;               :button (:toggle . projectile-mode)))
;; (define-key minormode-sub-menu [outline-minor-mode]
;;   '(menu-item "outline-minor-mode" outline-minor-mode
;;               :enable (fboundp 'outline-minor-mode)
;;               :button (:toggle . outline-minor-mode)))
;; (define-key minormode-sub-menu [nyan-mode]
;;   '(menu-item "nyan-mode" nyan-mode
;;               :enable (fboundp 'nyan-mode)
;;               :button (:toggle . nyan-mode)))
;; (define-key minormode-sub-menu [global-linum-mode]
;;   '(menu-item "linum-mode (global)" global-linum-mode
;;               :enable (fboundp 'global-linum-mode)
;;               :button (:toggle . global-linum-mode)))
;; (define-key minormode-sub-menu [linum-mode]
;;   '(menu-item "linum-mode" linum-mode
;;               :enable (fboundp 'linum-mode)
;;               :button (:toggle . linum-mode)))
;; (define-key minormode-sub-menu [iimage-mode]
;;   '(menu-item "iimage-mode" iimage-mode
;;               :enable (fboundp 'iimage-mode)
;;               :button (:toggle . iimage-mode)))
;; (define-key minormode-sub-menu [global-hl-line-mode]
;;   '(menu-item "hl-line-mode (global)" global-hl-line-mode
;;               :enable (fboundp 'global-hl-line-mode)
;;               :button (:toggle . global-hl-line-mode)))
;; (define-key minormode-sub-menu [highlight-tail-mode]
;;   '(menu-item "highlight-tail-mode" highlight-tail-mode
;;               :enable (fboundp 'highlight-tail-mode)
;;               :button (:toggle . highlight-tail-mode)))
;; (define-key minormode-sub-menu [highlight-symbol-mode]
;;   '(menu-item "highlight-symbol-mode" highlight-symbol-mode
;;               :enable (fboundp 'highlight-symbol-mode)
;;               :button (:toggle . highlight-symbol-mode)))
;; (define-key minormode-sub-menu [highlight-parentheses-mode]
;;   '(menu-item "highlight-parentheses-mode" highlight-parentheses-mode
;;               :enable (fboundp 'highlight-parentheses-mode)
;;               :button (:toggle . highlight-parentheses-mode)))
;; (define-key minormode-sub-menu [hideshowvis-minor-mode]
;;   '(menu-item "hideshowvis-minor-mode" hideshowvis-minor-mode
;;               :enable (fboundp 'hideshowvis-minor-mode)
;;               :button (:toggle . hideshowvis-minor-mode)))
;; (define-key minormode-sub-menu [goto-address-mode]
;;   '(menu-item "goto-address-mode" goto-address-mode
;;               :enable (fboundp 'goto-address-mode)
;;               :button (:toggle . goto-address-mode)))
;; (define-key minormode-sub-menu [flymake-find-file-hook]
;;   '(menu-item "flymake-find-file-hook"
;;               (lambda ()
;;                 (interactive)
;;                 (if (memq 'flymake-find-file-hook find-file-hook)
;;                     (remove-hook 'find-file-hook 'flymake-find-file-hook)
;;                   (add-hook 'find-file-hook 'flymake-find-file-hook)))
;;               :enable (fboundp 'flymake-find-file-hook)
;;               :button
;;               (:toggle . (memq 'flymake-find-file-hook find-file-hook))))
;; (define-key minormode-sub-menu [flymake-mode]
;;   '(menu-item "flymake-mode" flymake-mode
;;               :enable (fboundp 'flymake-mode)
;;               :button (:toggle . flymake-mode)))
;; (define-key minormode-sub-menu [electric-pair-mode]
;;   '(menu-item "electric-pair-mode" electric-pair-mode
;;               :enable (fboundp 'electric-pair-mode)
;;               :button (:toggle . electric-pair-mode)))
;; (define-key minormode-sub-menu [drag-stuff-global-mode]
;;   '(menu-item "drag-stuff-mode (global)" drag-stuff-global-mode
;;               :enable (fboundp 'drag-stuff-global-mode)
;;               :button (:toggle . drag-stuff-global-mode)))
;; (define-key minormode-sub-menu [global-diff-hl-mode]
;;   '(menu-item "diff-hl-mode (global)" global-diff-hl-mode
;;               :enable (fboundp 'global-diff-hl-mode)
;;               :button (:toggle . global-diff-hl-mode)))
;; (define-key minormode-sub-menu [global-auto-complete-mode]
;;   '(menu-item "auto-complete-mode (global)" global-auto-complete-mode
;;               :enable (fboundp 'global-auto-complete-mode)
;;               :button (:toggle . global-auto-complete-mode)))
;; (define-key minormode-sub-menu [artist-mode]
;;   '(menu-item "artist-mode" artist-mode
;;               :enable (fboundp 'artist-mode)
;;               :button (:toggle . artist-mode)))
;; (define-key toggle-toolbar-menu [minor-mode]
;;   (list 'menu-item "Minor mode" minormode-sub-menu))

;; (defvar language-sub-menu (make-sparse-keymap "Major mode"))
;; (define-key language-sub-menu [xml-mode]
;;   '(menu-item "XML" nxml-mode
;;               :visible (fboundp 'nxml-mode)
;;               :button (:toggle . (eq major-mode 'nxml-mode))))
;; (define-key language-sub-menu [text-mode]
;;   '(menu-item "Text" text-mode
;;               :visible (fboundp 'text-mode)
;;               :button (:toggle . (eq major-mode 'text-mode))))
;; (define-key language-sub-menu [tcl-mode]
;;   '(menu-item "Tcl" tcl-mode
;;               :visible (fboundp 'tcl-mode)
;;               :button (:toggle . (eq major-mode 'tcl-mode))))
;; (define-key language-sub-menu [sql-mode]
;;   '(menu-item "SQL" sql-mode
;;               :visible (fboundp 'sql-mode)
;;               :button (:toggle . (eq major-mode 'sql-mode))))
;; (define-key language-sub-menu [sh-mode]
;;   '(menu-item "Shell" sh-mode
;;               :visible (fboundp 'sh-mode)
;;               :button (:toggle . (eq major-mode 'sh-mode))))
;; (define-key language-sub-menu [scheme-mode]
;;   '(menu-item "Scheme" scheme-mode
;;               :visible (fboundp 'scheme-mode)
;;               :button (:toggle . (eq major-mode 'scheme-mode))))
;; (define-key language-sub-menu [ruby-mode]
;;   '(menu-item "Ruby" ruby-mode
;;               :visible (fboundp 'ruby-mode)
;;               :button (:toggle . (eq major-mode 'ruby-mode))))
;; (define-key language-sub-menu [rst-mode]
;;   '(menu-item "ReST" rst-mode
;;               :visible (fboundp 'rst-mode)
;;               :button (:toggle . (eq major-mode 'rst-mode))))
;; (define-key language-sub-menu [python-mode]
;;   '(menu-item "Python" python-mode
;;               :visible (fboundp 'python-mode)
;;               :button (:toggle . (eq major-mode 'python-mode))))
;; (define-key language-sub-menu [plsql-mode]
;;   '(menu-item "PL/SQL" plsql-mode
;;               :visible (fboundp 'plsql-mode)
;;               :button (:toggle . (eq major-mode 'plsql-mode))))
;; (define-key language-sub-menu [php-mode]
;;   '(menu-item "PHP" php-mode
;;               :visible (fboundp 'php-mode)
;;               :button (:toggle . (eq major-mode 'php-mode))))
;; (defvar perl-sub-mode-menu (make-sparse-keymap "Perl"))
;; (define-key perl-sub-mode-menu [cperl-mode]
;;   '(menu-item "CPerl" cperl-mode
;;               :visible (fboundp 'cperl-mode)
;;               :button (:toggle . (eq major-mode 'cperl-mode))))
;; (define-key perl-sub-mode-menu [perl-mode]
;;   '(menu-item "Perl" perl-mode
;;               :visible (fboundp 'perl-mode)
;;               :button (:toggle . (eq major-mode 'perl-mode))))
;; (define-key language-sub-menu [perl]
;;   (list 'menu-item "Perl" perl-sub-mode-menu))
;; (define-key language-sub-menu [pascal-mode]
;;   '(menu-item "Pascal" pascal-mode
;;               :visible (fboundp 'pascal-mode)
;;               :button (:toggle . (eq major-mode 'pascal-mode))))
;; (define-key language-sub-menu [outline-mode]
;;   '(menu-item "Outline" outline-mode
;;               :visible (fboundp 'outline-mode)
;;               :button (:toggle . (eq major-mode 'outline-mode))))
;; (define-key language-sub-menu [org-mode]
;;   '(menu-item "Org" org-mode
;;               :visible (fboundp 'org-mode)
;;               :button (:toggle . (eq major-mode 'org-mode))))
;; (define-key language-sub-menu [objc-mode]
;;   '(menu-item "ObjC" objc-mode
;;               :visible (fboundp 'objc-mode)
;;               :button (:toggle . (eq major-mode 'objc-mode))))
;; (defvar makefile-sub-mode-menu (make-sparse-keymap "Makefile"))
;; (define-key makefile-sub-mode-menu [makefile-makepp-mode]
;;   '(menu-item "Makepp" makefile-makepp-mode
;;               :visible (fboundp 'makefile-makepp-mode)
;;               :button (:toggle . (eq major-mode 'makefile-makepp-mode))))
;; (define-key makefile-sub-mode-menu [makefile-imake-mode]
;;   '(menu-item "Imake" makefile-imake-mode
;;               :visible (fboundp 'makefile-imake-mode)
;;               :button (:toggle . (eq major-mode 'makefile-imake-mode))))
;; (define-key makefile-sub-mode-menu [makefile-mode]
;;   '(menu-item "Classic" makefile-mode
;;               :visible (fboundp 'makefile-mode)
;;               :button (:toggle . (eq major-mode 'makefile-mode))))
;; (define-key makefile-sub-mode-menu [makefile-bsdmake-mode]
;;   '(menu-item "BSD" makefile-bsdmake-mode
;;               :visible (fboundp 'makefile-bsdmake-mode)
;;               :button (:toggle . (eq major-mode 'makefile-bsdmake-mode))))
;; (define-key makefile-sub-mode-menu [makefile-automake-mode]
;;   '(menu-item "Automake" makefile-automake-mode
;;               :visible (fboundp 'makefile-automake-mode)
;;               :button (:toggle . (eq major-mode 'makefile-automake-mode))))
;; (define-key makefile-sub-mode-menu [makefile-gmake-mode]
;;   '(menu-item "GNU make" makefile-gmake-mode
;;               :visible (fboundp 'makefile-gmake-mode)
;;               :button (:toggle . (eq major-mode 'makefile-gmake-mode))))
;; (define-key language-sub-menu [makefile]
;;   (list 'menu-item "Makefile" makefile-sub-mode-menu))
;; (define-key language-sub-menu [lisp-mode]
;;   '(menu-item "Lisp" lisp-mode
;;               :visible (fboundp 'lisp-mode)
;;               :button (:toggle . (eq major-mode 'lisp-mode))))
;; (define-key language-sub-menu [latex-mode]
;;   '(menu-item "LaTeX" latex-mode
;;               :visible (fboundp 'latex-mode)
;;               :button (:toggle . (eq major-mode 'latex-mode))))
;; (define-key language-sub-menu [js-mode]
;;   '(menu-item "Javascript" js-mode
;;               :visible (fboundp 'js-mode)
;;               :button (:toggle . (eq major-mode 'js-mode))))
;; (define-key language-sub-menu [java-mode]
;;   '(menu-item "Java" java-mode
;;               :visible (fboundp 'java-mode)
;;               :button (:toggle . (eq major-mode 'java-mode))))
;; (define-key language-sub-menu [html-mode]
;;   '(menu-item "HTML" html-mode
;;               :visible (fboundp 'html-mode)
;;               :button (:toggle . (eq major-mode 'html-mode))))
;; (define-key language-sub-menu [hexl-mode]
;;   '(menu-item "Hex" hexl-mode
;;               :visible (fboundp 'hexl-mode)
;;               :button (:toggle . (eq major-mode 'hexl-mode))))
;; (defvar fortran-sub-mode-menu (make-sparse-keymap "Fortran"))
;; (define-key fortran-sub-mode-menu [f90-mode]
;;   '(menu-item "F90" f90-mode
;;               :visible (fboundp 'f90-mode)
;;               :button (:toggle . (eq major-mode 'f90-mode))))
;; (define-key fortran-sub-mode-menu [fortran-mode]
;;   '(menu-item "Fortran" fortran-mode
;;               :visible (fboundp 'fortran-mode)
;;               :button (:toggle . (eq major-mode 'fortran-mode))))
;; (define-key language-sub-menu [fortran]
;;   (list 'menu-item "Fortran" fortran-sub-mode-menu))
;; (define-key language-sub-menu [emacs-lisp-mode]
;;   '(menu-item "Emacs-Lisp" emacs-lisp-mode
;;               :visible (fboundp 'emacs-lisp-mode)
;;               :button (:toggle . (eq major-mode 'emacs-lisp-mode))))
;; (define-key language-sub-menu [delphi-mode]
;;   '(menu-item "Delphi" delphi-mode
;;               :visible (fboundp 'delphi-mode)
;;               :button (:toggle . (eq major-mode 'delphi-mode))))
;; (define-key language-sub-menu [css-mode]
;;   '(menu-item "CSS" css-mode
;;               :visible (fboundp 'css-mode)
;;               :button (:toggle . (eq major-mode 'css-mode))))
;; (define-key language-sub-menu [csv-mode]
;;   '(menu-item "CSV" csv-mode
;;               :visible (fboundp 'csv-mode)
;;               :button (:toggle . (eq major-mode 'csv-mode))))
;; (defvar conf-sub-mode-menu (make-sparse-keymap "Conf"))
;; (define-key conf-sub-mode-menu [conf-windows-mode]
;;   '(menu-item "Windows" conf-windows-mode
;;               :visible (fboundp 'conf-windows-mode)
;;               :button (:toggle . (eq major-mode 'conf-windows-mode))))
;; (define-key conf-sub-mode-menu [conf-javaprop-mode]
;;   '(menu-item "Java properties" conf-javaprop-mode
;;               :visible (fboundp 'conf-javaprop-mode)
;;               :button (:toggle . (eq major-mode 'conf-javaprop-mode))))
;; (define-key conf-sub-mode-menu [conf-space-mode]
;;   '(menu-item "Space keywords" conf-space-mode
;;               :visible (fboundp 'conf-space-mode)
;;               :button (:toggle . (eq major-mode 'conf-space-mode))))
;; (define-key conf-sub-mode-menu [conf-ppd-mode]
;;   '(menu-item "PPD" conf-ppd-mode
;;               :visible (fboundp 'conf-ppd-mode)
;;               :button (:toggle . (eq major-mode 'conf-ppd-mode))))
;; (define-key conf-sub-mode-menu [conf-colon-mode]
;;   '(menu-item "Colon" conf-colon-mode
;;               :visible (fboundp 'conf-colon-mode)
;;               :button (:toggle . (eq major-mode 'conf-colon-mode))))
;; (define-key conf-sub-mode-menu [conf-unix-mode]
;;   '(menu-item "Unix" conf-unix-mode
;;               :visible (fboundp 'conf-unix-mode)
;;               :button (:toggle . (eq major-mode 'conf-unix-mode))))
;; (define-key conf-sub-mode-menu [conf-xdefaults-mode]
;;   '(menu-item "Xdefaults" conf-xdefaults-mode
;;               :visible (fboundp 'conf-xdefaults-mode)
;;               :button (:toggle . (eq major-mode 'conf-xdefaults-mode))))
;; (define-key conf-sub-mode-menu [conf-mode]
;;   '(menu-item "Auto detect..." conf-mode
;;               :visible (fboundp 'conf-mode)))
;; (define-key language-sub-menu [conf]
;;   (list 'menu-item "Conf" conf-sub-mode-menu))
;; (define-key language-sub-menu [csharp-mode]
;;   '(menu-item "C#" csharp-mode
;;               :visible (fboundp 'csharp-mode)
;;               :button (:toggle . (eq major-mode 'csharp-mode))))
;; (define-key language-sub-menu [c++-mode]
;;   '(menu-item "C++" c++-mode
;;               :visible (fboundp 'c++-mode)
;;               :button (:toggle . (eq major-mode 'c++-mode))))
;; (define-key language-sub-menu [c-mode]
;;   '(menu-item "C" c-mode
;;               :visible (fboundp 'c-mode)
;;               :button (:toggle . (eq major-mode 'c-mode))))
;; (define-key language-sub-menu [autoconf-mode]
;;   '(menu-item "Autoconf" autoconf-mode
;;               :visible (fboundp 'autoconf-mode)
;;               :button (:toggle . (eq major-mode 'autoconf-mode))))
;; (define-key language-sub-menu [asm-mode]
;;   '(menu-item "Assembler" asm-mode
;;               :visible (fboundp 'asm-mode)
;;               :button (:toggle . (eq major-mode 'asm-mode))))
;; (define-key language-sub-menu [ada-mode]
;;   '(menu-item "Ada" ada-mode
;;               :visible (fboundp 'ada-mode)
;;               :button (:toggle . (eq major-mode 'ada-mode))))
;; (define-key toggle-toolbar-menu [major-mode]
;;   (list 'menu-item "Major mode" language-sub-menu))
;; (define-key toggle-toolbar-menu [separatore-encoding]
;;   '(menu-item "--"))

;; (define-key toggle-toolbar-menu [toolbarshow-toggle-emms]
;;   '(menu-item "Emms toolbar" (lambda ()
;;                                (interactive)
;;                                (toolbarshow-toggle 'toolbarshow-emms))
;;               :help "Turn emms toolbar on/off"
;;               :button (:toggle . toolbarshow-emms)))
;; (define-key toggle-toolbar-menu [toolbarshow-toggle-remember]
;;   '(menu-item "Remember toolbar" (lambda ()
;;                                (interactive)
;;                                (toolbarshow-toggle 'toolbarshow-remember))
;;               :help "Turn remember toolbar on/off"
;;               :button (:toggle . toolbarshow-remember)))
;; (define-key toggle-toolbar-menu [toolbarshow-toggle-flymake]
;;   '(menu-item "Flymake toolbar" (lambda ()
;;                                   (interactive)
;;                                   (toolbarshow-toggle 'toolbarshow-flymake))
;;               :help "Turn flymake toolbar on/off"
;;               :button (:toggle . toolbarshow-flymake)))
;; (define-key toggle-toolbar-menu [toolbarshow-toggle-program]
;;   '(menu-item "Program toolbar" (lambda ()
;;                                   (interactive)
;;                                   (toolbarshow-toggle 'toolbarshow-program))
;;               :help "Turn program toolbar on/off"
;;               :button (:toggle . toolbarshow-program)))
;; (define-key toggle-toolbar-menu [toolbarshow-toggle-view]
;;   '(menu-item "View toolbar" (lambda ()
;;                                   (interactive)
;;                                   (toolbarshow-toggle 'toolbarshow-view))
;;               :help "Turn view toolbar on/off"
;;               :button (:toggle . toolbarshow-view)))
;; (define-key toggle-toolbar-menu [toolbarshow-toggle-bookmark]
;;   '(menu-item "Bookmark toolbar" (lambda ()
;;                                   (interactive)
;;                                   (toolbarshow-toggle 'toolbarshow-bookmark))
;;               :help "Turn bookmark toolbar on/off"
;;               :button (:toggle . toolbarshow-bookmark)))
;; (define-key toggle-toolbar-menu [toolbarshow-toggle-search]
;;   '(menu-item "Search toolbar" (lambda ()
;;                                   (interactive)
;;                                   (toolbarshow-toggle 'toolbarshow-search))
;;               :help "Turn search toolbar on/off"
;;               :button (:toggle . toolbarshow-search)))
;; (define-key toggle-toolbar-menu [toolbarshow-toggle-edit]
;;   '(menu-item "Edit toolbar" (lambda ()
;;                                   (interactive)
;;                                   (toolbarshow-toggle 'toolbarshow-edit))
;;               :help "Turn edit toolbar on/off"
;;               :button (:toggle . toolbarshow-edit)))

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
(tool-bar-add-item "separator" nil 'separator-edit-toolbar
                   :visible 'toolbarshow-edit
                   :enable nil)
(tool-bar-add-item "upcase" 'upcase-region 'upcase-region
                   :visible 'toolbarshow-edit
                   :enable '(region-active-p)
                   :help '(concat "Convert the region to upper case"
                                  (key4cmd 'upcase-region)))
(tool-bar-add-item "downcase" 'downcase-region 'downcase-region
                   :visible 'toolbarshow-edit
                   :enable '(region-active-p)
                   :help '(concat "Convert the region to lower case"
                                  (key4cmd 'downcase-region)))
;; (tool-bar-add-item "comment-toggle" 'comment-dwim 'comment-dwim
;;                    :visible 'toolbarshow-edit
;;                    ;; :enable '(region-active-p)
;;                    :help '(concat "Call the comment command you want"
;;                                   (key4cmd 'comment-dwim)))
(tool-bar-add-item "format-region" 'format-region 'format-region
                   :visible 'toolbarshow-edit
                   :enable '(fboundp 'format-region)
                   :help '(concat "Format region or all buffer"
                                  (key4cmd 'format-region)))

;; search toolbar
(tool-bar-add-item "separator" nil 'separator-search-toolbar
                   :visible 'toolbarshow-search
                   :enable nil)
(tool-bar-add-item "recent-backward" 'recent-jump-jump-backward
                   'recent-jump-jump-backward
                   :visible 'toolbarshow-search
                   :enable '(fboundp 'recent-jump-jump-backward)
                   :help '(concat "Backward in the history"
                                  (key4cmd 'recent-jump-jump-backward)))
(tool-bar-add-item "recent-forward" 'recent-jump-jump-forward
                   'recent-jump-jump-forward
                   :visible 'toolbarshow-search
                   :enable '(fboundp 'recent-jump-jump-forward)
                   :help '(concat "Forward in the history"
                                  (key4cmd 'recent-jump-jump-forward)))
(tool-bar-add-item "find" 'isearch-forward 'find
                   :visible 'toolbarshow-search
                   :help '(concat "Forward String..."
                                  (key4cmd 'isearch-forward)))
(tool-bar-add-item "find-next" 'isearch-repeat-forward 'find-next
                   :visible 'toolbarshow-search
                   :help '(concat "Repeat Forward String"
                                  (key4cmd 'isearch-repeat-forward)))
(tool-bar-add-item "replace" 'query-replace 'query-replace
                   :visible 'toolbarshow-search
                   :help '(concat "Replace String..."
                                  (key4cmd 'query-replace)))
(tool-bar-add-item "grep-current-dir" 'grep-current-dir 'grep-current-dir
                   :visible 'toolbarshow-search
                   :help '(concat "Find current word in current directory"
                                  (key4cmd 'grep-current-dir)))
(tool-bar-add-item "moccur-all-buffers" 'moccur-all-buffers 'moccur-all-buffers
                   :visible 'toolbarshow-search
                   :help '(concat "Find current word in all buffers"
                                  (key4cmd 'moccur-all-buffers)))
(tool-bar-add-item "todo-grep" 'grep-todo-current-dir 'todo-grep
                   :visible 'toolbarshow-search
                   :help '(concat "Find 'TODO' in current directory"
                                  (key4cmd 'grep-todo-current-dir)))
(tool-bar-add-item "todo-moccur" 'moccur-todo-all-buffers 'todo-moccur
                   :visible 'toolbarshow-search
                   :help '(concat "Find 'TODO' in all buffers"
                                  (key4cmd 'moccur-todo-all-buffers)))

;; bookmark toolbar
(tool-bar-add-item "separator" nil 'separator-bookmark-toolbar
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
                   :help '(concat "Toggle bookmark at point"
                                  (key4cmd 'bm-toggle
                                           'viss-bookmark-toggle)))
(tool-bar-add-item "bm-next"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-next)
                         (call-interactively (bm-next))
                       (viss-bookmark-next-buffer)))
                   'bm-next
                   :visible 'toolbarshow-bookmark
                   :help '(concat "Goto next bookmark"
                                  (key4cmd 'bm-next
                                           'viss-bookmark-next-buffer)))
(tool-bar-add-item "bm-previous"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-previous)
                         (call-interactively (bm-previous))
                       (viss-bookmark-prev-buffer)))
                   'bm-previous
                   :visible 'toolbarshow-bookmark
                   :help '(concat "Goto previous bookmark"
                                  (key4cmd 'bm-previous
                                           'viss-bookmark-prev-buffer)))
(tool-bar-add-item "bm-clear"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'bm-remove-all-current-buffer)
                         (bm-remove-all-current-buffer)
                       (viss-bookmark-clear-all-buffer)))
                   'bm-remove-all-current-buffer
                   :visible 'toolbarshow-bookmark
                   :help '(concat "Delete all bookmarks in current buffer"
                                  (key4cmd 'bm-remove-all-current-buffer
                                           'viss-bookmark-clear-all-buffer)))

;; view toolbar
(tool-bar-add-item "separator" nil 'separator-view-toolbar
                   :visible 'toolbarshow-view
                   :enable nil)
(tool-bar-add-item "line-wrap" 'toggle-truncate-lines 'line-wrap
                   :visible 'toolbarshow-view
                   :enable '(not (truncated-partial-width-window-p))
                   :button '(:radio . (and (null truncate-lines)
                                           (not word-wrap)))
                   :help "Line Wrap")
(tool-bar-add-item "linum" 'linum-mode 'linum-mode
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'linum-mode)
                   :button '(:toggle . linum-mode)
                   :help '(concat "Toggle Linum mode"
                                  (key4cmd 'linum-mode)))
(tool-bar-add-item "whitespace" 'whitespace-mode 'whitespace-mode
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'whitespace-mode)
                   :button '(:toggle . whitespace-mode)
                   :help '(concat "Toggle whitespace minor mode visualization"
                                  (key4cmd 'whitespace-mode)))
(tool-bar-add-item "folding" 'hs-toggle-hiding 'folding
                   :visible 'toolbarshow-view
                   :enable '(and hs-minor-mode (fboundp 'hs-toggle-hiding))
                   :help '(concat "Toggle hiding/showing of a block"
                                  (key4cmd 'hs-toggle-hiding)))
(tool-bar-add-item "ecb" 'ecb-minor-mode 'ecb-minor-mode
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'ecb-minor-mode)
                   :button '(:toggle . ecb-minor-mode)
                   :help '(concat "Toggle ECB"
                                  (key4cmd 'ecb-minor-mode)))

;; program toolbar
(tool-bar-add-item "separator" nil 'separator-program-toolbar
                   :visible 'toolbarshow-program
                   :enable nil)
(tool-bar-add-item "semantic-jump-back" 'semantic-ia-fast-jump-back
                   'semantic-ia-fast-jump-back
                   :visible 'toolbarshow-program
                   :enable (fboundp 'semantic-ia-fast-jump-back)
                   :help '(concat "Jump back to previous tag (Semantic)"
                                  (key4cmd 'semantic-ia-fast-jump-back)))
(tool-bar-add-item "semantic-jump" 'semantic-ia-fast-jump 'semantic-ia-fast-jump
                   :visible 'toolbarshow-program
                   :enable (fboundp 'semantic-ia-fast-jump)
                   :help '(concat "Jump to the tag at point (Semantic)"
                                  (key4cmd 'semantic-ia-fast-jump-or-back
                                           'semantic-ia-fast-jump)))
(tool-bar-add-item "ff-find-other-file" 'ff-find-other-file 'ff-find-other-file
                   :visible 'toolbarshow-program
                   :enable (fboundp 'ff-find-other-file)
                   :help '(concat "Switch header and body file"
                                  (key4cmd 'ff-find-other-file)))
(tool-bar-add-item "compile" 'compile 'compile
                   :visible 'toolbarshow-program
                   :help '(concat "Compile..."
                                  (key4cmd 'compile)))
(tool-bar-add-item "debug" 'gdb 'gdb
                   :visible 'toolbarshow-program
                   :help '(concat "Debugger (GDB)..."
                                  (key4cmd 'gdb)))

;; flymake toolbar
(tool-bar-add-item "separator" nil 'separator-flymake-toolbar
                   :visible 'toolbarshow-flymake
                   :enable nil)
(tool-bar-add-item "flymake-mode" 'flymake-mode 'flymake-mode
                   :visible 'toolbarshow-flymake
                   :button '(:toggle . flymake-mode)
                   :help '(concat "Toggle flymake minor mode"
                                  (key4cmd 'flymake-mode)))
(tool-bar-add-item "flymake-check"
                   'flymake-start-syntax-check
                   'flymake-check
                   :visible 'toolbarshow-flymake
                   :enable 'flymake-mode
                   :help '(concat "Flymake - Start syntax checking"
                                  (key4cmd 'flymake-start-syntax-check)))
(tool-bar-add-item "flymake-prev" 'flymake-goto-prev-error 'flymake-prev
                   :visible 'toolbarshow-flymake
                   :enable 'flymake-mode
                   :help '(concat "Flymake - Go to prev error"
                                  (key4cmd 'flymake-goto-prev-error)))
(tool-bar-add-item "flymake-next" 'flymake-goto-next-error 'flymake-next
                   :visible 'toolbarshow-flymake
                   :enable 'flymake-mode
                   :help '(concat "Flymake - Go to next error"
                                  (key4cmd 'flymake-goto-next-error)))
(tool-bar-add-item "flymake-err-menu"
                   'flymake-display-err-menu-for-current-line
                   'flymake-err-menu
                   :visible 'toolbarshow-flymake
                   :enable 'flymake-mode
                   :help '(concat "Flymake - Display a errors/warnings menu"
                                  (key4cmd
                                   'flymake-display-err-menu-for-current-line)))

;; remember toolbar
(tool-bar-add-item "separator" nil 'separator-remember-toolbar
                   :visible 'toolbarshow-remember
                   :enable nil)
(tool-bar-add-item "remember" 'remember 'remember
                   :visible '(and toolbarshow-remember
                                  (not (eq major-mode 'remember-mode)))
                   :help '(concat "Remember"
                                  (key4cmd 'remember)))
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
                   :help '(concat "Remember the contents of the current buffer"
                                  (key4cmd 'remember-finalize)))
(tool-bar-add-item "remember-destroy" 'remember-destroy 'remember-destroy
                   :visible '(and toolbarshow-remember
                                  (eq major-mode 'remember-mode))
                   :help '(concat "Destroy the current *Remember* buffer"
                                  (key4cmd 'remember-destroy)))

;; emms toolbar
(tool-bar-add-item "separator" nil 'separator-emms-toolbar
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
                   :help '(concat "Emms"
                                  (key4cmd 'emms-dir-tree
                                           'emms)))
(tool-bar-add-item "emms-previous" 'emms-previous 'emms-previous
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-previous)
                   :help '(concat "Previous track"
                                  (key4cmd 'emms-previous)))
(tool-bar-add-item "emms-seek-backward" 'emms-seek-backward 'emms-seek-backward
                   :visible 'toolbarshow-emms
                   :enable '(and (fboundp 'emms-seek-backward)
                                 emms-player-playing-p)
                   :help '(concat "Seek backward"
                                  (key4cmd 'emms-seek-backward)))
(tool-bar-add-item "emms-start" 'emms-start 'emms-start
                   :visible '(and toolbarshow-emms
                                  (or (not (boundp 'emms-player-playing-p))
                                      (not emms-player-playing-p)))
                   :enable '(fboundp 'emms-start)
                   :help '(concat "Start"
                                  (key4cmd 'emms-start)))
(tool-bar-add-item "emms-stop" 'emms-stop 'emms-stop
                   :visible '(and toolbarshow-emms
                                  emms-player-playing-p)
                   :enable '(fboundp 'emms-stop)
                   :help '(concat "Stop"
                                  (key4cmd 'emms-stop)))
(tool-bar-add-item "emms-pause" 'emms-pause 'emms-pause
                   :visible 'toolbarshow-emms
                   :enable '(and (fboundp 'emms-pause)
                                 emms-player-playing-p)
                   :button '(:toggle . emms-player-paused-p)
                   :help '(concat (if (and (boundp 'emms-player-paused-p)
                                           emms-player-paused-p)
                                      "Resume"
                                    "Pause")
                                  (key4cmd 'emms-pause)))
(tool-bar-add-item "emms-seek-forward" 'emms-seek-forward 'emms-seek-forward
                   :visible 'toolbarshow-emms
                   :enable '(and (fboundp 'emms-seek-forward)
                                 emms-player-playing-p)
                   :help '(concat "Seek forward"
                                  (key4cmd 'emms-seek-forward)))
(tool-bar-add-item "emms-next" 'emms-next 'emms-next
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-next)
                   :help '(concat "Next track"
                                  (key4cmd 'emms-next)))


(provide 'init-toolbar)
