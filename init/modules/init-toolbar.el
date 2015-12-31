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

(require 'easymenu)

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

(defcustom toolbarshow-flycheck nil
  "If show flycheck toolbar."
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

;; toggle toolbar menu
(easy-menu-define toggle-toolbar-menu nil
  "toolbar menu"
  '("Menu"
    ("Major Mode"
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

    ("Minor Mode"
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
     ["flycheck-mode" flycheck-mode :active (fboundp 'flycheck-mode)
      :style toggle :selected flycheck-mode]
     ["flycheck-mode (global)" global-flycheck-mode :active (fboundp 'global-flycheck-mode)
      :style toggle :selected global-flycheck-mode]
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
     ["nyan-mode" nyan-mode :active (fboundp 'nyan-mode)
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
     ["US-ASCII" (lambda () (interactive) (revert-buffer-with-coding-system 'us-ascii))]
     ["UTF-8" (lambda () (interactive) (revert-buffer-with-coding-system 'utf-8))]
     ["UTF-8(BOM)" (lambda () (interactive) (revert-buffer-with-coding-system 'utf-8-with-signature))]
     ["UTF-16" (lambda () (interactive) (revert-buffer-with-coding-system 'utf-16))]
     ["UTF-16(BOM)" (lambda () (interactive) (revert-buffer-with-coding-system 'utf-16be-with-signature))]
     ["UTF-16-LE" (lambda () (interactive) (revert-buffer-with-coding-system 'utf-16-le))]
     ["UTF-16-LE(BOM)" (lambda () (interactive) (revert-buffer-with-coding-system 'utf-16le-with-signature))]
     ["GBK" (lambda () (interactive) (revert-buffer-with-coding-system 'gbk))]
     ["BIG5" (lambda () (interactive) (revert-buffer-with-coding-system 'big5))]
     ["SHIFT_JIS" (lambda () (interactive) (revert-buffer-with-coding-system 'sjis))])

    ("Change encoding"
     ["US-ASCII" (lambda () (interactive) (set-buffer-file-coding-system 'us-ascii))]
     ["UTF-8" (lambda () (interactive) (set-buffer-file-coding-system 'utf-8))]
     ["UTF-8(BOM)" (lambda () (interactive) (set-buffer-file-coding-system 'utf-8-with-signature))]
     ["UTF-16" (lambda () (interactive) (set-buffer-file-coding-system 'utf-16))]
     ["UTF-16(BOM)" (lambda () (interactive) (set-buffer-file-coding-system 'utf-16be-with-signature))]
     ["UTF-16-LE" (lambda () (interactive) (set-buffer-file-coding-system 'utf-16-le))]
     ["UTF-16-LE(BOM)" (lambda () (interactive) (set-buffer-file-coding-system 'utf-16le-with-signature))]
     ["GBK" (lambda () (interactive) (set-buffer-file-coding-system 'gbk))]
     ["BIG5" (lambda () (interactive) (set-buffer-file-coding-system 'big5))]
     ["SHIFT_JIS" (lambda () (interactive) (set-buffer-file-coding-system 'sjis))])

    "--"

    ("Toolbar"
     ["Edit Toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-edit))
      :style toggle :selected toolbarshow-edit]
     ["Search Toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-search))
      :style toggle :selected toolbarshow-search]
     ["Bookmark Toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-bookmark))
      :style toggle :selected toolbarshow-bookmark]
     ["View Toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-view))
      :style toggle :selected toolbarshow-view]
     ["Program Toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-program))
      :style toggle :selected toolbarshow-program]
     ["Flycheck Toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-flycheck))
      :style toggle :selected toolbarshow-flycheck]
     ["Remember Toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-remember))
      :style toggle :selected toolbarshow-remember]
     ["Emms Toolbar"
      (lambda () (interactive) (toolbarshow-toggle 'toolbarshow-emms))
      :style toggle :selected toolbarshow-emms])

    ("Emms"
     ["Emms" (lambda ()
               (interactive)
               (if (fboundp 'emms-dir-tree)
                   (emms-dir-tree)
                 (emms)))
      :active (or (fboundp 'emms-dir-tree) (fboundp 'emms))]
     ["Previous Track" emms-previous (fboundp 'emms-previous)]
     ["Seek Backward" emms-seek-backward
      :active (and (fboundp 'emms-seek-backward) emms-player-playing-p)]
     ["Start" emms-start
      :active (and (fboundp 'emms-start) (not emms-player-playing-p))]
     ["Pause" emms-pause
      :active (and emms-player-playing-p (not emms-player-paused-p))
      :visible emms-player-playing-p]
     ["Resume" emms-pause
      :active (and emms-player-paused-p emms-player-paused-p)
      :visible emms-player-playing-p]
     ["Stop" emms-stop
      :active (and (fboundp 'emms-stop) emms-player-playing-p)]
     ["Seek Forward" emms-seek-forward
      :active (and (fboundp 'emms-seek-forward) emms-player-playing-p)]
     ["Next Track" emms-next (fboundp 'emms-next)])

    ("Misc"
     ["find-dotemacs-file" find-dotemacs-file (fboundp 'find-dotemacs-file)]
     ["multi-term" multi-term (fboundp 'multi-term)]
     ["cfw:open-org-calendar" cfw:open-org-calendar (fboundp 'cfw:open-org-calendar)]
     ["format-cxx-directory" format-cxx-directory (fboundp 'format-cxx-directory)]
     ["format-xml-directory" format-xml-directory (fboundp 'format-xml-directory)]
     ["ielm" ielm (fboundp 'ielm)]
     ["proced" proced (fboundp 'proced)]
     ["ascii-display" ascii-display (fboundp 'ascii-display)])))

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
                   :help '(concat "Upcase Region" (key4cmd 'upcase-region)))
(tool-bar-add-item "downcase" 'downcase-region 'downcase-region
                   :visible 'toolbarshow-edit
                   :enable '(region-active-p)
                   :help '(concat "Downcase Region" (key4cmd 'downcase-region)))
(tool-bar-add-item "comment" 'comment-dwim 'comment-dwim
                   :visible 'toolbarshow-edit
                   ;; :enable '(region-active-p)
                   :help '(concat "Comment Dwim" (key4cmd 'comment-dwim)))
(tool-bar-add-item "format-region" 'format-region 'format-region
                   :visible 'toolbarshow-edit
                   :enable '(fboundp 'format-region)
                   :help '(concat "Format Fegion" (key4cmd 'format-region)))

;; search toolbar
(tool-bar-add-item "separator" nil 'separator-search-toolbar
                   :visible 'toolbarshow-search
                   :enable nil)
;; (tool-bar-add-item "recent-backward" 'recent-jump-jump-backward 'recent-jump-jump-backward
;;                    :visible 'toolbarshow-search
;;                    :enable '(fboundp 'recent-jump-jump-backward)
;;                    :help '(concat "Backward" (key4cmd 'recent-jump-jump-backward)))
;; (tool-bar-add-item "recent-forward" 'recent-jump-jump-forward 'recent-jump-jump-forward
;;                    :visible 'toolbarshow-search
;;                    :enable '(fboundp 'recent-jump-jump-forward)
;;                    :help '(concat "Forward" (key4cmd 'recent-jump-jump-forward)))
(tool-bar-add-item "find" 'isearch-forward 'find
                   :visible 'toolbarshow-search
                   :help '(concat "Find..." (key4cmd 'isearch-forward)))
(tool-bar-add-item "find-next" 'isearch-repeat-forward 'find-next
                   :visible 'toolbarshow-search
                   :help '(concat "Find next" (key4cmd 'isearch-repeat-forward)))
(tool-bar-add-item "replace" 'query-replace 'query-replace
                   :visible 'toolbarshow-search
                   :help '(concat "Replace..." (key4cmd 'query-replace)))
(tool-bar-add-item "grep-current-dir" 'grep-current-dir 'grep-current-dir
                   :visible 'toolbarshow-search
                   :enable '(fboundp 'grep-current-dir)
                   :help '(concat "Grep Current Directory" (key4cmd 'grep-current-dir)))
(tool-bar-add-item "moccur-all-buffers" 'moccur-all-buffers 'moccur-all-buffers
                   :visible 'toolbarshow-search
                   :enable '(fboundp 'moccur-all-buffers)
                   :help '(concat "Moccur All Buffers" (key4cmd 'moccur-all-buffers)))
(tool-bar-add-item "todo-grep" 'grep-todo-current-dir 'todo-grep
                   :visible 'toolbarshow-search
                   :enable '(fboundp 'grep-todo-current-dir)
                   :help '(concat "Grep 'TODO' Current Directory" (key4cmd 'grep-todo-current-dir)))
(tool-bar-add-item "todo-moccur" 'moccur-todo-all-buffers 'todo-moccur
                   :visible 'toolbarshow-search
                   :enable '(fboundp 'moccur-todo-all-buffers)
                   :help '(concat "Moccur 'TODO' All Buffers" (key4cmd 'moccur-todo-all-buffers)))

;; bookmark toolbar
(tool-bar-add-item "separator" nil 'separator-bookmark-toolbar
                   :visible 'toolbarshow-bookmark
                   :enable nil)
(tool-bar-add-item "bm-toggle" 'bm-toggle 'bm-toggle
                   :visible 'toolbarshow-bookmark
                   :enable '(fboundp 'bm-toggle)
                   :help '(concat "Toggle Bookmark" (key4cmd 'bm-toggle)))
(tool-bar-add-item "bm-next" 'bm-next 'bm-next
                   :visible 'toolbarshow-bookmark
                   :enable '(fboundp 'bm-next)
                   :help '(concat "Nnext Bookmark" (key4cmd 'bm-next)))
(tool-bar-add-item "bm-previous" 'bm-previous 'bm-previous
                   :visible 'toolbarshow-bookmark
                   :enable '(fboundp 'bm-previous)
                   :help '(concat "Previous Bookmark" (key4cmd 'bm-previous)))
(tool-bar-add-item "bm-clear"' bm-remove-all-current-buffer 'bm-remove-all-current-buffer
                   :visible 'toolbarshow-bookmark
                   :enable '(fboundp 'bm-remove-all-current-buffer)
                   :help '(concat "Clear All Bookmarks" (key4cmd 'bm-remove-all-current-buffer)))

;; view toolbar
(tool-bar-add-item "separator" nil 'separator-view-toolbar
                   :visible 'toolbarshow-view
                   :enable nil)
(tool-bar-add-item "line-wrap" 'toggle-truncate-lines 'line-wrap
                   :visible 'toolbarshow-view
                   :enable '(not (truncated-partial-width-window-p))
                   :button '(:radio . (and (null truncate-lines) (not word-wrap)))
                   :help '(concat "Wrap" (key4cmd 'toggle-truncate-lines)))
(tool-bar-add-item "linum" 'linum-mode 'linum-mode
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'linum-mode)
                   :button '(:toggle . linum-mode)
                   :help '(concat "Linum" (key4cmd 'linum-mode)))
(tool-bar-add-item "whitespace" 'whitespace-mode 'whitespace-mode
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'whitespace-mode)
                   :button '(:toggle . whitespace-mode)
                   :help '(concat "Whitespace" (key4cmd 'whitespace-mode)))
(tool-bar-add-item "folding" 'hs-toggle-hiding 'folding
                   :visible 'toolbarshow-view
                   :enable '(and hs-minor-mode (fboundp 'hs-toggle-hiding))
                   ;; :button '(:toggle . (hs-already-hidden-p))
                   :help '(concat "Toggle Hiding" (key4cmd 'hs-toggle-hiding)))
(tool-bar-add-item "ecb" 'ecb-minor-mode 'ecb-minor-mode
                   :visible 'toolbarshow-view
                   :enable '(fboundp 'ecb-minor-mode)
                   :button '(:toggle . ecb-minor-mode)
                   :help '(concat "ECB" (key4cmd 'ecb-minor-mode)))

;; program toolbar
(tool-bar-add-item "separator" nil 'separator-program-toolbar
                   :visible 'toolbarshow-program
                   :enable nil)
(tool-bar-add-item "semantic-jump-back" 'semantic-ia-fast-jump-back 'semantic-ia-fast-jump-back
                   :visible 'toolbarshow-program
                   :enable (fboundp 'semantic-ia-fast-jump-back)
                   :help '(concat "Backward(Semantic)" (key4cmd 'semantic-ia-fast-jump-back)))
(tool-bar-add-item "semantic-jump" 'semantic-ia-fast-jump 'semantic-ia-fast-jump
                   :visible 'toolbarshow-program
                   :enable (fboundp 'semantic-ia-fast-jump)
                   :help '(concat "Forward(Semantic)"
                                  (key4cmd 'semantic-ia-fast-jump-or-back 'semantic-ia-fast-jump)))
(tool-bar-add-item "ff-find-other-file" 'ff-find-other-file 'ff-find-other-file
                   :visible 'toolbarshow-program
                   :enable (fboundp 'ff-find-other-file)
                   :help '(concat "Find other file" (key4cmd 'ff-find-other-file)))
(tool-bar-add-item "compile" 'compile 'compile
                   :visible 'toolbarshow-program
                   :help '(concat "Compile..." (key4cmd 'compile)))
(tool-bar-add-item "debug" 'gdb 'gdb
                   :visible 'toolbarshow-program
                   :help '(concat "Debugger (GDB)..." (key4cmd 'gdb)))

;; flycheck toolbar
(tool-bar-add-item "separator" nil 'separator-flycheck-toolbar
                   :visible 'toolbarshow-flycheck
                   :enable nil)
(tool-bar-add-item "flycheck-mode" 'flycheck-mode 'flycheck-mode
                   :visible 'toolbarshow-flycheck
                   :button '(:toggle . flycheck-mode)
                   :help '(concat "Flycheck Mode" (key4cmd 'flycheck-mode)))
(tool-bar-add-item "flycheck-buffer" 'flycheck-buffer 'flycheck-buffer
                   :visible 'toolbarshow-flycheck
                   :enable 'flycheck-mode
                   :help '(concat "Flycheck - Check" (key4cmd 'flycheck-buffer)))
(tool-bar-add-item "flycheck-clear" 'flycheck-clear 'flycheck-clear
                   :visible 'toolbarshow-flycheck
                   :enable 'flycheck-mode
                   :help '(concat "Flycheck - Clear" (key4cmd 'flycheck-clear)))
(tool-bar-add-item "flycheck-previous-error" 'flycheck-previous-error 'flycheck-previous-error
                   :visible 'toolbarshow-flycheck
                   :enable 'flycheck-mode
                   :help '(concat "Flycheck - Prev Error" (key4cmd 'flycheck-previous-error)))
(tool-bar-add-item "flycheck-next-error" 'flycheck-next-error 'flycheck-next-error
                   :visible 'toolbarshow-flycheck
                   :enable 'flycheck-mode
                   :help '(concat "Flycheck - Next Error" (key4cmd 'flycheck-next-error)))
(tool-bar-add-item "flycheck-list-errors" 'flycheck-list-errors 'flycheck-list-errors
                   :visible 'toolbarshow-flycheck
                   :enable 'flycheck-mode
                   :help '(concat "Flycheck - List Errors" (key4cmd 'flycheck-list-errors)))

;; remember toolbar
(tool-bar-add-item "separator" nil 'separator-remember-toolbar
                   :visible 'toolbarshow-remember
                   :enable nil)
(tool-bar-add-item "remember" 'remember 'remember
                   :visible '(and toolbarshow-remember (not (eq major-mode 'remember-mode)))
                   :help '(concat "Remember" (key4cmd 'remember)))
(tool-bar-add-item "remember-open"
                   (lambda ()
                     (interactive)
                     (require 'remember nil 'noerror)
                     (find-file remember-data-file))
                   'remember-open
                   :visible '(and toolbarshow-remember (not (eq major-mode 'remember-mode)))
                   :help "Open Remember Data File")
(tool-bar-add-item "remember-finalize" 'remember-finalize 'remember-finalize
                   :visible '(and toolbarshow-remember (eq major-mode 'remember-mode))
                   :help '(concat "Remember Finalize" (key4cmd 'remember-finalize)))
(tool-bar-add-item "remember-destroy" 'remember-destroy 'remember-destroy
                   :visible '(and toolbarshow-remember (eq major-mode 'remember-mode))
                   :help '(concat "Remember Destroy" (key4cmd 'remember-destroy)))

;; emms toolbar
(tool-bar-add-item "separator" nil 'separator-emms-toolbar
                   :visible 'toolbarshow-emms
                   :enable nil)
(tool-bar-add-item "emms"
                   (lambda ()
                     (interactive)
                     (if (fboundp 'emms-dir-tree) (emms-dir-tree) (emms)))
                   'emms
                   :visible 'toolbarshow-emms
                   :help '(concat "Emms" (key4cmd 'emms-dir-tree 'emms)))
(tool-bar-add-item "emms-previous" 'emms-previous 'emms-previous
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-previous)
                   :help '(concat "Previous Track" (key4cmd 'emms-previous)))
(tool-bar-add-item "emms-seek-backward" 'emms-seek-backward 'emms-seek-backward
                   :visible 'toolbarshow-emms
                   :enable '(and (fboundp 'emms-seek-backward) emms-player-playing-p)
                   :help '(concat "Seek Backward" (key4cmd 'emms-seek-backward)))
(tool-bar-add-item "emms-start" 'emms-start 'emms-start
                   :visible '(and toolbarshow-emms
                                  (or (not (boundp 'emms-player-playing-p))
                                      (not emms-player-playing-p)))
                   :enable '(fboundp 'emms-start)
                   :help '(concat "Start" (key4cmd 'emms-start)))
(tool-bar-add-item "emms-stop" 'emms-stop 'emms-stop
                   :visible '(and toolbarshow-emms emms-player-playing-p)
                   :enable '(fboundp 'emms-stop)
                   :help '(concat "Stop" (key4cmd 'emms-stop)))
(tool-bar-add-item "emms-pause" 'emms-pause 'emms-pause
                   :visible 'toolbarshow-emms
                   :enable '(and (fboundp 'emms-pause) emms-player-playing-p)
                   :button '(:toggle . emms-player-paused-p)
                   :help '(concat (if (and (boundp 'emms-player-paused-p)
                                           emms-player-paused-p)
                                      "Resume"
                                    "Pause")
                                  (key4cmd 'emms-pause)))
(tool-bar-add-item "emms-seek-forward" 'emms-seek-forward 'emms-seek-forward
                   :visible 'toolbarshow-emms
                   :enable '(and (fboundp 'emms-seek-forward) emms-player-playing-p)
                   :help '(concat "Seek Forward" (key4cmd 'emms-seek-forward)))
(tool-bar-add-item "emms-next" 'emms-next 'emms-next
                   :visible 'toolbarshow-emms
                   :enable '(fboundp 'emms-next)
                   :help '(concat "Next Track" (key4cmd 'emms-next)))

(provide 'init-toolbar)
