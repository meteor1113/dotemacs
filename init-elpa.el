;;; -*- mode: emacs-lisp; mode: goto-address; coding: utf-8; -*-
;; Copyright (C) 2008-2015 Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @date 2015-04-28
;; @URL http://git.oschina.net/meteor1113/dotemacs


;; package
(setq package-user-dir
      (expand-file-name "elpa" (file-name-directory
                                (or load-file-name (buffer-file-name)))))
(when (require 'package nil 'noerror)
  ;; (add-to-list 'package-archives
  ;;              '("marmalade" . "https://marmalade-repo.org/packages/"))
  ;; (add-to-list 'package-archives
  ;;              '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")))
(defun compile-all-package ()
  "Byte-compile all installed package."
  (interactive)
  (dolist (elt package-alist)
    (package--compile (car (cdr elt)))))

;; anything
;; (autoload 'anything "anything" nil t)
(setq anything-command-map-prefix-key "")
;; (eval-after-load "anything"
;;   '(require 'anything-config nil 'noerror))

;; auto-complete
(setq ac-use-comphist nil)
(setq ac-disable-faces nil)
;; (global-set-key (kbd "M-n") 'auto-complete)
(add-hook 'after-init-hook '(lambda () (ignore-errors (ac-config-default))))
(eval-after-load "auto-complete"
  '(progn
     (define-key ac-completing-map [return] 'ac-complete)
     (setq ac-modes
           (append ac-modes
                   '(org-mode objc-mode csharp-mode jde-mode sql-mode
                              plsql-mode sqlplus-mode eshell-mode
                              inferior-emacs-lisp-mode change-log-mode
                              text-mode xml-mode nxml-mode html-mode
                              tex-mode latex-mode plain-tex-mode
                              conf-unix-mode conf-windows-mode
                              conf-colon-mode conf-space-mode
                              conf-javaprop-mode inetd-conf-generic-mode
                              etc-services-generic-mode etc-passwd-generic-mode
                              etc-fstab-generic-mode etc-sudoers-generic-mode
                              resolve-conf-generic-mode
                              etc-modules-conf-generic-mode
                              apache-conf-generic-mode apache-log-generic-mode
                              samba-generic-mode reg-generic-mode
                              fvwm-generic-mode ini-generic-mode
                              x-resource-generic-mode
                              hosts-generic-mode inf-generic-mode
                              bat-generic-mode javascript-generic-mode
                              vrml-generic-mode java-manifest-generic-mode
                              java-properties-generic-mode
                              alias-generic-mode rc-generic-mode
                              makefile-gmake-mode makefile-bsdmake-mode
                              autoconf-mode makefile-automake-mode)))
     (defadvice ac-update-word-index-1 (around exclude-hidden-buffer activate)
       "Exclude hidden buffer, hack for eim."
       (unless (string= (substring (buffer-name) 0 1) " ")
         ad-do-it))))
(eval-after-load "auto-complete-config"
  '(progn
     (add-hook 'ielm-mode-hook 'ac-emacs-lisp-mode-setup)
     (add-hook 'eshell-mode-hook 'ac-emacs-lisp-mode-setup)
     (defun ac-semantic-setup ()
       ;; (setq ac-sources (append '(ac-source-semantic) ac-sources))
       (local-set-key (kbd "M-n") 'ac-complete-semantic))
     (add-hook 'c-mode-common-hook 'ac-semantic-setup)
     (when (require 'auto-complete-clang nil 'noerror)
       (setq ac-clang-flags
             '("-I.." "-I../include" "-I../inc" "-I../common" "-I../public"
               "-I../.." "-I../../include" "-I../../inc" "-I../../common"
               "-I../../public"))
       (when (fboundp 'semantic-gcc-get-include-paths)
         (let ((dirs (semantic-gcc-get-include-paths "c++")))
           (dolist (dir dirs)
             (add-to-list 'ac-clang-flags (concat "-I" dir)))))
       (defun ac-clang-setup ()
         (local-set-key (kbd "M-p") 'ac-complete-clang))
       (add-hook 'c-mode-common-hook 'ac-clang-setup))
     (setq ac-source-ropemacs              ; Redefine ac-source-ropemacs
           '((candidates . (lambda ()
                             (setq ac-ropemacs-completions-cache
                                   (mapcar
                                    (lambda (completion)
                                      (concat ac-prefix completion))
                                    (ignore-errors
                                      (rope-completions))))))
             (prefix . c-dot)
             (requires . 0)))
     (defun ac-complete-ropemacs ()
       (interactive)
       (auto-complete '(ac-source-ropemacs)))
     (defun ac-ropemacs-setup ()
       (when (locate-library "pymacs")
         (ac-ropemacs-require)
         ;; (setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
         (local-set-key (kbd "M-n") 'ac-complete-ropemacs)))
     (ac-ropemacs-initialize)
     (defun ac-yasnippet-setup ()
       (add-to-list 'ac-sources 'ac-source-yasnippet))
     (add-hook 'auto-complete-mode-hook 'ac-yasnippet-setup)))

;; bm
(setq bm-restore-repository-on-load t)
(add-hook 'after-init-hook '(lambda () (require 'bm nil 'noerror)))
(setq-default bm-buffer-persistence t)
(setq bm-cycle-all-buffers t)
(setq bm-highlight-style
      (if (and window-system (> emacs-major-version 21))
          'bm-highlight-only-fringe
        'bm-highlight-only-line))
(defun bm-next-or-previous (&optional previous)
  (interactive "P")
  (if previous
      (bm-previous)
    (bm-next)))
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key [M-f2] 'bm-toggle)
(global-set-key (kbd "ESC <f2>") 'bm-toggle) ; putty
(global-set-key (kbd "<f2>")   'bm-next-or-previous)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key [f14] 'bm-previous)   ; S-f2
;; (global-set-key (kbd "ESC ESC <f2>") 'bm-previous)
(global-set-key (kbd "<C-S-f2>") 'bm-remove-all-current-buffer)
;; (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
;; (global-set-key (kbd "<left-fringe> <mouse-2>") 'bm-toggle-mouse)
;; (global-set-key (kbd "<left-fringe> <mouse-3>") 'bm-next-mouse)
;; (global-set-key [left-margin mouse-1] 'bm-toggle-mouse)
(global-set-key [left-margin mouse-1] 'bm-toggle-mouse)
(global-set-key [left-margin mouse-3] 'bm-next-mouse)
(eval-after-load "bm"
  '(progn
     ;; (add-hook' after-init-hook 'bm-repository-load)
     ;; (add-hook 'after-save-hook 'bm-buffer-save)
     ;; (add-hook 'after-revert-hook 'bm-buffer-restore)
     (add-hook 'find-file-hooks 'bm-buffer-restore)
     (add-hook 'kill-buffer-hook 'bm-buffer-save)
     (add-hook 'kill-emacs-hook '(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))
     (defadvice bm-next (after pulse-advice activate)
       "After bm-next, pulse the line the cursor lands on."
       (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                  (interactive-p))
         (pulse-momentary-highlight-one-line (point))))
     (defadvice bm-previous (after pulse-advice activate)
       "After bm-previous, pulse the line the cursor lands on."
       (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                  (interactive-p))
         (pulse-momentary-highlight-one-line (point))))
     (defadvice bm-next-or-previous (after pulse-advice activate)
       "After bm-next-or-previous, pulse the line the cursor lands on."
       (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                  (interactive-p))
         (pulse-momentary-highlight-one-line (point))))
     (defadvice bm-next-mouse (after pulse-advice activate)
       "After bm-next-mouse, pulse the line the cursor lands on."
       (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                  (interactive-p))
         (pulse-momentary-highlight-one-line (point))))
     (defadvice bm-previous-mouse (after pulse-advice activate)
       "After bm-previous-mouse, pulse the line the cursor lands on."
       (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                  (interactive-p))
         (pulse-momentary-highlight-one-line (point))))))

;; browse-kill-ring
(add-hook 'after-init-hook
          '(lambda () (ignore-errors (browse-kill-ring-default-keybindings))))

;; calfw
(autoload 'cfw:open-org-calendar "calfw-org" nil t)
(autoload 'cfw:open-diary-calendar "calfw-cal" nil t)
;; (autoload 'cfw:open-calendar-buffer "calfw" nil t)
;; (eval-after-load "calfw"
;;   '(when (require 'calfw-org nil 'noerror)
;;      (cfw:install-org-schedules)))

;; color-theme
;; (require 'color-theme-autoloads nil 'noerror)

;; company
(setq company--disabled-backends '(company-pysmell))
;; (autoload 'company-mode "company" nil t)
;; (autoload 'global-company-mode "company" nil t)
(eval-after-load "company"
  '(progn
     (setq company-idle-delay nil)
     ;; (setq company-idle-delay t
     ;;       company-minimum-prefix-length 1
     ;;       company-begin-commands '(self-insert-command c-electric-lt-gt))
     (define-key company-mode-map (kbd "M-n") 'company-select-next)
     (define-key company-mode-map (kbd "M-p") 'company-select-previous)))

;; csharp-mode
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(add-hook 'csharp-mode-hook
          (lambda ()
            (c-set-style "c#")
            (setq imenu-generic-expression cc-imenu-java-generic-expression)
            (imenu-add-menubar-index)))

;; csv-mode
;; (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;; (autoload 'csv-mode "csv-mode"
;;   "Major mode for editing comma-separated value files." t)

;; cursor-chg
(autoload 'change-cursor-mode "cursor-chg" nil t)
;; (when (require 'cursor-chg nil 'noerror)
;;   ;; (toggle-cursor-type-when-idle 1)
;;   (change-cursor-mode 1))

;; diff-hl
;; (autoload 'diff-hl-mode "diff-hl" nil t)
;; (autoload 'global-diff-hl-mode "diff-hl" nil t)
;; (autoload 'diff-hl-dired-mode "diff-hl-dired" nil t)
;; (add-hook 'dired-mode-hook '(lambda () (ignore-errors (diff-hl-dired-mode 1))))
(add-hook 'after-init-hook '(lambda () (ignore-errors (global-diff-hl-mode 1))))

;; dired+
(when window-system
  (eval-after-load "dired"
    '(when (require 'dired+ nil 'noerror)
       (define-key dired-mode-map [mouse-2] 'diredp-mouse-find-file)
       (diredp-toggle-find-file-reuse-dir 1))))

;; drag-stuff
;; (autoload 'drag-stuff-global-mode "drag-stuff" "Toggle Drag-Stuff mode." t)
;; (when (and (ignore-errors (require 'drag-stuff nil 'noerror))
;;            (fboundp 'drag-stuff-global-mode))
;;   (drag-stuff-global-mode t))

;; ecb
;; (require 'ecb-autoloads nil 'noerror)
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
      ecb-source-path (quote ("/" ("c:" "c:")))
      ecb-layout-name 'left3
      ecb-toggle-layout-sequence '("left3"
                                   "left8"
                                   "left-analyse"
                                   "left-symboldef")
      ecb-windows-width 0.25
      ecb-compile-window-height 0.1
      ecb-compile-window-width 'edit-window
      ecb-compile-window-temporally-enlarge 'after-selection
      ;; ecb-enlarged-compilation-window-max-height 0.8
      ;; ecb-cedet-required-version-max '(2 0 4 9)
      ecb-tip-of-the-day nil
      ecb-auto-compatibility-check nil)
(unless (boundp 'stack-trace-on-error)
  (defvar stack-trace-on-error nil))
(when (fboundp 'ecb-minor-mode)
  (defvar ecb-minor-mode nil))
(eval-after-load "ecb-compilation"
  '(progn
     (setq ecb-compilation-buffer-names
           (append ecb-compilation-buffer-names '(("*Process List*")
                                                  ("*Proced*")
                                                  (".notes")
                                                  ("notes")
                                                  ("*appt-buf*")
                                                  ("*Compile-Log*")
                                                  ("*etags tmp*")
                                                  (" *svn-process*")
                                                  ("*svn-info-output*")
                                                  ("*Python Output*")
                                                  ("*Org Agenda*")
                                                  (" *EMMS Playlist*")
                                                  ("*Moccur*")
                                                  ("*Directory"))))
     (setq ecb-compilation-major-modes
           (append ecb-compilation-major-modes '(change-log-mode
                                                 calendar-mode
                                                 diary-mode
                                                 diary-fancy-display-mode
                                                 xgtags-select-mode
                                                 svn-status-mode
                                                 svn-info-mode
                                                 svn-status-diff-mode
                                                 svn-log-view-mode
                                                 svn-log-edit-mode
                                                 erc-mode
                                                 gud-mode)))))

;; emms
(autoload 'emms "emms-playlist-mode" nil t)
(eval-after-load "emms-playlist-mode"
  '(progn
     (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
     (define-key emms-playlist-mode-map [double-mouse-1]
       'emms-playlist-mode-play-current-track)))
(defun init-emms ()
  "Initial emms."
  (or (featurep 'emms-setup)
      (when (and (require 'emms-setup nil t)
                 (require 'emms-mode-line nil t)
                 (require 'emms-playing-time nil t))
        (emms-standard)
        (emms-default-players)
        (progn
          (define-emms-simple-player mpg123 '(file url)
            (emms-player-simple-regexp "mp3" "mp2") "mpg123")
          (add-to-list 'emms-player-list 'emms-player-mpg123 'append))
        (setq emms-repeat-playlist t)
        (setq emms-mode-line-format "[%s]")
        (defun emms-mode-line-playlist-current-nondirectory ()
          "Format the currently playing song."
          (format emms-mode-line-format
                  (file-name-nondirectory
                   (emms-track-description
                    (emms-playlist-current-selected-track)))))
        (setq emms-mode-line-mode-line-function
              'emms-mode-line-playlist-current-nondirectory)
        (setq emms-mode-line-titlebar-function
              'emms-mode-line-playlist-current-nondirectory)
        (emms-mode-line 1)
        (emms-mode-line-blank)
        ;; (setq emms-playing-time-style 'bar)
        ;; (emms-playing-time 1)
        t)))
(defadvice emms (before init-emms activate)
  "Initial emms first."
  (init-emms))
(defun emms-dir-tree ()
  "Query for a directory tree, or switch to the current emms-playlist buffer."
  (interactive)
  (if (init-emms)
      (if (or (null emms-playlist-buffer)
              (not (buffer-live-p emms-playlist-buffer)))
          (call-interactively 'emms-play-directory-tree)
        (emms-playlist-mode-go))
    (message "Initial emms failed.")))
(defadvice emms-history-save (around delete-empty-history activate)
  "If have not emms playlist, delete emms-history-file."
  (let (have-playlist)
    (dolist (buf (emms-playlist-buffer-list))
      (when (> (buffer-size buf) 0)
        (setq have-playlist t)))
    (if (not have-playlist)
        (when (file-exists-p emms-history-file)
          (delete-file emms-history-file))
      (ignore-errors (make-directory (file-name-directory emms-history-file)))
      ad-do-it)))
;; (when (and window-system
;;            (require 'emms-history nil t)
;;            (file-exists-p emms-history-file)
;;            (init-emms))
;;   (setq emms-history-start-playing t)
;;   (emms-history-load))

;; fci-mode
;; (autoload 'fci-mode "fill-column-indicator" nil t)

;; hideshowvis
;; (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions" t)

;; highlight-parentheses
;; (autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
;; (add-hook 'find-file-hooks
;;           (lambda ()
;;             (when (require 'highlight-parentheses nil 'noerror)
;;               (highlight-parentheses-mode t))))

;; highlight-symbol
(setq highlight-symbol-idle-delay 0.5)
(mapc (lambda (hook)
        (add-hook hook (lambda () (ignore-errors (highlight-symbol-mode 1)))))
      '(c-mode-common-hook
        fortran-mode-hook f90-mode-hook ada-mode-hook
        python-mode-hook ruby-mode-hook perl-mode-hook cperl-mode-hook
        emacs-lisp-mode-hook sh-mode-hook js-mode-hook js2-mode-hook
        nxml-mode-hook sgml-mode-hook sql-mode-hook))
(defun highlight-symbol-next-or-prev (&optional prev)
       (interactive "P")
       (if prev
           (highlight-symbol-prev)
         (highlight-symbol-next)))
(global-set-key [(meta f3)] 'highlight-symbol-at-point)
(global-set-key (kbd "ESC <f3>") 'highlight-symbol-at-point) ; putty
(global-set-key [f3] 'highlight-symbol-next-or-prev)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [f15] 'highlight-symbol-prev) ; S-f3
;; (global-set-key (kbd "ESC ESC <f3>") 'highlight-symbol-prev)
(global-set-key [(control f3)] 'highlight-symbol-query-replace)
(eval-after-load "highlight-symbol"
  '(progn
     (defadvice highlight-symbol-mode (after disable activate)
       "Disable highlight-symbol-mode-post-command."
       (remove-hook 'post-command-hook 'highlight-symbol-mode-post-command t))
     ;; (custom-set-faces
     ;;  '(highlight-symbol-face
     ;;    ((((class color) (background dark)) (:background "magenta"))
     ;;     (((class color) (background light)) (:background "gray83")))))
     (set-face-background 'highlight-symbol-face
                          (if window-system "gray83" "magenta"))
     ;; (if (eq frame-background-mode 'dark) "magenta" "gray83"))
     (if (daemonp)
         (add-hook 'after-make-frame-functions
                   (lambda (frame)
                     (with-selected-frame frame
                       (set-face-background 'highlight-symbol-face
                                            (if window-system "gray83" "magenta")
                                            frame)))))
     ;; (defun highlight-symbol-temp-highlight () ; Hack for emacs-21
     ;;   "Highlight the current symbol until a command is executed."
     ;;   (when highlight-symbol-mode
     ;;     (let ((symbol (highlight-symbol-get-symbol)))
     ;;       (unless (or (equal symbol highlight-symbol)
     ;;                   (member symbol highlight-symbol-list))
     ;;         (highlight-symbol-mode-remove-temp)
     ;;         (when symbol
     ;;           (setq highlight-symbol symbol)
     ;;           (if (< emacs-major-version 22)
     ;;               (let ((color `((background-color . ,"grey")
     ;;                              (foreground-color . "black"))))
     ;;                 (hi-lock-set-pattern `(,symbol (0 (quote ,color) t))))
     ;;             (hi-lock-set-pattern symbol 'highlight-symbol-face)))))))
     (defadvice highlight-symbol-next (after pulse-advice activate)
       "After highlight-symbol-next, pulse the line the cursor lands on."
       (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                  (interactive-p))
         (pulse-momentary-highlight-one-line (point))))
     (defadvice highlight-symbol-prev (after pulse-advice activate)
       "After highlight-symbol-prev, pulse the line the cursor lands on."
       (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                  (interactive-p))
         (pulse-momentary-highlight-one-line (point))))
     (defadvice highlight-symbol-next-or-prev (after pulse-advice activate)
       "After highlight-symbol-next-or-prev, pulse the line the cursor lands on."
       (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                  (interactive-p))
         (pulse-momentary-highlight-one-line (point))))))

;; highlight-tail
;; (autoload 'highlight-tail-mode "highlight-tail" nil t)

;; htmlize
;; (autoload 'htmlize-buffer "htmlize" nil t)

;; js2-mode
;; (autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-strict-missing-semi-warning nil)
(add-hook 'js2-mode-hook
          (lambda ()
            (linum-mode 1)
            (hs-minor-mode t)))
(eval-after-load 'js2-mode
  '(progn
     (ignore-errors (js2-imenu-extras-setup))))

;; magit
;; (autoload 'magit-status "magit" nil t)

;; markdown-mode
;; (autoload 'markdown-mode "markdown-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; mark-multiple
;; (require 'inline-string-rectangle)
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
;; (require 'mark-more-like-this)
;; (autoload 'mark-previous-like-this "mark-more-like-this" nil t)
;; (autoload 'mark-next-like-this "mark-more-like-this" nil t)
;; (autoload 'mark-more-like-this "mark-more-like-this" nil t)
;; (autoload 'mark-all-like-this "mark-more-like-this" nil t)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; multi-term
;; (autoload 'multi-term "multi-term" nil t)

;; nyan-mode
;; (autoload 'nyan-mode "nyan-mode" nil t)
(autoload 'nyan-start-animation "nyan-mode" nil t)
(autoload 'nyan-stop-animation "nyan-mode" nil t)
;; (setq nyan-wavy-trail t)
(setq nyan-bar-length 8)
(defadvice nyan-mode (after animation activate)
  (if nyan-mode
      (nyan-start-animation)
    (nyan-stop-animation)))
;; (ignore-errors (and window-system (nyan-mode t)))

;; php-mode
;; (autoload 'php-mode "php-mode" nil t)
;; (add-to-list 'auto-mode-alist
;;              '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

;; plsql
(autoload 'plsql-mode "plsql" nil t)
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.\\(p\\(?:k[bg]\\|ls\\)\\|[sS][qQ][lL]\\|[pP][rR][cC]\\)\\'"
;;           . plsql-mode))
;;        auto-mode-alist))

;; projectile
;; (autoload 'projectile-mode "projectile" nil t)
;; (autoload 'projectile-global-mode "projectile" nil t)
(add-hook 'after-init-hook
          '(lambda () (ignore-errors (projectile-global-mode 1))))

;; psvn
;; (autoload 'svn-status "psvn" nil t)
(eval-after-load "vc-svn"
  '(require 'psvn nil 'noerror))

;; rainbow-mode
;; (autoload 'rainbow-mode "rainbow-mode" nil t)

;; smart-compile
;; (autoload 'smart-compile "smart-compile" nil t)
(global-set-key [C-f7] 'smart-compile)

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

;; tabbar
(add-hook 'after-init-hook
          '(lambda ()
             (unless (locate-library "tabbar-ruler")
               (ignore-errors (tabbar-mode 1))) 'append))
(eval-after-load "tabbar"
  '(progn
     ;; backup tabbar.el's button image
     (setq tabbar-home-button-enabled-image-orig tabbar-home-button-enabled-image
           tabbar-home-button-disabled-image-orig tabbar-home-button-disabled-image
           tabbar-scroll-left-button-enabled-image-orig tabbar-scroll-left-button-enabled-image
           tabbar-scroll-right-button-enabled-image-orig tabbar-scroll-right-button-enabled-image)
     (defadvice tabbar-buffer-tab-label (after modified-flag activate)
       (setq ad-return-value
             (if (and (or (not (featurep 'tabbar-ruler))
                          (not window-system))
                      (buffer-modified-p (tabbar-tab-value tab)))
                 ;; (buffer-file-name (tabbar-tab-value tab))
                 (concat ad-return-value "*")
               ad-return-value)))
     (defun update-tabbar-modified-state ()
       (tabbar-set-template tabbar-current-tabset nil)
       (tabbar-display-update))
     (defadvice undo (after update-tabbar-tab-label activate)
       (update-tabbar-modified-state))
     (defadvice set-buffer-file-coding-system (after update-tabbar-tab-label activate)
       (update-tabbar-modified-state))
     (define-key tabbar-mode-map [C-prior] 'tabbar-backward)
     (define-key tabbar-mode-map [C-next] 'tabbar-forward)
     (add-hook 'first-change-hook 'update-tabbar-modified-state)
     (add-hook 'after-save-hook 'update-tabbar-modified-state)))

;; tabbar-ruler
(setq tabbar-ruler-invert-deselected nil)
(setq tabbar-ruler-movement-timer-delay 10000)
(add-hook 'after-init-hook '(lambda () (require 'tabbar-ruler nil 'noerror)) t)
(eval-after-load "tabbar-ruler"
  '(progn
     (tabbar-ruler-remove-caches)
     ;; restore tabbar.el's button image
     (setq tabbar-home-button-enabled-image tabbar-home-button-enabled-image-orig
           tabbar-home-button-disabled-image tabbar-home-button-disabled-image-orig
           tabbar-scroll-left-button-enabled-image tabbar-scroll-left-button-enabled-image-orig
           tabbar-scroll-right-button-enabled-image tabbar-scroll-right-button-enabled-image-orig)
     (setq tabbar-home-button
           (cons (cons "[o]" tabbar-home-button-enabled-image)
                 (cons "[x]" tabbar-home-button-disabled-image)))
     (setq tabbar-buffer-home-button
           (cons (cons "[+]" tabbar-home-button-enabled-image)
                 (cons "[-]" tabbar-home-button-disabled-image)))
     (setq tabbar-scroll-left-button
           (cons (cons " <" tabbar-scroll-left-button-enabled-image)
                 (cons " =" nil)))
     (setq tabbar-scroll-right-button
           (cons (cons " >" tabbar-scroll-right-button-enabled-image)
                 (cons " =" nil)))
     (defadvice tabbar-popup-menu (after add-menu-item activate)
       "Add customize menu item to tabbar popup menu."
       (setq ad-return-value
             (append ad-return-value
                     '("--"
                       ["Copy Buffer Name" (kill-new
                                            (buffer-name
                                             (tabbar-tab-value
                                              tabbar-last-tab)))]
                       ["Copy File Path" (kill-new
                                          (buffer-file-name
                                           (tabbar-tab-value
                                            tabbar-last-tab)))
                        :active (buffer-file-name
                                 (tabbar-tab-value tabbar-last-tab))]
                       ["Open Dired" dired-jump
                        :active (fboundp 'dired-jump)]
                       ["Open in Windows Explorer" (w32explore buffer-file-name)
                        :active (and buffer-file-name
                                     (eq system-type 'windows-nt)
                                     (require 'w32-browser nil 'noerror))]
                       ;; ["Open Dired" (dired
                       ;;                (let ((file (buffer-file-name
                       ;;                             (tabbar-tab-value
                       ;;                              tabbar-last-tab))))
                       ;;                  (if file
                       ;;                      (file-name-directory file)
                       ;;                    default-directory)))
                       ;;  :active (buffer-file-name
                       ;;           (tabbar-tab-value tabbar-last-tab))]
                       "--"
                       ["Undo Close Tab" undo-kill-buffer
                        :active (fboundp 'undo-kill-buffer)]))))
     (defadvice tabbar-line-tab (around window-or-terminal activate)
       "Fix tabbar-ruler in window-system and terminal"
       (if window-system
           ad-do-it
         (setq ad-return-value
               (let ((tab (ad-get-arg 0))
                     (tabbar-separator-value "|"))
                 (concat (propertize
                          (if tabbar-tab-label-function
                              (funcall tabbar-tab-label-function tab)
                            tab)
                          'tabbar-tab tab
                          'local-map (tabbar-make-tab-keymap tab)
                          'help-echo 'tabbar-help-on-tab
                          'mouse-face 'tabbar-highlight
                          'face (if (tabbar-selected-p tab
                                                       (tabbar-current-tabset))
                                    'tabbar-selected
                                  'tabbar-unselected)
                          'pointer 'hand)
                         tabbar-separator-value)))))
     ;; (unless (eq system-type 'windows-nt)
     (set-face-attribute 'tabbar-default nil
                         :family (face-attribute 'default :family))
     (add-hook 'after-make-frame-functions
               (lambda (frame)
                 (with-selected-frame frame
                   (set-face-attribute 'tabbar-default frame
                                       :family (face-attribute 'default
                                                               :family)))));; )
     (set-face-attribute 'tabbar-selected nil
                         :foreground "blue")
     (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
     (setq tabbar-ruler-excluded-buffers '())))

;; undo-tree
;; (autoload 'undo-tree-mode "undo-tree" nil t)
;; (autoload 'global-undo-tree-mode "undo-tree" nil t)

;; vlf
;; (autoload 'vlf "vlf" "View a Large File in Emacs." t)

;; volatile-highlights
(autoload 'volatile-highlights-mode "volatile-highlights" nil t)
(add-hook 'after-init-hook
          '(lambda () (ignore-errors (volatile-highlights-mode t))))

;; window-numbering
(add-hook 'after-init-hook
          '(lambda () (ignore-errors (window-numbering-mode 1))))

;; win-switch
;; (autoload 'win-switch-dispatch "win-switch" nil t)
;; (global-set-key "\C-xo" 'win-switch-dispatch)
(global-set-key "\C-xo"
                (lambda ()
                  (interactive)
                  (if (require 'win-switch nil 'noerror)
                      (win-switch-dispatch)
                    (other-window 1))))

;; xcscope
(add-hook 'after-init-hook
          '(lambda ()
             (when (executable-find "cscope")
               (when (require 'xcscope nil 'noerror)
                 (define-key cscope-list-entry-keymap [mouse-1]
                   'cscope-mouse-select-entry-other-window)))))

;; yasnippet
(setq yas-wrap-around-region t)
(add-hook 'after-init-hook '(lambda () (ignore-errors (yas-global-mode 1))))
(defvar custom-yas-snippet-dir
  (expand-file-name "etc/snippets"
                    (file-name-directory
                     (or load-file-name (buffer-file-name)))))
(eval-after-load "yasnippet"
  '(progn
     (add-to-list 'yas-snippet-dirs custom-yas-snippet-dir)))
(eval-after-load "org"
  '(add-hook 'org-mode-hook
             (let ((original-command (lookup-key org-mode-map [tab])))
               `(lambda ()
                  (setq yas-fallback-behavior
                        '(apply ,original-command))
                  (local-set-key [tab] 'yas-expand)))))

(provide 'init-elpa)
