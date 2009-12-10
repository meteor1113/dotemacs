;;;
;; Copyright (C) 2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2008-08-08


;;; load-path setting
(let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
       (lisp-dir (expand-file-name "lisp" dir)))
  (add-to-list 'load-path dir)
  (when (file-exists-p lisp-dir)
    (progn (add-to-list 'load-path lisp-dir)
           (let ((old-dir default-directory))
             (cd lisp-dir)
             (normal-top-level-add-subdirs-to-load-path)
             (cd old-dir)))))


;;; unicad setting
(require 'unicad nil t)


;;; smart-compile setting
(autoload 'smart-compile "smart-compile" nil t)
(global-set-key [C-f7] 'smart-compile)


;;; tabbar setting
(when (require 'tabbar nil t)
  (tabbar-mode t))


;;; window-numbering setting
(when (require 'window-numbering nil t)
  (window-numbering-mode 1))


;;; ascii setting
(autoload 'ascii-on        "ascii" "Turn on ASCII code display."   t)
(autoload 'ascii-off       "ascii" "Turn off ASCII code display."  t)
(autoload 'ascii-display   "ascii" "Toggle ASCII code display."    t)
(autoload 'ascii-customize "ascii" "Customize ASCII code display." t)


;;; recent-jump setting
(when (require 'recent-jump nil t)
  (global-set-key (kbd "<M-left>") 'recent-jump-jump-backward)
  (global-set-key (kbd "<M-right>") 'recent-jump-jump-forward))


;;; highlight-symbol setting
(when (require 'highlight-symbol nil t)
  (define-global-minor-mode global-highlight-symbol-mode
    highlight-symbol-mode (lambda () (highlight-symbol-mode 1)))
  (when window-system
    (global-highlight-symbol-mode t))
  (setq highlight-symbol-idle-delay 0.2)
  (global-set-key [(control f3)] 'highlight-symbol-at-point)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))


;;; highlight-80+ setting
(when (require 'highlight-80+ nil t)
  (let ((hl80+-modes
         '(emacs-lisp-mode-hook lisp-interaction-mode-hook
                                c-mode-common-hook
                                makefile-mode-hook
                                perl-mode-hook cperl-mode-hook
                                python-mode-hook ruby-mode-hook)))
    (dolist (hook hl80+-modes)
      (add-hook hook (lambda () (highlight-80+-mode 1))))))


;;; ifdef setting
(add-hook 'c-mode-common-hook
          '(lambda ()
             (when (require 'ifdef nil t)
               (global-set-key [?\C-c ?\C-i] 'mark-ifdef)
               (mark-ifdef))))


;;; doc-mode/doxymacs-mode setting
(add-hook 'c-mode-common-hook
          '(lambda ()
             (if (and (require 'semantic nil t)
                      (require 'doc-mode nil t))
                 (doc-mode t)
               (when (require 'doxymacs nil t)
                 (doxymacs-mode t)
                 (doxymacs-font-lock)))))


;;; sourcepair setting
(when (require 'sourcepair nil t)
  (define-key c-mode-map [M-f12] 'sourcepair-load)
  (define-key c++-mode-map [M-f12] 'sourcepair-load)
  (define-key objc-mode-map [M-f12] 'sourcepair-load)
  (setq sourcepair-source-extensions
        '(".cpp" ".cxx" ".c++" ".CC" ".cc" ".C" ".c" ".mm" ".m"))
  (setq sourcepair-header-extensions
        '(".hpp" ".hxx" ".h++" ".HH" ".hh" ".H" ".h"))
  (setq sourcepair-header-path '("." "include" ".." "../include" "../inc"
                                 "../../include" "../../inc" "../../*"))
  (setq sourcepair-source-path '("." "src" ".." "../src" "../*"))
  (setq sourcepair-recurse-ignore '("CVS" ".svn" ".git"
                                    "Obj" "Debug" "Release" "bin" "lib")))


;;; yasnippet setting
(when (or (require 'yasnippet-bundle nil t)
          (require 'yasnippet nil t))
  (unless (featurep 'yasnippet-bundle)
    (yas/initialize))
  (let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
         (snippets-dir (expand-file-name "snippets" dir)))
    (when (file-exists-p snippets-dir)
      (yas/load-directory snippets-dir)))
  (when (require 'org nil t)
    (add-hook 'org-mode-hook
              (let ((original-command (lookup-key org-mode-map [tab])))
                `(lambda ()
                   (setq yas/fallback-behavior
                         '(apply ,original-command))
                   (local-set-key [tab] 'yas/expand))))))


;;; auto-complete setting
(when (require 'auto-complete nil t)
  (setq-default ac-sources '(ac-source-abbrev
                             ac-source-words-in-buffer
                             ac-source-words-in-all-buffer
                             ac-source-files-in-current-dir
                             ac-source-filename
                             ac-source-imenu))
  (global-auto-complete-mode t)
  (add-to-list 'ac-modes 'objc-mode)
  (add-to-list 'ac-modes 'jde-mode))
(when (require 'auto-complete-config nil t)
  (when (ac-yasnippet-initialize)
    (setq-default ac-sources (append '(ac-source-yasnippet) ac-sources)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq ac-omni-completion-sources
                    '(("\\<require\s+'" ac-source-emacs-lisp-features)
                      ("\\<load\s+\"" ac-source-emacs-lisp-features)))
              ;; (push 'ac-source-emacs-lisp-features ac-sources)
              ;; (push 'ac-source-symbols ac-sources)
              ;; (setq ac-sources (append '(ac-source-yasnippet) ac-sources))
              ))
  (ac-c++-keywords-initialize)
  ;; (ac-css-keywords-initialize)
  ;; (ac-ropemacs-initialize)
  ;; (setq ac-trigger-commands '(self-insert-command c-electric-lt-gt))
  ;; (dolist (hook '(c-mode-hook c++-mode-hook jde-mode-hook java-mode-hook))
  ;;   (add-hook hook
  ;;             '(lambda ()
  ;;                (setq ac-omni-completion-sources
  ;;                      (list (cons "." '(ac-source-semantic))
  ;;                            (cons "->" '(ac-source-semantic)))))))
  )


;;; company setting
(when (require 'company nil t)
  (global-company-mode t)
  (setq company-idle-delay nil)
  (define-key company-mode-map (kbd "M-n") 'company-select-next)
  (define-key company-mode-map (kbd "M-p") 'company-select-previous))


;;; eim setting
(when (require 'eim nil t)
;  (setq eim-use-tooltip nil)
  (register-input-method
   "eim-wb" "euc-cn" 'eim-use-package
   "eim-wb" "eim-wb" "wb.txt")
  (register-input-method
   "eim-py" "euc-cn" 'eim-use-package
   "eim-py" "eim-py" "py.txt")
  (setq default-input-method "eim-wb"))
(when (require 'eim-extra nil t)
  (global-set-key ";" 'eim-insert-ascii))


(provide 'init-misc)
