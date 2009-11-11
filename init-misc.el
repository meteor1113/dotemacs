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


;;; misc setting
;; (require 'htmlize nil t)
(autoload 'smart-compile "smart-compile" nil t)
(require 'unicad nil t)


;;; tabbar setting
(when (require 'tabbar nil t)
  (tabbar-mode t))


;;; doxymacs setting
(add-hook 'c-mode-common-hook
          '(lambda ()
             (when (require 'doxymacs nil t)
               (doxymacs-mode t)
               (doxymacs-font-lock))))


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
  (setq-default ac-sources '(ac-source-words-in-all-buffer
                             ;; ac-source-words-in-buffer
                             ac-source-filename
                             ac-source-files-in-current-dir))
  (global-auto-complete-mode t)
  (add-to-list 'ac-modes 'objc-mode)
  (add-to-list 'ac-modes 'jde-mode))
(when (require 'auto-complete-cpp nil t)
  (ac-c++-init)
  (add-hook 'c-mode-hook 'ac-c++-setup))
;; (when (require 'auto-complete-css nil t)
;;   (ac-css-init))
;; (when (require 'auto-complete-emacs-lisp nil t)
;;   (ac-emacs-lisp-init))
;; (when (require 'auto-complete-python nil t)
;;   (ac-ropemacs-init))
;; (when (require 'auto-complete-semantic nil t)
;;   (dolist (hook '(c-mode-hook c++-mode-hook jde-mode-hook java-mode-hook))
;;     (add-hook hook
;;               (lambda ()
;;                 (add-to-list 'ac-sources 'ac-source-semantic)))))


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
