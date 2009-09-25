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
;; (require 'gtags nil t)
;; (require 'htmlize nil t)
;; (require 'smart-compile nil t)
(require 'unicad nil t)
(require 'xcscope nil t)

;; tabbar setting
(when (require 'tabbar nil t)
  (tabbar-mode t))

;; doxymacs setting
(add-hook 'c-mode-common-hook
          '(lambda ()
             (when (require 'doxymacs nil t)
               (doxymacs-mode t)
               (doxymacs-font-lock))))

;; yasnippet setting
(when (or (require 'yasnippet-bundle nil t)
          (require 'yasnippet nil t))
  (unless (featurep 'yasnippet-bundle)
    (yas/initialize))
  (let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
         (snippets-dir (expand-file-name "snippets" dir)))
    (when (file-exists-p snippets-dir)
      (yas/load-directory snippets-dir)))
  (add-hook 'org-mode-hook
            #'(lambda ()
                (local-set-key [tab] 'yas/expand))))

;; auto-complete setting
(when (require 'auto-complete nil t)
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

;; ropemacs setting
(add-hook 'python-mode-hook
          '(lambda ()
             (when (and (not (fboundp 'ropemacs-mode))
                        (require 'pymacs nil t)
                        (pymacs-load "ropemacs" "rope-" t))
               (setq ropemacs-enable-autoimport t)
               (ropemacs-mode t))))

;; company setting
(setq company-backends
      '(company-elisp company-nxml company-css
                      company-eclim company-semantic company-xcode
                      (company-gtags company-etags company-dabbrev-code
                                     company-keywords)
                      company-oddmuse company-files company-dabbrev))
(when (require 'company nil t)
  (global-company-mode t)
  (setq company-idle-delay nil)
  (define-key company-mode-map (kbd "M-n") 'company-select-next)
  (define-key company-mode-map (kbd "M-p") 'company-select-previous)
  (add-hook 'python-mode-hook
            '(lambda ()
               (when (fboundp 'rope-completions)
                 (add-to-list 'company-backends 'company-ropemacs)))
            'append))


(provide 'init-misc)
