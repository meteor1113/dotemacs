;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @URL https://github.com/meteor1113/dotemacs

;;; Commentary:

;;; Code:

;; semantic
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode
                                  global-semantic-decoration-mode
                                  ;; global-semantic-highlight-edits-mode
                                  global-semantic-show-unmatched-syntax-mode
                                  global-semantic-show-parser-state-mode))
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (semantic-mode t))))
;; (run-with-idle-timer 10 nil #'semantic-mode t)

(eval-after-load "semantic"
  '(progn
     (require 'semantic/decorate/include nil 'noerror)
     (require 'semantic/bovine/el nil 'noerror)
     (require 'semantic/analyze/refs)
     (semantic-toggle-decoration-style "semantic-tag-boundary" -1)

     (when (executable-find "global")
       (semanticdb-enable-gnu-global-databases 'c-mode)
       (semanticdb-enable-gnu-global-databases 'c++-mode)
       (setq ede-locate-setup-options '(ede-locate-global ede-locate-base)))

     (when (and (eq system-type 'windows-nt)
                (executable-find "gcc"))
       (if (and (boundp 'semantic-mode) semantic-mode)
           (require 'semantic/bovine/c nil 'noerror)
         (require 'semantic-c nil 'noerror))
       (ignore-errors (semantic-gcc-setup)))

     ;; (unless (executable-find "python")
     ;;   (remove-hook 'python-mode-hook 'wisent-python-default-setup)
     ;;   (setq semantic-new-buffer-setup-functions
     ;;         (delete (assq 'python-mode semantic-new-buffer-setup-functions)
     ;;                 semantic-new-buffer-setup-functions)))

     (defadvice push-mark (around semantic-mru-bookmark activate)
       "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
       (semantic-mrub-push semantic-mru-bookmark-ring (point) 'mark)
       ad-do-it)

     (defun semantic-ia-fast-jump-back ()
       (interactive)
       (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
           (error "Semantic Bookmark ring is currently empty"))
       (let* ((ring (oref semantic-mru-bookmark-ring ring))
              (alist (semantic-mrub-ring-to-assoc-list ring))
              (first (cdr (car alist))))
         ;; (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
         ;;     (setq first (cdr (car (cdr alist)))))
         (semantic-mrub-visit first)
         (ring-remove ring 0)))

     (defun semantic-ia-fast-jump-or-back (&optional back)
       (interactive "P")
       (if back
           (semantic-ia-fast-jump-back)
         (semantic-ia-fast-jump (point))))

     (global-set-key [f12] 'semantic-ia-fast-jump-or-back)
     (global-set-key [C-f12] 'semantic-ia-fast-jump-or-back)
     (global-set-key [S-f12] 'semantic-ia-fast-jump-back)
     (global-set-key [mouse-2] 'semantic-ia-fast-mouse-jump)
     (global-set-key [S-mouse-2] 'semantic-ia-fast-jump-back)
     (global-set-key [double-mouse-2] 'semantic-ia-fast-jump-back)
     (global-set-key [M-S-f12] 'semantic-analyze-proto-impl-toggle)))

;; pulse
(setq pulse-command-advice-flag t)    ; (if window-system 1 nil)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (require 'pulse nil 'noerror))))
;; (run-with-idle-timer 2 nil
;;                      #'(lambda ()
;;                          (require 'pulse nil 'noerror)))
(eval-after-load "pulse"
  '(progn
     (add-hook 'next-error-hook 'pulse-line-hook-function)

     (defadvice switch-to-buffer (after pulse-advice activate)
       (pulse-line-hook-function))

     (defadvice previous-buffer (after pulse-advice activate)
       (pulse-line-hook-function))

     (defadvice next-buffer (after pulse-advice activate)
       (pulse-line-hook-function))

     (defadvice ido-switch-buffer (after pulse-advice activate)
       (pulse-line-hook-function))

     (defadvice beginning-of-buffer (after pulse-advice activate)
       (when (called-interactively-p 'interactive) (pulse-line-hook-function)))

     (defadvice goto-line (after pulse-advice activate)
       (when (called-interactively-p 'interactive) (pulse-line-hook-function)))

     (defadvice find-tag (after pulse-advice activate)
       (when (called-interactively-p 'interactive) (pulse-line-hook-function)))

     (defadvice tags-search (after pulse-advice activate)
       (when (called-interactively-p 'interactive) (pulse-line-hook-function)))

     (defadvice tags-loop-continue (after pulse-advice activate)
       (when (called-interactively-p 'interactive) (pulse-line-hook-function)))

     (defadvice pop-tag-mark (after pulse-advice activate)
       (when (called-interactively-p 'interactive) (pulse-line-hook-function)))

     (defadvice imenu-default-goto-function (after pulse-advice activate)
       (when (called-interactively-p 'interactive) (pulse-line-hook-function)))

     (defadvice exchange-point-and-mark (after pulse-advice activate)
       (when (and (called-interactively-p 'interactive) (> (abs (- (point) (mark))) 400))
         (pulse-line-hook-function)))

     (defadvice cua-exchange-point-and-mark (after pulse-advice activate)
       (when (and (called-interactively-p 'interactive) (> (abs (- (point) (mark))) 400))
         (pulse-line-hook-function)))))

(provide 'init-cedet)

;;; init-cedet.el ends here
