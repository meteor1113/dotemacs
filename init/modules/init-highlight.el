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

;; highlight-parentheses
;; (autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
;; (add-hook 'find-file-hooks
;;           (lambda ()
;;             (when (require 'highlight-parentheses nil 'noerror)
;;               (highlight-parentheses-mode t))))

;; volatile-highlights
(autoload 'volatile-highlights-mode "volatile-highlights" nil t)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (volatile-highlights-mode t))))

(provide 'init-highlight)
