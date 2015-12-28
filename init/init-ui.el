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

;; ui
(ignore-errors (tool-bar-mode t))

(ignore-errors (set-scroll-bar-mode 'right))
(setq scroll-step 1)
;; (setq scroll-margin 3)
;; (setq scroll-conservatively 10000)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(column-number-mode t)
(size-indication-mode 1)
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (display-time-mode t)
(which-function-mode t)
(setq buffers-menu-max-size 30)
;; (setq linum-eager nil)
;; (when (fboundp 'global-linum-mode)
;;   (global-linum-mode 1))
;; (setq-default cursor-type 'bar)
;; (blink-cursor-mode -1)
(setq x-stretch-cursor t)
(xterm-mouse-mode 1)               ; (if window-system -1 1)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (xterm-mouse-mode 1))))
;; (mouse-avoidance-mode 'animate)
;; (setq mouse-autoselect-window t)
(setq-default indicate-buffer-boundaries (quote left))
(ignore-errors (winner-mode 1))
(setq frame-title-format
      '("[" (:eval (ignore-errors (projectile-project-name))) "]"
        (:eval (or buffer-file-name (buffer-name)))
        "[" (:eval (format "%s" buffer-file-coding-system)) "]"
        (:eval (if (buffer-modified-p) " * " " - "))
        invocation-name "@" system-name))
(when window-system (set-background-color "honeydew")) ; #f0fff0
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (and (framep frame)
                       (not (eq (framep frame) t)))
              (with-selected-frame frame
                ;; (when window-system
                (set-background-color "honeydew")))))
;; (unless window-system
;;   (setq frame-background-mode 'dark))
;; (set-frame-parameter (selected-frame) 'alpha (list 85 50)) ; Transparent
;; (add-to-list 'default-frame-alist (cons 'alpha (list 85 50)))

;; color-theme
;; (require 'color-theme-autoloads nil 'noerror)

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

;; rainbow-mode
;; (autoload 'rainbow-mode "rainbow-mode" nil t)

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

(provide 'init-ui)
