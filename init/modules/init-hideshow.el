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

;; hideshow
(defvar hs--overlay-keymap nil "keymap for folding overlay")

(let ((map (make-sparse-keymap)))
  (define-key map [mouse-1] 'hs-show-block)
  (setq hs--overlay-keymap map))

(setq hs-set-up-overlay
      (defun my-display-code-line-counts (ov)
        (when (eq 'code (overlay-get ov 'hs))
          (overlay-put ov 'display
                       (propertize
                        (format "...<%d lines>"
                                (count-lines (overlay-start ov)
                                             (overlay-end ov)))
                        'face 'mode-line))
          (overlay-put ov 'priority (overlay-end ov))
          (overlay-put ov 'keymap hs--overlay-keymap)
          (overlay-put ov 'pointer 'hand))))

(eval-after-load "hideshow"
  '(progn (define-key hs-minor-mode-map [(shift mouse-2)] nil)
          (define-key hs-minor-mode-map (kbd "C-+") 'hs-toggle-hiding)
          (define-key hs-minor-mode-map (kbd "<left-fringe> <mouse-2>")
            'hs-mouse-toggle-hiding)))

;; (global-set-key (kbd "C-?") 'hs-minor-mode)

(provide 'init-hideshow)
