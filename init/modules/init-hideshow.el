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

;; hideshowvis (so slow)
;; (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions" t)

(defadvice display-code-line-counts (after overlay-key-map (ov) activate)
  (overlay-put ov 'keymap hs--overlay-keymap)
  (overlay-put ov 'pointer 'hand))

;; (add-hook 'after-init-hook
;;           '(lambda ()
;;              (ignore-errors (hideshowvis-symbols))))


(provide 'init-hideshow)

;;; init-hideshow.el ends here
