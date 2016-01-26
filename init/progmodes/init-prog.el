;;; -*- mode: emacs-lisp; coding: utf-8; -*-

;; Copyright (C) 2008- Liu Xin
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Liu Xin <meteor1113@qq.com>
;; @URL http://git.oschina.net/meteor1113/dotemacs

;;; Commentary:

;;; Code:

;; prog-mode
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (ignore-errors (whitespace-mode t))
             ;; (when window-system (ignore-errors (fci-mode 1)))
             (linum-mode 1)
             ;; (or (ignore-errors (hideshowvis-minor-mode t)) (hs-minor-mode t))
             (hs-minor-mode t)
             (ignore-errors (imenu-add-menubar-index))))

;; ggtags
(when (executable-find "global")
  (add-hook 'prog-mode-hook
            '(lambda ()
               (ggtags-mode 1))))

(when (executable-find "chmod")
  (add-hook 'after-save-hook
            '(lambda ()
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
                    (not (file-executable-p buffer-file-name))
                    (shell-command (format "chmod +x '%s'" buffer-file-name))
                    (kill-buffer "*Shell Command Output*")))))

(provide 'init-prog)

;;; init-prog.el ends here
