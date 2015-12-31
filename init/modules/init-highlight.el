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

;; double click highlight
(defface hl-double-click
  '((default (:inherit region))
    (((class color) (background light)) (:background "lawn green"))
    (((class color) (background dark)) (:background "green" :foreground "black")))
  "*Face used by double click highlight.")

(defun highlight-text (txt prop face)
  (let ((start (point-min))
        (end (point-max)))
    (remove-overlays start end prop t)
    (unless (or (not txt)
                (string= txt "")
                (string-match "^[\t\n\s]*$" txt)
                (string-match "\n" txt))
      (save-excursion
        (goto-char start)
        (while (re-search-forward txt end t)
          (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'face face)
            (overlay-put overlay prop t))
          (goto-char (match-end 0)))))))

(defadvice mouse-start-end (after hl-double-click (start end mode) activate)
  (cond ((= mode 1)
         (highlight-text nil 'hl-double-click 'hl-double-click)
         (let* ((txt (buffer-substring-no-properties (nth 0 ad-return-value)
                                                     (nth 1 ad-return-value)))
                (regexp (concat "\\_<" (regexp-quote txt) "\\_>")))
           (highlight-text regexp 'hl-double-click 'hl-double-click)))
        ((= mode 2)
         (highlight-text "" 'hl-double-click 'hl-double-click))))

;; (defvar hl-double-click-regexp "")
;; (defadvice mouse-start-end (after hl-double-click (start end mode) activate)
;;   (cond ((= mode 1)
;;          (unhighlight-regexp hl-double-click-regexp)
;;          (let ((txt (buffer-substring-no-properties (nth 0 ad-return-value)
;;                                                     (nth 1 ad-return-value))))
;;            (unless (or (string= txt "")
;;                        (string-match "^[\t\n\s]*$" txt)
;;                        (string-match "\n" txt))
;;              (setq hl-double-click-regexp
;;                    (concat "\\_<" (regexp-quote txt) "\\_>"))
;;              (highlight-regexp hl-double-click-regexp 'hl-double-click))))
;;         ((= mode 2)
;;          (unhighlight-regexp hl-double-click-regexp))))

(defun highlight-text-at-point ()
  (interactive)
  (highlight-text nil 'hl-at-point 'show-paren-match)
  (let* ((target-symbol (symbol-at-point))
         (txt (symbol-name target-symbol))
         (regexp (concat "\\_<" (regexp-quote txt) "\\_>")))
    (when target-symbol
      (highlight-text regexp 'hl-at-point 'show-paren-match))))

(global-set-key [(meta f1)] 'highlight-text-at-point)

;; highlight-symbol
(setq highlight-symbol-idle-delay 0.5)
(add-hook 'prog-mode-hook
          '(lambda ()
             (ignore-errors (highlight-symbol-mode 1))))

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

     (eval-after-load "pulse"
	   '(progn
          (defadvice highlight-symbol-next (after pulse-advice activate)
            (pulse-line-hook-function))
          (defadvice highlight-symbol-prev (after pulse-advice activate)
            (pulse-line-hook-function))
          (defadvice highlight-symbol-next-or-prev (after pulse-advice activate)
            (pulse-line-hook-function))))))

;; highlight-tail
;; (autoload 'highlight-tail-mode "highlight-tail" nil t)

;; highlight-parentheses
;; (autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
;; (add-hook 'find-file-hooks
;;           (lambda ()
;;             (when (require 'highlight-parentheses nil 'noerror)
;;               (highlight-parentheses-mode t))))

;; hl-todo
(setq hl-todo-activate-in-modes
      '(prog-mode text-mode))
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (global-hl-todo-mode 1))))

;; diff-hl
;; (autoload 'diff-hl-mode "diff-hl" nil t)
;; (autoload 'global-diff-hl-mode "diff-hl" nil t)
;; (autoload 'diff-hl-dired-mode "diff-hl-dired" nil t)
;; (add-hook 'dired-mode-hook '(lambda () (ignore-errors (diff-hl-dired-mode 1))))
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (global-diff-hl-mode 1))))

;; volatile-highlights
(autoload 'volatile-highlights-mode "volatile-highlights" nil t)
(add-hook 'after-init-hook
          '(lambda ()
             (ignore-errors (volatile-highlights-mode t))))

(provide 'init-highlight)

;;; init-highlight.el ends here
