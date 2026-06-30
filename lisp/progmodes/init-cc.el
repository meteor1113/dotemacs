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

;; cc-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (ignore-errors (turn-on-cwarn-mode-if-enabled))))

(defun c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start
                                    (match-beginning 0)
                                    'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (font-lock-add-keywords
              nil
              '((c-mode-font-lock-if0
                 (0 font-lock-comment-face prepend)))
              'add-to-end)))

(defun cxx-file-p (file)
  (let ((file-extension (file-name-extension file)))
    (and file-extension
         (string= file (file-name-sans-versions file))
         (find file-extension
               '("h" "hpp" "hxx" "c" "cpp" "cxx")
               :test 'string=))))

(defun format-cxx-file (file)
  "Format a c/c++ file."
  (interactive "F")
  (if (cxx-file-p file)
      (let ((buffer (find-file-noselect file))) ;; open buffer
        (with-current-buffer buffer
          ;; (mark-whole-buffer)
          (when (fboundp 'whitespace-cleanup)
            (whitespace-cleanup))
          (untabify (point-min) (point-max))
          (indent-region (point-min) (point-max))
          (save-buffer)
          (kill-buffer)
          (message "Formated file:%s" file)))
    (message "%s isn't a c++ file" file)))

(defun format-cxx-directory (dirname)
  "Format all c/c++ file in a directory."
  (interactive "D")
  ;; (message "directory:%s" dirname)
  (let ((files (directory-files dirname t))
        (make-backup-files nil))
    (dolist (x files)
      (if (not (string= "." (substring (file-name-nondirectory x) 0 1)))
          (if (file-directory-p x)
              (format-cxx-directory x)
            (if (and (file-regular-p x)
                     (not (file-symlink-p x))
                     (cxx-file-p x))
                (format-cxx-file x)))))))

;; c-mode
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")))

;; c++-mode
(add-to-list 'auto-mode-alist '("\\.[ch]\\'" . c++-mode))
(add-hook 'c++-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             ;; (c-toggle-auto-hungry-state 1)
             (c-set-offset 'innamespace 0)))

;; objc-mode
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(when (boundp 'magic-mode-alist)
  ;; (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*#import" . objc-mode))
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*@implementation" . objc-mode))
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*@interface" . objc-mode))
  (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*@protocol" . objc-mode)))
(add-hook 'objc-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")))

;; find-file
(setq ff-always-try-to-create nil)
(setq cc-search-directories '("/usr/include" "/usr/local/include/*"
                              "." "./include" "./inc"
                              "./common" "./public" "./src"
                              ".." "../include" "../inc"
                              "../common" "../public" "../src"
                              "../.." "../../include" "../../inc"
                              "../../common" "../../public" "../../src"))
(add-hook 'c-mode-common-hook
          '(lambda()
             (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; ffap
(eval-after-load "ffap"
  '(setq ffap-c-path (append ffap-c-path cc-search-directories)))

;; filecache
(eval-after-load "filecache"
  '(progn (file-cache-add-directory-list cc-search-directories)
          (file-cache-add-directory-recursively "/usr/include/c++")
          (file-cache-add-directory-recursively "/usr/local/include")))

;; ac-clang
;; (defvar clang-cflags '("-I.." "-I../include" "-I../inc"
;;                                    "-I../common" "-I../public"
;;                                    "-I../.." "-I../../include" "-I../../inc"
;;                                    "-I../../common" "-I../../public"))
;; (when (fboundp 'semantic-gcc-get-include-paths)
;;   (let ((dirs (semantic-gcc-get-include-paths "c++")))
;;     (dolist (dir dirs)
;;       (add-to-list 'clang-cflags (concat "-I" dir)))))
(let ((cflags '("-I.." "-I../include" "-I../inc"
                "-I../common" "-I../public"
                "-I../.." "-I../../include" "-I../../inc"
                "-I../../common" "-I../../public")))
  (when (fboundp 'semantic-gcc-get-include-paths)
    (let ((dirs (semantic-gcc-get-include-paths "c++")))
      (dolist (dir dirs)
        (add-to-list 'cflags (concat "-I" dir)))))
  (setq-default ac-clang-cflags cflags))

(setq w32-pipe-read-delay 0)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (if (and (require 'ac-clang nil 'noerror)
                      (or ac-clang--server-executable
                          (executable-find
                           (or (plist-get ac-clang--server-binaries
                                          ac-clang-server-type) "")))
                      (ac-clang-initialize))
                 (progn
                   ;; (setq ac-clang-cflags clang-cflags)
                   (ac-clang-activate-after-modify)
                   (local-set-key (kbd "M-n") 'ac-complete-clang))
               ;; (setq ac-sources (append '(ac-source-semantic) ac-sources))
               (local-set-key (kbd "M-n") 'ac-complete-semantic))))

(provide 'init-cc)

;;; init-cc.el ends here
