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

;; cc-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (ignore-errors (whitespace-mode t))))

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

;; gdb
(require 'gdb-ui nil 'noerror)
(require 'gdb-mi nil 'noerror)

(defun gud-break-or-remove (&optional force-remove)
  "Set/clear breakpoin."
  (interactive "P")
  (save-excursion
    (if (or force-remove
            (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint))
        (gud-remove nil)
      (gud-break nil))))

(defun gud-enable-or-disable ()
  "Enable/disable breakpoint."
  (interactive)
  (let ((obj))
    (save-excursion
      (move-beginning-of-line nil)
      (dolist (overlay (overlays-in (point) (point)))
        (when (overlay-get overlay 'put-break)
          (setq obj (overlay-get overlay 'before-string))))
      (if  (and obj (stringp obj))
          (cond ((featurep 'gdb-ui)
                 (let* ((bptno (get-text-property 0 'gdb-bptno obj)))
                   (string-match "\\([0-9+]\\)*" bptno)
                   (gdb-enqueue-input
                    (list
                     (concat gdb-server-prefix
                             (if (get-text-property 0 'gdb-enabled obj)
                                 "disable "
                               "enable ")
                             (match-string 1 bptno) "\n")
                     'ignore))))
                ((featurep 'gdb-mi)
                 (gud-basic-call
                  (concat
                   (if (get-text-property 0 'gdb-enabled obj)
                       "-break-disable "
                     "-break-enable ")
                   (get-text-property 0 'gdb-bptno obj))))
                (t (error "No gud-ui or gui-mi?")))
        (message "May be there isn't have a breakpoint.")))))

(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  ;; (set-process-query-on-exit-flag (get-buffer-process gud-comint-buffer) nil)
  ;; (kill-buffer gud-comint-buffer))
  (dolist (buffer '(gdba gdb-stack-buffer gdb-breakpoints-buffer
                         gdb-threads-buffer gdb-inferior-io
                         gdb-registers-buffer gdb-memory-buffer
                         gdb-locals-buffer gdb-assembler-buffer))
    (when (gdb-get-buffer buffer)
      (let ((proc (get-buffer-process (gdb-get-buffer buffer))))
        (when proc (set-process-query-on-exit-flag proc nil)))
      (kill-buffer (gdb-get-buffer buffer)))))

(defadvice gdb (before ecb-deactivate activate)
  "if ecb activated, deactivate it."
  (when (and (boundp 'ecb-minor-mode) ecb-minor-mode)
    (ecb-deactivate)))

;; (defun gdb-tooltip-hook ()
;;   (gud-tooltip-mode 1)
;;   (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
;;     (when process
;;       (set-process-sentinel process
;;                             (lambda (proc change)
;;                               (let ((status (process-status proc)))
;;                                 (when (or (eq status 'exit)
;;                                           (eq status 'signal))
;;                                   (gud-tooltip-mode -1))))))))
;; (add-hook 'gdb-mode-hook 'gdb-tooltip-hook)

(add-hook 'gdb-mode-hook
          '(lambda ()
             (gud-tooltip-mode 1)))

(defadvice gud-kill-buffer-hook (after gud-tooltip-mode activate)
  "After gdb killed, disable gud-tooltip-mode."
  (gud-tooltip-mode -1))

(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
;; (gud-tooltip-mode t)
(define-key c-mode-base-map [f5] 'gdb)

(eval-after-load "gud"
  '(progn
     (define-key gud-minor-mode-map [f5]
       '(lambda (&optional kill)
          (interactive "P")
          (if kill (gud-kill) (gud-go))))
     (define-key gud-minor-mode-map [S-f5] 'gud-kill)
     (define-key gud-minor-mode-map [f17] 'gud-kill) ; S-f5
     (define-key gud-minor-mode-map [f8] 'gud-print)
     (define-key gud-minor-mode-map [C-f8] 'gud-pstar)
     (define-key gud-minor-mode-map [f9] 'gud-break-or-remove)
     (define-key gud-minor-mode-map [C-f9] 'gud-enable-or-disable)
     (define-key gud-minor-mode-map [S-f9] 'gud-watch)
     (define-key gud-minor-mode-map [f10] 'gud-next)
     (define-key gud-minor-mode-map [C-f10] 'gud-until)
     (define-key gud-minor-mode-map [C-S-f10] 'gud-jump)
     (define-key gud-minor-mode-map [f11] 'gud-step)
     (define-key gud-minor-mode-map [C-f11] 'gud-finish)))

(provide 'init-cc)

;;; init-cc.el ends here
