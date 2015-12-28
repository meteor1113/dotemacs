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

(defadvice find-tag (before tags-file-name-advice activate)
  "Find TAGS file in ./ or ../ or ../../ dirs"
  (let ((list (mapcar 'expand-file-name '("./TAGS" "../TAGS" "../../TAGS"))))
    (while list
      (if (file-exists-p (car list))
          (progn
            (setq tags-file-name (car list))
            (setq list nil))
        (setq list (cdr list))))))

(defun find-dotemacs-file ()
  "Open .emacs file"
  (interactive)
  (let* ((paths '("~/.emacs" "~/.emacs.el" "~/.emacs.d/init.el" "~/_emacs"))
         (dotemacs-path))
    (dolist (path paths)
      (and (not dotemacs-path)
           (file-exists-p path)
           (setq dotemacs-path path)))
    (find-file (or dotemacs-path
                   (locate-file "site-start.el" load-path)
                   "~/.emacs"))))

(defun move-line-up (p)
  "Move current line up, copy from crazycool@smth"
  (interactive "*p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (previous-line p)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defun move-line-down (p)
  "Move current line down, copy from crazycool@smth"
  (interactive "*p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (next-line p)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defun format-region ()
  "Format region, if no region actived, format current buffer.
Like eclipse's Ctrl+Alt+F."
  (interactive)
  (let ((start (point-min))
        (end (point-max)))
    (if (and (fboundp 'region-active-p) (region-active-p))
        (progn (setq start (region-beginning))
               (setq end (region-end)))
      (progn (when (fboundp 'whitespace-cleanup)
               (whitespace-cleanup))
             (setq end (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) end)
        (push-mark (point))
        (push-mark (point-max) nil t)
        (goto-char start)
        (when (fboundp 'whitespace-cleanup)
          (whitespace-cleanup))
        (untabify start (point-max))
        (indent-region start (point-max) nil)))))

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
        (save-excursion
          (set-buffer buffer)
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

(defun xml-file-p (file)
  (let ((file-extension (file-name-extension file)))
    (and file-extension
         (string= file (file-name-sans-versions file))
         (find file-extension
               '("xml")
               :test 'string=))))

(defun format-xml-file (file)
  "Format a xml file."
  (interactive "F")
  (if (xml-file-p file)
      (let ((buffer (find-file-noselect file))) ;; open buffer
        (save-excursion
          (set-buffer buffer)
          (format-xml)
          (save-buffer)
          (kill-buffer)
          (message "Formated file:%s" file)))
    (message "%s isn't a xml file" file)))

(defun format-xml-directory (dirname)
  "Format all xml file in a directory."
  (interactive "D")
  ;; (message "directory:%s" dirname)
  (let ((files (directory-files dirname t))
        (make-backup-files nil))
    (dolist (x files)
      (if (not (string= "." (substring (file-name-nondirectory x) 0 1)))
          (if (file-directory-p x)
              (format-xml-directory x)
            (if (and (file-regular-p x)
                     (not (file-symlink-p x))
                     (xml-file-p x))
                (format-xml-file x)))))))

(autoload 'grep-tag-default "grep")
(autoload 'grep-apply-setting "grep")
(defvar grep-dir-format
  (cond ((eq system-type 'aix)
         "grep -inrH '%s' . | grep -vE \"\.svn/|\.git/|\.hg/|\.bzr/|CVS/\"")
        ;; ((eq system-type 'gnu/linux)
        ;;  "grep -inrHI '%s' . | grep -vE \"\.svn/|\.git/|\.hg/|\.bzr/|CVS/\"")
        ;; ((eq system-type 'windows-nt)
        ;;  "grep --exclude-dir=.svn --exclude-dir=.git --exclude-dir=.hg \
        ;;   --exclude-dir=.bzr --exclude-dir=CVS -inrHI \"%s\" .")
        (t
         "grep --exclude-dir=.svn --exclude-dir=.git --exclude-dir=.hg \
--exclude-dir=.bzr --exclude-dir=CVS -inrHI '%s' .")))
(defun grep-current-dir (&optional prompt wd)
  "Run `grep' to find current word in current directory."
  (interactive "P")
  (let* ((word (or wd
                   (and (fboundp 'region-active-p)
                        (region-active-p)
                        (buffer-substring-no-properties (region-beginning)
                                                        (region-end)))
                   (grep-tag-default)))
         (cmd (format grep-dir-format word)))
    (grep-apply-setting 'grep-use-null-device nil)
    (if (or prompt (= (length word) 0))
        (grep (read-shell-command
               "Run grep (like this): " cmd 'grep-history))
      (if (= 0 (length word))
          (message "Word is blank.")
        (grep cmd)))))

(defun grep-todo-current-dir ()
  "Run `grep' to find 'TODO' in current directory."
  (interactive)
  (grep-current-dir nil "TODO|FIXME"))

(defun moccur-word-all-buffers (regexp)
  "Run `multi-occur' to find regexp in all buffers."
  (if (= 0 (length regexp))
      (message "Regexp is blank.")
    (let ((buffers (buffer-list)))
      (dolist (buffer buffers)
        (let ((pos (string-match " *\\*" (buffer-name buffer))))
          (when (and pos (= 0 pos))
            (setq buffers (remq buffer buffers)))))
      (multi-occur buffers regexp))))

(defun moccur-all-buffers (&optional prompt)
  "Run `multi-occur' to find current word in all buffers."
  (interactive "P")
  (let ((word (grep-tag-default)))
    (when (or prompt (= (length word) 0))
      (setq word (read-regexp "List lines matching regexp" word)))
    (moccur-word-all-buffers word)))

(defun moccur-todo-all-buffers ()
  "Run `multi-occur' to find 'TODO' in all buffers."
  (interactive)
  (moccur-word-all-buffers
   "\\<\\([Tt][Oo][Dd][Oo]\\|[Ff][Ii][Xx][Mm][Ee]\\)\\>"))

(defun switch-to-other-buffer ()
  "Switch to (other-buffer)."
  (interactive)
  (switch-to-buffer (other-buffer)))
(defadvice switch-to-other-buffer (after pulse-advice activate)
  "After switch-to-other-buffer, pulse the line the cursor lands on."
  (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
             (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defun mark-current-line ()
  "Put point at beginning of this line, mark at end."
  (interactive)
  (move-beginning-of-line 1)
  (set-mark (point))
  (move-end-of-line 1))

(defun mark-current-line-mouse (ev)
  "Mark current line with a mouse click. EV is the mouse event."
  (interactive "e")
  (mouse-set-point ev)
  (mark-current-line))

;; (defun goto-match-paren (arg)
;;   "Go to the matching parenthesis if on parenthesis, otherwise insert %.
;; vi style of % jumping to matching brace."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(unless (fboundp 'toggle-frame-fullscreen)
  (defun toggle-frame-fullscreen (&optional f)
    (interactive)
    (if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
        (modify-frame-parameters
         nil
         `((maximized
            . ,(unless (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized))))
      (modify-frame-parameters
       nil
       `((fullscreen
          . ,(unless (eq (frame-parameter nil 'fullscreen) 'maximized)
               'maximized)))))))

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

(defun highlight-text-at-point ()
  (interactive)
  (highlight-text nil 'hl-at-point 'show-paren-match)
  (let* ((target-symbol (symbol-at-point))
         (txt (symbol-name target-symbol))
         (regexp (concat "\\_<" (regexp-quote txt) "\\_>")))
    (when target-symbol
      (highlight-text regexp 'hl-at-point 'show-paren-match))))

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

(provide 'init-func)
