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

;; nxml-mode
(when (fboundp 'nxml-mode)
  (add-to-list 'auto-mode-alist
               '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
  (setq nxml-bind-meta-tab-to-complete-flag t))

(add-hook 'nxml-mode-hook
            '(lambda ()
               (linum-mode 1)
               (require 'sgml-mode)
               (set-syntax-table sgml-mode-syntax-table)))

(defun format-xml ()
  "Format XML markup in region.
The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
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
        ;; split <foo><foo> or </foo><foo>, but not <foo></foo>
        (while (search-forward-regexp ">[ \t]*<[^/]" end t)
          (backward-char 2) (insert "\n") (incf end))
        ;; split <foo/></foo> and </foo></foo>
        (goto-char start)
        (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
          (backward-char) (insert "\n") (incf end))
        (when (fboundp 'whitespace-cleanup)
          (goto-char start)
          (whitespace-cleanup))
        (indent-region start (point-max) nil)))))

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
        (with-current-buffer buffer
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

(eval-after-load "nxml-mode"
  `(progn
     (define-key nxml-mode-map [M-f8] 'format-xml)
     (define-key nxml-mode-map (kbd "ESC <f8>") 'format-xml) ; putty
     (define-key nxml-mode-map (kbd "C-S-f") 'format-xml)))

(provide 'init-xml)

;;; init-xml.el ends here
