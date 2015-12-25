;;; chinese-wbim-extra.el --- Enable Wubi(五笔) Input Method in Emacs.

;; Copyright (C) 2015-2016, Guanghui Qu

;; Author: Guanghui Qu<guanghui8827@gmail.com>
;; URL: https://github.com/andyque/chinese-wbim
;; Version: 0.1
;; Keywords: Wubi Input Method.
;;
;; This file is not part of GNU Emacs.

;;; Credits:

;; - Original Author: wenbinye@163.com

;;; License:

;; This file is part of chinese-wbim
;;
;; chinese-wbim is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; chinese-wbim is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'chinese-wbim)

(defvar chinese-wbim-punc-escape-list
  (number-sequence ?0 ?9)
  "Punctuation will not insert after this characters.
If you don't like this funciton, set the variable to nil")
(defvar chinese-wbim-insert-ascii-char (cons ?\; "；")
  "*Key used for `chinese-wbim-insert-ascii'.")

(defvar chinese-wbim-punc-translate-p t
  "*Non-nil means will translate punctuation.")

;;;_. handle punctuation
(defun chinese-wbim-read-punctuation (package)
  (let ((chinese-wbim-current-package package)
	buf punc-list punc)
    (setq buf (cdr (assoc "buffer" (car (chinese-wbim-buffer-list)))))
    (save-excursion
      (set-buffer buf)
      (save-restriction
        (widen)
        (let ((region (chinese-wbim-section-region "Punctuation")))
          (goto-char (car region))
          (while (< (point) (cdr region))
            (setq punc (chinese-wbim-line-content))
            (if (> (length punc) 3)
                (error "标点不支持多个转换！"))
            (add-to-list 'punc-list punc)
            (forward-line 1)))))
    punc-list))

(defun chinese-wbim-punc-translate (punc-list char)
  (if chinese-wbim-punc-translate-p
      (cond ((< char ? ) "")
            ((and chinese-wbim-insert-ascii-char
                  (= char (car chinese-wbim-insert-ascii-char)))
             (char-to-string char))
            (t (let ((str (char-to-string char))
                     punc)
                 (if (and (not (member (char-before) chinese-wbim-punc-escape-list))
                          (setq punc (cdr (assoc str punc-list))))
                     (progn
                       (if (= char (char-before))
                           (delete-char -1))
                       (if (= (safe-length punc) 1)
                           (car punc)
                         (setcdr (cdr punc) (not (cddr punc)))
                         (if (cddr punc)
                             (car punc)
                           (nth 1 punc))))
                   str))))
    (char-to-string char)))

(defun chinese-wbim-punc-translate-toggle (arg)
  (interactive "P")
  (setq chinese-wbim-punc-translate-p
        (if (null arg)
            (not chinese-wbim-punc-translate-p)
          (> (prefix-numeric-value arg) 0))))

;;;_. 一个快速插入英文的命令。按自己的需要绑定到 ";"
(defun chinese-wbim-insert-ascii ()
  (interactive)
  (if current-input-method
      (let (c)
        (message (format "自定义输入(直接空格%s, 回车%c): "
                         (cdr chinese-wbim-insert-ascii-char)
                         (car chinese-wbim-insert-ascii-char)))
        (setq c (read-event))
        (cond ((= c ? ) (insert (cdr chinese-wbim-insert-ascii-char)))
              ((= c ?\r) (insert-char (car chinese-wbim-insert-ascii-char) 1))
              (t
               (setq unread-command-events (list last-input-event))
               (insert (read-from-minibuffer "自定义输入: ")))))
    (call-interactively 'self-insert-command)))

;;;_. load and save history
(defun chinese-wbim-load-history (history-file package)
  (let* ((chinese-wbim-current-package package)
         (history (chinese-wbim-history))
         item)
    (when (file-exists-p history-file)
      (with-current-buffer (find-file-noselect history-file)
        (goto-char (point-min))
        (while (not (eobp))
          (if (and (setq item (chinese-wbim-line-content))
                   (= (length item) 2))
              (puthash (car item)
                       `(nil ("pos" . ,(string-to-number (cadr item))))
                       history))
          (forward-line 1))
        (kill-buffer (current-buffer))))))

(defun chinese-wbim-save-history (history-file package)
  (interactive)
  (let* ((chinese-wbim-current-package package)
         (history (chinese-wbim-history)))
    (with-temp-buffer
      (erase-buffer)
      (let (pos)
        (maphash (lambda (key val)
                   (unless (or (chinese-wbim-string-emptyp key)
                               (= (setq pos (cdr (assoc "pos" (cdr val)))) 1))
                     (insert key " " (number-to-string pos) "\n")))
                 history))
      (write-file history-file))))

;;;_. 增加两个快速选择的按键
(defun chinese-wbim-quick-select-1 ()
  "如果没有可选项，插入数字，否则选择对应的词条."
  (interactive)
  (if (car chinese-wbim-current-choices)
      (let ((index (chinese-wbim-page-start))
            (end (chinese-wbim-page-end)))
        (if (>= index end)
            (chinese-wbim-append-string (chinese-wbim-translate last-command-event))
          (chinese-wbim-remember-select (1+ index))
          (setq chinese-wbim-current-str (chinese-wbim-choice (nth index (car chinese-wbim-current-choices))))))
    (chinese-wbim-append-string (chinese-wbim-translate last-command-event)))
  (chinese-wbim-terminate-translation))

(defun chinese-wbim-quick-select-2 ()
  "如果没有可选项，插入数字，否则选择对应的词条."
  (interactive)
  (if (car chinese-wbim-current-choices)
      (let ((index (1+ (chinese-wbim-page-start)))
            (end (chinese-wbim-page-end)))
        (if (>= index end)
            (chinese-wbim-append-string (chinese-wbim-translate last-command-event))
          (chinese-wbim-remember-select (1+ index))
          (setq chinese-wbim-current-str (chinese-wbim-choice (nth index (car chinese-wbim-current-choices))))))
    (chinese-wbim-append-string (chinese-wbim-translate last-command-event)))
  (chinese-wbim-terminate-translation))

(defun chinese-wbim-describe-char (pos package)
  (interactive
   (list (point)
         (if (eq input-method-function 'chinese-wbim-input-method)
             (chinese-wbim-package-name)
           (let (chinese-wbim-current-package)
             (setq chinese-wbim-current-package
                   (if (= (length chinese-wbim-package-list) 1)
                       (cdar chinese-wbim-package-list)
                     (assoc
                      (completing-read "In package: "
                                       chinese-wbim-package-list nil t
                                       (caar chinese-wbim-package-list))
                      chinese-wbim-package-list)))
             (chinese-wbim-package-name)))))
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (let ((char (char-after pos))
        (func (intern-soft (format "%s-get-char-code" package)))
        code)
    (when func
      (setq code (funcall func char))
      (if code
          (message "Type %S to input %c for input method %s"
                   code char package)
        (message "Can't find char code for %c" char)))))

;;;_. char table
(defun chinese-wbim-make-char-table (chars table)
  "Set CHARS of `chinese-wbim-char-database' in TABLE."
  (dolist (char chars)
    (let ((code (car char)))
      (dolist (c (cdr char))
        (set (intern c table) code)))))

(defsubst chinese-wbim-get-char-code (char table)
  "Get the code of the character CHAR in TABLE."
  (symbol-value (intern-soft (char-to-string char) table)))

(provide 'chinese-wbim-extra)
;;; chinese-wbim-extra.el ends here
