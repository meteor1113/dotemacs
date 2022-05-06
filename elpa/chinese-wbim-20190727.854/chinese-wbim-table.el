;;; chinese-wbim-table.el --- Enable Wubi(五笔) Input Method in Emacs.

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

;; - punctuation-list: A symbol to translate punctuation
;; - translate-chars: The first letter which will invoke reverse
;;                   search the code for char
;; - max-length: max input string length
;; - char-table: a obarray to search code for char
;; - all-completion-limit: A minimal length to add all completions
;; - table-create-word-function
;; 
;; - table-user-file
;; - table-history-file

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'chinese-wbim-table)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'chinese-wbim)
(require 'chinese-wbim-extra)

(defun chinese-wbim-table-translate (char)
  (chinese-wbim-punc-translate (symbol-value (chinese-wbim-get-option 'punctuation-list))
                      char))

(defun chinese-wbim-table-get-char-code (char)
  (chinese-wbim-get-char-code char (chinese-wbim-get-option 'char-table)))

(defun chinese-wbim-table-format (key cp tp choice)
  (if (memq (aref key 0) (chinese-wbim-get-option 'translate-chars))
      (setq choice
            (mapcar (lambda (c)
                      (if (consp c)
                          (setq c (car c)))
                      (cons c
                            (chinese-wbim-table-get-char-code (aref c 0))))
                    choice)))
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key  cp tp
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i))
                                  (if (consp c)
                                      (concat (car c) (cdr c))
                                    c)))
                        choice) " "))))

;;;_. 增加补全
(defun chinese-wbim-table-add-completion ()
  (if (= (length chinese-wbim-current-key) 1)
      t
    (let ((reg (concat "^" (regexp-quote chinese-wbim-current-key)))
          (len (length chinese-wbim-current-key))
          (package chinese-wbim-current-package)
          (key chinese-wbim-current-key)
          line completion)
      (save-excursion
        (dolist (buf (mapcar 'cdar (chinese-wbim-buffer-list)))
          (set-buffer buf)
          (setq chinese-wbim-current-package package)
          (beginning-of-line)
          (if (or (string= (chinese-wbim-code-at-point) key)
                  (not (looking-at reg)))
              (forward-line 1))
          (while (looking-at reg)
            (setq line (chinese-wbim-line-content))
            (mapc (lambda (c)
                    (when (or (>= len (chinese-wbim-get-option 'all-completion-limit))
                              (= (length c) 1))
                      (push (cons c (substring
                                     (car line)
                                     len))
                            completion)))
                  (cdr line))
            (forward-line 1))))
      (setq completion (sort (delete-dups (nreverse completion))
                             (lambda (a b)
                               (< (length (cdr a)) (length (cdr b))))))
      ;;      (message "%s, %s" chinese-wbim-current-choices completion)
      (setcar chinese-wbim-current-choices (append (car chinese-wbim-current-choices)
                                          completion))
      ;;      (message "%s, %s" chinese-wbim-current-choices completion))
      t)))

(defun chinese-wbim-table-stop-function ()
  (if (memq (aref chinese-wbim-current-key 0) (chinese-wbim-get-option 'translate-chars))
      nil
    (> (length chinese-wbim-current-key) (chinese-wbim-get-option 'max-length))))

(defun chinese-wbim-table-active-function ()
  (setq chinese-wbim-add-completion-function 'chinese-wbim-table-add-completion
        chinese-wbim-translate-function 'chinese-wbim-table-translate
        chinese-wbim-format-function 'chinese-wbim-table-format
        chinese-wbim-stop-function 'chinese-wbim-table-stop-function))

;; user file and history file
;;;_. chinese-wbim-wb-add-user-file
(defun chinese-wbim-table-add-user-file (file)
  (when file
    (let* ((buflist (chinese-wbim-buffer-list))
           (ufile (expand-file-name file))
           user-buffer)
      (or (file-exists-p ufile)
          (setq ufile (locate-file file load-path)))
      (when (and ufile (file-exists-p ufile))
        ;; make sure the file not load again
        (mapc (lambda (buf)
                (if (string= (expand-file-name (cdr (assoc "file" buf)))
                             ufile)
                    (setq user-buffer (cdr (assoc "buffer" buf)))))
              buflist)
        (unless user-buffer
          (setq file (chinese-wbim-read-file ufile (format chinese-wbim-buffer-name-format
                                                  (chinese-wbim-package-name))))
          (chinese-wbim-make-char-table (chinese-wbim-table-get-user-char (cdar file)) (chinese-wbim-get-option 'char-table))
          (nconc buflist (list file))
          (chinese-wbim-set-option 'table-user-file (cons ufile (cdar file))))))))

(defun chinese-wbim-table-get-user-char (buf)
  "Add user characters. Currently chinese-wbim-wb may not contain all
chinese characters, so if you want more characters to input, you
can add here."
  (let (line chars)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (chinese-wbim-line-content))
        (forward-line 1)
        (if (and (= (length (cadr line)) 1)
                 (> (length (car line)) 2))
            (push line chars)))
      chars)))

(defun chinese-wbim-table-load-history (his-file)
  (when (and his-file (file-exists-p his-file))
    (ignore-errors
      (chinese-wbim-load-history his-file chinese-wbim-current-package)
      (chinese-wbim-set-option 'record-position t)
      (chinese-wbim-set-option 'table-history-file his-file))))

(defun chinese-wbim-table-save-history ()
  "Save history and user files."
  (dolist (package chinese-wbim-package-list)
    (let* ((chinese-wbim-current-package (cdr package))
           (his-file (chinese-wbim-get-option 'table-history-file))
           (user-file (chinese-wbim-get-option 'table-user-file)))
      (when (and his-file
                 (file-exists-p his-file)
                 (file-writable-p his-file))
        (chinese-wbim-save-history his-file chinese-wbim-current-package))
      (when (and user-file
                 (file-exists-p (car user-file))
                 (file-writable-p (car user-file)))
        (with-current-buffer (cdr user-file)
          (save-restriction
            (widen)
            (write-region (point-min) (point-max) (car user-file))))))))
;; 按 TAB 显示补全
(defun chinese-wbim-table-show-completion ()
  (interactive)
  (if (eq last-command 'chinese-wbim-table-show-completion)
      (ignore-errors
        (with-selected-window (get-buffer-window "*Completions*")
          (scroll-up)))
    (if (or (= (length chinese-wbim-current-key) 1) (= (aref chinese-wbim-current-key 0) ?z))
        nil
      (while (not (chinese-wbim-add-completion)))
      (let ((choices (car chinese-wbim-current-choices))
            completion)
        (dolist (c choices)
          (if (listp c)
              (push (list (format "%-4s %s"
                                  (concat chinese-wbim-current-key (cdr c))
                                  (car c)))
                    completion)))
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list
           (all-completions chinese-wbim-current-key (nreverse completion))
           chinese-wbim-current-key)))))
  (funcall chinese-wbim-handle-function))

;; 增加新词
(defvar chinese-wbim-table-minibuffer-map nil)
(defvar chinese-wbim-table-save-always nil)
(when (null chinese-wbim-table-minibuffer-map)
  (setq chinese-wbim-table-minibuffer-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map minibuffer-local-map)
          (define-key map "\C-e" 'chinese-wbim-table-minibuffer-forward-char)
          (define-key map "\C-a" 'chinese-wbim-table-minibuffer-backward-char)
          map)))
;;;_. 增加新词
(defun chinese-wbim-table-minibuffer-forward-char ()
  (interactive)
  (end-of-line)
  (let ((char (save-excursion
                (set-buffer buffer)
                (char-after end))))
    (when char
      (insert char)
      (incf end))))

(defun chinese-wbim-table-minibuffer-backward-char ()
  (interactive)
  (beginning-of-line)
  (let ((char (save-excursion
                (set-buffer buffer)
                (when (>= start (point-min))
                  (decf start)
                  (char-after start)))))
    (when char
      (insert char))))

(defun chinese-wbim-table-add-word ()
  "Create a map for word. The default word is the two characters
before cursor. You can use C-a and C-e to add character at the
begining or end of the word.

默认新词为光标前的两个字，通过两个按键延长这个词：
 C-e 在头部加入一个字
 C-a 在尾部加入一个字
"
  (interactive)
  (let* ((buffer (current-buffer))
         (end (point))
         (start (- (point) 2))
         (word (buffer-substring-no-properties
                start end))
         (user-file (chinese-wbim-get-option 'table-user-file))
         (func (chinese-wbim-get-option 'table-create-word-function))
         choice code words)
    (when func
      (setq word (read-from-minibuffer "加入新词: " word
                                       chinese-wbim-table-minibuffer-map)
            code (funcall func word))
      (setq choice (chinese-wbim-get code))
      (unless (member word (car choice))
        (if (buffer-live-p (cdr user-file))
            (save-excursion
              (set-buffer (cdr user-file))
              (if (string-match "^\\s-$" (buffer-string))
                  (insert "\n" code " " word)
                (chinese-wbim-bisearch-word code (point-min) (point-max))
                (let ((words (chinese-wbim-line-content)))
                  (goto-char (line-end-position))
                  (if (string= (car words) code)
                      (insert " " word)
                    (insert "\n" code " " word))))
              (setcar choice (append (car choice) (list word)))
              (if chinese-wbim-table-save-always
                  (save-restriction
                    (widen)
                    (write-region (point-min) (point-max) (car user-file)))))
          (error "the user buffer is closed!")))))
  (message nil))

(add-hook 'kill-emacs-hook 'chinese-wbim-table-save-history)

(provide 'chinese-wbim-table)
;;; chinese-wbim-table.el ends here
