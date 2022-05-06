;;; chinese-wbim-eb.el --- Enable Wubi(五笔) Input Method in Emacs.

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'chinese-wbim-eb)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'chinese-wbim-table)

(defvar chinese-wbim-eb-user-file nil)
(defvar chinese-wbim-eb-history-file nil)
(defvar chinese-wbim-eb-package nil)
(defvar chinese-wbim-eb-punctuation-list nil)
(defvar chinese-wbim-eb-load-hook nil)
(defvar chinese-wbim-eb-initialized nil)
(defvar chinese-wbim-eb-char-table (make-vector 1511 0))

(defun chinese-wbim-eb-create-word (word)
  "Insert word to database and write into user file."
  (let ((len (length word))
        code)
    (setq code
     (cond
      ((= len 2)
       (concat (substring (chinese-wbim-table-get-char-code (aref word 0)) 0 2)
               (substring (chinese-wbim-table-get-char-code (aref word 1)) 0 2)))
      ((= len 3)
       (concat (substring (chinese-wbim-table-get-char-code (aref word 0)) 0 2)
               (substring (chinese-wbim-table-get-char-code (aref word 1)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word 2)) 0 1)))
      (t
       (concat (substring (chinese-wbim-table-get-char-code (aref word 0)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word 1)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word 2)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word (1- (length word)))) 0 1)))))))

(unless chinese-wbim-eb-initialized
  (setq chinese-wbim-eb-package chinese-wbim-current-package)
  (setq chinese-wbim-eb-punctuation-list
        (chinese-wbim-read-punctuation chinese-wbim-eb-package))
  (run-hooks 'chinese-wbim-eb-load-hook)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path "chinese-wbim-eb-map")))
  (let ((map (chinese-wbim-mode-map)))
    (define-key map "\t" 'chinese-wbim-table-show-completion))
  
  (chinese-wbim-table-add-user-file chinese-wbim-eb-user-file)
  (chinese-wbim-table-load-history chinese-wbim-eb-history-file)
  (chinese-wbim-set-option 'table-create-word-function 'chinese-wbim-eb-create-word)
  (chinese-wbim-set-option 'char-table chinese-wbim-eb-char-table)
  (chinese-wbim-set-option 'punctuation-list 'chinese-wbim-eb-punctuation-list)
  (chinese-wbim-set-option 'max-length 4)
  (chinese-wbim-set-option 'translate-chars '(?\[))
  (chinese-wbim-set-option 'all-completion-limit 3)
  (chinese-wbim-set-active-function 'chinese-wbim-table-active-function)
  (setq chinese-wbim-eb-initialized t))

(provide 'chinese-wbim-eb)
;;; chinese-wbim-eb.el ends here
