;;; chinese-wbim-wb.el --- Enable Wubi(五笔) Input Method in Emacs.

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

;;; Features:
;; 1. 能导入输入历史
;; 2. 提供造词的命令
;; 3. 提供候选的单字
;; 4. 拼音输入，提示五笔字根
;; 5. 处理标点
;; 6. 使用 ; ' 快速选择

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'chinese-wbim-table)

(defgroup chinese-wbim-wb nil
  "chinese-wbim wubi input method"
  :group 'chinese-wbim)
  
(defcustom chinese-wbim-wb-history-file "~/.emacs.d/wbx-history"
  "保存选择的历史记录."
  :type 'file
  :group 'chinese-wbim-wb)

(defcustom chinese-wbim-wb-user-file "mywb.txt"
  "保存用户自造词."
  :type 'file
  :group 'chinese-wbim-wb)

(defcustom chinese-wbim-wb-save-always nil
  "是否每次加入新词都要保存.
当然设置为 nil，也会在退出 Emacs 里保存一下的."
  :type 'boolean
  :group 'chinese-wbim-wb)

(defcustom chinese-wbim-wb-add-all-completion-limit 3
  "在超过输入字符串超过这个长度时会添加所有补全."
  :type 'integer
  :group 'chinese-wbim-wb)

(defvar chinese-wbim-wb-load-hook nil)
(defvar chinese-wbim-wb-package nil)
(defvar chinese-wbim-wb-char-table (make-vector 1511 0))
(defvar chinese-wbim-wb-punctuation-list nil)
(defvar chinese-wbim-wb-initialized nil)

(defun chinese-wbim-wb-create-word (word)
  "Insert WORD to database and write into user file."
  (let ((len (length word))
        code)
    (setq code
     (cond
      ((= len 2)
       (concat (substring (chinese-wbim-table-get-char-code (aref word 0)) 0 2)
               (substring (chinese-wbim-table-get-char-code (aref word 1)) 0 2)))
      ((= len 3)
       (concat (substring (chinese-wbim-table-get-char-code (aref word 0)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word 1)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word 2)) 0 2)))
      (t
       (concat (substring (chinese-wbim-table-get-char-code (aref word 0)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word 1)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word 2)) 0 1)
               (substring (chinese-wbim-table-get-char-code (aref word (1- (length word)))) 0 1)))))))

;;;_. load it
(unless chinese-wbim-wb-initialized
  (setq chinese-wbim-wb-package chinese-wbim-current-package)
  (setq chinese-wbim-wb-punctuation-list
        (chinese-wbim-read-punctuation chinese-wbim-wb-package))
  (let ((map (chinese-wbim-mode-map)))
    (define-key map "\t" 'chinese-wbim-table-show-completion)
    (define-key map ";" 'chinese-wbim-quick-select-1)
    (define-key map "'" 'chinese-wbim-quick-select-2))
  (defvar chinese-wbim-wb-use-gbk nil)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path
                  (if (and (boundp 'chinese-wbim-wb-use-gbk)
                           chinese-wbim-wb-use-gbk)
                      "chinese-wbim-wb-gbk" "chinese-wbim-wb-gb2312"))))

  (chinese-wbim-table-add-user-file chinese-wbim-wb-user-file)
  (chinese-wbim-table-load-history chinese-wbim-wb-history-file)
  (run-hooks 'chinese-wbim-wb-load-hook)
  (chinese-wbim-set-option 'table-create-word-function 'chinese-wbim-wb-create-word)
  (chinese-wbim-set-option 'punctuation-list 'chinese-wbim-wb-punctuation-list)
  (chinese-wbim-set-option 'max-length 4)
  (chinese-wbim-set-option 'translate-chars '(?z))
  (chinese-wbim-set-option 'all-completion-limit chinese-wbim-wb-add-all-completion-limit)
  (chinese-wbim-set-option 'char-table chinese-wbim-wb-char-table)
  (chinese-wbim-set-active-function 'chinese-wbim-table-active-function)
  (setq chinese-wbim-wb-initialized t))

(provide 'chinese-wbim-wb)
;;; chinese-wbim-wb.el ends here
