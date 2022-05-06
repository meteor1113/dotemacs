;;; chinese-wbim-cj.el --- Enable Wubi(五笔) Input Method in Emacs.

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

(require 'chinese-wbim-table)

(defvar chinese-wbim-cj-package nil)
(defvar chinese-wbim-cj-punctuation-list nil)
(defvar chinese-wbim-cj-initialized nil)
(defvar chinese-wbim-cj-load-hook nil)
(defvar chinese-wbim-cj-char-table (make-vector 1511 0))

(unless chinese-wbim-cj-initialized
  (setq chinese-wbim-cj-package chinese-wbim-current-package)
  (setq chinese-wbim-cj-punctuation-list
        (chinese-wbim-read-punctuation chinese-wbim-cj-package))
  (run-hooks 'chinese-wbim-cj-load-hook)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path "chinese-wbim-cj-chars")))
  (chinese-wbim-set-option 'char-table chinese-wbim-cj-char-table)
  (chinese-wbim-set-option 'punctuation-list 'chinese-wbim-cj-punctuation-list)
  (chinese-wbim-set-option 'max-length 5)
  (chinese-wbim-set-option 'translate-chars '(?x ?z))
  (chinese-wbim-set-option 'all-completion-limit 3)
  (chinese-wbim-set-active-function 'chinese-wbim-table-active-function)
  (setq chinese-wbim-cj-initialized t))

(provide 'chinese-wbim-cj)
;;; chinese-wbim-cj.el ends here
