;;; ac-racer.el --- auto-complete source of racer

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-ac-racer
;; Package-Version: 20170114.809
;; Package-Commit: 4408c2d652dec0432e20c05e001db8222d778c6b
;; Version: 0.02
;; Package-Requires: ((emacs "24.3") (auto-complete "1.5.0") (racer "0.0.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ac-racer.el provides auto-complete source for Rust programming language.

;;; Code:

(require 'auto-complete)
(require 'racer)
(require 'cl-lib)

(defgroup ac-racer nil
  "auto-complete source of racer"
  :group 'auto-complete)

(defvar ac-racer--tempfile (concat temporary-file-directory "ac-racer-complete"))

(defun ac-racer--collect-candidates ()
  (goto-char (point-min))
  ;; MATCH (1:candidate),line,column,filepath,(2:type),(3:signature)
  (let ((re "^MATCH \\([^,]+\\),[^,]+,[^,]+,[^,]+,\\([^,]+\\),\\(.+\\)"))
    (cl-loop while (re-search-forward re nil t)
             for candidate = (match-string-no-properties 1)
             for type = (match-string-no-properties 2)
             for signature = (match-string-no-properties 3)
             collect
             (popup-make-item candidate :document signature :summary type))))

(defun ac-racer--prefix ()
  (save-excursion
    (skip-syntax-backward "w_")
    (point)))

(defun ac-racer--candidates ()
  (let ((process-environment (if racer-rust-src-path
                                 (cons (concat "RUST_SRC_PATH=" racer-rust-src-path)
                                       process-environment)
                               process-environment))
        (line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column)))
        (file (or (buffer-file-name) "")))
    (write-region (point-min) (point-max) ac-racer--tempfile nil 'no-message)
    (with-temp-buffer
      (let ((ret (process-file racer-cmd nil t nil
                               "complete" line column file ac-racer--tempfile)))
        (when (zerop ret)
          (ac-racer--collect-candidates))))))

;;;###autoload
(defun ac-racer-setup ()
  (interactive)
  (auto-complete-mode +1)
  (add-to-list 'ac-sources 'ac-source-racer))

(ac-define-source racer
  '((prefix . ac-racer--prefix)
    (candidates . ac-racer--candidates)
    (requires . -1)))

(provide 'ac-racer)

;;; ac-racer.el ends here
