;; Copyright (C) 2010  Brian Jiang

;; Author: Brian Jiang <brianjcj@gmail.com>
;; Keywords: Programming
;; Version: 0.1

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

(defvar codepilot-mark-tag 'codepilot-mark)

(defface codepilot-mark-face
  '((default (:inherit region))
    (((class color) (background light)) (:background "lawn green"))
    (((class color) (background dark)) (:background "green" :foreground "black")))
  "*Font used by mymark."
  :group 'codepilot)

(defvar codepilot-mark-face-var 'codepilot-mark-face)

(defun codepilot-mark-region (b e)
  (let ((ov (make-overlay b e)))
    (overlay-put ov 'face codepilot-mark-face-var)
    (overlay-put ov 'tag codepilot-mark-tag)))

(defun codepilot-mark-line ()
  (interactive)
  (codepilot-mark-region (line-beginning-position) (line-end-position)))

(defun codepilot-unmark-line ()
  (interactive)
  (dolist (o (overlays-in (line-beginning-position) (line-end-position)))
    (when (eq (overlay-get o 'tag) codepilot-mark-tag)
      (delete-overlay o))))

(defun codepilot-mark-regexp (regexp)
  (interactive "sRegexp:")
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (codepilot-mark-region (match-beginning 0) (match-end 0)))))))

(defun codepilot-mark-words (wd)
  (interactive
   (list
    (let ((cur (current-word)))
      (read-string
       (concat "Word" (if cur (concat " (default " cur ")") "") ": ")
       nil nil cur))))
  (codepilot-mark-regexp (concat "\\_<" wd "\\_>")))

(defun codepilot-mark-words-1 (wd)
  (interactive
   (list
    (let ((cur (current-word)))
      (read-string
       (concat "Word" (if cur (concat " (default " cur ")") "") ": ")
       nil nil cur))))
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (word-search-forward wd nil t)
          (codepilot-mark-region (match-beginning 0) (match-end 0)))))))

(defun codepilot-mark-string (str)
  (interactive
   (list
    (let ((cur (current-word)))
      (read-string
       (concat "Word" (if cur (concat " (default " cur ")") "") ": ")
       nil nil cur))))
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (search-forward str nil t)
          (codepilot-mark-region (match-beginning 0) (match-end 0)))))))

(defun codepilot-unmark-all ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (dolist (o (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get o 'tag) codepilot-mark-tag)
          (delete-overlay o))))))

(defun codepilot-unmark-all-in-region (from to)
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (dolist (o (overlays-in from to))
        (when (eq (overlay-get o 'tag) codepilot-mark-tag)
          (delete-overlay o))))))


(defface codepilot-mark-hl-text-face
  '((default (:inherit region))
    (((class color) (background light)) (:background "DarkOliveGreen2"))
    ;; (((class color) (background dark)) (:background "DarkOliveGreen2" :foreground "black"))
    (((class color) (background dark)) (:background "SeaGreen" :foreground "white")))
  "*Font used by folding overlay."
  :group 'codepilot)

(defvar codepilot-highlight-one-line nil)

(defun codepilot-highlight-one-line ()
  "Highlight the current line, unhighlighting a previously jumped to line."
  (codepilot-unhighlight-one-line)
  (setq codepilot-highlight-one-line
	(make-overlay (line-beginning-position) (line-end-position)))
  (overlay-put codepilot-highlight-one-line 'face 'codepilot-mark-hl-text-face)
  (add-hook 'pre-command-hook 'codepilot-unhighlight-one-line))

(defun codepilot-unhighlight-one-line ()
  "Unhighlight the currently highlighted line."
  (if codepilot-highlight-one-line
      (progn
	(delete-overlay codepilot-highlight-one-line)
	(setq codepilot-highlight-one-line nil)))
  (remove-hook 'pre-command-hook 'codepilot-unhighlight-one-line))


(defvar codepilot-highlight-one-line-1 nil)
(make-variable-buffer-local 'codepilot-highlight-one-line-1)

(defun codepilot-highlight-one-line-1 ()
  "Highlight the current line, unhighlighting a previously jumped to line."
  (codepilot-unhighlight-one-line-1)
  (setq codepilot-highlight-one-line-1
	(make-overlay (line-beginning-position) (line-beginning-position 2)))
  (overlay-put codepilot-highlight-one-line-1 'face 'codepilot-mark-hl-text-face))

(defun codepilot-unhighlight-one-line-1 ()
  "Unhighlight the currently highlighted line."
  (if codepilot-highlight-one-line-1
      (progn
	(delete-overlay codepilot-highlight-one-line-1)
	(setq codepilot-highlight-one-line-1 nil))))


(provide 'cp-mark)