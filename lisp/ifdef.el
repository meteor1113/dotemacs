;;;; ifdef - Parse the #if...#elif...#else...#endif block in a C file.
;;;; Mark them with different colors according to the nest level.
;;;; Author: Dai Yuwen
;;;; License: GPL
;;;; $Date: 2006/05/22 02:45:46 $

;;;; Usage: open a C file, then M-x mark-ifdef
;;;; You can add a hook to the C-mode, and bind `mark-ifdef' with C-c C-i:
;;;; (add-hook 'c-mode-common-hook '(lambda ()
;;;;				 (require 'ifdef)
;;;;				 (global-set-key [?\C-c ?\C-i] 'mark-ifdef)))

(defun get-end-of-line ()
  "Get the position of the end of the current line."
  (save-excursion
    (let ((junk (end-of-line)))
      (point))))

(defface ifdef-highlight-face1
  '((((type tty pc) (class color))
     (:background "turquoise3"))
    (((class color) (background light))
     (:background "paleturquoise"))
    (((class color) (background dark))
     (:background "paleturquoise4"))
    (t (:underline t)))
  "The face of the out most #if...#endif block.")

(defface ifdef-highlight-face4
  '((((type tty pc) (class color))
     (:background "pink3"))
    (((class color) (background light))
     (:background "pink"))
    (((class color) (background dark))
     (:background "pink4"))
    (t (:underline t)))
  "The face of the in most #if...#endif block.")

(defface ifdef-highlight-face3
  '((((type tty pc) (class color))
     (:background "yellow3"))
    (((class color) (background light))
     (:background "yellow"))
    (((class color) (background dark))
     (:background "yellow4"))
    (t (:underline t)))
  "The face of the 3rd level #if...#endif block.")

(defface ifdef-highlight-face2
  '((((type tty pc) (class color))
     (:background "paleGreen1"))
    (((class color) (background light))
     (:background "paleGreen2"))
    (((class color) (background dark))
     (:background "paleGreen3"))
    (t (:underline t)))
  "The face of the 2nd level #if...#endif block.")

(defvar ifdef-highlight-face1 'ifdef-highlight-face1)
(defvar ifdef-highlight-face2 'ifdef-highlight-face2)
(defvar ifdef-highlight-face3 'ifdef-highlight-face3)
(defvar ifdef-highlight-face4 'ifdef-highlight-face4)

;; put the faces in a hash table, only 4 colors 
(defvar face-table (make-hash-table :test 'eql :size 16))
(puthash 1 ifdef-highlight-face1 face-table)
(puthash 2 ifdef-highlight-face2 face-table)
(puthash 3 ifdef-highlight-face3 face-table)
(puthash 4 ifdef-highlight-face4 face-table)

;; the overlay list
(defvar ifdef-overlay-list nil)
(defvar ifdef-marked-flag nil)

(defun mark-line (level begin end)
  "Mark the region from BEGIN to END with the LEVELth face."
  (let ((ov (make-overlay begin end))
        (face (gethash level face-table)))
    (when face
      (overlay-put ov 'face face)
      (overlay-put ov 'priority 0)
      (push ov ifdef-overlay-list))))


(defun mark-ifdef3 (start end other)
  "Mark the block with delimiter START and END.
OTHERS is intermediate mark, which can be nil. "
  (make-variable-buffer-local 'ifdef-marked-flag)
  (make-variable-buffer-local 'ifdef-overlay-list)
  (if ifdef-marked-flag          ; if already marked, remove the marks
      (ifdef-remove-marks)
    (save-excursion
      (let ((nest 0)
            (continue-flag t))
        (goto-char (point-min))
        (while continue-flag
          (when (re-search-forward  start
                                    (get-end-of-line) t 1) ; find START delimiter
            (setq nest (1+ nest))
            (if (> nest 0)
                (mark-line nest (match-beginning 0) (match-end 0))))

          (if other                     ; if other is not nil
              (when (re-search-forward  other
                                        (get-end-of-line) t 1) ; find a #else or #elif
                (if (> nest 0)
                    (mark-line nest (match-beginning 0) (match-end 0)))))

          (when (re-search-forward end
                                   (get-end-of-line) t 1) ; find END delimiter
            (if (> nest 0)
                (mark-line nest (match-beginning 0) (match-end 0)))
            (setq nest (1- nest))
            (if (<= nest 0)       ; found the out most START delimiter
                (setq nest 0)))
          (if (= 1 (forward-line 1))  ; we reach the end of file, exit
              (setq continue-flag nil))))
      (setq ifdef-marked-flag t))))


(defun ifdef-remove-marks ()
  (dolist (ov ifdef-overlay-list)
    (delete-overlay ov)
    (setq ifdef-overlay-list nil)
    (setq ifdef-marked-flag nil)))  ; reset the flag


(defun mark-blocks ()
  "Mark blocks delimited by { and }. "
  (interactive)
  (mark-ifdef3 "{" "}" nil))

(defun mark-if-makefile ()
  "Mark if ... else ... endif in GNU makefile."
  (interactive)
  (mark-ifdef3 "^[ 	]*if.*$"  "^[ 	]*endif.*$"  "^[ 	]*el.*$"))

(defun mark-ifdef ()
  "Mark if ... else ... endif in GNU makefile."
  (interactive)
  (mark-ifdef3 "^[ 	]*#[ 	]*if.*$"  "^[ 	]*#[ 	]*endif.*$"  "^[ 	]*#[ 	]*el.*$"))

  
;;;; add (require 'ifdef) in your .emacs file
(provide 'ifdef)