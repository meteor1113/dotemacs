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

;; double click highlight

(require 'cp-mark)

(defvar smart-hl-highlight-txt "")
(make-variable-buffer-local 'smart-hl-highlight-txt)

(defvar smart-hl-highlight-last-start 0)
(defvar smart-hl-highlight-last-end 0)
(make-variable-buffer-local 'smart-hl-highlight-last-start)
(make-variable-buffer-local 'smart-hl-highlight-last-end)

(defvar smart-hl-highlight-update-needed nil)
(make-variable-buffer-local 'smart-hl-highlight-update-needed)



(defun smart-hl-highlight-text-in-window ()
  (let ((start (window-start))
        (end (window-end))
        ;;(start (point-min))
        ;;(end (point-max))
        (codepilot-mark-tag 'smart-hl))
    (setq smart-hl-highlight-last-start start)
    (setq smart-hl-highlight-last-end end)
    (setq smart-hl-highlight-update-needed nil)

    (cond ((string= smart-hl-highlight-txt "")
           (codepilot-unmark-all-in-region start end))
          (t
           (save-excursion
             (save-match-data
               (goto-char start)
               (codepilot-unmark-all-in-region start end)
               (while (re-search-forward smart-hl-highlight-txt end t)
                 (codepilot-mark-region (match-beginning 0) (match-end 0)))))))))

;; (defadvice mouse-move-drag-overlay (after smart-hl (ol start end mode))
;;   ;; don't delete the dedicated windows

;;   (cond ((= mode 1)
;;          (let ((txt (buffer-substring-no-properties (overlay-start ol) (overlay-end ol))))
;;            (cond ((or (string= txt "")
;;                       (string-match "^[\t\n\s]*$" txt)
;;                       (string-match "\n" txt))
;;                   (setq smart-hl-highlight-txt "")
;;                   (smart-hl-highlight-text-in-window))
;;                  (t
;;                   (setq smart-hl-highlight-txt (concat "\\_<" (regexp-quote txt) "\\_>"))
;;                   (smart-hl-highlight-text-in-window)))))
;;         ((= mode 2)
;;          (setq smart-hl-highlight-txt "")
;;          (smart-hl-highlight-text-in-window))))

;; (ad-activate 'mouse-move-drag-overlay)

(defadvice mouse-start-end (after smart-hl (start end mode))
  ; ad-return-value
  (cond ((= mode 1)
         (let ((txt (buffer-substring-no-properties (nth 0 ad-return-value) (nth 1 ad-return-value))))
           (cond ((or (string= txt "")
                      (string-match "^[\t\n\s]*$" txt)
                      (string-match "\n" txt))
                  (setq smart-hl-highlight-txt "")
                  (smart-hl-highlight-text-in-window))
                 (t
                  (setq smart-hl-highlight-txt (concat "\\_<" (regexp-quote txt) "\\_>"))
                  (smart-hl-highlight-text-in-window)))))
        ((= mode 2)
         (setq smart-hl-highlight-txt "")
         (smart-hl-highlight-text-in-window))))

(ad-activate 'mouse-start-end)


(defun smart-hl-highlight-text-in-window-scroll (win pos)

  ;;(run-with-idle-timer 0 nil #'smart-hl-highlight-text-in-window (window-buffer win))
  (with-current-buffer (window-buffer win)
    ;;(print win)
    (setq smart-hl-highlight-update-needed nil)
    (let* ((start (window-start))
           (end (window-end))
           s e
           (codepilot-mark-tag 'smart-hl))

      (when (or (< start smart-hl-highlight-last-start)
                (> end smart-hl-highlight-last-end))

        (cond ((< start smart-hl-highlight-last-start)
               (setq s start)
               (setq e (min end smart-hl-highlight-last-start)))
              (t
               (setq s (max start smart-hl-highlight-last-end))
               (setq e end)))

        (setq smart-hl-highlight-last-start start)
        (setq smart-hl-highlight-last-end end)

        (cond ((string= smart-hl-highlight-txt "")
               (codepilot-unmark-all-in-region s e))
              (t
               (codepilot-unmark-all-in-region s e)
               (save-excursion
                 (save-match-data
                   (goto-char s)
                   (while (re-search-forward smart-hl-highlight-txt e t)
                     (codepilot-mark-region (match-beginning 0) (match-end 0)))))))))))

(defun smart-hl-highlight-text-in-window-schedule (win pos)
  (let* ((start (window-start))
         (end (min (+ start 8000) (point-max)))
         e s
         (codepilot-mark-tag 'smart-hl))
    (when (or (< start smart-hl-highlight-last-start)
              (> end smart-hl-highlight-last-end))

      (cond ((< start smart-hl-highlight-last-start)
             (setq s start)
             (setq e (min end smart-hl-highlight-last-start)))
            (t
             (setq s (max start smart-hl-highlight-last-end))
             (setq e end)))

      (codepilot-unmark-all-in-region s e)
      ))
  (unless smart-hl-highlight-update-needed
    (setq smart-hl-highlight-update-needed t)
    (run-with-idle-timer 0 nil #'smart-hl-highlight-text-in-window-scroll win pos)))

;; (add-hook 'window-scroll-functions 'smart-hl-highlight-text-in-window-scroll)
(add-hook 'window-scroll-functions 'smart-hl-highlight-text-in-window-schedule)


;; (remove-hook 'window-scroll-functions 'smart-hl-highlight-text-in-window-scroll)
;; (remove-hook 'window-scroll-functions 'smart-hl-highlight-text-in-window-schedule)

(defun smart-hl-text (text)
  (interactive "sRegexp: ")
  (cond (text
         (setq smart-hl-highlight-txt text)
         (smart-hl-highlight-text-in-window))
        (t
         (setq smart-hl-highlight-txt "")
         (smart-hl-highlight-text-in-window))))

(defun smart-hl-current-word ()
  ""
  (interactive)
  (let ((cw (current-word)))
    (cond (cw
           (smart-hl-text (concat "\\_<" (regexp-quote cw) "\\_>")))
          (t
           (smart-hl-text cw)))))

(provide 'smart-hl)
