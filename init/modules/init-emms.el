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

;; emms
(autoload 'emms "emms-playlist-mode" nil t)

(eval-after-load "emms-playlist-mode"
  '(progn
     (define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
     (define-key emms-playlist-mode-map [double-mouse-1]
       'emms-playlist-mode-play-current-track)))

(defun init-emms ()
  "Initial emms."
  (or (featurep 'emms-setup)
      (when (and (require 'emms-setup nil t)
                 (require 'emms-mode-line nil t)
                 (require 'emms-playing-time nil t))
        (emms-standard)
        (emms-default-players)
        (progn
          (define-emms-simple-player mpg123 '(file url)
            (emms-player-simple-regexp "mp3" "mp2") "mpg123")
          (add-to-list 'emms-player-list 'emms-player-mpg123 'append))
        (setq emms-repeat-playlist t)
        (setq emms-mode-line-format "[%s]")
        (defun emms-mode-line-playlist-current-nondirectory ()
          "Format the currently playing song."
          (format emms-mode-line-format
                  (file-name-nondirectory
                   (emms-track-description
                    (emms-playlist-current-selected-track)))))
        (setq emms-mode-line-mode-line-function
              'emms-mode-line-playlist-current-nondirectory)
        (setq emms-mode-line-titlebar-function
              'emms-mode-line-playlist-current-nondirectory)
        (emms-mode-line 1)
        (emms-mode-line-blank)
        ;; (setq emms-playing-time-style 'bar)
        ;; (emms-playing-time 1)
        t)))

(defun emms-dir-tree ()
  "Query for a directory tree, or switch to the current emms-playlist buffer."
  (interactive)
  (if (init-emms)
      (if (or (null emms-playlist-buffer)
              (not (buffer-live-p emms-playlist-buffer)))
          (call-interactively 'emms-play-directory-tree)
        (emms-playlist-mode-go))
    (message "Initial emms failed.")))

(defadvice emms (before init-emms activate)
  "Initial emms first."
  (init-emms))

(defadvice emms-history-save (around delete-empty-history activate)
  "If have not emms playlist, delete emms-history-file."
  (let (have-playlist)
    (dolist (buf (emms-playlist-buffer-list))
      (when (> (buffer-size buf) 0)
        (setq have-playlist t)))
    (if (not have-playlist)
        (when (file-exists-p emms-history-file)
          (delete-file emms-history-file))
      (ignore-errors (make-directory (file-name-directory emms-history-file)))
      ad-do-it)))

;; (when (and window-system
;;            (require 'emms-history nil t)
;;            (file-exists-p emms-history-file)
;;            (init-emms))
;;   (setq emms-history-start-playing t)
;;   (emms-history-load))

(provide 'init-emms)

;;; init-emms.el ends here
