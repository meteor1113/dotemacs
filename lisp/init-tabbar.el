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

;; tabbar
(add-hook 'after-init-hook
          '(lambda ()
             (when (or (daemonp) (not (locate-library "tabbar-ruler")))
               (ignore-errors (tabbar-mode 1))))
          'append)

(eval-after-load "tabbar"
  '(progn
     ;; backup tabbar.el's button image
     ;; (setq tabbar-home-button-enabled-image-orig tabbar-home-button-enabled-image
     ;;       tabbar-home-button-disabled-image-orig tabbar-home-button-disabled-image
     ;;       tabbar-scroll-left-button-enabled-image-orig tabbar-scroll-left-button-enabled-image
     ;;       tabbar-scroll-right-button-enabled-image-orig tabbar-scroll-right-button-enabled-image)

     (defadvice tabbar-buffer-tab-label (after modified-flag activate)
       (setq ad-return-value
             (if (and (or (not (featurep 'tabbar-ruler))
                          (not window-system))
                      (buffer-modified-p (tabbar-tab-value tab)))
                 ;; (buffer-file-name (tabbar-tab-value tab))
                 (concat ad-return-value "*")
               ad-return-value)))

     (defun update-tabbar-modified-state ()
       (tabbar-set-template tabbar-current-tabset nil)
       (tabbar-display-update))

     (defadvice undo (after update-tabbar-tab-label activate)
       (update-tabbar-modified-state))

     (defadvice set-buffer-file-coding-system (after update-tabbar-tab-label activate)
       (update-tabbar-modified-state))

     (define-key tabbar-mode-map [C-prior] 'tabbar-backward)
     (define-key tabbar-mode-map [C-next] 'tabbar-forward)
     (add-hook 'first-change-hook 'update-tabbar-modified-state)
     (add-hook 'after-save-hook 'update-tabbar-modified-state)))

;; tabbar-ruler
(setq tabbar-ruler-invert-deselected nil)
(setq tabbar-ruler-movement-timer-delay 10000)
(add-hook 'after-init-hook
          '(lambda ()
             (when (not (daemonp))
               (require 'tabbar-ruler nil 'noerror)))
          t)

(eval-after-load "tabbar-ruler"
  '(progn
     (tabbar-ruler-remove-caches)

     ;; restore tabbar.el's button image
     ;; (setq tabbar-home-button-enabled-image tabbar-home-button-enabled-image-orig
     ;;       tabbar-home-button-disabled-image tabbar-home-button-disabled-image-orig
     ;;       tabbar-scroll-left-button-enabled-image tabbar-scroll-left-button-enabled-image-orig
     ;;       tabbar-scroll-right-button-enabled-image tabbar-scroll-right-button-enabled-image-orig)
     ;; (setq tabbar-home-button
     ;;       (cons (cons "[o]" tabbar-home-button-enabled-image)
     ;;             (cons "[x]" tabbar-home-button-disabled-image)))
     ;; (setq tabbar-buffer-home-button
     ;;       (cons (cons "[+]" tabbar-home-button-enabled-image)
     ;;             (cons "[-]" tabbar-home-button-disabled-image)))
     ;; (setq tabbar-scroll-left-button
     ;;       (cons (cons " <" tabbar-scroll-left-button-enabled-image)
     ;;             (cons " =" nil)))
     ;; (setq tabbar-scroll-right-button
     ;;       (cons (cons " >" tabbar-scroll-right-button-enabled-image)
     ;;             (cons " =" nil)))

     (defadvice tabbar-popup-menu (after add-menu-item activate)
       "Add customize menu item to tabbar popup menu."
       (setq ad-return-value
             (append ad-return-value
                     '("--"
                       ["Copy Buffer Name" (kill-new
                                            (buffer-name
                                             (tabbar-tab-value
                                              tabbar-last-tab)))]
                       ["Copy File Path" (kill-new
                                          (buffer-file-name
                                           (tabbar-tab-value
                                            tabbar-last-tab)))
                        :active (buffer-file-name
                                 (tabbar-tab-value tabbar-last-tab))]
                       ["Open Dired" dired-jump
                        :active (fboundp 'dired-jump)]
                       ["Open in Windows Explorer" (w32explore buffer-file-name)
                        :active (and buffer-file-name
                                     (eq system-type 'windows-nt)
                                     (require 'w32-browser nil 'noerror))]
                       "--"
                       ["Undo Close Tab" undo-kill-buffer
                        :active (fboundp 'undo-kill-buffer)]))))

     (defadvice tabbar-line-tab (around window-or-terminal activate)
       "Fix tabbar-ruler in terminal"
       (if window-system
           ad-do-it
         (setq ad-return-value
               (let ((tab (ad-get-arg 0))
                     (tabbar-separator-value "|"))
                 (concat (propertize
                          (if tabbar-tab-label-function
                              (funcall tabbar-tab-label-function tab)
                            tab)
                          'tabbar-tab tab
                          'local-map (tabbar-make-tab-keymap tab)
                          'help-echo 'tabbar-help-on-tab
                          'mouse-face 'tabbar-highlight
                          'face (if (tabbar-selected-p tab
                                                       (tabbar-current-tabset))
                                    'tabbar-selected
                                  'tabbar-unselected)
                          'pointer 'hand)
                         tabbar-separator-value)))))

     ;; ;; (unless (eq system-type 'windows-nt)
     ;; (set-face-attribute 'tabbar-default nil
     ;;                     :family (face-attribute 'default :family))
     ;; (add-hook 'after-make-frame-functions
     ;;           (lambda (frame)
     ;;             (with-selected-frame frame
     ;;               (set-face-attribute 'tabbar-default frame
     ;;                                   :family (face-attribute 'default
     ;;                                                           :family)))));; )
     ;; (setq tabbar-ruler-excluded-buffers '())
     (set-face-attribute 'tabbar-selected nil
                         :foreground "blue")
     (tabbar-ruler-group-buffer-groups)))

(provide 'init-tabbar)

;;; init-tabbar.el ends here
