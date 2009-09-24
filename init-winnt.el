;;;
;; Copyright (C) 2009 Meteor Liu
;;
;; This code has been released into the Public Domain.
;; You may do whatever you like with it.
;;
;; @file
;; @author Meteor Liu <meteor1113@gmail.com>
;; @date 2008-08-08


;;; windows-nt setting
(let ((include-dir
       '("C:/MinGW/include"
         "C:/MinGW/include/c++/3.4.5"
         "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include")))
  (setq ffap-c-path (append ffap-c-path include-dir))
  (if (fboundp 'semantic-add-system-include)
      (dolist (dir include-dir)
        (semantic-add-system-include dir 'c++-mode))))

;; hack server-start on windows
;; (and (= emacs-major-version 23)
;;      (defun server-ensure-safe-dir (dir) "Noop" t))


(provide 'init-winnt)
