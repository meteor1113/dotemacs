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
(let ((include-dirs
       '("C:/MinGW/include"
         "C:/MinGW/include/c++/3.4.5"
         "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include")))
  (setq ffap-c-path (append ffap-c-path include-dirs))
  (if (fboundp 'semantic-add-system-include)
      (mapc (lambda (dir)
              (semantic-add-system-include dir 'c++-mode)
              (when (require 'semantic-c nil t)
                (semantic-add-system-include dir 'c-mode)))
            include-dirs)))


(provide 'init-winnt)
