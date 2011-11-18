;;; code-imports.el --- A module for organizing and adding to code imports.
;; Copyright (C) 2011 Andrew Hyatt
;;
;;     This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This module defines import/include handling for C++ and Java.
;;
;; To use, add this to your .emacs:
;;    (require 'code-imports)
;;
;; You will also need to define `code-imports-project-directory' to
;; define the project root so that this module can make sense of
;; grabbed imports.
;;
;; Then, when you are on a .h and .java file that you want to import
;; into .cc or .java file, respectively, use:
;;
;; M-x code-imports-grab-import
;;
;; Grab as many imports as you want, then go to the .cc or .java file
;; you want to use, and use:
;;
;; M-x code-imports-add-grabbed-imports
;;
;; This will add the imports to the file, and then organize your
;; imports in accordance with a particlar style (by default set to the
;; Google style guide).
;;
;; You can also just organize your imports with:
;;
;; M-x code-imports-organize-imports
;;
;;; Code:

(when (featurep 'xemacs)
  (error "The code-imports library is not compatible with Xemacs"))

(defvar code-imports-project-directory nil
  "A regex matching the root of the project.")

(defvar code-imports-clipboard nil
  "An alist of modes to import clipboards.
Each mode stores the clipboard data in whatever format is useful
for that mode.")

(defconst code-imports-import-regex "^\\(import\\|#include\\)"
  "Regex to identify Java or C++ imports.")

(defvar code-imports-c++-ordering
  '(self "\\.h>" "<.*[^.].>" t)
  "Ordering of import regexes for c++.
Each one defines a block that matches the regex and is separated
from other blocks by a line.  A 'self matches the corresponding
.h file, and t matches anything not matched by another pattern.
The final t is implied if not specified.")
(put 'code-imports-c++-ordering 'safe-local-variable 'listp)

(defvar code-imports-java-ordering
  '("^static" "^com" "^junit" "^net" "^org" "^java[^x]" "^javax" t)
  "Ordering of import regexes for Java.
Each one defines a block where the import matches the regex and
is separated from the other blocks by a line.  A t matches
anything not matched by another pattern.")
(put 'code-imports-java-ordering 'safe-local-variable 'listp)

(defun code-imports-clear-clipboards ()
  "Clears the clipboards.
For use when you have something in the clipboard you want to get
rid of without adding it."
  (interactive)
  (setq code-imports-clipboard nil))

(defun code-imports--current-line ()
  "Return the current line in the buffer."
  (save-excursion
    (let ((begin (progn (beginning-of-line)
                        (point)))
          (end (progn (end-of-line)
                      (point))))
      (buffer-substring-no-properties begin end))))

(defun code-imports--blank-line-p (line)
  "True LINE is functionally blank."
  (string-match "^\s*$" line))

(defun code-imports--valid-import-block-line-p (line)
  "Predicate for detecting import lines.
Returns true if LINE matches something to be found in
an import block, such as an import statement or a blank line"
  (or (string-match code-imports-import-regex line)
      (code-imports--blank-line-p line)))

(defun code-imports--guess-mode-for-filename (fn)
  "Guess the mode for the given file name.
Argument FN is the filename."
  (assoc-default fn auto-mode-alist 'string-match))

(defun code-imports--is-cc-mode (mode)
  "Determines whether the given MODE symbol is a valid `cc-mode'."
  (member mode '(c-mode cc-mode c++-mode objc-mode java-mode
                        idl-mode pike-mode awk-mode)))

(defun code-imports--add-import-to-clipboard (file mode)
  "Put import for FILE in the clipboard for MODE."
  (unless (assoc-default mode code-imports-clipboard)
    (add-to-list 'code-imports-clipboard (list mode)))
  (setf (cdr (assoc mode code-imports-clipboard))
        (cons file (assoc-default mode code-imports-clipboard))))

(defun code-imports-line-to-relative-file (import-line)
  "Extrace the relative file name from an import line.
Currently this does not work for Java.  If the relative
file cannot be deduced, we return NIL."
  (when (string-match "\"\\(.*\\)\"" import-line)
    (match-string 1 import-line)))

(defun code-imports--guess-import-root (filename import-files)
  "Return the root for FILENAME, given IMPORT-FILES.
If the import root directory cannot be guessed, return nil.
This will not work for Java."
  (dolist (import import-files)
    (when import
      ;; Check to see if the import has the same root as FILENAME
      (let ((current-dir (file-name-directory filename)))
        (while (not (equal current-dir "/"))
          (if (file-exists-p (concat current-dir
                                     (if (string-match "/\\'" current-dir) "" "/")
                                     import))
              (return (if (string-match "/\\'" current-dir)
                          current-dir
                        (concat current-dir "/"))))
          (setq current-dir (file-truename (concat current-dir "/.."))))))))

(defun code-imports--make-relative (filename)
  "Make a FILENAME into a project-relative path.
This uses `code-imports-project-directory' as a prefix to look
for.  If there is no prefix match, we return FILENAME unchanged.
Advising this function is a reasonable alternative to using
`code-imports-project-directory', although you still need to set
that variable."
  (replace-regexp-in-string (concat (expand-file-name
                                       code-imports-project-directory) "/?")
                              "" filename))

(defun code-imports-grab-import ()
  "Grab the current file as an import target.
Use this in conjunction with `code-imports-add-grabbed-import'."
  (interactive)
  (unless (code-imports--is-cc-mode major-mode)
    (error "Must be run from a C++ or Java mode buffer."))
  (code-imports--add-import-to-clipboard
   (cond ((eq major-mode 'java-mode)
          (code-imports--extract-relative-java-file
           (buffer-string) buffer-file-name))
         (t
          (unless code-imports-project-directory
            (error "code-imports-project-directory must be defined."))
            (code-imports--make-relative buffer-file-name))) major-mode))

(defun code-imports--extract-relative-java-file (buffer-string file-name)
  (mapconcat 'identity
             (append
              (when (string-match "^package \\(.*\\);$" buffer-string)
                (list (replace-regexp-in-string "\\." "/"
                       (match-string 1 buffer-string))))
              (list (file-name-nondirectory file-name))) "/"))

(defun code-imports--is-dot-h-file (filename)
  "Return whether FILENAME is a c++ .h file."
  (string-match "\.h$" (buffer-file-name)))

(defun code-imports-add-grabbed-imports ()
  "Add the grabbed imports to the current file's imports.
Afterwards, organize the imports."
  (interactive)
  (unless (code-imports--is-cc-mode major-mode)
    (error "Must be run from a C++ or Java mode buffer"))
  (unless (or (eq major-mode 'java-mode) code-imports-project-directory)
    (error "Need to define `code-imports-project-directory'."))
  (code-imports--detect-unorganizable)
  (let ((clipboard (assoc-default major-mode code-imports-clipboard)))
    (unless clipboard
      (error "Import clipboard has no data for mode %s" major-mode))
    (code-imports--add-imports clipboard)
    (setf code-imports-clipboard nil)))

(defun code-imports-fix-java-import (import)
  "Fix the import IMPORT to a canonical format.
For example, a path to a java file should be formatted to a
com.google.package.ClassName"
  (replace-regexp-in-string
   "/" "."
   (replace-regexp-in-string "\\.java$\\|^.*java/+" "" import)))

(defun code-imports--cut-imports ()
  "Remove the imports from a buffer.
This returns a cons of the cut-point and a list of import
destinations."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward code-imports-import-regex nil t)
        (progn
          (beginning-of-line)
          (let ((cut-point (point)))
            (while (and (code-imports--valid-import-block-line-p
                         (code-imports--current-line))
                        (not (eq (point) (buffer-end 1))))
              (forward-line))
            ;; we've now passed onto a non-valid line.  Back up..
            (forward-line -1)
            ;; now back up past any blank lines
            (while (equal "" (code-imports--current-line))
              (forward-line -1))
            ;; We're now at the last non-blank line, let's
            ;; set the position at the beginning of the next line.
            (forward-line)
            (beginning-of-line)
            (let ((retval (cons cut-point
                                (split-string
                                         (buffer-substring-no-properties
                                          cut-point (point))
                                         "\n" t))))
              (delete-region cut-point (point))
              retval)))
      ;; If there are no imports, let's find the first appropriate
      ;; blank line after a package definition and insert imports
      ;; after that.
      (cond ((eq major-mode 'java-mode)
             (re-search-forward "^package " nil t)
             (forward-line))
            ((code-imports--is-dot-h-file buffer-file-name)
             (re-search-forward "^#define" nil t)
             (forward-line)))
      ;; If the next line is blank, let's consider that a blank line
      ;; that will come before the imports.
      (when (code-imports--blank-line-p (code-imports--current-line))
        (forward-line))
      (cons (point) nil))))

(defun code-imports--paste-imports (cut-point imports)
  "Paste raw IMPORTS (not lines) at CUT-POINT.
The imports are transformed back into lines before pasting."
  (save-excursion
    (goto-char cut-point)
    (insert (mapconcat (lambda (import) (unless (eq import 'blank ) import))
                       imports "\n"))
    (insert "\n")))

(defun code-imports--file-to-line (import)
  (cond ((string-match "java$" import)
         (format "import %s;"
                (replace-regexp-in-string
                 ".java" ""
                 (replace-regexp-in-string "/" "." import))))
        (t (format "#include \"%s\"" import))))

(defun code-imports--add-imports (new-import-files)
  "Add a list of NEW-IMPORT-FILES to the current buffer."
  (destructuring-bind (cut-point . existing-imports)
        (code-imports--cut-imports)
      (code-imports--paste-imports
       cut-point
       (code-imports--sort-imports
        (append (remove-if 'code-imports-unused-import-p existing-imports)
                (mapcar 'code-imports--file-to-line new-import-files))
        (cond ((eq major-mode 'java-mode)
               code-imports-java-ordering)
              (t
               code-imports-c++-ordering))
        (unless (eq major-mode 'java-mode)
          (let ((code-imports-project-directory
                 (or code-imports-project-directory
                     (code-imports--guess-import-root
                      (buffer-file-name)
                      (mapcar 'code-imports-line-to-relative-file
                              existing-imports)))))
            (unless code-imports-project-directory
              (error "code-imports-project-directory not defined or guessable"))
            (code-imports--make-relative (buffer-file-name))))))))

(defun code-imports--import-in-group-p (import-line group &optional self-file)
  "Returns t if IMPORT-LINE is in GROUP.
GROUP is one of the elements of the ordering such as
`code-imports-c++-ordering'. SELF-FILE is the .h file
corresponding to the file being modified (or nil if we're not in
a c mode)."
  (cond ((eq group 'self)
           ;; right now self can only refer to c/c++ mode.
           (string-match (regexp-quote self-file) import-line))
        ;; Ignore the t group for these kinds of matches, otherwise
        ;; it won't match just things not matched by other groups.
        ;; t-matching will happen elsewhere.
        ((eq group t)
         nil)
        (t (string-match group import-line))))

(defun code-imports--sort-imports (imports import-order &optional self-file)
  "Sort IMPORTS according to the given IMPORT-ORDER.
SELF-FILE is the .h file for the current .cc file (if we're in
cc-mode).  Otherwise SELF-FILE is nil."
  (let ((group-alist (mapcar 'list (if (member 't import-order)
                                       import-order
                                     (append import-order '(t)))))
        (used))
    (dolist (import imports)
      (dolist (group-cons group-alist)
          (when (and (not (eq (car group-cons) t))
                     (code-imports--import-in-group-p import
                                                      (car group-cons)
                                                      self-file))
            (unless (member import used) ;; no dups
              (setf (cdr group-cons) (cons import (cdr group-cons))))
            (add-to-list 'used import)
            (return))))
    ;; cdr to remove the prepending 'blank
    (cdr (append (mapcan (lambda (group-cons)
                           (let ((result
                                  (if (eq t (car group-cons))
                                      (remove-if
                                       (lambda (import)
                                         (member import used)) imports)
                                    (when (cdr group-cons)
                                      (cdr group-cons)))))
                             (when result
                               (cons 'blank
                                     (sort result
                                           'code-imports--sort-predicate)))))
                     group-alist)))))

(defun code-imports-organize-imports ()
  "Organize c++ or Java imports"
  (interactive)
  (unless (code-imports--is-cc-mode major-mode)
    (error "Must be run from a C++ or Java mode buffer"))
  (code-imports--detect-unorganizable)
  (code-imports--add-imports '()))

(defun code-imports--sort-predicate (import-a import-b)
  "Returns t if IMPORT-A should be before IMPORT-B.
We have to strip out semicolons, which can interfere with proper
sorting for Java inner-classes."
  (flet ((canonicalize (import)
                       (replace-regexp-in-string ";$" "" import)))
    (string< (canonicalize import-a) (canonicalize import-b))))

(defun code-imports-unused-import-p (import-line)
  "Return t if IMPORT-LINE is an unused Java import."
  (when (eq major-mode 'java-mode)
    (let ((last-word (car (last (butlast (split-string
                                          (replace-regexp-in-string
                                           "import " ""
                                           import-line) "[\\.;]"))))))
      (if (string= last-word "*")
         nil
        (save-excursion
          (let ((case-fold-search nil))
            (goto-char (point-min))
            (re-search-forward "[@{]\\|class\\s " nil t)
            (null
             (re-search-forward (concat "\\<" last-word "\\>") nil t))))))))

(defun code-imports--detect-unorganizable ()
  "Detects whether the imports are unorganizable."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat code-imports-import-regex
                   ".*\n\\(//\\|/\\*\\|#ifdef\\|#else\\|#endif\\).*\n"
                   "\\(import\\|#include\\)") nil t)
      (error (concat "Cannot manipulate imports when comments "
                     "or precompiler statements are interspersed")))))

(provide 'code-imports)

;;; code-imports.el ends here
