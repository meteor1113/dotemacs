;;; doc-mode.el --- convenient editing of in-code documentation
;;
;; Copyright (C) 2007, 2009 Nikolaj Schumacher
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.2
;; Keywords: convenience tools
;; URL: http://nschum.de/src/emacs/doc-mode/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This mode requires the Semantic package to be installed and running:
;; http://cedet.sourceforge.net/
;;
;; doc-mode allows easy creation and editing of JavaDoc or Doxygen comment
;; blocks in your code.  It also greatly improves readability of code by folding
;; the blocks, so they don't take up precious screen lines.
;;
;; Add the following to your .emacs file:
;; (require 'doc-mode)
;; (add-hook 'c-mode-common-hook 'doc-mode)
;;
;; The command `doc-mode-fix-tag-doc' or "C-cdd" adds or replaces the
;; documentation for the function, variable, or class at point.
;; `doc-mode-remove-tag-doc' or "C-cdr" removes it.
;;
;; You can fold the comments by using `doc-mode-toggle-tag-doc-folding' or
;; `doc-mode-fold-all'.
;;
;;; Change Log:
;;
;; 2009-03-22 (0.2)
;;    Added `doc-mode-keywords-from-tag-func' as customizable option.
;;    Improved parameter list change recognition.
;;    `doc-mode-jump-to-template' now enables jumping to the latest comment.
;;    `doc-mode-first-template' now jumps to the first template in this buffer.
;;
;; 2007-09-09 (0.1.1)
;;    Fixed return value detection.
;;    Actual keyword highlighting.
;;
;; 2007-09-07 (0.1)
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'semantic)
(require 'cc-mode)
(require 'newcomment) ;comment-fill-column

(dolist (err `("^No tag found$" "^Semantic can't parse buffer$"
               "^No template found$" "^doc-mode not enabled$"))
  (add-to-list 'debug-ignored-errors err))

;; semantic-after-auto-parse-hooks

(defgroup doc-mode nil
  "Minor mode for editing in-code documentation."
  :group 'convenience
  :group 'tools)

(defcustom doc-mode-auto-check-p t
  "*Should the buffer documentation be checked after a Semantic reparse."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom doc-mode-jump-to-template t
  "*Should the point be moved inside the template after inserting a doc."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom doc-mode-template-start "/**"
  "*The string to insert at the beginning of a comment."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-end " */"
  "*The string to insert at the end of a comment."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-continue " * "
  "*The string to insert at the beginning of each line in a comment."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-single-line-start "/** "
  "*The string to insert at the beginning of a single-line comment.
For using single-line comments, see `doc-mode-allow-single-line-comments'"
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-single-line-end " */"
  "*The string to insert at the end of a single-line comment.
For using single-line comments, see `doc-mode-allow-single-line-comments'"
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-keyword-char "@"
  "*The character used to begin keywords."
  :group 'doc-mode
  :type '(choice (const :tag "@" "@")
                 (const :tag "\\" "\\")
                 (string :tag "Other")))

(defcustom doc-mode-template-empty-line-after-summary nil
  "*Whether to put an empty line after the first one in the comment."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom doc-mode-template-empty-line-before-keywords nil
  "*Whether to put an empty line before the keyword list in a comment."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom doc-mode-template-keywords
  '("deprecated" "param" "return" "author" "exception" "throws" "version"
    "since" "see" "sa" "todo")
  "*Keywords that should be listed in this order.
All other keywords will be considered regular text."
  :group 'doc-mode
  :type '(repeat string))

(defcustom doc-mode-allow-single-line-comments t
  "*Whether to allow a more space-saving format for very short comments.
When this is enabled, `doc-mode-template-single-line-start' and
`doc-mode-template-single-line-end' will be used to format single-line
comments instead of `doc-mode-template-start', `doc-mode-template-end' and
`doc-mode-template-continue'."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom doc-mode-fold-single-line-comments nil
  "*Whether to bother folding comments that are already a single line."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom doc-mode-align-keyword-arguments t
  "*Whether to align the arguments to a keyword continued in the next line.
This may also be a number, describing how far to indent the argument list."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (integer :tag "Indent" nil)
                 (const :tag "On" t)))

(defcustom doc-mode-fill-column nil
  "*The column at which to break text when formatting it.
If this is nil, `comment-fill-column' is used."
  :group 'doc-mode
  :type '(choice (const :tag "Default" nil)
                 (integer :tag "Fill Column")))

(defcustom doc-mode-keywords-from-tag-func 'doc-mode-keywords-from-tag
  "*Function used to generate keywords for a tag.
This must be a function that takes two arguments.  The first argument is the
Semantic tag for which to generate keywords, the second is a list of existing
keywords taken from the current doc comment.  It should return the new list of
keywords.  Each element in a keyword list can be either a string or a list with
a keyword, optional argument and optional description.  Additional entries with
undetermined content should be created with `doc-mode-new-keyword'."
  :group 'doc-mode
  :type 'function)

;;; keywords ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst doc-mode-font-lock-keywords
  (eval-when-compile
    `((,(concat "[\\@]"
         (regexp-opt
          '("addindex" "addtogroup" "anchor" "arg" "author" "brief" "callgraph"
            "callergraph" "category" "code" "cond" "copydoc" "date" "defgroup"
            "deprecated" "details" "dir" "dontinclude" "dot" "dotfile" "e"
            "else" "elseif" "em" "endcode" "endcond" "enddot" "endhtmlonly"
            "endif" "endlatexonly" "endlink" "endmanonly" "endmsc" "endverbatim"
            "endxmlonly" "example" "f$" "f[" "f]" "file" "fn" "hideinitializer"
            "htmlinclude" "htmlonly" "if" "ifnot" "image" "include"
            "includelineno" "ingroup" "internal" "invariant" "latexonly" "li"
            "line" "link" "mainpage" "manonly" "msc" "name" "nosubgrouping"
            "note" "overload" "package" "page" "par" "paragraph"  "post" "pre"
            "private" "privatesection" "property" "protected" "protectedsection"
            "public" "publicsection" "ref" "remarks" "return" "retval" "sa"
            "section" "see" "serial" "serialData" "serialField"
            "showinitializer" "since" "skip" "skipline" "subpage" "subsection"
            "subsubsection" "test" "typedef" "until" "defvar" "verbatim"
            "verbinclude" "version" "weakgroup" "xmlonly" "xrefitem" "$" "@"
            "\\" "&" "~" "<" ">" "#" "%") t)
         "\\>")
       (0 font-lock-keyword-face prepend))
      ;; don't highlight \n, it's too common in code
      ("@n" (0 font-lock-keyword-face prepend))
      (,(concat "\\([@\\]"
         (regexp-opt '("class" "struct" "union" "exception" "enum" "throw"
                       "throws") t)
         "\\)\\>\\(?:[ \t]+\\(\\sw+\\)\\)?")
       (1 font-lock-keyword-face prepend)
       (3 font-lock-type-face prepend))
      (,(concat "\\([@\\]"
         (regexp-opt '("param" "param[in]" "param[out]" "param[in+out]" "a"
                       "namespace" "relates" "relatesalso" "def") t)
         "\\)\\>\\(?:[ \t]+\\(\\sw+\\)\\)?")
       (1 font-lock-keyword-face prepend)
       (3 font-lock-variable-name-face prepend))
      (,(concat "\\([@\\]retval\\)\\>\\(?:[ \t]+\\(\\sw+\\)\\)?")
       (1 font-lock-keyword-face prepend)
       (2 font-lock-function-name-face prepend))
      (,(concat "[@\\]" (regexp-opt '("attention" "warning" "todo" "bug") t)
                "\\>")
       (0 font-lock-warning-face prepend))
      (,(concat "{@"
         (regexp-opt '("docRoot" "inheritDoc" "link" "linkplain" "value") t)
         "}")
       (0 font-lock-keyword-face prepend))
      ("\\([@\\]b\\)[ \t\n]+\\([^ \t\n]+\\)"
       (1 font-lock-keyword-face prepend)
       (2 'bold prepend))
      ("\\([@\\]em?\\)[ \t\n]+\\([^ \t\n]+\\)"
       (1 font-lock-keyword-face prepend)
       (2 'italic prepend))
      ("\\([@\\][cp]\\)[ \t\n]+\\([^ \t\n]+\\)"
       (1 font-lock-keyword-face prepend)
       (2 'underline prepend)))))

;;; templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar doc-mode-templates nil)
(make-variable-buffer-local 'doc-mode-templates)

(defun doc-mode-add-template (beg end)
  (let ((overlay (make-overlay beg (point))))
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'face 'highlight)
    (overlay-put overlay 'insert-in-front-hooks '(doc-mode-replace-overlay))
    (overlay-put overlay 'modification-hooks '(doc-mode-delete-overlay))
    (push overlay doc-mode-templates)))

(defvar doc-mode-temp nil)

(defun doc-mode-delete-overlay (ov after-p beg end &optional r)
  (unless after-p
    (mapc 'doc-mode-unfold-by-overlay
          (overlays-in (1- (overlay-start ov)) (1+ (overlay-end ov))))
    (delete-overlay ov)
    (setq doc-mode-templates (delq ov doc-mode-templates))))

(defun doc-mode-replace-overlay (ov after-p beg end &optional r)
  (unless after-p
    (let ((inhibit-modification-hooks nil))
      (delete-region (overlay-start ov) (overlay-end ov)))))

;;;###autoload
(defun doc-mode-next-template (&optional pos limit)
  "Jump to the next unfinished documentation template in this buffer."
  (interactive)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-max)))
  (let ((min-start limit)
        start)
    (dolist (ov doc-mode-templates)
      (setq start (overlay-start ov))
      (and (> start pos)
           (< start min-start)
           (setq min-start start)))
    (when (= min-start limit)
      (error "End of buffer"))
    (push-mark)
    (goto-char min-start)))

;;;###autoload
(defun doc-mode-previous-template (&optional pos limit)
  "Jump to the previous unfinished documentation template in this buffer."
  (interactive)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-min)))
  (let ((max-start limit)
        start)
    (dolist (ov doc-mode-templates)
      (setq start (overlay-start ov))
      (and (< start pos)
           (> start max-start)
           (setq max-start start)))
    (when (= max-start limit)
      (error "Beginning of buffer"))
    (push-mark)
    (goto-char max-start)))

;;;###autoload
(defun doc-mode-first-template ()
  "Jump to the first unfinished documentation template in this buffer."
  (interactive)
  (condition-case err
      (doc-mode-next-template (point-min))
    (error (error "No template found"))))

;;; mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar doc-mode-lighter " doc")

(defvar doc-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'doc-mode-fix-tag-doc)
    (define-key map "c" 'doc-mode-check-tag-doc)
    (define-key map "t" 'doc-mode-toggle-tag-doc-folding)
    (define-key map "f" 'doc-mode-fold-tag-doc)
    (define-key map "u" 'doc-mode-unfold-tag-doc)
    (define-key map "r" 'doc-mode-remove-tag-doc)
    (define-key map "i" 'doc-mode-add-tag-doc)
    (define-key map "e" 'doc-mode-next-faulty-doc)
    (define-key map "n" 'doc-mode-next-template)
    (define-key map "\C-c" 'doc-mode-check-buffer)
    (define-key map "\C-f" 'doc-mode-fold-all)
    (define-key map "\C-u" 'doc-mode-unfold-all)
    map))

(defvar doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-d" doc-mode-prefix-map)
    map)
  "Keymap used for `doc-mode'.")

;;;###autoload
(define-minor-mode doc-mode
  "Minor mode for editing in-code documentation."
  nil doc-mode-lighter doc-mode-map
  (if doc-mode
      (progn
        (font-lock-add-keywords nil doc-mode-font-lock-keywords)
        (when doc-mode-auto-check-p
          (add-hook 'semantic-after-auto-parse-hooks 'doc-mode-check-buffer
                    nil t)
          (add-hook 'semantic-after-idle-scheduler-reparse-hooks
                    'doc-mode-check-buffer nil t)))
    (dolist (ov doc-mode-templates)
      (delete-overlay ov))
    (kill-local-variable 'doc-mode-templates)
    (doc-mode-unfold-all)
    (font-lock-remove-keywords nil doc-mode-font-lock-keywords)
    (remove-hook 'semantic-after-auto-parse-hooks 'doc-mode-check-buffer t)
    (remove-hook 'semantic-after-idle-scheduler-reparse-hooks
                 'doc-mode-check-buffer t))

  (when font-lock-mode
    (font-lock-fontify-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-current-tag ()
  (when (semantic-parse-tree-unparseable-p)
    (error "Semantic can't parse buffer"))
  (when (or (semantic-parse-tree-needs-rebuild-p)
            (semantic-parse-tree-needs-update-p))
    (condition-case nil
        (semantic-fetch-tags)
      (error (error "Semantic can't parse buffer"))))
  (save-excursion
    (or (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (progn (beginning-of-line) (skip-chars-forward " \t\n") nil)
        (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (if (not (looking-at "/\\*\\*"))
            (semantic-current-tag-of-class 'type)
          (progn (search-forward "*/" nil t)
                 (skip-chars-forward " \t\n")
                 nil))
        (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (semantic-current-tag-of-class 'type))))

(defun doc-mode-current-tag-or-bust ()
  (or (doc-mode-current-tag) (error "No tag found")))

;;; insertion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-line-indent (keyword)
  "Determine left side offset when indenting LINE."
  (if (numberp doc-mode-align-keyword-arguments)
      doc-mode-align-keyword-arguments
    (+ 1 (length (car keyword))
       (if (equal (car keyword) "param")
           (1+ (length (cdr keyword)))
         0))))

(defun doc-mode-insert (text)
  "Insert TEXT if a string, or a template if 'prompt."
  (if (stringp text)
      (insert text)
    (let ((beg (point)))
      (insert (cadr text))
      (when doc-mode
        (doc-mode-add-template beg (point))))))

(defun doc-mode-insert-markup (markup &optional argument description)
  (insert doc-mode-template-keyword-char markup)
  (when argument
    (insert " ")
    (doc-mode-insert argument))
  (when description
    (insert " ")
    (doc-mode-insert description)))

(defun doc-mode-insert-line (line indent)
  (indent-to-column indent)
  (let ((beg (point)))
    (insert doc-mode-template-continue)
    (if (and (consp line) (not (eq (car line) 'prompt)))
        (apply 'doc-mode-insert-markup line)
      (doc-mode-insert line))
    (delete-char (- (skip-chars-backward " \t")))
    (when (> (point) (+ beg 2))
      (save-excursion (fill-region beg (point) 'left t)))
    (insert "\n")))

(defun doc-mode-insert-keyword (keyword indent)
  (indent-to-column indent)
  (let ((fill-column (or doc-mode-fill-column comment-fill-column fill-column))
        (fill-prefix (when doc-mode-align-keyword-arguments
                       (concat (buffer-substring (point-at-bol) (point))
                               doc-mode-template-continue
                               (make-string (doc-mode-line-indent keyword)
                                            ? )))))
    (doc-mode-insert-line keyword indent)))

(defun doc-mode-insert-doc (keywords &optional pos)
  "Insert a documentation at POS.
LINES is a list of keywords."
  (save-excursion
    (if pos
        (goto-char pos)
      (setq pos (point)))
    (let ((indent (current-column)))

      (if (and (not (cdr keywords)) doc-mode-allow-single-line-comments)
          (progn (insert doc-mode-template-single-line-start)
                 (doc-mode-insert (car keywords))
                 (insert doc-mode-template-single-line-end "\n"))
        (insert doc-mode-template-start "\n")

        ;; first line
        (when (or (stringp (car keywords))
                  (eq 'prompt (caar keywords)))
          (doc-mode-insert-line (pop keywords) indent))

        (when (and doc-mode-template-empty-line-after-summary
                   (or (null doc-mode-template-empty-line-before-keywords)
                       (stringp (cadr keywords))))
          (doc-mode-insert-line "" indent))

        ;; paragraphs
        (if (cdr keywords)
            (while (stringp (car keywords))
              (doc-mode-insert-line (pop keywords) indent)
              (when (stringp (car keywords))
                (doc-mode-insert-line "" indent)))
          (while (stringp (car keywords))
            (doc-mode-insert-line (pop keywords) indent)))

        (when doc-mode-template-empty-line-before-keywords
          (doc-mode-insert-line "" indent))

        ;; keywords
        (while keywords
          (doc-mode-insert-keyword (pop keywords) indent))
        (indent-to-column indent)
        (insert doc-mode-template-end "\n"))

      ;; re-indent original line
      (if (< (current-column) indent)
          (indent-to-column indent)
        (move-to-column indent t))))

    (and doc-mode-jump-to-template doc-mode-templates
         (ignore-errors (doc-mode-next-template pos (point)))))

(defun doc-mode-remove-doc (point)
  "Remove the documentation before POINT."
  (let* ((bounds (doc-mode-find-doc-bounds point))
         (beg (plist-get bounds :beg))
         (end (plist-get bounds :end)))
    (when bounds
      (save-excursion
        (goto-char beg)
        (incf beg (skip-chars-backward " \t"))
        (goto-char end)
        (incf end (skip-chars-forward " \t"))
        (when (eolp) (incf end))
        (delete-region beg end)))))

;;;###autoload
(defun doc-mode-remove-tag-doc (tag)
  "Remove the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
  (doc-mode-remove-doc (semantic-tag-start tag)))

;;; registering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-find-doc-bounds (pos)
  "Find the documentation right before POS.
If there is anything but whitespace between the documentation and POS, nil is
returned.  Otherwise a cons of the doc's beginning and end is given."
  (let (end)
    (save-excursion
      (goto-char pos)
      (when (re-search-backward "[ \t]*\n[ \t]*\\=" nil t)
        (setq end (point))
        (cond
         ;; /// Doxygen comment */
         ((looking-back "[ \t]*//[/!]\\(.*\\)$")
          (forward-line -1)
          (while (looking-at "[ \t]*//[/!]\\(.*\\)$")
            (forward-line -1))
          (forward-line 1)
          (skip-chars-forward " \t")
          `(:beg ,(point) :end ,end :column ,(current-indentation)))
         ;; /** JavaDoc comment */
         ((looking-back "\\*/")
          (goto-char (match-beginning 0))
          ;; search for /*, not allowing any */ in between
          (when (and (re-search-backward "\\(/\\*\\)\\|\\*/" nil t)
                     (match-beginning 1)
                     (memq (char-after (1+ (match-beginning 1))) '(?! ?*)))
            `(:beg ,(point) :end ,end :column ,(current-column)))))))))

;;; formating ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-new-keyword (keyword &optional argument)
  (if (equal keyword "param")
      (list keyword argument '(prompt "<doc>"))
    (list keyword '(prompt "<doc>"))))

(defun doc-mode-has-return-value-p (tag)
  "Test if TAG has a return value to format."
  (and (eq (semantic-tag-class tag) 'function)
       (not (equal (semantic-tag-type tag) "void"))
       (not (semantic-tag-get-attribute tag :constructor-flag))
       (or (not (equal (semantic-tag-type tag) "int"))
           ;; semantic bug, constructors sometimes appear to have int type
           (save-excursion (goto-char (semantic-tag-start tag))
                           (and (re-search-forward "\\(\\<int\\>\\)\\|{\\|;"
                                                   (semantic-tag-end tag) t)
                                (match-beginning 1))))))

;;; extracting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-extract-summary (beg end)
  (let ((bounds (doc-mode-find-summary beg end)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun doc-mode-find-summary (beg end)
  (save-excursion
    (goto-char beg)
    (if (or (re-search-forward "^[@\\]brief \\([^\t ][^\n]*\n\\)" end t)
            (re-search-forward "\\<\\(.*\\)\\(\\*+/\\|\n\\)" end t))
        (cons (match-beginning 1) (match-end 1))
      (cons beg beg))))

(defconst doc-mode-begin-regexp
  (eval-when-compile (concat "[ \t\n]*"
                             "\\("
                             "/\\*\\(\\*+\\|!\\)"
                             "\\|"
                             "//[!/]"
                             "\\)[ \t]*")))

(defun doc-mode-clean-doc (beg end)
  "Remove the comment delimiters between BEG and END."
  (save-excursion
    (goto-char beg)
    (when (looking-at doc-mode-begin-regexp)
      (setq beg (match-end 0)))
    (goto-char end)
    (when (looking-back "[ \t\n\r]*\\*+/" nil t)
      (setq end (match-beginning 0)))
    (let ((lines (split-string (buffer-substring-no-properties beg end)
                               "[ \t]*\n[ \t]*\\(\\*/?\\|//[!/]\\)?[ \t]*")))
      (while (equal (car lines) "")
        (pop lines))
      (mapconcat 'identity lines "\n"))))

(defun doc-mode-extract-keywords (beg end)
  "Extract documentation keywords between BEG and END.
Returns a alist of keywords, where each element is the list (keyword
argument value) or (keyword argument)."
  (let* ((paragraphs (doc-mode-clean-doc beg end))
         (doc "")
         (pos 0)
         match results)

    (when (string-match
           "[ \t\n]*\\(\\(.\\|\n\\)*?\\)\\([@\\]\\<\\(.\\|\n\\)*\\'\\)"
           paragraphs)
      (setq doc (match-string-no-properties 3 paragraphs)
            paragraphs (match-string-no-properties 1 paragraphs)))

    ;; first line summary
    (when (string-match "\\`[ \t\n]*\\(.+\\.\\)\\([ \n]+\\|\\'\\)" paragraphs)
      (push (match-string 1 paragraphs) results)
      (setq pos (match-end 0)))

    ;; other paragraphs
    (dolist (paragraph (split-string (substring paragraphs pos)
                                     "[ \t]*\n\\(\n+[ \t]*\\|$\\)" t))
      (push (replace-regexp-in-string "[\n\r]" " " paragraph) results))

    ;; keywords
    (dolist (keyword (cdr (split-string doc "[@\\]\\<")))
      (setq match (split-string keyword))
      (push (if (equal (car match) "param")
                (list (car match) (cadr match)
                      (mapconcat 'identity (cddr match) " "))
              (list (car match) (mapconcat 'identity (cdr match) " ")))
            results))
    (nreverse results)))

(defun doc-mode-extract-keywords-for-tag (tag)
  (let ((bounds (doc-mode-find-doc-bounds (semantic-tag-start tag))))
    (when bounds (doc-mode-extract-keywords (plist-get bounds :beg)
                                            (plist-get bounds :end)))))

(defun doc-mode-find-keyword (keyword keywords)
  (when keywords
    (if (and (consp (car keywords)) (string= (car (car keywords)) keyword))
        (cons (car keywords) (doc-mode-find-keyword keyword (cdr keywords)))
      (doc-mode-find-keyword keyword (cdr keywords)))))

(defun doc-mode-filter-keyword (keyword keywords)
  (when keywords
    (if (and (consp (car keywords)) (string= (car (car keywords)) keyword))
        (doc-mode-filter-keyword keyword (cdr keywords))
      (cons (car keywords) (doc-mode-filter-keyword keyword (cdr keywords))))))

(defun doc-mode-find-eligible-tags ()
  (when buffer-file-name
    (unless (or (semantic-parse-tree-unparseable-p)
                (semantic-parse-tree-needs-rebuild-p)
                (semantic-parse-tree-needs-update-p))
      (ignore-errors
        (let (tags)
          (semantic-brute-find-tag-by-function
           (lambda (tag)
             (when (semantic-tag-start tag)
               (case (semantic-tag-class tag)
                 ((function variable) (push tag tags))
                 (type (setq tags
                             (nconc (semantic-tag-type-members tag)
                                    tags))))))
           (semanticdb-file-stream buffer-file-name))
          tags)))))

;;; checking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst doc-mode-position (element list)
  "Return the first position of ELEMENT in LIST.
Returns (length LIST) if no occurrence was found."
  (let ((pos 0))
    (while (and list (not (equal element (pop list))))
      (incf pos))
    pos))

(defun doc-mode-keyword< (a b tag)
  (if (equal (car a) "param")
      (let* ((args (mapcar 'semantic-tag-name
                          (semantic-tag-get-attribute tag :arguments)))
             (a-param (cadr a))
             (b-param (cadr b))
             (a-pos (doc-mode-position a-param args))
             (b-pos (doc-mode-position b-param args)))
        (if (= a-pos b-pos) 
             (string< a-param b-param)
          (< a-pos b-pos)))
    (string< (cadr a) (cadr b))))

(defun doc-mode-sort-keywords (keywords tag)
  (let ((lists (make-vector (1+ (length doc-mode-template-keywords)) nil))
        description)
    (dolist (k keywords)
      (if (or (stringp k) (and (eq (car k) 'prompt)))
          (push k description)
        (push k (elt lists (doc-mode-position (car k)
                                              doc-mode-template-keywords)))))
    (let ((i (length lists)) result)
      (while (> i 0)
        (setq result (nconc (sort (elt lists (decf i))
                                  (lambda (a b) (doc-mode-keyword< a b tag)))
                            result)))
      (nconc (nreverse description) result))))

(defun doc-mode-update-parameters (old new)
  "Cleanse and sort NEW parameters according to OLD parameter list."
  (let (params car-new)
    (while (setq car-new (pop new))
      (push (or (dolist (p old) ;; search for match in old
                  (when (equal (cadr p) car-new)
                    (setq old (delete p old))
                    (return p)))
                ;; this parameter wasn't there before
                (if (or (null old) (member (cadr (car old)) new))
                    ;; insertion, new
                    (doc-mode-new-keyword "param" car-new)
                  ;; the old parameter at this pos isn't there anymore, rename
                  (list* "param" car-new (cddr (pop old)))))
            params))
    (nreverse params)))

(defun doc-mode-keywords-from-tag (tag keywords)
  "Create keywords for a Semantic TAG, taking descriptions from old KEYWORDS"
  (let ((old-params (doc-mode-find-keyword "param" keywords))
        (new-params (mapcar 'semantic-tag-name
                            (semantic-tag-get-attribute tag :arguments))))
    ;; fix return value
    (if (doc-mode-has-return-value-p tag)
        ;; add
        (unless (doc-mode-find-keyword "return" keywords)
          (push (doc-mode-new-keyword "return") keywords))
      ;; remove
      (setq keywords (doc-mode-filter-keyword "return" keywords)))
    (unless (stringp (car keywords))
      (push `(prompt ,(format "Description for %s." (semantic-tag-name tag)))
            keywords))
    (doc-mode-sort-keywords (nconc (doc-mode-update-parameters old-params
                                                               new-params)
                                   (doc-mode-filter-keyword "param" keywords))
                            tag)))

;;;###autoload
(defun doc-mode-fix-tag-doc (tag)
  (interactive (list (doc-mode-current-tag-or-bust)))
  (let ((keywords (funcall doc-mode-keywords-from-tag-func
                           tag (doc-mode-extract-keywords-for-tag tag))))
    (doc-mode-remove-tag-doc tag)
    (doc-mode-insert-doc keywords (semantic-tag-start tag))
    ;; update lighter
    (doc-mode-check-buffer)))

;;;###autoload
(defalias 'doc-mode-add-tag-doc 'doc-mode-fix-tag-doc)

(defun doc-mode-format-message (type parameters)
  (when parameters
    (concat (case type
              ('missing "Missing")
              ('invalid "Invalid"))
            " parameter" (when (cdr parameters) "s") ": "
            (mapconcat 'identity parameters ", "))))

;;;###autoload
(defun doc-mode-check-tag-doc (tag &optional print-message-p)
  (interactive (list (doc-mode-current-tag-or-bust) t))
  (let* ((actual (doc-mode-extract-keywords-for-tag tag))
         (expected (mapcar 'semantic-tag-name
                           (semantic-tag-get-attribute tag :arguments))))
    (if actual
        (let ((no-doc-p (not (stringp (car actual))))
              ;; we only report parameters
              (actual (mapcar 'cadr (doc-mode-find-keyword "param"
                                                           actual)))
              invalid)
          (dolist (keyword actual)
            (if (member keyword expected)
                (setq expected (delete keyword expected))
              (push keyword invalid)))
          (when print-message-p
            (message "%s" (concat (and no-doc-p "Missing documentation")
                                  (and no-doc-p expected "\n")
                                  (doc-mode-format-message 'missing expected)
                                  (and (or no-doc-p expected) invalid "\n")
                                  (doc-mode-format-message 'invalid invalid))))
          (or no-doc-p expected invalid))
      (when print-message-p
        (message "Missing comment"))
      t)))

;;;###autoload
(defun doc-mode-check-buffer ()
  (interactive)
  (kill-local-variable 'doc-mode-lighter)
  (dolist (tag (doc-mode-find-eligible-tags))
    (when (doc-mode-check-tag-doc tag)
      (set (make-local-variable 'doc-mode-lighter) " doc!")
      (return t))))

(defun doc-mode-first-faulty-tag-doc ()
  (dolist (tag (sort (doc-mode-find-eligible-tags)
                     (lambda (a b) (< (semantic-tag-start a)
                                      (semantic-tag-start b)))))
    (when (doc-mode-check-tag-doc tag)
      (return tag))))

;;;###autoload
(defun doc-mode-next-faulty-doc ()
  "Jump to the next faulty documentation and print error."
  (interactive)
  (let ((tag (or (doc-mode-first-faulty-tag-doc)
                 (error "End of buffer"))))
    (push-mark)
    (goto-char (semantic-tag-start tag))
    ;; check again with message
    (doc-mode-check-tag-doc tag t)))

;;; folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar doc-mode-folds nil)
(make-variable-buffer-local 'doc-mode-folds)

(defun doc-mode-fold-doc (point)
  (let ((bounds (doc-mode-find-doc-bounds point)))
    (when bounds
      (let* ((beg (plist-get bounds :beg))
             (end (plist-get bounds :end))
             (summary-bounds (doc-mode-find-summary beg end))
             (before-overlay (make-overlay beg (car summary-bounds)))
             (after-overlay (make-overlay (cdr summary-bounds) end))
             (siblings (list before-overlay after-overlay)))
        (when (or doc-mode-fold-single-line-comments
                  (> (count-lines beg end) 1))
          (dolist (ov siblings)
            (overlay-put ov 'invisible t)
            (overlay-put ov 'isearch-open-invisible-temporary
                         'doc-mode-unfold-by-overlay-temporary)
            (overlay-put ov 'isearch-open-invisible 'doc-mode-unfold-by-overlay)
            (overlay-put ov 'doc-mode-fold siblings))
          (setq doc-mode-folds (nconc doc-mode-folds siblings)))))))

;;;###autoload
(defun doc-mode-fold-tag-doc (tag)
  "Fold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
  (unless doc-mode
    (error "doc-mode not enabled"))
  (doc-mode-fold-doc (semantic-tag-start tag)))

(defun doc-mode-unfold-by-overlay (overlay &rest foo)
  "Unfold OVERLAY and its siblings permanently"
  (dolist (ov (overlay-get overlay 'doc-mode-fold))
    ;; remove overlay
    (setq doc-mode-folds (delq ov doc-mode-folds))
    (delete-overlay ov)
    ;; don't let isearch do anything with it
    (setq isearch-opened-overlays (delq ov isearch-opened-overlays))))

(defun doc-mode-unfold-by-overlay-temporary (overlay invisible)
  "Unfold OVERLAY and its siblings temporarily."
  (dolist (ov (overlay-get overlay 'doc-mode-fold))
    (overlay-put ov 'invisible invisible)))

;;;###autoload
(defun doc-mode-unfold-doc (point)
  "Unfold the comment before POINT."
  (interactive "d")
  (unless doc-mode
    (error "doc-mode not enabled"))
  (let ((bounds (doc-mode-find-doc-bounds point)))
    (when bounds
      (let* ((beg (plist-get bounds :beg))
             (end (plist-get bounds :end))
             (overlays (overlays-in beg end))
             anything-done)
        (dolist (ov overlays)
          (when (overlay-get ov 'doc-mode-fold)
            (setq anything-done t)
            (delete-overlay ov)
            (setq doc-mode-folds (delq ov doc-mode-folds))))
        ;; return non-nil, if anything unfolded
        ;; this is used to toggle
        anything-done))))

;;;###autoload
(defun doc-mode-unfold-tag-doc (tag)
  "Unfold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
  (unless doc-mode
    (error "doc-mode not enabled"))
  (doc-mode-unfold-doc (semantic-tag-start tag)))

;;;###autoload
(defun doc-mode-fold-all (&optional arg)
  (interactive "P")
  (unless doc-mode
    (error "doc-mode not enabled"))
  (if arg
      (doc-mode-unfold-all)
    (dolist (tag (doc-mode-find-eligible-tags))
      (doc-mode-fold-tag-doc tag))))

;;;###autoload
(defun doc-mode-unfold-all ()
  (interactive)
  (dolist (ov doc-mode-folds)
    (delete-overlay ov))
  (kill-local-variable 'doc-mode-folds))

;;; toggle

;;;###autoload
(defun doc-mode-toggle-tag-doc-folding (tag)
  "Toggle folding of TAG's documentation.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
  (or (doc-mode-unfold-tag-doc tag)
      (doc-mode-fold-tag-doc tag)))

(provide 'doc-mode)

;;; doc-mode.el ends here
