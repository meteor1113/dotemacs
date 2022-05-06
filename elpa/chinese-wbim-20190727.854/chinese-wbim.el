;;; chinese-wbim.el --- Enable Wubi(五笔) Input Method in Emacs.

;; Copyright (C) 2015-2016, Guanghui Qu

;; Author: Guanghui Qu<guanghui8827@gmail.com>
;; URL: https://github.com/andyque/chinese-wbim
;; Version: 0.1
;; Keywords: Wubi Input Method.
;;
;; This file is not part of GNU Emacs.

;;; Credits:

;; - Original Author: wenbinye@163.com

;;; License:

;; This file is part of chinese-wbim
;;
;; chinese-wbim is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; chinese-wbim is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; Put this file into your load-path and the following into your ~/.emacs:

;; (register-input-method
;;  "chinese-wbim" "euc-cn" 'chinese-wbim-use-package
;;  "五笔" "汉字五笔输入法" "wb.txt")

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'help-mode)

(defvar chinese-wbim-version "2.4")

;;;_. emacs21 compatible
(when (not (fboundp 'number-sequence))
  (defun number-sequence (from &optional to inc)
    (if (and to (<= from to))
        (cons from
              (number-sequence (+ from (or inc 1)) to inc)))))

(when (not (fboundp 'delete-dups))
  (defun delete-dups (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
    (let ((tail list))
      (while tail
        (setcdr tail (delete (car tail) (cdr tail)))
        (setq tail (cdr tail))))
    list))

;;;_. customize varable
(defgroup chinese-wbim nil
  "chinese-wbim: emacs input method"
  :group 'lchinese-wbim)
(defvar chinese-wbim-page-length 7 "每页显示的词条数目")

(defface chinese-wbim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'chinese-wbim)

;;;_. variable declare
(defvar chinese-wbim-package-list nil "所有正在使用的输入法")
(defvar chinese-wbim-current-package (make-vector 5 nil)
  "当前使用的输入法，一个 vector，有五个部分: package-name,
buffer-list,history, keymap, active-function.

buffer-list 中的每个 buffer 是这样的一个 Association List：
----------------------------------------
buffer         对应的 buffer
param          Parameter 部分的参数
file           对应的文件名
")
(defvar chinese-wbim-first-char (number-sequence ?a ?z) "Table 中所有首字母列表")
(defvar chinese-wbim-total-char (number-sequence ?a ?z) "所有可能的字符")
(defvar chinese-wbim-do-completion t "是否读入可能的补全")

(defvar chinese-wbim-current-key "" "已经输入的代码")
(defvar chinese-wbim-current-str "" "当前选择的词条")
(defvar chinese-wbim-current-choices nil "所有可选的词条。

这个 list 的 CAR 是可选的词条，一般是一个字符串列表，但是也可以含有
list。但是这个 list 的第一个元素必须是将要插入的字符串。

CDR 部分是一个 Association list。通常含有这样的内容：
---------------------------
pos         上次选择的位置
completion  下一个可能的字母（如果 chinese-wbim-do-completion 为 t）
")
(defvar chinese-wbim-current-pos nil "当前选择的词条在 chinese-wbim-current-choices 中的位置")
(defvar chinese-wbim-guidance-str "" "显示可选词条的字符串")
(defvar chinese-wbim-translating nil "记录是否在转换状态")
(defvar chinese-wbim-overlay nil "显示当前选择词条的 overlay")
(defvar chinese-wbim-guidance-frame nil)
(defvar chinese-wbim-guidance-buf nil)

(defvar chinese-wbim-load-hook nil)
(defvar chinese-wbim-active-hook nil)

(defvar chinese-wbim-stop-function nil)
(defvar chinese-wbim-translate-function nil)
(defvar chinese-wbim-add-completion-function nil)
(defvar chinese-wbim-format-function 'chinese-wbim-format)
(defvar chinese-wbim-handle-function 'chinese-wbim-handle-string)

(defvar chinese-wbim-use-tooltip (not (or noninteractive
                                 emacs-basic-display
                                 (not (display-graphic-p))
                                 (not (fboundp 'x-show-tip)))))
(defvar chinese-wbim-tooltip-timeout 15)

(defvar chinese-wbim-buffer-name-format " *%s*"
  "buffer 的名字格式，%s 对应 package name")

(defvar chinese-wbim-mode-map
  (let ((map (make-sparse-keymap))
        (i ?\ ))
    (while (< i 127)
      (define-key map (char-to-string i) 'chinese-wbim-self-insert-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'chinese-wbim-self-insert-command)
      (setq i (1+ i)))
    (dolist (i (number-sequence ?1 ?9))
      (define-key map (char-to-string i) 'chinese-wbim-number-select))
    (define-key map " " 'chinese-wbim-select-current)
    (define-key map [backspace] 'chinese-wbim-delete-last-char)
    (define-key map [delete] 'chinese-wbim-delete-last-char)
    (define-key map "\177" 'chinese-wbim-delete-last-char)
    (define-key map "\C-n" 'chinese-wbim-next-page)
    (define-key map "\C-p" 'chinese-wbim-previous-page)
    (define-key map "\C-m" 'chinese-wbim-quit-no-clear)
    (define-key map "\C-c" 'chinese-wbim-quit-clear)
    (define-key map "\C-g" 'chinese-wbim-quit-clear)
    map)
  "Keymap")

(defvar chinese-wbim-local-variable-list
  '(chinese-wbim-current-package

    chinese-wbim-page-length
    chinese-wbim-first-char
    chinese-wbim-total-char
    chinese-wbim-do-completion

    chinese-wbim-current-key
    chinese-wbim-current-str
    chinese-wbim-current-choices
    chinese-wbim-current-pos
    chinese-wbim-guidance-str
    chinese-wbim-translating
    chinese-wbim-overlay
    chinese-wbim-guidance-frame
    chinese-wbim-guidance-buf

    chinese-wbim-load-hook
    chinese-wbim-active-hook

    chinese-wbim-translate-function
    chinese-wbim-format-function
    chinese-wbim-handle-function
    chinese-wbim-add-completion-function
    chinese-wbim-stop-function

    input-method-function
    inactivate-current-input-method-function
    describe-current-input-method-function)
  "A list of buffer local variable")

(dolist (var chinese-wbim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

;;;_ , package contents
(defsubst chinese-wbim-package-name ()
  (aref chinese-wbim-current-package 0))

(defsubst chinese-wbim-buffer-list ()
  (aref chinese-wbim-current-package 1))

(defsubst chinese-wbim-history ()
  "保存输入过的词的选择，另一方面加快搜索。另外在这里来处理标点。
这个散列中的每个元素都有这样的格式：
  ((list WORDS) other-properties)
OTHER-PROPERTIES 是一些其它的属性，比如，上次的位置，用来输入标点等。"
  (aref chinese-wbim-current-package 2))

(defsubst chinese-wbim-mode-map ()
  (aref chinese-wbim-current-package 3))

(defsubst chinese-wbim-options ()
  (aref chinese-wbim-current-package 4))

(defsubst chinese-wbim-active-function ()
  (aref chinese-wbim-current-package 5))

(defsubst chinese-wbim-set-package-name (name)
  (aset chinese-wbim-current-package 0 name))

(defsubst chinese-wbim-set-buffer-list (list)
  (aset chinese-wbim-current-package 1 list))

(defsubst chinese-wbim-set-history (history)
  (aset chinese-wbim-current-package 2 history))

(defsubst chinese-wbim-set-mode-map (map)
  (aset chinese-wbim-current-package 3 map))

(defsubst chinese-wbim-set-options (options)
  (aset chinese-wbim-current-package 4 options))

(defsubst chinese-wbim-set-active-function (func)
  (aset chinese-wbim-current-package 5 func))

(defun chinese-wbim-get-option (option)
  (cdr (assoc option (chinese-wbim-options))))
(defun chinese-wbim-set-option (option flag)
  (let ((options (chinese-wbim-options))
        opt)
    (if (setq opt (assoc option options))
        (setcdr opt flag)
      (push (cons option flag) options)
      (chinese-wbim-set-options options))))

;;;_. read file functions
(defun chinese-wbim-load-file (file)
  (let ((bufname (format chinese-wbim-buffer-name-format (chinese-wbim-package-name)))
        buflist buf param files)
    (save-excursion
      (setq buf (chinese-wbim-read-file file bufname t))
      (setq param (cdr (assoc "param" buf)))
      (setq buflist (append buflist (list buf)))
      (when (setq files (assoc "other-files" param))
        (setq files (split-string (cadr files) ";"))
        (dolist (f files)
          (if (file-exists-p (expand-file-name f))
              (setq f (expand-file-name f))
            (setq f (locate-file f load-path)))
          (setq buflist (append buflist (list (chinese-wbim-read-file f bufname))))))
      buflist)))

(defun chinese-wbim-read-file (file name &optional read-param)
  (let (param region)
    (save-excursion
      (set-buffer (generate-new-buffer name))
      (insert-file-contents file)
      (if read-param
          (setq param (chinese-wbim-read-parameters)))
      (setq region (chinese-wbim-section-region "Table"))
      (narrow-to-region (car region) (cdr region))
      `(("buffer" . ,(current-buffer))
        ("param" . ,param)
        ("file" . ,file)))))

(defun chinese-wbim-section-region (sec)
  "得到一个部分的起点和终点位置，忽略最后的空行"
  (let ((reg (concat "^\\[" sec "\\]\n")))
    (save-excursion
      (if (not (re-search-forward reg nil t))
          (if (re-search-backward reg nil t)
              (forward-line 1)
            (error "文件类型错误！没有 %s 部分！" sec)))
      (cons (point) (progn
                      (if (re-search-forward "^\\[\\sw+\\]\n" nil t)
                          (forward-line -1)
                        (goto-char (point-max)))
                      (re-search-backward "[^  \t\n]" nil t)
                      (1+ (point)))))))

(defun chinese-wbim-read-parameters ()
  "得到 [Parameter] 部分的参数，以 assoc list 的形式返回"
  (let* ((r (chinese-wbim-section-region "Parameter"))
         param pair)
    (goto-char (car r))
    (while (< (point) (cdr r))
      (when (setq pair (chinese-wbim-line-content "=" t))
        (add-to-list 'param pair))
      (forward-line 1))
    param))

;;;_. common functions

(defsubst chinese-wbim-delete-region ()
  "Delete the text in the current translation region of E+."
  (if (overlay-start chinese-wbim-overlay)
      (delete-region (overlay-start chinese-wbim-overlay)
                     (overlay-end chinese-wbim-overlay))))

;;; steal from emms-compat.el. Is this a good idea?
(when (not (fboundp 'emms-delete-if))
  (defun emms-delete-if (predicate seq)
    "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
    ;; remove from car
    (while (when (funcall predicate (car seq))
             (setq seq (cdr seq))))
    ;; remove from cdr
    (let ((ptr seq)
          (next (cdr seq)))
      (while next
        (when (funcall predicate (car next))
          (setcdr ptr (if (consp next)
                          (cdr next)
                        nil)))
        (setq ptr (cdr ptr))
        (setq next (cdr ptr))))
    seq))

(defun chinese-wbim-subseq (list from &optional to)
  (if (null to) (nthcdr from list)
    (butlast (nthcdr from list) (- (length list) to))))

(defun chinese-wbim-mod (x y)
  "like `mod', but when result is 0, return Y"
  (let ((base (mod x y)))
    (if (= base 0)
        y
      base)))

(defun chinese-wbim-string-emptyp (str)
  (not (string< "" str)))

(defun chinese-wbim-line-content (&optional seperaters omit-nulls)
  "用 SEPERATERS 分解当前行，所有参数传递给 split-string 函数"
  (let ((items   (split-string
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)) seperaters)))
    (if omit-nulls
        (emms-delete-if 'chinese-wbim-string-emptyp items)
      items)))

(defsubst chinese-wbim-delete-line ()
  (delete-region (line-beginning-position) (min (+ (line-end-position) 1)
                                                (point-max))))

(defsubst chinese-wbim-append-string (str)
  "append STR to chinese-wbim-current-str"
  (setq chinese-wbim-current-str (concat chinese-wbim-current-str str)))

;;;_. code search
(defun chinese-wbim-get (code)
  (when (and (stringp code) (not (chinese-wbim-string-emptyp code)))
    (let ((history (gethash code (chinese-wbim-history)))
          pos words completions)
      (if (and (car history) (assoc "completions" (cdr history)))
          history
        (dolist (buf (chinese-wbim-buffer-list))
          (with-current-buffer (cdr (assoc "buffer" buf))
            (setq words (append words
                                (cdr
                                 (chinese-wbim-bisearch-word code
                                                    (point-min)
                                                    (point-max)))))
            (if chinese-wbim-do-completion
                (setq completions (chinese-wbim-completions code completions)))))
        (setq words (delete-dups words))
        (puthash code (list words
                            (cons "pos" (or (cdr (assoc "pos" (cdr history))) 1))
                            (cons "completions" completions))
                 (chinese-wbim-history))))))

(defun chinese-wbim-completions (code completions)
  (let ((maxln 200)
        (cnt 0)
        (len (length code))
        (reg (concat "^" (regexp-quote code))))
    (save-excursion
      (forward-line 1)
      (while (and (looking-at reg)
                  (< cnt maxln))
        (add-to-list 'completions (buffer-substring-no-properties
                                   (+ (point) len)
                                   (+ (point) len 1)))
        (forward-line 1)
        (setq cnt (1+ cnt)))
      completions)))

(defun chinese-wbim-bisearch-word (code start end)
  (let ((mid (/ (+ start end) 2))
        ccode)
    (goto-char mid)
    (beginning-of-line)
    (setq ccode (chinese-wbim-code-at-point))
    ;;    (message "%d, %d, %d: %s" start mid end ccode)
    (if (string= ccode code)
        (chinese-wbim-line-content)
      (if (> mid start)
          (if (string< ccode code)
              (chinese-wbim-bisearch-word code mid end)
            (chinese-wbim-bisearch-word code start mid))))))

(defun chinese-wbim-code-at-point ()
  "Before calling this function, be sure that the point is at the
beginning of line"
  (save-excursion
    (if (re-search-forward "[ \t]" (line-end-position) t)
        (buffer-substring-no-properties (line-beginning-position) (1- (point)))
      (error "文件类型错误！%s 的第 %d 行没有词条！" (buffer-name) (line-number-at-pos)))))

;;;_. interface
(defun chinese-wbim-check-buffers ()
  "检查所有的 buffer 是否还存在，如果不存在，重新打开文件，如果文件不
存在，从 buffer-list 中删除这个 buffer"
  (let ((buflist (chinese-wbim-buffer-list))
        (bufname (chinese-wbim-package-name))
        buffer file)
    (dolist (buf buflist)
      (unless (buffer-live-p (cdr (setq buffer (assoc "buffer" buf))))
        (if (file-exists-p (setq file (cdr (assoc "file" buf))))
            (with-current-buffer (format "*%s*" (generate-new-buffer bufname))
              (insert-file-contents file)
              (setcdr buffer (current-buffer)))
          (message "%s for %s is not exists!" file bufname)
          (setq buflist (remove buf buflist)))))
    t))

(defun chinese-wbim-install-variable ()
  (let ((param (cdr (assoc "param" (car (chinese-wbim-buffer-list))))))
    (mapc (lambda (p)
            (let ((sym (intern-soft (concat "chinese-wbim-" (car p)))))
              (if sym
                  (set sym (mapconcat 'identity (cdr p) "=")))))
          param)
    (if (stringp chinese-wbim-page-length)
        (setq chinese-wbim-page-length (string-to-number chinese-wbim-page-length)))
    (setq chinese-wbim-first-char (append chinese-wbim-first-char nil)
          chinese-wbim-total-char (append chinese-wbim-total-char nil))))

;;;_ , chinese-wbim-use-package
(defun chinese-wbim-use-package (package-name &optional word-file active-func)
  (interactive)
  (mapc 'kill-local-variable chinese-wbim-local-variable-list)
  (mapc 'make-local-variable chinese-wbim-local-variable-list)
  (if (assoc package-name chinese-wbim-package-list)
      (setq chinese-wbim-current-package (cdr (assoc package-name
                                            chinese-wbim-package-list)))
    ;; make more room for extension
    (setq chinese-wbim-current-package (make-vector 9 nil)))
  (if (functionp active-func)
      (funcall active-func))
  (unless (and (chinese-wbim-package-name)
               (chinese-wbim-check-buffers))
    (if (and word-file
             (if (file-exists-p (expand-file-name word-file))
                 (setq word-file (expand-file-name word-file))
               (setq word-file (locate-file word-file load-path))))
        (progn
          (chinese-wbim-set-package-name package-name)
          (chinese-wbim-set-buffer-list (chinese-wbim-load-file word-file))
          (chinese-wbim-set-history (make-hash-table :test 'equal))
          (chinese-wbim-set-mode-map (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map chinese-wbim-mode-map)
                              map))
          (add-to-list 'chinese-wbim-package-list (cons package-name chinese-wbim-current-package))
          (let ((param (cdr (assoc "param" (car (chinese-wbim-buffer-list))))))
            (if (assoc "lib" param)
                (load (cadr (assoc "lib" param)))))
          (run-hooks 'chinese-wbim-load-hook)
          (message nil))
      (error "没有这个文件: %s" word-file)))
  (chinese-wbim-install-variable)
  (setq input-method-function 'chinese-wbim-input-method)
  (setq inactivate-current-input-method-function 'chinese-wbim-inactivate)
  (setq describe-current-input-method-function 'chinese-wbim-help)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'chinese-wbim-exit-from-minibuffer))
  (run-hooks 'chinese-wbim-active-hook)
  (if (functionp (chinese-wbim-active-function))
      (funcall (chinese-wbim-active-function))))

(defun chinese-wbim-inactivate ()
  (interactive)
  (mapc 'kill-local-variable chinese-wbim-local-variable-list))

(defun chinese-wbim-help (&optional package)
  "Show input method docstring"
  (save-excursion
    (let ((map (chinese-wbim-mode-map))
          (elt (assoc (chinese-wbim-package-name) input-method-alist))
          reg desc docstr buf)
      (setq buf (cdr (assoc "buffer" (car (chinese-wbim-buffer-list)))))
      (set-buffer buf)
      (save-restriction
        (widen)
        (setq reg (condition-case err
                      (chinese-wbim-section-region "Description")
                    (error nil))
              desc (if reg
                       (buffer-substring-no-properties (car reg) (cdr reg))
                     "")
              docstr (format "Input method: %s (`%s' in mode line) for %s\n  %s\n%s\n\n%s\n"
                             (nth 0 elt) (nth 3 elt) (nth 1 elt) (nth 4 elt)
                             desc
                             (substitute-command-keys "\\{map}")))
        (help-setup-xref (list #'describe-input-method (nth 0 elt))
                         (interactive-p))
        (with-output-to-temp-buffer (help-buffer)
          (princ docstr))))))

;;;_ , page format
(defsubst chinese-wbim-choice (choice)
  (if (consp choice)
      (car choice)
    choice))

(defun chinese-wbim-add-completion ()
  "注意, chinese-wbim-add-completion-function 在没有完补全之前返回 nil, 在加完所
有补全之后一定要返回一个 t"
  (if (functionp chinese-wbim-add-completion-function)
      (funcall chinese-wbim-add-completion-function)
    t))

(defun chinese-wbim-format (key cp tp choice)
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key  cp tp
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i)) c))
                        choice) " "))))

(defun chinese-wbim-format-page ()
  "按当前位置，生成候选词条"
  (let ((end (chinese-wbim-page-end)))
    (if (car chinese-wbim-current-choices)
        (let* ((start (1- (chinese-wbim-page-start)))
               (choices (car chinese-wbim-current-choices))
               (choice (chinese-wbim-subseq choices start end))
               (pos (1- (min chinese-wbim-current-pos (length choices))))
               (i 0))
          (setq chinese-wbim-current-str (chinese-wbim-choice (nth pos choices)))
          (setq chinese-wbim-guidance-str
                (funcall chinese-wbim-format-function chinese-wbim-current-key (chinese-wbim-current-page)
                         (chinese-wbim-total-page) choice))
          ;; (message "%d, %s, %s" pos chinese-wbim-current-str chinese-wbim-guidance-str)
          (chinese-wbim-show))
      (setq chinese-wbim-current-str chinese-wbim-current-key)
      (setq chinese-wbim-guidance-str
            (concat chinese-wbim-current-key
                    (if (cdr (assoc "completions" (cdr chinese-wbim-current-choices)))
                        (format "[%s]: "
                                (mapconcat 'identity
                                           (cdr (assoc
                                                 "completions"
                                                 (cdr chinese-wbim-current-choices)))
                                           "")))))
      (chinese-wbim-show))))

(defun chinese-wbim-current-page ()
  (1+ (/ (1- chinese-wbim-current-pos) chinese-wbim-page-length)))

(defun chinese-wbim-total-page ()
  (1+ (/ (1- (length (car chinese-wbim-current-choices))) chinese-wbim-page-length)))

(defun chinese-wbim-page-start ()
  "计算当前所在页的第一个词条的位置"
  (let ((pos (min (length (car chinese-wbim-current-choices)) chinese-wbim-current-pos)))
    (1+ (- pos (chinese-wbim-mod pos chinese-wbim-page-length)))))

(defun chinese-wbim-page-end (&optional finish)
  "计算当前所在页的最后一个词条的位置，如果 chinese-wbim-current-choices 用
完，则检查是否有补全。如果 FINISH 为 non-nil，说明，补全已经用完了"
  (let* ((whole (length (car chinese-wbim-current-choices)))
         (len chinese-wbim-page-length)
         (pos chinese-wbim-current-pos)
         (last (+ (- pos (chinese-wbim-mod pos len)) len)))
    (if (< last whole)
        last
      (if finish
          whole
        (chinese-wbim-page-end (chinese-wbim-add-completion))))))

;;;_ , commands
(defun chinese-wbim-next-page (arg)
  (interactive "p")
  (if (> (length chinese-wbim-current-key) 0)
      (let ((new (+ chinese-wbim-current-pos (* chinese-wbim-page-length arg) 1)))
        (setq chinese-wbim-current-pos (if (> new 0) new 1)
              chinese-wbim-current-pos (chinese-wbim-page-start))
        (chinese-wbim-format-page))
    (message "%c" last-command-event)
    (chinese-wbim-append-string (chinese-wbim-translate last-command-event))
    (chinese-wbim-terminate-translation)))

(defun chinese-wbim-previous-page (arg)
  (interactive "p")
  (chinese-wbim-next-page (- arg)))

(defun chinese-wbim-delete-last-char ()
  (interactive)
  (if (> (length chinese-wbim-current-key) 1)
      (progn
        (setq chinese-wbim-current-key (substring chinese-wbim-current-key 0 -1))
        (funcall chinese-wbim-handle-function))
    (setq chinese-wbim-current-str "")
    (chinese-wbim-terminate-translation)))

(defun chinese-wbim-self-insert-command ()
  "如果在 chinese-wbim-first-char 列表中，则查找相应的词条，否则停止转换，插入对应的字符"
  (interactive "*")
  ;; (message "%s" (current-buffer))
  (if (if (chinese-wbim-string-emptyp chinese-wbim-current-key)
          (member last-command-event chinese-wbim-first-char)
        (member last-command-event chinese-wbim-total-char))
      (progn
        (setq chinese-wbim-current-key (concat chinese-wbim-current-key (char-to-string last-command-event)))
        (funcall chinese-wbim-handle-function))
    (chinese-wbim-append-string (chinese-wbim-translate last-command-event))
    (chinese-wbim-terminate-translation)))

(defun chinese-wbim-select-current ()
  "如果没有可选项，而且是用空格来绑定这个键，就插入空格，否则选择第一
个词条"
  (interactive)
  (if (null (car chinese-wbim-current-choices))
      (setq chinese-wbim-current-str
            (if (> (length chinese-wbim-current-str) 0)
                ""
              (chinese-wbim-translate last-command-event)))
    (chinese-wbim-remember-select))
  (chinese-wbim-terminate-translation))

(defun chinese-wbim-remember-select (&optional pos)
  (let ((rest (emms-delete-if (lambda (p) (string= (car p) "pos"))
                              (cdr chinese-wbim-current-choices))))
    (setq rest (append rest (list (cons "pos" (or pos
                                                  chinese-wbim-current-pos)))))
    (puthash chinese-wbim-current-key (cons (car chinese-wbim-current-choices)
                                   rest) (chinese-wbim-history))))

(defun chinese-wbim-number-select ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car chinese-wbim-current-choices)
      (let ((index (+ (chinese-wbim-page-start) (- last-command-event ?2)))
            (end (chinese-wbim-page-end)))
        (if (>= index end)
            (chinese-wbim-show)
          (chinese-wbim-remember-select (1+ index))
          (setq chinese-wbim-current-str (chinese-wbim-choice (nth index (car chinese-wbim-current-choices))))
          (chinese-wbim-terminate-translation)))
    (chinese-wbim-append-string (char-to-string last-command-event))
    (chinese-wbim-terminate-translation)))

(defun chinese-wbim-quit-clear ()
  (interactive)
  (setq chinese-wbim-current-str "")
  (chinese-wbim-terminate-translation))

(defun chinese-wbim-quit-no-clear ()
  (interactive)
  (setq chinese-wbim-current-str chinese-wbim-current-key)
  (chinese-wbim-terminate-translation))

(defun chinese-wbim-terminate-translation ()
  "Terminate the translation of the current key."
  (setq chinese-wbim-translating nil)
  (chinese-wbim-delete-region)
  (setq chinese-wbim-current-choices nil)
  (setq chinese-wbim-guidance-str "")
  (when chinese-wbim-use-tooltip
    (x-hide-tip)))

;;;_ , chinese-wbim-handle-string
(defun chinese-wbim-handle-string ()
  (if (and (functionp chinese-wbim-stop-function)
           (funcall chinese-wbim-stop-function))
      (progn
        (setq unread-command-events
              (list (aref chinese-wbim-current-key (1- (length chinese-wbim-current-key)))))
        (chinese-wbim-terminate-translation))
    (setq chinese-wbim-current-choices (chinese-wbim-get chinese-wbim-current-key)
          chinese-wbim-current-pos
          (if (chinese-wbim-get-option 'record-position)
              (cdr (assoc "pos" (cdr chinese-wbim-current-choices)))
            1))
    (chinese-wbim-format-page)))

(defun chinese-wbim-translate (char)
  (if (functionp chinese-wbim-translate-function)
      (funcall chinese-wbim-translate-function char)
    (char-to-string char)))

;;;_ , Core function of input method (stole from quail)
(defun chinese-wbim-exit-from-minibuffer ()
  (inactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer)))

(defun chinese-wbim-setup-overlays ()
  (let ((pos (point)))
    (if (overlayp chinese-wbim-overlay)
        (move-overlay chinese-wbim-overlay pos pos)
      (setq chinese-wbim-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put chinese-wbim-overlay 'face 'chinese-wbim-string-face)))))

(defun chinese-wbim-delete-overlays ()
  (if (and (overlayp chinese-wbim-overlay) (overlay-start chinese-wbim-overlay))
      (delete-overlay chinese-wbim-overlay)))

(defun chinese-wbim-show ()
  (unless enable-multibyte-characters
    (setq chinese-wbim-current-key nil
          chinese-wbim-current-str nil)
    (error "Can't input characters in current unibyte buffer"))
  (chinese-wbim-delete-region)
  (insert chinese-wbim-current-str)
  (move-overlay chinese-wbim-overlay (overlay-start chinese-wbim-overlay) (point))
  ;; Then, show the guidance.
  (when (and (not input-method-use-echo-area)
             (null unread-command-events)
             (null unread-post-input-method-events))
    (if (eq (selected-window) (minibuffer-window))
        ;; Show the guidance in the next line of the currrent
        ;; minibuffer.
        (chinese-wbim-minibuffer-message
         (format "  [%s]\n%s"
                 current-input-method-title chinese-wbim-guidance-str))
      ;; Show the guidance in echo area without logging.
      (let ((message-log-max nil))
        (if chinese-wbim-use-tooltip
            (let ((pos (string-match ": " chinese-wbim-guidance-str)))
              (if pos
                  (setq chinese-wbim-guidance-str
                        (concat (substring chinese-wbim-guidance-str 0 pos)
                                "\n"
                                (make-string (/ (- (string-width chinese-wbim-guidance-str) pos) 2) (decode-char 'ucs #x2501))
                                "\n"
                                (substring chinese-wbim-guidance-str (+ pos 2)))))
              (chinese-wbim-show-tooltip chinese-wbim-guidance-str))
          (message "%s" chinese-wbim-guidance-str))))))

(defun chinese-wbim-make-guidance-frame ()
  "Make a new one-line frame for Quail guidance."
  (let* ((fparam (frame-parameters))
         (top (cdr (assq 'top fparam)))
         (border (cdr (assq 'border-width fparam)))
         (internal-border (cdr (assq 'internal-border-width fparam)))
         (newtop (- top
                    (frame-char-height) (* internal-border 2) (* border 2))))
    (if (< newtop 0)
        (setq newtop (+ top (frame-pixel-height) internal-border border)))
    (make-frame (append '((user-position . t) (height . 1)
                          (minibuffer)
                          (menu-bar-lines . 0) (tool-bar-lines . 0))
                        (cons (cons 'top newtop) fparam)))))

(defun chinese-wbim-minibuffer-message (string)
  (message nil)
  (let ((point-max (point-max))
        (inhibit-quit t))
    (save-excursion
      (goto-char point-max)
      (insert string))
    (sit-for 1000000)
    (delete-region point-max (point-max))
    (when quit-flag
      (setq quit-flag nil
            unread-command-events '(7)))))

(defun chinese-wbim-input-method (key)
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
    ;; (message "call with key: %c" key)
    (chinese-wbim-setup-overlays)
    (let ((modified-p (buffer-modified-p))
          (buffer-undo-list t)
          (inhibit-modification-hooks t))
      (unwind-protect
          (let ((input-string (chinese-wbim-start-translation key)))
            ;;   (message "input-string: %s" input-string)
            (setq chinese-wbim-guidance-str "")
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                (chinese-wbim-input-string-to-events input-string))))
        (chinese-wbim-delete-overlays)
        (set-buffer-modified-p modified-p)
        ;; Run this hook only when the current input method doesn't
        ;; require conversion. When conversion is required, the
        ;; conversion function should run this hook at a proper
        ;; timing.
        (run-hooks 'input-method-after-insert-chunk-hook)))))

(defun chinese-wbim-start-translation (key)
  "Start translation of the typed character KEY by the current Quail package.
Return the input string."
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map (chinese-wbim-mode-map))
             (generated-events nil)
             (input-method-function nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)
        (setq chinese-wbim-current-str ""
              chinese-wbim-current-key ""
              chinese-wbim-translating t)
        (if key
            (setq unread-command-events
                  (cons key unread-command-events)))
        (while chinese-wbim-translating
          (set-buffer-modified-p modified-p)
          (let* ((prompt (if input-method-use-echo-area
                             (format "%s%s %s"
                                     (or input-method-previous-message "")
                                     chinese-wbim-current-key
                                     chinese-wbim-guidance-str)))
                 (keyseq (read-key-sequence prompt nil nil t))
                 (cmd (lookup-key (chinese-wbim-mode-map) keyseq)))
            ;;             (message "key: %s, cmd:%s\nlcmd: %s, lcmdv: %s, tcmd: %s"
            ;;                      key cmd last-command last-command-event this-command)
            (if (if key
                    (commandp cmd)
                  (eq cmd 'chinese-wbim-self-insert-command))
                (progn
                  ;; (message "keyseq: %s" keyseq)
                  (setq last-command-event (aref keyseq (1- (length keyseq)))
                        last-command this-command
                        this-command cmd)
                  (setq key t)
                  (condition-case err
                      (call-interactively cmd)
                    (error (message "%s" (cdr err)) (beep))))
              ;; KEYSEQ is not defined in the translation keymap.
              ;; Let's return the event(s) to the caller.
              (setq unread-command-events
                    (string-to-list (this-single-command-raw-keys)))
              ;; (message "unread-command-events: %s" unread-command-events)
              (chinese-wbim-terminate-translation))))
        ;;    (1message "return: %s" chinese-wbim-current-str)
        chinese-wbim-current-str)
    ;; Since KEY doesn't start any translation, just return it.
    ;; But translate KEY if necessary.
    (char-to-string key)))

(defun chinese-wbim-input-string-to-events (str)
  (let ((events (mapcar
                 (lambda (c)
                   ;; This gives us the chance to unify on input
                   ;; (e.g. using ucs-tables.el).
                   (or (and translation-table-for-input
                            (aref translation-table-for-input c))
                       c))
                 str)))
    (if (or (get-text-property 0 'advice str)
            (next-single-property-change 0 'advice str))
        (setq events
              (nconc events (list (list 'chinese-wbim-advice str)))))
    events))

(defun chinese-wbim-advice (args)
  (interactive "e")
  (let* ((string (nth 1 args))
         (func (get-text-property 0 'advice string)))
    (if (functionp func)
        (funcall func string))))

(global-set-key [chinese-wbim-advice] 'chinese-wbim-advice)

;;; borrow from completion-ui
(defun chinese-wbim-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window."
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges))
          (+ (cdr x-y) (cadr edges)))))

(defface chinese-wbim-tooltip-face '((((class color)) :inherit tooltip))
  "face to display items"
  :group 'chinese-wbim)

(defun chinese-wbim-show-tooltip (text)
  "Show tooltip text near cursor."
  (let ((pos (chinese-wbim-frame-posn-at-point))
        (fg (face-attribute 'chinese-wbim-tooltip-face :foreground nil 'tooltip))
        (bg (face-attribute 'chinese-wbim-tooltip-face :background nil 'tooltip))
        (params tooltip-frame-parameters)
        ;; seem the top position should add 65 pixel to make
        ;; the text display under the baseline of cursor
        (top-adjust 65)
        (frame-height (frame-pixel-height))
        (frame-width (frame-pixel-width))
        (lines (split-string text "\n"))
        width height left top)
    (setq width (* (frame-char-width) (apply 'max (mapcar 'string-width lines)))
          height (* (frame-char-height) (length lines)))
    (setq left (frame-parameter nil 'left)
          top (frame-parameter nil 'top))
    ;; if the cursor is at near the right frame fringe or at bottom
    ;; of the bottom fringe, move the frame to
    ;; -frame-width or -frame-height from right or bottom
    (if (< (- frame-width (car pos)) width)
        (setq left (+ left (max 0 (- frame-width width))))
      (setq left (+ left (car pos))))
    (if (< (- frame-height (cdr pos)) (+ height top-adjust))
        (setq top (+ top (max 0 (- frame-height height))))
      (setq top (+ top (cdr pos))))
    (setq top (+ top top-adjust))
    (when (stringp fg)
      (setq params (append params `((foreground-color . ,fg)
                                    (border-color . ,fg)))))
    (when (stringp bg)
      (setq params (append params `((background-color . ,bg)))))
    (setq params (append params `((left . ,left) (top . ,top))))
    (x-show-tip (propertize text 'face 'chinese-wbim-tooltip-face)
                nil params chinese-wbim-tooltip-timeout)))

;;;_. utils
;;;###autoload
(defun chinese-wbim-create-word-file ()
  "创建一个能用于 chinese-wbim 的新文件，按说明填入相应的内容就能生成对应的输入法"
  (interactive)
  (let ((buffer (generate-new-buffer "chinese-wbim-word")))
    (switch-to-buffer buffer)
    (insert
     "[Comment]\n"
     "要创建一个新的 chinese-wbim 输入法文件，最简单的方法是只要在 Table 部分填入码表\n"
     "就行了。更多的设置如下：\n"
     "# 控制是否进入转换。一般设置成所有词库中的首字母\n"
     "first-char=\n"
     "# 控制是否退出转换，一般设置成所有词库中的字母\n"
     "total-char=\n"
     "# 在启动时 load 的 elisp 文件\n"
     "lib=\n"
     "# 其它词库文件，用 ; 隔开\n"
     "other-files=\n"
     "# 每页显示的词条数目\n"
     "page-length=\n\n"
     "如果需要加入标点，加入一个 Punctuation 部分。然后设置 chinese-wbim-translate-fuction。\n"
     "如果需要排序，或者合并相同编码的词条，使用 C-c C-c 或者 M-x chinese-wbim-build-table。\n"
     "如果有需要，可能还要修改 first-char 和 total-char\n\n"
     "[Parameter]\n"
     "first-char=abcdefghijklmnopqrstuvwxyz\n"
     "total-char=abcdefghijklmnopqrstuvwxyz\n\n"
     "[Description]\n"
     "\n\n"
     "[Table]\n"
     )
    (local-set-key "\C-c\C-c" 'chinese-wbim-build-table)))

;;;###autoload
(defun chinese-wbim-build-table ()
  (interactive)
  (save-restriction
    (let ((table (chinese-wbim-section-region "Table"))
          (param (chinese-wbim-section-region "Parameter"))
          (lastw "")
          first-char total-char currw)
      (narrow-to-region (car table) (cdr table))
      (perform-replace "[ \t]+$" "" nil t nil nil nil (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^[ \t]*$")     ; 如果有空行，删除
            (chinese-wbim-delete-line)
          (setq currw (chinese-wbim-code-at-point))
          (add-to-list 'first-char (aref currw 0))
          (mapc (lambda (c) (add-to-list 'total-char c)) (append currw nil))
          (if (string= currw lastw)
              (delete-region (1- (point)) (+ (point) (length currw))))
          (setq lastw currw)
          (forward-line 1)))
      (narrow-to-region (car param) (cdr param))
      (goto-char (point-min))
      (insert "first-char=" (concat first-char) "\n"
              "total-char=" (concat total-char) "\n")
      (while (not (eobp))
        (if (or (looking-at "^first-char=")
                (looking-at "^total-char="))
            (chinese-wbim-delete-line)
          (forward-line 1)))
      (if (looking-at "^$")
          (delete-backward-char 1)))))

(provide 'chinese-wbim)
;;; chinese-wbim.el ends here
