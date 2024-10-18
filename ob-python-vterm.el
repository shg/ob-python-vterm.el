;;; ob-python-vterm.el --- Babel functions for Python that work with python-vterm -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Shigeaki Nishina

;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; Created: September 4, 2023
;; URL: https://github.com/shg/ob-python-vterm.el
;; Package-Requires: ((emacs "26.1") (python-vterm "0.1") (queue "0.2"))
;; Version: 0.1
;; Keywords: python, org, outlines, literate programming, reproducible research

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Org-Babel support for Python source code block using python-vterm.

;;; Requirements:

;; This package uses python-vterm to run Python code.
;;
;; - https://github.com/shg/python-vterm.el
;;
;; See https://github.com/shg/ob-python-vterm.el for installation
;; instructions.

;;; Code:

(require 'ob)
(require 'org-id)
(require 'queue)
(require 'filenotify)
(require 'python-vterm)

(defun ob-python-vterm-wrap-body (session body)
  "Make Python code that execute-s BODY and obtains the results, depending on SESSION."
  (concat
   (if session "" "")
   body
   (if session "\n" "\n")))

(defun ob-python-vterm-make-str-to-run (uuid params src-file out-file)
  "Make Python code that execute-s the code in SRC-FILE depending on PARAMS.
The results are saved in OUT-FILE.  UUID is a unique id assigned
to the evaluation."
  (format
   (pcase (cdr (assq :result-type params))
     ('output "\
#OB-PYTHON-VTERM_BEGIN %s
import sys

with open('%s', 'r') as file:
    __code_str = file.read()

__orig_stdout = sys.stdout

with open('%s', 'w') as file:
    sys.stdout = file
    try:
        exec(__code_str)
    except (NameError, SyntaxError) as e:
        print(type(e).__name__ + ': ' + str(e))

sys.stdout = __orig_stdout
None
# %s %s %s
#OB-PYTHON-VTERM_END\n")
     ('value "\
#OB-PYTHON-VTERM_BEGIN %s
import ast

with open('%s', 'r') as file:
    __code_str = file.read()

g = globals()
try:
    st = list(ast.iter_child_nodes(ast.parse(__code_str)))
    if not st:
        __result = None
    elif isinstance(st[-1], ast.Expr):
        if len(st) > 1:
            exec(compile(ast.Module(body=st[:-1], type_ignores=[]), '<string>', 'exec'), g)
        __result = eval(compile(ast.Expression(st[-1].value), '<string>', 'eval'), g)
    else:
        exec(compile(ast.Module(body=st, type_ignores=[]), '<string>', 'exec'), g)
        __result = None
except (NameError, SyntaxError) as e:
    __result = type(e).__name__ + ': ' + str(e)

with open('%s', 'w') as file:
    file.write(str(__result))

__result
# %s %s %s
#OB-PYTHON-VTERM_END\n"))
   (substring uuid 0 8) src-file out-file
   (if (member "pp" (cdr (assq :result-params params))) "" "")
   (if (member "nolimit" (cdr (assq :result-params params))) "" "")
   (if (not (member (cdr (assq :debug params)) '(nil "no"))) "" "")))

(defun org-babel-execute:python-vterm (body params)
  "Execute a block of Python code with Babel.
This function is called by `org-babel-execute-src-block'.
BODY is the contents and PARAMS are header arguments of the code block."
  (let* ((session-name (cdr (assq :session params)))
	 (session (pcase session-name ('nil "main") ("none" nil) (_ session-name)))
	 (var-lines (org-babel-variable-assignments:python-vterm params))
	 (result-params (cdr (assq :result-params params))))
    (with-current-buffer (python-vterm-repl-buffer session)
      (add-hook 'python-vterm-repl-filter-functions #'ob-python-vterm-output-filter))
    (ob-python-vterm-evaluate (current-buffer)
			      session
			      (org-babel-expand-body:generic body params var-lines)
			      params)))

(defun org-babel-variable-assignments:python-vterm (params)
  "Return list of Python statements assigning variables based on variable-value pairs in PARAMS."
  (mapcar
   (lambda (pair)
     (format "%s = %s" (car pair) (ob-python-vterm-value-to-python (cdr pair))))
   (org-babel--get-vars params)))

(defun ob-python-vterm-escape-string (str)
  "Escape special characters in STR for Python variable assignments."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun ob-python-vterm-value-to-python (value)
  "Convert an emacs-lisp VALUE to a string of python code for the value."
  (cond
   ((listp value) (format "\"%s\"" value))
   ((numberp value) value)
   ((stringp value) (or (org-babel--string-to-number value)
			(concat "\"" (ob-python-vterm-escape-string value) "\"")))
   ((symbolp value) (ob-python-vterm-escape-string (symbol-name value)))
   (t value)))

(defun ob-python-vterm-check-long-line (str)
  "Return t if STR is too long for org-babel result."
  (catch 'loop
    (dolist (line (split-string str "\n"))
      (if (> (length line) 12000)
	  (throw 'loop t)))))

(defvar-local ob-python-vterm-evaluation-queue nil)
(defvar-local ob-python-vterm-evaluation-watches nil)

(defun ob-python-vterm-add-evaluation-to-evaluation-queue (session evaluation)
  "Add an EVALUATION of a source block to SESSION's evaluation queue."
  (with-current-buffer (python-vterm-repl-buffer session)
    (if (not (queue-p ob-python-vterm-evaluation-queue))
	(setq ob-python-vterm-evaluation-queue (queue-create)))
    (queue-append ob-python-vterm-evaluation-queue evaluation)))

(defun ob-python-vterm-evaluation-completed-callback-func (session)
  "Return a callback function to be called when an evaluation in SESSION is completed."
  (lambda (event)
    (if (eq 'changed (cadr event))
	(with-current-buffer (python-vterm-repl-buffer session)
	  (if (and (queue-p ob-python-vterm-evaluation-queue)
		   (> (queue-length ob-python-vterm-evaluation-queue) 0))
	      (let-alist (queue-first ob-python-vterm-evaluation-queue)
		(with-current-buffer .buf
		  (save-excursion
		    (goto-char .src-block-begin)
		    (when (and (not (equal .src-block-begin .src-block-end))
			       (or (eq (org-element-type (org-element-context)) 'src-block)
				   (eq (org-element-type (org-element-context)) 'inline-src-block)))
		      (ob-python-vterm-wait-for-file-change .out-file 10 0.1)
		      (let ((result (with-temp-buffer
				      (insert-file-contents .out-file)
				      (buffer-string)))
			    (result-params (cdr (assq :result-params .params))))
			(cond ((member "file" result-params)
			       (org-redisplay-inline-images))
			      ((not (member "none" result-params))
			       (org-babel-insert-result
				(if (ob-python-vterm-check-long-line result)
				    "Output suppressed (line too long)"
				  (org-babel-result-cond result-params
				    result
				    (org-babel-reassemble-table
				     result
				     (org-babel-pick-name (cdr (assq :colname-names .params))
							  (cdr (assq :colnames .params)))
				     (org-babel-pick-name (cdr (assq :rowname-names .params))
							  (cdr (assq :rownames .params))))))
				result-params
				(org-babel-get-src-block-info 'light))))))))
		(queue-dequeue ob-python-vterm-evaluation-queue)
		(file-notify-rm-watch (cdr (assoc .uuid ob-python-vterm-evaluation-watches)))
		(setq ob-python-vterm-evaluation-watches
		      (delete (assoc .uuid ob-python-vterm-evaluation-watches)
			      ob-python-vterm-evaluation-watches))
		(ob-python-vterm-process-evaluation-queue .session)))))))

(defvar-local ob-python-vterm-output-suppress-state nil)

(defun ob-python-vterm-output-filter (str)
  "Remove the pasted python code from STR."
  (let ((begin (string-match "#OB-PYTHON-VTERM_BEGIN" str))
	(end (string-match "#OB-PYTHON-VTERM_END" str))
	(state ob-python-vterm-output-suppress-state))
    (if begin (setq ob-python-vterm-output-suppress-state 'suppress))
    (if end (setq ob-python-vterm-output-suppress-state nil))
    str))

(defun ob-python-vterm-wait-for-file-change (file sec interval)
  "Wait up to SEC seconds synchronously until FILE becomes non-empty.
The file is checked at INTERVAL second intervals while waiting."
  (let ((c 0))
    (while (and (< c (/ sec interval))
		(= 0 (file-attribute-size (file-attributes file))))
      (sleep-for interval)
      (setq c (1+ c)))))

(defun ob-python-vterm-process-one-evaluation-sync (session evaluation)
  "Execute the first EVALUATION in SESSION's queue synchronously.
Return the result."
  (with-current-buffer (python-vterm-repl-buffer session)
    (while (not (eq (python-vterm-repl-buffer-status) :ipython))
      (message "Waiting REPL becomes ready")
      (sleep-for 0.1))
    (let-alist evaluation
      (python-vterm-paste-string
       (ob-python-vterm-make-str-to-run .uuid
				        .params
				        .src-file
				        .out-file)
       .session)
      (ob-python-vterm-wait-for-file-change .out-file 10 0.1)
      (with-temp-buffer
	(insert-file-contents .out-file)
	(buffer-string)))))

(defun ob-python-vterm-process-one-evaluation-async (session)
  "Execute the first evaluation in SESSION's queue asynchronously.
Always return nil."
  (with-current-buffer (python-vterm-repl-buffer session)
    (if (eq (python-vterm-repl-buffer-status) :ipython)
	(let-alist (queue-first ob-python-vterm-evaluation-queue)
	  (unless (assoc .uuid ob-python-vterm-evaluation-watches)
	    (let ((desc (file-notify-add-watch .out-file
					       '(change)
					       (ob-python-vterm-evaluation-completed-callback-func session))))
	      (push (cons .uuid desc) ob-python-vterm-evaluation-watches))
	    (python-vterm-paste-string
	     (ob-python-vterm-make-str-to-run .uuid
					      .params
					      .src-file
					      .out-file)
	     .session)))
      (if (null ob-python-vterm-evaluation-watches)
	  (run-at-time 0.1 nil #'ob-python-vterm-process-evaluation-queue session))))
  nil)

(defun ob-python-vterm-process-evaluation-queue (session)
  "Process the evaluation queue for SESSION.
If ASYNC is non-nil, the next evaluation will be executed asynchronously."
  (with-current-buffer (python-vterm-repl-buffer session)
    (if (and (queue-p ob-python-vterm-evaluation-queue)
	     (not (queue-empty ob-python-vterm-evaluation-queue)))
	(ob-python-vterm-process-one-evaluation-async session)
      (message "Queue empty"))))

(defun ob-python-vterm-get-queue (session)
  "Get the evaluation queue for SESSION."
  (with-current-buffer (python-vterm-repl-buffer session)
    ob-python-vterm-evaluation-queue))

(defun ob-python-vterm-abort-evaluation (session)
  "Abort the execution of SESSION by clearing its evaluation queue."
  (with-current-buffer (python-vterm-repl-buffer session)
    (if (queue-p ob-python-vterm-evaluation-queue)
        (queue-clear ob-python-vterm-evaluation-queue))))

(defun ob-python-vterm-evaluate (buf session body params)
  "Evaluate a Python code block in BUF in a python-vterm REPL specified with SESSION.
BODY contains the source code to be evaluated, and PARAMS contains header arguments."
  (let* ((uuid (org-id-uuid))
	 (src-file (org-babel-temp-file "python-vterm-src-"))
	 (out-file (org-babel-temp-file "python-vterm-out-"))
	 (result-params (cdr (assq :result-params params)))
	 (async (not (member 'org-babel-ref-resolve (mapcar #'cadr (backtrace-frames))))))
    (with-temp-file src-file (insert (ob-python-vterm-wrap-body session body)))
    (let ((elm (org-element-context))
	  (src-block-begin (make-marker))
	  (src-block-end (make-marker)))
      (set-marker src-block-begin (org-element-property :begin elm))
      (set-marker src-block-end (org-element-property :end elm))
      (let ((evaluation (list (cons 'uuid uuid)
			      (cons 'async async)
			      (cons 'buf buf)
			      (cons 'session session)
			      (cons 'params params)
			      (cons 'src-file src-file)
			      (cons 'out-file out-file)
			      (cons 'src-block-begin src-block-begin)
			      (cons 'src-block-end src-block-end))))
	(if (not async)
	    (ob-python-vterm-process-one-evaluation-sync session evaluation)
	  (ob-python-vterm-add-evaluation-to-evaluation-queue session evaluation)
	  (ob-python-vterm-process-evaluation-queue session)
	  (concat "Executing... " (substring uuid 0 8)))))))

(add-to-list 'org-src-lang-modes '("python-vterm" . python))

(provide 'ob-python-vterm)

;;; ob-python-vterm.el ends here
