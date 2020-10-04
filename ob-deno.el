;;; ob-deno.el --- Babel Functions for Javascript/TypeScript with Deno      -*- lexical-binding: t; -*-

;; Copyright (C) 2020 HIGASHI Taiju

;; Author: HIGASHI Taiju
;; Keywords: literate programming, reproducible research, javascript, typescript, tools
;; Homepage: https://github.com/taiju/ob-deno
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; ob-deno is based on ob-js.

;;; Requirements:

;; - Deno https://deno.land/

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:deno '()
  "Default header arguments for js/ts code blocks.")

(defcustom ob-deno-cmd "deno"
  "Name of command used to evaluate js/ts blocks."
  :group 'ob-deno
  :type 'string)

(defvar ob-deno-function-wrapper
  "Deno.stdout.write(new TextEncoder().encode(Deno.inspect((() => {%s})())));"
  "Javascript/TypeScript code to print value of body.")

(defun org-babel-execute:deno (body params)
  "Execute a block of Javascript/TypeScript code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((no-color-env (getenv "NO_COLOR"))
	 (ob-deno-cmd (or (cdr (assq :cmd params)) (format "%s run" ob-deno-cmd)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:deno params)))
	 (result (let ((script-file (org-babel-temp-file "deno-script-")))
		   (with-temp-file script-file
		     (insert
		      ;; return the value or the output
		      (if (string= result-type "value")
			  (format ob-deno-function-wrapper full-body)
			full-body)))
		   (setenv "NO_COLOR" "true")
		   (org-babel-eval
		    (format "%s %s" ob-deno-cmd
			    (org-babel-process-file-name script-file)) ""))))
    (setenv "NO_COLOR" no-color-env)
    (org-babel-result-cond (cdr (assq :result-params params))
      result (ob-deno-read result))))

(defun ob-deno-read (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results)
	    (string-prefix-p "[" results)
	    (string-suffix-p "]" results))
       (org-babel-read
        (concat "'"
                (replace-regexp-in-string
                 "\\[" "(" (replace-regexp-in-string
                            "\\]" ")" (replace-regexp-in-string
                                       ",[[:space:]]" " "
				       (replace-regexp-in-string
					"'" "\"" results))))))
     results)))

(defun ob-deno-var-to-deno (var)
  "Convert VAR into a js/ts variable.
Convert an elisp value into a string of js/ts source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'ob-deno-var-to-deno var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun org-babel-variable-assignments:deno (params)
  "Return list of Javascript/TypeScript statements assigning the block's variables."
  (mapcar
   (lambda (pair) (format "var %s=%s;"
			  (car pair) (ob-deno-var-to-deno (cdr pair))))
   (org-babel--get-vars params)))

(provide 'ob-deno)

;;; ob-deno.el ends here
