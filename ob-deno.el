;;; ob-deno.el --- Babel Functions for Javascript/TypeScript with Deno      -*- lexical-binding: t; -*-

;; Copyright (C) 2020 HIGASHI Taiju

;; Author: HIGASHI Taiju
;; Keywords: literate programming, reproducible research, javascript, typescript, tools
;; Homepage: https://github.com/taiju/ob-deno
;; Version: 1.0.1
;; Package-Requires: ((emacs "26.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ob-deno is babel functions for Javascript/TypeScript with Deno.  It's based on ob-js.

;; Parameters supported:
;;   - :cmd
;;   - :result-type
;;   - :var (You can specify a variable prefix with ob-deno-variable-prefix.)
;; Parameters not supported:
;;   - :session
;; Original Parameters:
;;   - :allow (Specifies a permission list for the deno command)

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

(defcustom ob-deno-variable-prefix "const"
  "Type of variable prefix."
  :group 'ob-deno
  :type '(choice (const "const")
                 (const "let")
                 (const "var"))
  :safe #'stringp)

(defcustom ob-deno-function-wrapper
  "Deno.stdout.write(new TextEncoder().encode(Deno.inspect(await (async () => {%s})())));"
  "Javascript/TypeScript code to print value of body.
%s is replaced with code body, without the imports.  Imports are
 injected to the beginning of the file."
  :group 'ob-deno
  :type 'string)

(defconst ob-deno--treesit-imports-query
  (treesit-query-compile
   'typescript
   "[(import_statement) @import]")
  "Treesit query to find import statements for given typescript code.")

(defun ob-deno--split-imports-and-rest (body)
  "Split BODY into import statements and the rest of the lines return them."
  (with-temp-buffer
    (insert body)
    (let* ((root-node (treesit-buffer-root-node 'typescript))
           (imports (mapcar #'cdr (treesit-query-capture root-node ob-deno--treesit-imports-query)))
           (import-end-pos (if imports (treesit-node-end (car (last imports))) 0)))
      (list
       ;; :imports
       (string-join (--map (treesit-node-text it t) imports) "\n")
       ;; :rest
       (save-excursion
         (goto-char import-end-pos)
         (buffer-substring-no-properties
          (point)
          (point-max)))))))

(defun ob-deno--expand-body (imports rest params)
  (concat
   imports
   "\n\n"
   (org-babel-expand-body:generic
    rest params (org-babel-variable-assignments:deno params))))

(defun org-babel-expand-body:deno (body params var-lines)
  "Expand BODY with PARAMS.
This takes care of injecting parameters after the imports so that
produced code is a valid TypeScript code."
  (pcase-let* ((`(,imports ,rest) (ob-deno--split-imports-and-rest body)))
    (ob-deno--expand-body imports rest params)))

(defun org-babel-execute:deno (body params)
  "Execute a block of Javascript/TypeScript code in `BODY' with org-babel.
  You can also specify parameters in `PARAMS'.
  This function is called by `org-babel-execute-src-block'."
  (pcase-let* ((no-color-env (getenv "NO_COLOR"))
               (ob-deno-cmd (or (cdr (assq :cmd params)) (format "%s run" ob-deno-cmd)))
               (allow (ob-deno-allow-params (cdr (assq :allow params))))
               (ob-deno-cmd-with-permission (concat ob-deno-cmd " " allow))
               (result-type (cdr (assq :result-type params)))
               (`(,imports ,rest) (ob-deno--split-imports-and-rest body))
               (result (let ((script-file (concat (org-babel-temp-file "deno-script-") ".ts")))
                         (with-temp-file script-file
                           (insert
                            (ob-deno--expand-body
                             imports
                             ;; return the value or the output
                             (if (string= result-type "value")
                                 (format ob-deno-function-wrapper rest)
                               rest)
                             params)))
                         (setenv "NO_COLOR" "true")
                         (org-babel-eval
                          (format "%s %s" ob-deno-cmd-with-permission
                                  (org-babel-process-file-name script-file)) ""))))
    (setenv "NO_COLOR" no-color-env)
    (org-babel-result-cond (cdr (assq :result-params params))
      result (ob-deno-read result))))

(defun ob-deno-allow-params (allow-params)
  "Convert ALLOW-PARAMS to deno's allow-list parameter."
  (if (listp allow-params)
      (mapconcat #'ob-deno-allow-param-to-allow-list-str allow-params " ")
    (ob-deno-format-allow-param allow-params)))

(defun ob-deno-allow-param-to-allow-list-str (allow-param)
  "Convert ALLOW-PARAM to deno's allow-list parameter string."
  (if (listp allow-param)
      (ob-deno-format-allow-param (car allow-param) (cdr allow-param))
    (ob-deno-format-allow-param allow-param)))

(defun ob-deno-format-allow-param (allow-param &optional values)
  "Format ALLOW-PARAM to allow-list.
You can also specify values for the allow-list,
which can be specified by VALUES."
  (if values
      (format "--allow-%s=%s" allow-param (mapconcat (lambda (s) (format "%s" s)) values ","))
    (format "--allow-%s" allow-param)))

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
  "Return list of Javascript/TypeScript statements assigning the block's variables in PARAMS."
  (mapcar
   (lambda (pair) (format "%s %s = %s;"
                     ob-deno-variable-prefix (car pair) (ob-deno-var-to-deno (cdr pair))))
   (org-babel--get-vars params)))

(provide 'ob-deno)

;;; ob-deno.el ends here
