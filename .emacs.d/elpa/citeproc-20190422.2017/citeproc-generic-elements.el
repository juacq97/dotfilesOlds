;;; citeproc-generic-elements.el --- render generic CSL elements -*- lexical-binding: t; -*-

;; Copyright (C) 2017 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functions corresponding to the generic CSL rendering elements cs:layout,
;; cs:group and cs:text. With the exception of `citeproc--layout' they return a
;; (CONTENT . CONTENT-TYPE) pair, where CONTENT is the rendered rich-text output
;; and CONTENT-TYPE is one of the symbols `text-only', `empty-vars' and
;; `present-var.' In contrast, `citeproc--layout' returns only the rendered
;; rich-text content.

;;; Code:

(require 'dash)
(require 'let-alist)

(require 'citeproc-rt)
(require 'citeproc-context)
(require 'citeproc-macro)

(defun citeproc--layout (attrs context &rest body)
  "Render the content of a layout element with ATTRS and BODY."
  (let* ((attrs (if (eq (citeproc-context-mode context) 'bib) attrs
		  nil)) ; No attrs if mode is cite -- they are used for citations
	 (spliced-body (citeproc-lib-splice-into body 'splice))
	 (suffix (alist-get 'suffix attrs))
	 (attrs-wo-suffix (--remove (eq (car it) 'suffix) attrs))
	 (rendered-body (--map (car it) spliced-body)))
    ;; Handle second-field align
    (when (and (alist-get 'second-field-align (citeproc-context-opts context))
	       (> (length rendered-body) 1))
      (setq rendered-body `((((display . "left-margin")) ,(car rendered-body))
			    (((display . "right-inline")) ,@(cdr rendered-body)))))
    (let* ((affix-rendered (citeproc-rt-render-affixes
			    (citeproc-rt-dedup (citeproc-rt-join-formatted attrs-wo-suffix rendered-body
									   context))))
	   (last-elt (and (listp affix-rendered) (-last-item affix-rendered)))
	   (last-display (and (consp last-elt) (car last-elt)
			      (--any-p (eq (car it) 'display) (car last-elt)))))
      (when suffix
	(if last-display
	    ;; If the last element has a display attribute then we have to append the
	    ;; suffix string to this last element
	    (setf (nth (1- (length affix-rendered)) affix-rendered)
		  (-snoc (-last-item affix-rendered) suffix))
	  ;; Else we simply append it to the already rendered content
	  (setq affix-rendered (if (listp affix-rendered)
				   (-snoc affix-rendered suffix)
				 (concat affix-rendered suffix)))))
      affix-rendered)))

(defun citeproc--group (attrs context &rest body)
  "Render the content of a group element with ATTRS and BODY."
  (-let* ((spliced-body (citeproc-lib-splice-into body 'splice))
	  (types (--map (cdr it) spliced-body))
	  (type (cond ((--all? (eq it 'text-only) types)
		       'text-only)
		      ((--any? (eq it 'present-var) types)
		       'present-var)
		      (t 'empty-vars))))
    (cons (if (or (eq type 'text-only)
		  (eq type 'present-var))
	      (citeproc-rt-join-formatted attrs (--map (car it) spliced-body) context)
	    nil)
	  type)))

(defun citeproc--text (attrs context &rest _body)
  "Render the content of a text element with ATTRS and BODY."
  (let-alist attrs
    (let ((content nil)
	  (type 'text-only))
      (cond (.value (setq content .value))
	    (.variable
	     (let ((val (citeproc-var-value (intern .variable) context (citeproc-lib-intern
									.form))))
	       (setq content val)
	       (if val
		   (progn
		     (setq type 'present-var)
		     (push `(rendered-var . ,(intern .variable)) attrs))
		 (setq type 'empty-vars))))
	    (.term (setq .form (if .form (intern .form) 'long)
			 .plural (if (or (not .plural)
					 (string= .plural "false"))
				     'single 'multiple)
			 content (citeproc-term-inflected-text .term .form .plural context)))
	    (.macro (let ((macro-val (citeproc-macro-output .macro context)))
		      (setq content (car macro-val))
		      (setq type (cdr macro-val)))))
      (cons (citeproc-rt-format-single attrs content context) type))))

(provide 'citeproc-generic-elements)

;;; citeproc-generic-elements.el ends here
