;; citeproc-formatters.el --- output formatters -*- lexical-binding: t; -*-

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

;; Provides a general framework for defining citeproc-el output formatters, and,
;; using this framework, defines formatters for the formats `raw' (the rich-text
;; internal representation), `plain' (plain-text), `html', `org' and `latex'.
;; Further output formatters can easily be added.

;;; Code:

(require 'subr-x)
(require 's)
(require 'cl-lib)

(cl-defstruct (citeproc-formatter (:constructor citeproc-formatter-create))
  "Output formatter struct with slots RT, CITE, BIB-ITEM and BIB.
RT is a one-argument function mapping a rich-text to its
  formatted version,
CITE is a one-argument function mapping the output of RT for a
  citation rich-text to a fully formatted citation,
BIB-ITEM is a two-argument function mapping the output of RT for
  a bibliography-item rich-text and a BIB-FORMAT alist (see
  below) to a fully formatted bibliography item,
BIB is a two-argument function mapping a list of formatted
  bibliography items and a FORMATTING-PARAMETERS alist (see
  `citeproc-render-bib' for details) to a fully formatted
  bibliography."
  rt (cite #'identity) (bib-item (lambda (x _) x))
  (bib (lambda (x _) (mapconcat #'identity x "\n\n"))))

(defun citeproc-formatter-fun-create (fmt-alist)
  "Return a rich-text formatter function based on FMT-ALIST.
FMT-ALIST is an alist with some or all of the following symbols
as keys:
- unformatted
- font-style-italic, font-style-oblique, font-style-normal
- font-variant-small-caps, font-variant-normal
- font-weight-bold, font-weight-light, font-weight-normal
- text-decoration-underline, text-decoration-normal
- vertical-align-sub, vertical-align-sup, vertical-align-baseline
- display-block, display-left-margin, display-right-inline,
  display-indent
- rendered-var-url
- cited-item-no, bib-item-no
with the exceptions listed below the values should be
one-argument formatting functions that format the input string
according to the attribute-value pair specified by the key.
  The exceptions are the keys `unformatted', for which the value
should be a one-argument function converting unformatted text
into the required format (e.g., by escaping), and `cited-item-no'
and `bib-item-no' whose associated values should be two-argument
functions, which are called with the already formatted
cites/bibliography item text and the number of the bibliography
item as a string."
  (cl-labels
      ((rt-fmt
	(rt)
	(pcase rt
	  ((pred stringp) (funcall (alist-get 'unformatted fmt-alist) rt))
	  ((pred consp)
	   (let ((attrs (car rt))
		 (result (mapconcat #'rt-fmt (cdr rt) "")))
	     (dolist (attr attrs)
	       (let ((key (car attr)))
		 (if (or (eq key 'cited-item-no) (eq key 'bib-item-no))
		     (-when-let (fmt-fun (alist-get key fmt-alist))
		       (setq result (funcall fmt-fun result (cdr attr))))
		   (-when-let
		       (fmt-fun
			(alist-get
			 (pcase attr
			   ('(font-style . "italic") 'font-style-italic)
			   ('(font-weight . "bold") 'font-weight-bold)
			   ('(rendered-var . URL) 'rendered-var-url)
			   ('(display . "indent") 'display-indent)
			   ('(display . "left-margin") 'display-left-margin)
			   ('(display . "right-inline") 'display-right-inline)
			   ('(display . "block") 'display-block)
			   ('(vertical-align . "sup") 'vertical-align-sup)
			   ('(vertical-align . "baseline") 'vertical-align-baseline)
			   ('(font-variant . "small-caps") 'font-variant-small-caps)
			   ('(text-decoration . "underline") 'text-decoration-underline)
			   ('(font-style . "oblique") 'font-style-oblique)
			   ('(font-style . "normal") 'font-style-normal)
			   ('(font-variant . "normal") 'font-variant-normal)
			   ('(font-weight . "light") 'font-weight-light)
			   ('(font-weight . "normal") 'font-weight-normal)
			   ('(text-decoration . "normal") 'text-decoration-normal)
			   ('(vertical-align . "sub") 'vertical-align-sub))
			 fmt-alist))
		     (setq result (funcall fmt-fun result))))))
	     result))
	  (_ rt))))
    #'rt-fmt))

;;;; Specific formatters

;; Org

(defconst citeproc-fmt--org-alist
  `((unformatted . identity)
    (cited-item-no . ,(lambda (x y) (concat "[[citeproc_bib_item_" y "][" x "]]")))
    (bib-item-no . ,(lambda (x y) (concat "<<citeproc_bib_item_" y ">>" x)))
    (font-style-italic . ,(lambda (x) (concat "/" x "/")))
    (font-style-oblique . ,(lambda (x) (concat "/" x "/")))
    (font-variant-small-caps . ,(lambda (x) (upcase x)))
    (font-weight-bold . ,(lambda (x) (concat "*" x "*")))
    (text-decoration-underline . ,(lambda (x) (concat "_" x "_")))
    (vertical-align-sub . ,(lambda (x) (concat "_{" x "}")))
    (vertical-align-sup . ,(lambda (x) (concat "^{" x "}")))
    (display-left-margin . ,(lambda (x) (concat x " ")))))

;; HTML

(defun citeproc-fmt--xml-escape (s)
  "Return the xml-escaped version of string S.
Only '&', '<' and '>' are escaped to keep compatibility with the
CSL tests."
  (s-replace-all '(("&" . "&#38;") ("<" . "&#60;") (">" . "&#62;"))
		 s))

(defconst citeproc-fmt--html-alist
  `((unformatted . citeproc-fmt--xml-escape)
    (cited-item-no . ,(lambda (x y) (concat "<a href=\"#citeproc_bib_item_" y "\">"
					    x "</a>")))
    (bib-item-no . ,(lambda (x y) (concat "<a name=\"citeproc_bib_item_" y "\"></a>"
					  x)))
    (font-style-italic . ,(lambda (x) (concat "<i>" x "</i>")))
    (font-style-oblique . ,(lambda (x)
			     (concat "<span style=\"font-style:oblique;\"" x "</span>")))
    (font-variant-small-caps . ,(lambda (x)
				  (concat
				   "<span style=\"font-variant:small-caps;\">" x "</span>")))
    (font-weight-bold . ,(lambda (x) (concat "<b>" x "</b>")))
    (text-decoration-underline .
     ,(lambda (x)
	(concat
	 "<span style=\"text-decoration:underline;\">" x "</span>")))
    (rendered-var-url . ,(lambda (x) (concat "<a href=\"" x "\">" x "</a>")))
    (vertical-align-sub . ,(lambda (x) (concat "<sub>" x "</sub>")))
    (vertical-align-sup . ,(lambda (x) (concat "<sup>" x "</sup>")))
    (vertical-align-baseline . ,(lambda (x) (concat "<span style=\"baseline\">" x "</span>")))
    (display-left-margin . ,(lambda (x) (concat "\n    <div class=\"csl-left-margin\">"
						x "</div>")))
    (display-right-inline . ,(lambda (x) (concat "<div class=\"csl-right-inline\">"
						 x "</div>\n  ")))
    (display-block . ,(lambda (x) (concat "\n\n    <div class=\"csl-block\">"
					  x "</div>\n")))
    (display-indent . ,(lambda (x) (concat "<div class=\"csl-indent\">" x "</div>\n  ")))))

(defconst citeproc-fmt--csl-test-alist
  `((unformatted . citeproc-fmt--xml-escape)
    (cited-item-no . ,(lambda (x y) (concat "<a href=\"#citeproc_bib_item_" y "\">"
					    x "</a>")))
    (bib-item-no . ,(lambda (x y) (concat "<a name=\"citeproc_bib_item_" y "\"></a>"
					  x)))
    (font-style-italic . ,(lambda (x) (concat "<i>" x "</i>")))
    (font-style-oblique . ,(lambda (x)
			     (concat "<span style=\"font-style:oblique;\"" x "</span>")))
    (font-variant-small-caps . ,(lambda (x)
				  (concat
				   "<span style=\"font-variant:small-caps;\">" x "</span>")))
    (font-weight-bold . ,(lambda (x) (concat "<b>" x "</b>")))
    (text-decoration-underline .
     ,(lambda (x)
	(concat
	 "<span style=\"text-decoration:underline;\">" x "</span>")))
    (vertical-align-sub . ,(lambda (x) (concat "<sub>" x "</sub>")))
    (vertical-align-sup . ,(lambda (x) (concat "<sup>" x "</sup>")))
    (vertical-align-baseline . ,(lambda (x) (concat "<span style=\"baseline\">" x "</span>")))
    (display-left-margin . ,(lambda (x) (concat "\n    <div class=\"csl-left-margin\">"
						x "</div>")))
    (display-right-inline . ,(lambda (x) (concat "<div class=\"csl-right-inline\">"
						 x "</div>\n  ")))
    (display-block . ,(lambda (x) (concat "\n\n    <div class=\"csl-block\">"
					  x "</div>\n")))
    (display-indent . ,(lambda (x) (concat "<div class=\"csl-indent\">" x "</div>\n  ")))))

(defun citeproc-fmt--html-bib-formatter (items _bib-format)
  "Return a html bibliography from already formatted ITEMS."
  (concat "<div class=\"csl-bib-body\">\n"
	  (mapconcat (lambda (i)
		       (concat "  <div class=\"csl-entry\">" i "</div>\n"))
		     items
		     "")
	  "</div>"))

;; LaTeX

(defun citeproc-fmt--latex-escape (s)
  "Return the LaTeX-escaped version of string S."
  (s-replace-all '(("_" . "\\_") ("{" . "\\{") ("}" . "\\}") ("&" . "\\&")) s))

(defconst citeproc-fmt--latex-alist
  `((unformatted . citeproc-fmt--latex-escape)
    (font-style-italic . ,(lambda (x) (concat "\\textit{" x "}")))
    (font-weight-bold . ,(lambda (x) (concat "\\textbf{" x "}")))
    (cited-item-no . ,(lambda (x y) (concat "\\hyperlink{citeproc_bib_item_" y "}{"
					    x "}")))
    (bib-item-no . ,(lambda (x y) (concat "\\hypertarget{citeproc_bib_item_" y "}{"
					  x "}")))
    (font-variant-small-caps . ,(lambda (x) (concat "\\textsc{" x "}")))
    (text-decoration-underline . ,(lambda (x) (concat "\\underline{" x "}")))
    (vertical-align-sup . ,(lambda (x) (concat "^{" x "}")))
    (rendered-var-url . ,(lambda (x) (concat "\\url{" x "}")))
    (display-left-margin . ,(lambda (x) (concat x " ")))
    (vertical-align-sub . ,(lambda (x) (concat "_{" x "}")))
    (font-style-oblique . ,(lambda (x) (concat "\\textsl{" x "}")))))

;; Define the formatters alist

 (defvar citeproc-fmt--formatters-alist
   `((html . ,(citeproc-formatter-create
	       :rt (citeproc-formatter-fun-create citeproc-fmt--html-alist)
	       :bib #'citeproc-fmt--html-bib-formatter))
     (csl-test . ,(citeproc-formatter-create
		   :rt (citeproc-formatter-fun-create citeproc-fmt--csl-test-alist)
		   :bib #'citeproc-fmt--html-bib-formatter))
     (raw . ,(citeproc-formatter-create :rt #'identity :bib (lambda (x _) x)))
     (org . ,(citeproc-formatter-create
	      :rt (citeproc-formatter-fun-create citeproc-fmt--org-alist)))
     (latex . ,(citeproc-formatter-create
		:rt (citeproc-formatter-fun-create citeproc-fmt--latex-alist)))
     (plain . ,(citeproc-formatter-create :rt #'citeproc-rt-to-plain)))
   "Alist mapping supported output formats to formatter structs.")

(defun citeproc-formatter-for-format (format)
  "Return the formatter struct belonging to FORMAT.
FORMAT is a symbol"
  (-if-let (formatter (alist-get format citeproc-fmt--formatters-alist))
      formatter
    (error "No formatter for citeproc format `%s'" format)))

(provide 'citeproc-formatters)

;;; citeproc-formatters.el ends here
