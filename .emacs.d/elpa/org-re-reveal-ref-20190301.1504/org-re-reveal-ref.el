;;; org-re-reveal-ref.el --- Citations and bibliography for org-re-reveal  -*- lexical-binding: t; -*-
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2018-2019 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jens Lechtenbörger
;; URL: https://gitlab.com/oer/org-re-reveal-ref
;; Version: 0.9.1
;; Package-Requires: ((emacs "24.4") (org-ref "1.1.1") (org-re-reveal "0.9.3"))
;; Keywords: hypermedia, tools, slideshow, presentation, bibliography

;;; License:
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see http://www.gnu.org/licenses/ or write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package extends `org-re-reveal' with support for a
;; bibliography slide based on package `org-ref'.  Thus, `cite'
;; commands of `org-ref' are translated into hyperlinks to the
;; bibliography slide upon export by `org-re-reveal'.  Also, export to
;; PDF via LaTeX and export to HTML with Org's usual export
;; functionality work.
;;
;; * Install
;; 0. Install reveal.js: https://revealjs.com/
;; 1. Activate org-re-reveal-ref.
;;    (a) Place this directory into your load path or install it from MELPA
;;        (https://melpa.org/#/getting-started).
;;    (b) Load package manually ("M-x load-library" followed by
;;        "org-re-reveal-ref") or place "(require 'org-re-reveal-ref)" into
;;        your ~/.emacs and restart.
;; 2. Load an Org file and export it to HTML.
;;    (a) Make sure that reveal.js is available in your current directory
;;        (e.g., as sub-directory or symbolic link).
;;    (b) Load "README.org" (coming with org-re-reveal-ref).
;;    (c) Export to HTML: Key bindings depend upon version of org-re-reveal.
;;        Starting with version 1.0.0, press "C-c C-e v r" (write HTML file)
;;        or "C-c C-e v b" (write HTML file and open in browser)
;;
;; * Customizable options
;; Customizable variables are `org-re-reveal-ref-bib' and
;; `org-re-reveal-ref-class'.
;; The value of `org-re-reveal-ref-bib' is used to generate hyperlinks
;; to the bibliography.  You must use its value as CUSTOM_ID on your
;; bibliography slide.
;; The value of `org-re-reveal-ref-class' is assigned as class
;; attribute of hyperlinks to the bibliography slide.
;; Furthermore, the following variables of `org-ref' are changed by
;; this package:
;; - `org-ref-bib-html' is set to the empty string
;; - `org-ref-printbibliography-cmd' is configured not to produce a
;;   heading (as the bibliography slide has a heading already)
;; - `org-ref-ref-html' is configured to link to the bibliography

;;; Code:
(require 'org-ref)
(require 'org-re-reveal)

(defcustom org-re-reveal-ref-bib "bibliography"
  "Specify name for link targets generated from citations.
Use that name as CUSTOM_ID for your bibliography slide."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-ref-class "org-ref-reference"
  "Specify class of hyperlinks generated from citations.
Set to empty string if no class should be assigned."
  :group 'org-export-re-reveal
  :type 'string)

(defun org-re-reveal-ref-filter-bib-para (text backend info)
  "Replace incorrect p tags around bibliography.
This function is added to `org-export-filter-paragraph-functions',
where TEXT is the paragraph, BACKEND is checked for `re-reveal' or
`html', and INFO is unused."
  (ignore info) ; Silence byte compiler
  (when (and (or (org-export-derived-backend-p backend 're-reveal)
		 (org-export-derived-backend-p backend 'html))
	     (string-match-p "<p>[ \n]*<ul" text))
    (replace-regexp-in-string
     "<p>[ \n]*<ul" "<ul"
     (replace-regexp-in-string "</p>\n" "" text))))
(add-to-list 'org-export-filter-paragraph-functions
	     #'org-re-reveal-ref-filter-bib-para)

(setq org-ref-bib-html ""
      org-ref-printbibliography-cmd "\\printbibliography[heading=none]"
      org-ref-ref-html (concat
			"<a"
			(if (< 0 (length org-re-reveal-ref-class))
			    (format " class=\"%s\"" org-re-reveal-ref-class)
			  "")
			" href=\"#"
			org-re-reveal--href-fragment-prefix
			org-re-reveal-ref-bib
			"\">[%s]</a>"))

(provide 'org-re-reveal-ref)
;;; org-re-reveal-ref.el ends here
