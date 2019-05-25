;;; citeproc-org.el --- Render org-mode references in CSL styles -*- lexical-binding: t; -*-

;; Copyright (C) 2018 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>
;; Maintainer: András Simonyi <andras.simonyi@gmail.com>
;; URL: https://github.com/andras-simonyi/citeproc-org
;; Keywords: org-ref, org-mode, cite, bib
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (org "9") (f "0.18.0") (citeproc "0.1") (org-ref "1.1.1"))
;; Version: 0.2.2

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

;; Functions to render org-mode bibliographic references in Citation Style
;; Language (CSL) styles using the citeproc-el library. See the accompanying
;; README for full documentation.

;;; Code:

(require 's)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'dash)
(require 'map)
(require 'f)
(require 'let-alist)
(require 'org-ref)

(require 'citeproc)

(defgroup citeproc-org nil
  "Settings for rendering org-mode citations in CSL styles."
  :tag "Citeproc Org"
  :group 'org)

(defcustom citeproc-org-default-style-file nil
  "Default CSL style file.
If nil then the chicago-author-date style is used as a fallback."
  :type 'file
  :group 'citeproc-org)

(defcustom citeproc-org-locales-dir nil
  "Directory of CSL locale files.
If nil then only the fallback en-US locale will be available."
  :type 'dir
  :group 'citeproc-org)

(defcustom citeproc-org-html-bib-header
  "<h2 class='citeproc-org-bib-h2'>Bibliography</h2>\n"
  "HTML bibliography header to use for HTML export."
  :type 'string
  :group 'citeproc-org)

(defcustom citeproc-org-latex-bib-header "\\section*{Bibliography}\n\n"
  "LaTeX bibliography header to use for LaTeX export."
  :type 'string
  :group 'citeproc-org)

(defcustom citeproc-org-org-bib-header "* Bibliography\n"
  "Org bibliography header to use for non-HTML and non-LaTeX export."
  :type 'string
  :group 'citeproc-org)

(defcustom citeproc-org-suppress-affixes-cite-link-types '("citealt")
  "Suppress citation affixes for these cite link types."
  :type '(repeat :tag "List of citation link types" string)
  :group 'citeproc-org)

(defcustom citeproc-org-suppress-author-cite-link-types '("citeyear")
  "Suppress author for these cite link types."
  :type '(repeat :tag "List of citation link types" string)
  :group 'citeproc-org)

(defcustom citeproc-org-link-cites t
  "Link cites to references."
  :type 'boolean
  :group 'citeproc-org)

(defcustom citeproc-org-bibtex-export-use-affixes nil
  "Use separate prefix and suffix cite arguments for LaTeX export.
Some BibTeX packages (notably, NatBib) support separate prefix
and postfix arguments. If non-nil then affixes will be passed as
separate arguments."
  :type 'boolean
  :group 'citeproc-org)

(defcustom citeproc-org-html-backends '(html twbs)
  "Use the HTML formatter for these org export backends."
  :type '(repeat symbol)
  :group 'citeproc-org)

(defcustom citeproc-org-latex-backends '(latex beamer)
  "Use the LaTeX formatter for these org export backends."
  :type '(repeat symbol)
  :group 'citeproc-org)

(defcustom citeproc-org-no-citelinks-backends '(ascii)
  "Backends for which cite linking should always be turned off."
  :type '(repeat symbol)
  :group 'citeproc-org)

(defcustom citeproc-org-ignore-backends '(latex beamer)
  "List of backends whose output shouldn't be processed by citeproc-org."
  :type '(repeat symbol)
  :group 'citeproc-org)

(defcustom citeproc-org-html-hanging-indent "1.5em"
  "The size of hanging-indent for HTML ouput in valid CSS units.
Used only when hanging-indent is activated by the used CSL
style."
  :type 'string
  :group 'citeproc-org)

(defcustom citeproc-org-html-label-width-per-char "0.6em"
  "Character width in CSS units for calculating entry label widths.
Used only when second-field-align is activated by the used CSL
style."
  :type 'string
  :group 'citeproc-org)

(defcustom citeproc-org-latex-hanging-indent "1.5em"
  "The size of hanging-indent for LaTeX ouput in valid LaTeX units.
Always used for LaTeX output."
  :type 'string
  :group 'citeproc-org)

(defvar citeproc-org--proc-cache nil
  "Cached citeproc processor for citeproc-org.
Its value is either nil or a list of the form
\(PROC STYLE-FILE BIBTEX-FILE LOCALE).")

(defconst citeproc-org--load-dir (f-dirname load-file-name)
  "The dir from which this file was loaded.")

(defconst citeproc-org--fallback-style-file
  (f-join citeproc-org--load-dir  "styles" "chicago-author-date.csl")
  "Default CSL style file.")

(defconst citeproc-org--fallback-locales-dir
  (f-join citeproc-org--load-dir "locales")
  "Directory of CSL locale files.")

(defconst citeproc-org--label-alist
  '(("bk." . "book")
    ("bks." . "book")
    ("book" . "book")
    ("chap." . "chapter")
    ("chaps." . "chapter")
    ("chapter" . "chapter")
    ("col." . "column")
    ("cols." . "column")
    ("column" . "column")
    ("figure" . "figure")
    ("fig." .  "figure")
    ("figs." .  "figure")
    ( "folio" . "folio")
    ("fol." .  "folio")
    ("fols." .  "folio")
    ("number" . "number")
    ("no." .  "number")
    ("nos." .  "number")
    ("line" . "line")
    ("l." .  "line")
    ("ll." .  "line")
    ("note" . "note")
    ("n." .  "note")
    ("nn." .  "note")
    ("opus" . "opus")
    ("op." .  "opus")
    ("opp." .  "opus")
    ("page" . "page")
    ("p." .  "page")
    ("pp." .  "page")
    ("paragraph" . "paragraph")
    ("para." .  "paragraph")
    ("paras." .  "paragraph")
    ("¶" . "paragraph")
    ("¶¶" . "paragraph")
    ("§" . "paragraph")
    ("§§" . "paragraph")
    ("part" . "part")
    ("pt." .  "part")
    ("pts." .  "part")
    ("section" . "section")
    ("sec." .  "section")
    ("secs." .  "section")
    ("sub verbo" . "sub verbo")
    ("s.v." .  "sub verbo")
    ("s.vv." . "sub verbo")
    ("verse" . "verse")
    ("v." .  "verse")
    ("vv." .  "verse")
    ("volume" . "volume")
    ("vol." .  "volume")
    ("vols." .  "volume"))
  "Alist mapping locator names to locators.")

(defconst citeproc-org--label-regex
  (let ((labels (map-keys citeproc-org--label-alist)))
    (concat "\\<\\("
	    (mapconcat (lambda (x) (s-replace "." "\\." x))
		       labels "\\|")
	    "\\)[ $]")))

;;; Utility functions

(defun citeproc-org--element-boundaries (element)
  "Return the boundaries of an org ELEMENT.
Returns a (BEGIN END) list -- post-blank positions are not
considered when calculating END."
  (let ((begin (org-element-property :begin element))
	(end (org-element-property :end element))
	(post-blank (org-element-property :post-blank element)))
    (list begin (- end post-blank))))

(defun citeproc-org--fn-pos (elt)
  "Return info about the footnote position of org element ELT.
The returned value is
- nil if ELT is not in a footnote,
- t if ELT is in an unlabelled footnote
- and the footnote label if it is in a labelled footnote."
  (let ((curr (org-element-property :parent elt))
	result)
    (while (and curr (not result))
      (when (memq (org-element-type curr)
		  '(footnote-definition footnote-reference))
	(setq result (or (org-element-property :label curr) t)))
      (setq curr (org-element-property :parent curr)))
    result))

(defun citeproc-org--get-option-val (opt)
  "Return the value of org mode option OPT."
  (goto-char (point-min))
  (if (re-search-forward
       (concat "^#\\+" opt ":\\(.+\\)$")
       nil t)
      (let* ((match (match-data))
	     (start (elt match 2))
	     (end (elt match 3)))
	(s-trim (buffer-substring-no-properties start end)))
    nil))

;;; Org-ref and org citation parsers

(defun citeproc-org--parse-locator-affix (s)
  "Parse string S as a cite's locator and affix description.
Return the parse as an alist with `locator', `label', `prefix'
and `suffix' keys."
  (if (s-blank-p s) nil
    (let ((label-matches (s-matched-positions-all citeproc-org--label-regex s 1))
	  (digit-matches (s-matched-positions-all "\\<\\w*[[:digit:]]+" s))
	  (comma-matches (s-matched-positions-all "," s))
	  label locator prefix suffix location)
      (let ((last-comma-pos (and comma-matches
				 (cdr (-last-item comma-matches)))))
	(if (or label-matches digit-matches)
	    (let (label-exp loc-start loc-end)
	      (if (null label-matches)
		  (setq loc-start (caar digit-matches)
			loc-end (cdr (-last-item digit-matches))
			label "page")
		(setq label-exp (substring s (caar label-matches) (cdar label-matches))
		      label (assoc-default label-exp citeproc-org--label-alist))
		(if (null digit-matches)
		      (setq loc-start (caar label-matches)
			    loc-end (cdr (-last-item label-matches)))
		    (setq loc-start (min (caar label-matches) (caar digit-matches))
			  loc-end (max (cdr (-last-item label-matches))
				       (cdr (-last-item digit-matches))))))
	      (when (> loc-start 0) (setq prefix (substring s 0 loc-start)))
	      (if (and last-comma-pos (> last-comma-pos loc-end))
		  (setq suffix (substring s last-comma-pos)
			loc-end (1- last-comma-pos))
		(setq loc-end nil))
	      (setq location (substring s loc-start loc-end)
		    locator (if label-exp (s-replace label-exp "" location) location)
		    locator (s-trim locator)))
	  (if last-comma-pos
	      (setq prefix (substring s 0 (1- last-comma-pos))
		    suffix (substring s last-comma-pos))
	    (setq prefix s))))
      `((locator . ,locator) (label . ,label) (location . ,location)
	(prefix . ,prefix) (suffix . ,suffix)))))

(defun citeproc-org--parse-orgref-link (link footnote-no new-fn
					     &optional capitalize-outside-fn)
  "Return a citeproc citation corresponding to an `org-ref' cite LINK.
FOOTNOTE-NO is nil if LINK is not in a footnote or the number of
the link's footnote. If NEW-FN is non-nil the the link was not in
a footnote. If CAPITALIZE-OUTSIDE-FN is non-nil then set the
`capitalize-first' slot of the citation struct to t when the link
is not in a footnote."
  (let* ((type (org-element-property :type link))
	 (path (org-element-property :path link))
	 (content (let ((c-begin (org-element-property :contents-begin link))
			(c-end (org-element-property :contents-end link)))
		    (if (and c-begin c-end)
			(buffer-substring-no-properties c-begin c-end)
		      nil)))
	 (itemids (split-string path ","))
	 (cites-ids (--map (cons 'id it)
			   itemids)))
    (citeproc-citation-create
     :note-index footnote-no
     :cites
     (let ((cites
	    (if content
		(let* ((cites-rest (mapcar #'citeproc-org--parse-locator-affix
					   (split-string content ";")))
		       (cites-no (length cites-ids))
		       (rest-no (length cites-rest))
		       (diff (- cites-no rest-no))
		       (cites-rest-filled
			(let* ()
			  (if (> diff 0)
			      (-concat cites-rest (make-list diff nil))
			    cites-rest))))
		  (-zip cites-ids cites-rest-filled))
	      (mapcar #'list cites-ids))))
       (if (member type citeproc-org-suppress-author-cite-link-types)
	   (cons (cons '(suppress-author . t) (car cites)) (cdr cites))
	 cites))
     :capitalize-first (and capitalize-outside-fn
			    new-fn)
     :suppress-affixes (member type
			       citeproc-org-suppress-affixes-cite-link-types))))

;; TODO: Deal with the common prefix and suffix
;; NOTE: There is no way to express author suppression in the present org
;; citation syntax
(defun citeproc-org--parse-org-cite (cite footnote-no new-fn
					  &optional capitalize-outside-fn)
  "Return a citeproc citation corresponding to an org CITE.
FOOTNOTE-NO is nil if LINK is not in a footnote or the number of
the link's footnote. If NEW-FN is non-nil the the link was not in
a footnote. If CAPITALIZE-OUTSIDE-FN is non-nil then set the
`capitalize-first' slot of the citation struct to t when the link
is not in a footnote."
  (citeproc-citation-create
   :note-index footnote-no
   :cites
   (let ((refs (org-element-contents cite)))
     (mapcar
      (lambda (ref)
	(let* ((ref-prefix (--when-let (car (org-element-property :prefix ref))
			     (substring-no-properties (org-element-interpret-data it))))
	       (id (org-element-property :key ref))
	       (parsed-suffix
		(--when-let (car (org-element-property :suffix ref))
		  (citeproc-org--parse-locator-affix
		   (substring-no-properties (org-element-interpret-data it)))))
	       (prefix-in-parsed-suffix (alist-get 'prefix parsed-suffix))
	       (location (alist-get 'location parsed-suffix)))
	  (setf (alist-get 'prefix parsed-suffix)
		(if (and location
			 (not (s-blank-str-p prefix-in-parsed-suffix)))
		    (concat ref-prefix prefix-in-parsed-suffix)
		  ref-prefix))
	  (unless location (setf (alist-get 'suffix parsed-suffix)
				 prefix-in-parsed-suffix))
	  (cons (cons 'id id) parsed-suffix)))
      refs))
   :capitalize-first (and capitalize-outside-fn
			  new-fn)
   :suppress-affixes (not (org-element-property :parenthetical cite))))

;;; Cite syntax independent code

(defun citeproc-org--get-cleared-proc (bibtex-file)
  "Return a cleared citeproc processor reading items from BIBTEX-FILE.
Clear and return the buffer's cached processor if it is available
and had the same parameters. Create and return a new processor
otherwise."
  (let ((style-file (or (citeproc-org--get-option-val "csl_style")
			citeproc-org-default-style-file
			citeproc-org--fallback-style-file))
	(locale (or (citeproc-org--get-option-val "language") "en"))
	result)
    (-when-let ((c-proc c-style-file c-bibtex-file c-locale)
		citeproc-org--proc-cache)
      (when (and (string= style-file c-style-file)
		 (string= locale c-locale))
	(unless (string= bibtex-file c-bibtex-file)
	  (setf (citeproc-proc-getter c-proc)
		(citeproc-itemgetter-from-bibtex bibtex-file)
		(elt citeproc-org--proc-cache 1) bibtex-file))
	(citeproc-clear c-proc)
	(setq result c-proc)))
    (or result
	(let ((proc (citeproc-create
		     style-file
		     (citeproc-itemgetter-from-bibtex bibtex-file)
		     (citeproc-locale-getter-from-dir
		      (or citeproc-org-locales-dir
			  citeproc-org--fallback-locales-dir))
		     locale)))
	  (setq citeproc-org--proc-cache
		(list proc style-file bibtex-file locale))
	  proc))))

(defun citeproc-org--format-html-bib (bib parameters)
  "Format HTML bibliography BIB using formatting PARAMETERS."
  (let* ((char-width (car (s-match "[[:digit:].]+"
				   citeproc-org-html-label-width-per-char)))
	 (char-width-unit (substring citeproc-org-html-label-width-per-char
				     (length char-width))))
    (let-alist parameters
      (concat "\n#+BEGIN_EXPORT html\n"
	      (when .second-field-align
		(concat "<style>.csl-left-margin{float: left; padding-right: 0em;} "
			".csl-right-inline{margin: 0 0 0 "
			(number-to-string (* .max-offset (string-to-number char-width)))
			char-width-unit ";}</style>"))
	      (when .hanging-indent
		(concat "<style>.csl-entry{text-indent: -"
			citeproc-org-html-hanging-indent
			"; margin-left: "
			citeproc-org-html-hanging-indent
			";}</style>"))
	      citeproc-org-html-bib-header
	      bib
	      "\n#+END_EXPORT\n"))))

(defun citeproc-org--format-latex-bib (bib)
  "Format LaTeX bibliography BIB."
  (concat "#+latex_header: \\usepackage{hanging}\n#+BEGIN_EXPORT latex\n"
		 citeproc-org-latex-bib-header
		 "\\begin{hangparas}{" citeproc-org-latex-hanging-indent "}{1}"
		 bib "\n\\end{hangparas}\n#+END_EXPORT\n"))

(defun citeproc-org--bibliography (proc backend)
  "Return a bibliography using citeproc PROC for BACKEND."
  (cond ((memq backend citeproc-org-html-backends)
	 (-let ((rendered
		 (citeproc-render-bib proc 'html (not citeproc-org-link-cites))))
	   (citeproc-org--format-html-bib (car rendered) (cdr rendered))))
	((memq backend citeproc-org-latex-backends)
	 (citeproc-org--format-latex-bib
	  (car (citeproc-render-bib proc 'latex (not citeproc-org-link-cites)))))
	(t (concat citeproc-org-org-bib-header
		   (car (citeproc-render-bib
			 proc
			 'org
			 (or (memq backend citeproc-org-no-citelinks-backends)
			     (not citeproc-org-link-cites))))
		   "\n"))))

(defun citeproc-org--cites-and-notes (parsed-buffer mode)
  "Collect cite elements and info from PARSED-BUFFER in MODE.
PARSED-BUFFER is a buffer parse produced by
`org-element-parse-buffer', MODE is either `link' or `citation'.
Returns a list (CITES CITES-AND-NOTES CITE-COUNT FOOTNOTES-COUNT)
where CITES-AND-NOTES is the list of cite and footnote
representations (lists of the form (`cite' CITE-IDX CITE)
or (`footnote' FN-LABEL [CITE_n ... CITE_0])), in which CITE_n is
the n-th cite occurring in the footnote."
  (let* ((elt-types (list 'footnote-reference mode))
	 (elts (org-element-map parsed-buffer elt-types
		 (lambda (x)
		   (when (or (memq (org-element-type x) '(footnote-reference citation))
			     (member (org-element-property :type x) org-ref-cite-types))
		     x))))
	 cite-elts cites-and-notes
	 (act-cite-no 0)
	 (cite-count 0)
	 (footnotes-count 0))
    (dolist (elt elts)
      (if (eq 'footnote-reference (org-element-type elt))
	  (progn
	    (cl-incf footnotes-count)
	    ;; footnotes repesented as ('footnote <label> <cite_n> ... <cite_0>)
	    (push (list 'footnote (org-element-property :label elt))
		  cites-and-notes))
	(push elt cite-elts)
	(cl-incf cite-count)
	(let ((fn-pos (citeproc-org--fn-pos elt))
	      ;; cites as ('cite <cite-idx> cite)
	      (indexed (list 'cite act-cite-no elt)))
	  (cl-incf act-cite-no)
	  (pcase fn-pos
	    ;; not in footnote
	    ((\` nil) (push indexed cites-and-notes))
	    ;; unlabelled, in the last footnote
	    ('t (push indexed (cddr (car cites-and-notes))))
	    ;; labelled footnote
	    (_ (let ((fn-with-label (--first (and (eq (car it) 'footnote)
						  (string= fn-pos
							   (cadr it)))
					     cites-and-notes)))
		 (if fn-with-label
		     (setf (cddr fn-with-label)
			   (cons indexed (cddr fn-with-label)))
		   (error
		    "No footnote reference before footnote definition with label %s"
		    fn-pos))))))))
    (list (nreverse cite-elts) cites-and-notes cite-count footnotes-count)))

(defun citeproc-org--assemble-cite-info
    (cites-and-notes cite-count footnote-count &optional all-cites-are-notes)
  "Return position and note info using CITES-AND-NOTES info.
The format and content of CITES-AND-NOTES is as described in the
documentation of `citeproc-org--cites-and-notes'. CITE-COUNT and
FOOTNOTE-COUNT is the number of links and footnotes in
CITES-AND-NOTES. If optional ALL-CITES-ARE-NOTES is non-nil then
treat all links as footnotes (used for note CSL styles)."
  (let (cite-info
	(act-fn-no (let ((cites-and-notes-count (length cites-and-notes)))
		     (1+ (if all-cites-are-notes
			     cites-and-notes-count
			   footnote-count))))
	(act-cite-no cite-count))
    (dolist (elt cites-and-notes)
      (pcase (car elt)
	('cite
	 (push (list
		:elt (cl-caddr elt)
		:elt-no (cadr elt)
		:cite-no (cl-decf act-cite-no)
		:fn-no (if all-cites-are-notes
			   (cl-decf act-fn-no)
			 nil)
		:new-fn all-cites-are-notes)
	       cite-info))
	('footnote
	 (cl-decf act-fn-no)
	 (dolist (link (cddr elt))
	   (push (list
		  :elt (cl-caddr link)
		  :elt-no (cadr link)
		  :cite-no (cl-decf act-cite-no)
		  :fn-no act-fn-no)
		 cite-info)))))
    cite-info))

(defun citeproc-org--append-and-render-citations (cite-info proc backend mode
							    &optional no-links)
  "Render citations using CITE-INFO and PROC for BACKEND in MODE.
If optional NO-LINKS is given then don't link citations to bib
items. Return the list of corresponding rendered citations."
  (let* ((is-note-style (citeproc-style-cite-note (citeproc-proc-style proc)))
	 (parser-fun (pcase mode
		       ('link #'citeproc-org--parse-orgref-link)
		       ('citation #'citeproc-org--parse-org-cite)))
	 (citations (--map (funcall parser-fun
				    (plist-get it :elt)
				    (plist-get it :fn-no)
				    (plist-get it :new-fn)
				    is-note-style)
			   cite-info)))
    (citeproc-append-citations citations proc)
    (let* ((rendered
	    (cond ((memq backend citeproc-org-html-backends)
		   (--map (concat "@@html:" it "@@")
			  (citeproc-render-citations proc 'html no-links)))
		  ((memq backend citeproc-org-latex-backends)
		   (--map (concat "@@latex:" it "@@")
			  (citeproc-render-citations proc 'latex no-links)))
		  (t (citeproc-render-citations
		      proc 'org (or (memq backend citeproc-org-no-citelinks-backends)
				    no-links))))))
      (setq rendered (cl-loop for l-i in cite-info
			      for rendered-citation in rendered
			      collect (if (plist-get l-i :new-fn)
					  (concat "[fn::" rendered-citation "]")
					rendered-citation)))
      (citeproc-org--reorder-rendered-citations rendered cite-info))))

(defun citeproc-org--reorder-rendered-citations (rendered-citations cite-info)
  "Put RENDERED-CITATIONS into insertion order using CITE-INFO."
  (let ((sorted (cl-sort  cite-info #'< :key (lambda (x) (plist-get x :elt-no)))))
    (--map (elt rendered-citations (plist-get it :cite-no)) sorted)))

(defun citeproc-org--determine-mode (parsed-buffer)
  "Determine the type of citation entities used in PARSED-BUFFER.
Return `link' `citation' or nil if there are no citations."
  (org-element-map parsed-buffer '(link citation)
    (lambda (x)
      (cond ((eq (org-element-type x) 'citation) 'citation)
	    ((member (org-element-property :type x)
		     org-ref-cite-types)
	     'link)
	    (t nil)))
    nil t))

(defun citeproc-org--get-link-bib-info (parsed-buffer)
  "Return link-based bibliography information from PARSED-BUFFER.
Returns a (BIB-FILE BIB-ELT-BEGIN BIB-ELT-END PRINT-BIB) list."
  (let ((bib-link
	 (org-element-map parsed-buffer 'link
	   (lambda (x)
	     (when (member (org-element-property :type x)
			   '("bibliography" "nobibliography"))
	       x))
	   nil t))
	bib-file bib-elt-begin bib-elt-end print-bib)
    (if bib-link
	(-let ((path (org-element-property :path bib-link))
	       (type (org-element-property :type bib-link))
	       ((begin end) (citeproc-org--element-boundaries bib-link)))
	  (setq bib-elt-begin begin
		bib-elt-end end)
	  (when (string= type "bibliography")
	    (setq print-bib t))
	  (unless (string= path "here")
	    (setq bib-file path)))
      (setq print-bib t)
      (unless (string= (buffer-substring (1- (point-max)) (point-max)) "\n")
	(goto-char (point-max))
	(insert "\n"))
      (setq bib-elt-begin (point-max)
	    bib-elt-end (point-max)))
    (unless bib-file
      (if (and (boundp 'org-ref-default-bibliography)
	       org-ref-default-bibliography)
	  (setq bib-file (car org-ref-default-bibliography))
	(error "No bibliography file was specified")))
    (list bib-file bib-elt-begin bib-elt-end print-bib)))

(defun citeproc-org--get-keyword-bib-info (parsed-buffer)
  "Return keyword-based bibliography information from PARSED-BUFFER.
Returns a (BIB-FILE BIB-ELT-BEGIN BIB-ELT-END PRINT-BIB) list."
  (-if-let (bib-file (citeproc-org--get-option-val "bibliography"))
      (let ((bib-place
	     (org-element-map parsed-buffer 'keyword
	       (lambda (x)
		 (when (and (eq 'keyword (org-element-type x))
			    (string= (org-element-property :key x)
				     "BIBLIOGRAPHY")
			    (string= (org-element-property :value x)
				     "here"))
		   x))
	       nil t)))
	`(,bib-file
	  ,@(if bib-place (citeproc-org--element-boundaries bib-place)
	      (list nil nil))
	  ,(not (not bib-place))))
    (error "No bibliography file was specified")))

(defun citeproc-org--get-bib-info (parsed-buffer mode)
  "Return bibliography information from PARSED-BUFFER for MODE.
MODE is either `link' or `citation'. Returns a (BIB-FILE
BIB-ELT-BEGIN BIB-ELT-END PRINT-BIB) list."
  (pcase mode
    ('link (citeproc-org--get-link-bib-info parsed-buffer))
    ('citation (citeproc-org--get-keyword-bib-info parsed-buffer))))

(defun citeproc-org--citelink-content-to-legacy (content)
  "Convert a parsed citelink CONTENT to a legacy one."
  (let* ((first-item (car (split-string content ";")))
	 (parsed (citeproc-org--parse-locator-affix first-item))
	 prefix suffix)
    (let-alist parsed
      (if (not citeproc-org-bibtex-export-use-affixes)
	  (concat .prefix .location .suffix)
	(setq prefix .prefix
	      suffix (concat .location .suffix))
	(if (null suffix) prefix (concat prefix "::" suffix))))))

(defun citeproc-org--citelinks-to-legacy ()
  "Replace cite link contents with their legacy `org-ref' versions."
  (interactive)
  (let ((links (org-element-map (org-element-parse-buffer) 'link
		 (lambda (x)
		   (when (and (member (org-element-property :type x)
				      org-ref-cite-types)
			      (org-element-property :contents-begin x))
		     x))))
	(offset 0))
    (dolist (link links)
      (-let* (((begin end) (citeproc-org--element-boundaries link))
	      (raw-link (org-element-property :raw-link link))
	      (c-begin (+ offset (org-element-property :contents-begin link)))
	      (c-end (+ offset (org-element-property :contents-end link)))
	      (content (buffer-substring-no-properties c-begin c-end))
	      (new-content (citeproc-org--citelink-content-to-legacy content))
	      (new-link (if (s-blank-p new-content)
			    (concat "[[" raw-link "]]")
			  (concat "[[" raw-link "][" new-content "]]"))))
	(setf (buffer-substring (+ begin offset) (+ end offset))
	      new-link)
	(cl-incf offset (- (length new-link) (- end begin)))))))

;;;###autoload
(defun citeproc-org-render-references (backend)
  "Render cites and bibliography for export with BACKEND."
  (if (not (memq backend citeproc-org-ignore-backends))
      (let* ((parsed-buffer (org-element-parse-buffer))
	     (mode (citeproc-org--determine-mode parsed-buffer)))
	(when mode
	  (-let* (((cite-ents cites-and-notes cite-count footnote-count)
		   (citeproc-org--cites-and-notes parsed-buffer mode))
		  ((bib-file bib-begin bib-end print-bib)
		   (citeproc-org--get-bib-info parsed-buffer mode))
		  (proc (citeproc-org--get-cleared-proc bib-file))
		  (cite-info
		   (citeproc-org--assemble-cite-info
		    cites-and-notes cite-count footnote-count
		    (citeproc-style-cite-note (citeproc-proc-style proc))))
		  (citeproc-org-link-cites (and print-bib citeproc-org-link-cites))
		  (rendered-cites
		   (citeproc-org--append-and-render-citations
		    cite-info proc backend mode
		    (not (and print-bib citeproc-org-link-cites))))
		  (rendered-bib (if print-bib (citeproc-org--bibliography proc backend) ""))
		  (offset 0)
		  (bib-inserted nil))
	    (cl-loop for rendered in rendered-cites
		     for cite-ent in cite-ents
		     do
		     (-let* (((begin end) (citeproc-org--element-boundaries cite-ent)))
		       (when (and bib-end (> begin bib-end))
			 ;; Reached a cite after the bibliography location
			 ;; indicator so we insert the rendered bibliography
			 ;; before it
			 (setf (buffer-substring (+ bib-begin offset) (+ bib-end offset))
			       rendered-bib)
			 (setq bib-inserted t)
			 (cl-incf offset (- (length rendered-bib) (- bib-end bib-begin))))
		       (when (and (string= "[fn::" (substring rendered 0 5))
				  (= (char-before (+ begin offset)) ?\s))
			 ;; Remove (a single) space before the footnote
			 (cl-decf begin 1))
		       (setf (buffer-substring (+ begin offset) (+ end offset))
			     rendered)
		       (cl-incf offset (- (length rendered) (- end begin)))))
	    (when (and bib-end (not bib-inserted))
	      ;; The bibliography location indicator was after all cites
	      (setf (buffer-substring (+ bib-begin offset) (+ bib-end offset))
		    rendered-bib)))))
    (citeproc-org--citelinks-to-legacy))
  nil)

(provide 'citeproc-org)

;;; citeproc-org.el ends here
