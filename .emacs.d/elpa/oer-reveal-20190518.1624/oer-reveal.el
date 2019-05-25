;;; oer-reveal.el --- OER with reveal.js, plugins, and org-re-reveal  -*- lexical-binding: t; -*-
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2017-2019 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jens Lechtenbörger
;; URL: https://gitlab.com/oer/oer-reveal
;; Version: 0.9.9.9.1
;; Package-Requires: ((emacs "24.4") (org-re-reveal "1.0.3"))
;;    Emacs 24.4 adds advice-add and advice-remove.  Thus, Emacs
;;    should not be older.
;; Keywords: hypermedia, tools, slideshow, presentation, OER

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
;; Package `oer-reveal' bundles resources for the creation of
;; reveal.js presentations as Open Educational Resources (OER) from
;; Org source files.  This package builds upon `org-re-reveal' for
;; export from Org mode to HTML with reveal.js.  It provides help in
;; installing and configuring reveal.js and several of its plugins.
;;
;; Notably, `oer-reveal' simplifies one traditionally cumbersome task
;; for OER creators, namely the re-use of figures under free licenses
;; that require proper attribution.  Towards that end, macros
;; `revealimg', `reveallicense', and `revealgrid' are defined and
;; documented in file "org/config.org".
;;
;; * Usage
;; Variable `oer-reveal-dir' points to the directory of oer-reveal and
;; its embedded resources.  You may want to use that variable in your
;; own publication code, for which some pointers are provided in
;; function `oer-reveal-publish-all' of file "oer-reveal-publish.el".
;; Note that subdirectory "title-slide" contains some variants for
;; title slides of presentations, and subdirectory "css" contains
;; sample CSS.  Subdirectory "org" contains Org files to embed in
;; presentations.  Please be warned that included resources, in
;; particular CSS files, may change in incompatible ways.  You may
;; want to work with your own copies.
;;
;; Function `oer-reveal-setup-submodules' downloads and installs
;; reveal.js and some of its plugins into the directory
;; `oer-reveal-submodules-dir'.  Function
;; `oer-reveal-generate-include-files' generates Org files under
;; `oer-reveal-org-includes-dir', which include Org files coming with
;; `oer-reveal'; when installing `oer-reveal' from MELPA (with
;; changing directories upon updates) you can include those generated
;; files at stable locations in your own Org files.
;;
;; Function `oer-reveal-publish-setq-defaults' changes variables from
;; other packages, which may offer some suggestions what to adapt in
;; your own configuration.
;;
;; Note that the file "emacs-reveal.el", hosted at
;; https://gitlab.com/oer/emacs-reveal
;; provides sample initialization code for oer-reveal.
;;
;; * Customizable options
;; Variable `oer-reveal-script-files' lists JavaScript files to load
;; when initializing reveal.js.  If you use the version of reveal.js
;; coming with oer-reveal, you may want to assign the value of
;; `oer-reveal-script-files' to `org-re-reveal-script-files'.  This
;; also happens in `oer-reveal-publish-setq-defaults'.
;;
;; Variable `oer-reveal-plugins' lists reveal.js plugins to be
;; activated.  Remove those that you do not need.  Function
;; `oer-reveal-setup-plugins' adds plugin initialization code to
;; `org-re-reveal-external-plugins'.
;;
;; When generating image grids, `oer-reveal-export-dir' specifies
;; the directory into which to generate CSS code.  This should
;; probably be the directory into which you publish your HTML code.
;; I set this to "./" before exporting with `C-c C-e v b'.
;; The names of generated CSS files for image grids are determined by
;; `oer-reveal-css-filename-template'.

;;; Code:
(require 'cl-lib) ; cl-mapcar
(require 'subr-x) ; string-trim

(unless (fboundp 'alist-get)
  ;; Following based on subr.el, Emacs 27.0.50.  Argument testfn removed
  ;; as assoc in older Emacsen only accepts two arguments.
  (defun alist-get (key alist &optional default remove)
    "Return the value associated with KEY in ALIST.
If KEY is not found in ALIST, return DEFAULT.
Use TESTFN to lookup in the alist if non-nil.  Otherwise, use `assq'.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT."
    (ignore remove) ;;Silence byte-compiler.
    (let ((x (assq key alist)))
      (if x (cdr x) default))))

;; Customizable options
(defcustom oer-reveal-script-files '("js/reveal.js")
  "Value to apply to `org-re-reveal-script-files'.
By default, `org-re-reveal' also loads head.min.js, which has been removed
from the dev branch of reveal.js on 2018-10-04."
  :group 'oer-reveal
  :type '(repeat string))

(defcustom oer-reveal-plugins
  '("reveal.js-plugins" "Reveal.js-TOC-Progress" "reveal.js-jump-plugin"
    "reveal.js-quiz" "reveal.js-coursemod")
  "List of `plugin' components to initialize.
Each element here is supposed to be the directory name of the plugin.
If you remove a plugin from this list, it will no longer be initialized.
If you add plugins to this list, you need to provide suitable
initialization code yourself.  (E.g., see the code concerning
\"reveal.js-plugins\" in the file defining `oer-reveal-plugins'.)"
  :group 'oer-reveal
  :type '(repeat string))

(defcustom oer-reveal-audio-slideshow-dependency
  " { src: '%splugin/audio-slideshow/audio-slideshow.js', condition: function( ) { return !!document.body.classList && !Reveal.isSpeakerNotes(); } }"
  "Dependency to initialize audio-slideshow plugin."
  :group 'oer-reveal
  :type 'string)

(defcustom oer-reveal-audio-slideshow-config
  "audioStartAtFragment: true,
audio: {
    advance: -1, autoplay: true, defaultDuration: 0, defaultAudios: false, playerOpacity: 0.3, playerStyle: 'position: fixed; bottom: 9.5vh; left: 0%; width: 30%; height:30px; z-index: 33;' }"
  "Configuration for audio-slideshow plugin:
- Do not advance after end of audio.
- Start playing audio automatically.
- Do not display controls if no local audio file is given.
- Do not try to download audio files with default names.
- Increase opacity when unfocused (students found default too easy to miss).
- Display audio controls at bottom left (to avoid overlap)."
  :group 'oer-reveal
  :type 'string)

(defcustom oer-reveal-latex-figure-float "htp"
  "Define position for floating figures in LaTeX export.
You may want to use \"H\" with the float package."
  :group 'oer-reveal
  :type 'string)

;; Variables about installation location and reveal.js plugins follow.
(defconst oer-reveal-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory of oer-reveal containing code and resources.
Useful for `org-publish-all' to publish resources that are also
contained in this directory.")
(defconst oer-reveal-submodules-url
  "https://gitlab.com/oer/emacs-reveal-submodules.git"
  "Git URL for submodules of reveal.js and plugins.")
(defconst oer-reveal-submodules-version "0.9.6.1"
  "Version of submodules to check out.")
(defconst oer-reveal-buffer "*oer-reveal git output*"
  "Name of buffer holding Git output.")
(defcustom oer-reveal-submodules-dir
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory)
	  (file-name-sans-extension
	   (file-name-nondirectory oer-reveal-submodules-url))))
  "Directory with submodules of oer-reveal.
Submodules include reveal.js and its plugins.
If this directory does not exist, installation is offered.
If this directory exists, it must have been cloned via git from
`oer-reveal-submodules-url'.  If that condition is violated, strange
things may happen.
This directory must not be a relative path (but can start with \"~\")."
  :group 'oer-reveal
  :type 'directory)

;; Variables to control generation of files to include Org files.
(defcustom oer-reveal-generate-org-includes-p nil
  "Set to t for question whether to generate include files upon loading.
Used in `oer-reveal-generate-include-files'."
  :group 'oer-reveal
  :type 'boolean)

(defcustom oer-reveal-org-includes-dir
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory) "oer-reveal-org"))
  "Target directory for `oer-reveal-generate-include-files'."
  :group 'oer-reveal
  :type 'directory)

;; Functions to install and update submodules.
(defun oer-reveal-clone-submodules ()
  "Clone submodules from `oer-reveal-submodules-url'.
Target directory is `oer-reveal-submodules-dir'.
Output of Git goes to buffer `oer-reveal-buffer'."
  (let ((parent (file-name-directory
		 (directory-file-name oer-reveal-submodules-dir))))
    (unless (file-writable-p parent)
      (error "Directory to install submodules not writable: %s" parent))
    (save-excursion
      (pop-to-buffer (get-buffer-create oer-reveal-buffer) nil t)
      (let ((default-directory parent)
	    ;; In newer Emacsen, call-process starts in default-directory,
	    ;; which is what we want.  In Emacs 24.5.1, this does not happen.
	    ;; Instead, assign filename to buffer, from which call-process
	    ;; obtains its directory.
	    (buffer-file-name (concat parent oer-reveal-buffer)))
	(insert "Performing git clone in: ")
	(call-process "pwd" nil t t)
	(call-process "git" nil t t "clone" oer-reveal-submodules-url)
	(insert "...done\n\n")))
    (unless (file-readable-p oer-reveal-submodules-dir)
      (error "Cloning of submodules failed.  Directory not readable: %s"
	     oer-reveal-submodules-dir))))

(defun oer-reveal-submodules-ok-p ()
  "Return t if submodules have correct version.
Check that \"git describe --tags\" in `oer-reveal-submodules-dir'
returns the version `oer-reveal-submodules-version'."
  (string=
   oer-reveal-submodules-version
   (string-trim (shell-command-to-string
		 (format "cd %s; git describe --tags"
			 (shell-quote-argument
			  (expand-file-name oer-reveal-submodules-dir)))))))

(defun oer-reveal-update-submodules ()
  "Update submodules for this version of oer-reveal.
Do not update if `oer-reveal-submodules-ok-p' returns t.
Output of Git goes to buffer `oer-reveal-buffer'."
  (unless (file-writable-p oer-reveal-submodules-dir)
    (error "Directory of submodules not writable: %s"
	   oer-reveal-submodules-dir))
  (when (not (oer-reveal-submodules-ok-p))
    (save-excursion
      (pop-to-buffer (get-buffer-create oer-reveal-buffer) nil t)
      (let ((default-directory
	      (file-name-as-directory oer-reveal-submodules-dir))
	    ;; As explained above, also assign value to buffer-file-name.
	    (buffer-file-name
	     (concat (file-name-as-directory oer-reveal-submodules-dir)
		     oer-reveal-buffer)))
	(insert "Performing git pull and checkout in: ")
	(call-process "pwd" nil t t)
	(call-process "git" nil t t "checkout" "master")
	(call-process "git" nil t t "pull")
	(call-process "git" nil t t "checkout" oer-reveal-submodules-version)
	(insert "...done\n\nPerforming submodule install...\n")
	(call-process "git" nil t t "submodule" "sync" "--recursive")
	(call-process "git" nil t t "submodule" "update" "--init" "--recursive")
	(insert "...done\n\n"))))
  (unless (oer-reveal-submodules-ok-p)
    (error "Submodule update failed")))

(defun oer-reveal-install-submodules ()
  "Install reveal.js and plugins as submodules.
Software is cloned from `oer-reveal-submodules-url' into
`oer-reveal-submodules-dir'."
  (oer-reveal-clone-submodules)
  (oer-reveal-update-submodules))

(defun oer-reveal-setup-submodules (&optional force)
  "Install or update submodules of oer-reveal.
If optional FORCE is t, do not ask when `oer-reveal-submodules-dir' is
missing but install submodules silently."
  (interactive "P")
  (if (file-exists-p oer-reveal-submodules-dir)
      (oer-reveal-update-submodules)
    (when (or force
	      (y-or-n-p (format "Directory \"%s\" for reveal.js and plugins does not exist.  Type \"y\" to have it set up for you (needs to download about 26 MB).  Type \"n\" to install necessary submodules yourself or customize `oer-reveal-submodules-dir'.  Your choice? "
				oer-reveal-submodules-dir)))
      (oer-reveal-install-submodules))))

(defun oer-reveal--generate-include-file (source-file type)
  "Generate include file for SOURCE-FILE.
The TYPE can be \"org\", to generate a file that includes the source file,
or \"title-slide\", to generate a files that defines \"REVEAL_TITLE_SLIDE\".
The resulting file is stored under `oer-reveal-org-includes-dir'."
  (let* ((source-base (file-name-nondirectory source-file))
         (target-org (concat (file-name-sans-extension source-base) ".org"))
	 (target-file (concat
		       (file-name-as-directory oer-reveal-org-includes-dir)
		       target-org)))
    (with-temp-file target-file
      (insert
       (format "# Generated file.  Will be overwritten without warning.\n"))
      (cond ((string= type "org")
             (insert (format "#+INCLUDE: \"%s\"\n" source-file)))
            ((string= type "title-slide")
             (insert (format "#+REVEAL_TITLE_SLIDE: %s\n" source-file)))
            (t (user-error "Unexpected type `%s' for file `%s'"
                           type source-file))))))

(defun oer-reveal-generate-include-files (&optional force)
  "Generate files that include Org configuration files of oer-reveal.
If `oer-reveal-org-includes-dir' does not exist and
`oer-reveal-generate-org-includes-p' is t, ask user whether that directory
should be created to store generated files.
If optional FORCE is t, create directory without questions.
This provides a stable location for \"#+INCLUDE\" statements in your
Org files."
  (catch 'aborted
    (if (not (file-exists-p oer-reveal-org-includes-dir))
	(if (or force
		(and oer-reveal-generate-org-includes-p
		     (y-or-n-p
		      (format "Directory \"%s\" does not exist.  Create and populate for you (if not, maybe customize `oer-reveal-generate-org-includes-p')? "
			      oer-reveal-org-includes-dir))))
	    (make-directory oer-reveal-org-includes-dir t)
	  (throw 'aborted nil)))
    (dolist (spec
             (list
              (cons "org" "\\.org$")
              (cons "title-slide" "\\.html$"))
             nil)
      (let* ((source-dir (file-name-as-directory
			  (concat (file-name-as-directory
				   (expand-file-name oer-reveal-dir))
                                  (car spec))))
	     (source-files (directory-files source-dir t (cdr spec))))
        (mapc (lambda (source-file)
                (funcall #'oer-reveal--generate-include-file
                         source-file (car spec)))
              source-files)))))

;; The following options are only relevant if you use
;; oer-reveal-export-image-grid to generate image grids.
;; Then, the options control in what directory generated CSS is saved.
(defcustom oer-reveal-export-dir "public/"
  "Directory into which HTML, CSS, and Javascript is published.
The default supposes that `org-publish-all' publishes into a
subdirectory of `public/'.
This is only used to publish CSS of image grids with
`oer-reveal-export-image-grid'."
  :group 'oer-reveal
  :type 'directory)

(defcustom oer-reveal-css-filename-template
  "figures/internal_grid_css/grid%s.css"
  "Template for filename of CSS generated for image grid.
This must contain `%s' as placeholder for the grid's identifier.
Note that this filename is exported into a subdirectory of
`oer-reveal-export-dir' under the current directory."
  :group 'oer-reveal
  :type 'string)

;;; Configuration of various components.
(require 'org)
(require 'org-re-reveal)

(defun oer-reveal-add-to-init-script (initstring)
  "Add INITSTRING to `org-re-reveal-init-script'.
If `org-re-reveal-init-script' is a non-empty string, concatenate INITSTRING
after comma; otherwise, just `setq' to INITSTRING."
  (setq org-re-reveal-init-script
	(if (and (stringp org-re-reveal-init-script)
		 (< 0 (length org-re-reveal-init-script)))
	    (concat org-re-reveal-init-script ",\n  "
		    initstring)
	  initstring)))

(defun oer-reveal-setup-plugins ()
  "Setup `org-re-reveal-external-plugins'.
For elements of `oer-reveal-plugins', add initialization code to
`org-re-reveal-external-plugins'."
  (when (member "reveal.js-plugins" oer-reveal-plugins)
    ;; Activate and configure audio-slideshow plugin.
    (add-to-list 'org-re-reveal-external-plugins
		 (cons 'audio-slideshow
		       oer-reveal-audio-slideshow-dependency))
    (oer-reveal-add-to-init-script oer-reveal-audio-slideshow-config)

    ;; Activate anything plugin.
    (add-to-list 'org-re-reveal-external-plugins
		 (cons 'anything " { src: '%splugin/anything/anything.js' }"))
    (oer-reveal-add-to-init-script "anything: [
        // Following initialization code for class animate from anything-demo.html.
        // Copyright (c) 2016 Asvin Goel, under The MIT License (MIT).
	{className: \"animate\",  initialize: (function(container, options){
		Reveal.addEventListener( 'fragmentshown', function( event ) {
			if (typeof event.fragment.beginElement === \"function\" ) {
				event.fragment.beginElement();
			}
		});
		Reveal.addEventListener( 'fragmenthidden', function( event ) {
			if (event.fragment.hasAttribute('data-reverse') ) {
				var reverse = event.fragment.parentElement.querySelector('[id=\\\"' + event.fragment.getAttribute('data-reverse') + '\\\"]');
				if ( reverse && typeof reverse.beginElement === \"function\" ) {
					reverse.beginElement();
				}
			}
		});
		if ( container.getAttribute(\"data-svg-src\") ) {
			var xhr = new XMLHttpRequest();
			xhr.onload = function() {
				if (xhr.readyState === 4) {
					var svg = container.querySelector('svg');
					container.removeChild( svg );
					container.innerHTML = xhr.responseText + container.innerHTML;
					if ( svg ) {
						container.querySelector('svg').innerHTML = container.querySelector('svg').innerHTML + svg.innerHTML;
					}
				}
				else {
					console.warn( \"Failed to get file. ReadyState: \" + xhr.readyState + \", Status: \" + xhr.status);
				}
			};
			xhr.open( 'GET', container.getAttribute(\"data-svg-src\"), true );
			xhr.send();
		}
	}) },
	{className: \"randomPic\",
	 defaults: {imgalt: \"Dummy alt text\",
		    imgcaption: \"Image by {name}\",
		    choices: [ {name: \"dummyname\", path: \"dummypath\"} ]},
	 initialize: (function(container, options){
	     var choice = Math.trunc( Math.random()*(options.choices.length) );
	     var img = \"<img src='\" + options.choices[choice].path + \"' alt='\" + options.choices[choice].imgalt + \"' />\";
	     var caption = options.imgcaption.replace(new RegExp('\\{name\\}', 'gm'), options.choices[choice].name);
	     container.innerHTML = img + caption;
	 }) },
	{className: \"notes\",
	 initialize: (function(container, options){
	     container.addEventListener('click', function(e) { RevealNotes.open(); });
	 }) }
]"))

  (when (member "Reveal.js-TOC-Progress" oer-reveal-plugins)
    ;; Activate TOC progress plugin.
    ;; If there are lots of subsections, 'scroll'ing can be enabled or
    ;; the font size can be 'reduce'd.  Go for the latter.
    (add-to-list 'org-re-reveal-external-plugins
		 (cons 'toc-progress
		       " { src: '%splugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize('reduce', 'rgba(120,138,130,0.2)'); toc_progress.create(); } }")))

  (when (member "reveal.js-jump-plugin" oer-reveal-plugins)
    ;; Activate jump plugin.
    (add-to-list 'org-re-reveal-external-plugins
		 (cons 'jump "{ src: '%splugin/jump/jump.js', async: true }")))

  (when (member "reveal.js-quiz" oer-reveal-plugins)
    ;; Activate quiz plugin.
    (add-to-list 'org-re-reveal-external-plugins
		 (cons 'quiz "{ src: '%splugin/quiz/js/quiz.js', async: true, callback: function() { prepareQuizzes({preventUnanswered: true, skipStartButton: true}); } }")))

  (when (member "reveal.js-coursemod" oer-reveal-plugins)
    ;; Enable courseware plugin, but do not show it.
    (oer-reveal-add-to-init-script "coursemod: { enabled: true, shown: false }")
    (add-to-list 'org-re-reveal-external-plugins
		 (cons 'coursemod "{ src: '%splugin/coursemod/coursemod.js', async: true }"))))

;;; Allow colored text.
;; The FAQ at http://orgmode.org/worg/org-faq.html contains a recipe
;; based on the obsolete function (since Org 9.0) org-add-link-type.
;; Adapted to use org-link-set-parameters:
(org-link-set-parameters
 "color"
 :follow (lambda (path)
	   (message (concat "color "
			    (progn (add-text-properties
				    0 (length path)
				    (list 'face `((t (:foreground ,path))))
				    path) path))))
 :export (lambda (path desc backend)
	   (cond
	    ((eq backend 'html)
	     (format "<span style=\"color:%s;\">%s</span>" path desc))
	    ((eq backend 'latex)
	     (format "{\\color{%s}%s}" path desc)))))

;;; Function to generate proper CC attribution for images.
;; Function oer-reveal-export-attribution is used in macros in org/config.org.
;; See emacs-reveal-howto for sample use:
;; https://gitlab.com/oer/emacs-reveal-howto
;;;###autoload
(defun oer-reveal-export-attribution (&rest args)
  "Generate HTML and LaTeX code for image with license attribution.
Essentially, this function calls `oer-reveal--export-attribution-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2."
  ;; The first argument is the file name for metadata.  If that
  ;; starts with a quotation mark, arguments have been quoted.
  ;; (You don't start file names with quotation marks, do you?)
  (let ((metadata (car args)))
    (if (string-prefix-p "\"" metadata)
	(apply #'oer-reveal--export-attribution-helper
	       (mapcar #'oer-reveal--read-from-string args))
      (apply #'oer-reveal--export-attribution-helper args))))

(defun oer-reveal--check-symbol (object)
  "Helper function for `oer-reveal--read-from-string'.
Different Org versions treat macro arguments differently.  Check whether
OBJECT is a quoted symbol, where no quoting is necessary.  Notify user
if applicable.  Raise `user-error' in case of unknown type."
  (if (and (consp object) (eq 'quote (car object)))
      (progn
	(message
	 "Explicit quoting of symbol in `%s' not necessary (with your Org version)"
	 object)
	(sit-for 2)
	(cadr object))
    (user-error "Unexpected type `%s' in `%s'" (type-of object) object)))

(defun oer-reveal--read-from-string (object)
  "Undo potential quoting in OBJECT for strings with Org 9.2.
If OBJECT is a string, then use `read-from-string' to return
a boolean, integer, string, or symbol.
If OBJECT is not a string, return it unchanged."
  (if (stringp object)
      (if (= 0 (length object))
	  nil
	(let ((first (car (read-from-string object))))
	  (if (or (booleanp first) (integerp first) (stringp first)
		  (symbolp first))
	      first
	    (oer-reveal--check-symbol first))))
    object))

(defun oer-reveal--export-attribution-helper
    (metadata
     &optional caption maxheight divclasses shortlicense embed-svg)
  "Display image from METADATA.
Produce string for HTML and LaTeX exports to be embedded in Org files.
METADATA is a text file including licensing information.
If optional CAPTION is not nil, it can either be a string or t.  In that
case, display text underneath the image: If CAPTION is t, display whatever
the meta-data knows as title, otherwise display the string CAPTION, but
replace cite-links if present.  If CAPTION is t, the title is not repeated
as part of the license information.
If CAPTION is nil, a LaTeX caption is generated anyways to have a numbered
figure (and frequently to also display license information).
Optional MAXHEIGHT restricts the height of the image and of the license
information in HTML.  MAXHEIGHT needs be a full specification including
the unit, e.g. `50vh'.
If present, optional DIVCLASSES must be a string with space separated
classes for the div element, including `figure'.
If optional SHORTLICENSE is the symbol `none', do not display license
text (useful if image license agrees with document license);
if it is t, display license based on `oer-reveal--short-license-template'
\(instead of default (long) license text).
If optional EMBED-SVG is non-nil, embed XML code of SVG image directly.  In
this case, the maximum height on the image does not have any effect.
For LaTeX, the METADATA file may specify a texwidth, which is embedded in
the width specification as fraction of `linewidth'; 0.9 by default."
  (let ((org (oer-reveal--attribution-strings
	      metadata caption maxheight divclasses shortlicense embed-svg)))
    (concat (if caption
		(concat "@@html: </p><div class=\"imgcontainer\">"
			(car org)
			"</div><p>@@")
	      (concat "@@html: </p>" (car org) "<p>@@"))
	    "\n"
	    (cdr org))))

(defvar oer-reveal--short-license-template "[[%s][Figure]] under [[%s][%s]]")
(defvar oer-reveal--figure-div-template  "<div about=\"%s\" class=\"%s\"><p><img data-src=\"%s\" alt=\"%s\" %s/></p>%s%s</div>")
(defvar oer-reveal--svg-div-template  "<div about=\"%s\" class=\"%s\"><p>%s</p>%s%s</div>")
(defvar oer-reveal--figure-latex-caption-template "#+BEGIN_EXPORT latex\n\\begin{figure}[%s] \\centering\n  \\includegraphics[width=%s\\linewidth]{%s} \\caption{%s (%s)}\n  \\end{figure}\n#+END_EXPORT\n")
(defvar oer-reveal--figure-latex-template "         #+BEGIN_EXPORT latex\n     \\begin{figure}[%s] \\centering\n       \\includegraphics[width=%s\\linewidth]{%s} \\caption{%s}\n     \\end{figure}\n         #+END_EXPORT\n")
(defvar oer-reveal--figure-external-latex-template "         #+BEGIN_EXPORT latex\n     \\textbf{Warning!} External figure \\textbf{not} included: %s \\newline (See HTML presentation instead.)\n         #+END_EXPORT\n")
(defvar oer-reveal--figure-unsupported-latex-template "         #+BEGIN_EXPORT latex\n     \\textbf{Warning!} Figure omitted as %s format \\textbf{not} supported in \\LaTeX: “%s”\\newline (See HTML presentation instead.)\n         #+END_EXPORT\n")
(defvar oer-reveal--unsupported-tex-figure-formats '("gif"))
(defvar oer-reveal--default-copyright "by")

;; Image grid variables
(defvar oer-reveal--css-grid-img-class-template "grid%s-img%d"
  "Template for name of grid class.")
(defvar oer-reveal--css-grid-img-template
  (concat "." oer-reveal--css-grid-img-class-template
	  " { grid-area: ga%d; }")
  "Template for CSS of img element.")
(defvar oer-reveal--css-repeat-template "repeat(%s, 1fr)"
  "Template for size of rows and columns.")
(defvar oer-reveal--css-grid-template ".grid%s {
  display: grid;
  height: %svh;
  grid-template-columns: %s;
  grid-template-rows: %s;
  grid-gap: 5px;
  align-items: center;
  grid-template-areas: %s; }
"
  "Template for CSS of grid.")
(defvar oer-reveal--css-grid-img-all ".grid-img img {
  max-width: 90%; }
"
  "CSS for all images of grid.")

(defun oer-reveal--export-figure-latex
    (filename texwidth texfilename texlicense &optional latexcaption)
  "Generate LaTeX for figure at FILENAME.
If FILENAME is a full HTTP(S) URL, use
`oer-reveal--figure-external-latex-template' as placeholder.
If FILENAME has an unsupported extension (included in
`oer-reveal--unsupported-tex-figure-formats'), use
`oer-reveal--figure-unsupported-latex-template' as placeholder.
Otherwise, include graphics at TEXFILENAME of width TEXWIDTH
with caption TEXLICENSE.  Optional LATEXCAPTION determines whether
`oer-reveal--figure-latex-template' or
`oer-reveal--figure-latex-caption-template' is used to generate LaTeX code."
  (cond ((string-match-p "^https?://" filename)
	 (format oer-reveal--figure-external-latex-template texlicense))
	((member (file-name-extension filename)
		 oer-reveal--unsupported-tex-figure-formats)
	 (format oer-reveal--figure-unsupported-latex-template
		 (file-name-extension filename) texlicense))
	(latexcaption
	 (format oer-reveal--figure-latex-caption-template
		 oer-reveal-latex-figure-float
		 texwidth texfilename latexcaption texlicense))
	(t (format oer-reveal--figure-latex-template
		   oer-reveal-latex-figure-float
		   texwidth texfilename texlicense))))

(defun oer-reveal--export-figure-html
    (filename divclasses htmlcaption htmllicense imgalt h-image
	      &optional embed-svg)
  "Generate HTML for figure at FILENAME.
DIVCLASSES is passed from `oer-reveal-export-attribution',
HTMLCAPTION and HTMLLICENSE caption and license information for
the figure in HTML format.
If optional EMBED-SVG is non-nil, the file must be an SVG image
which is embedded directly.  SVG images are also embedded directly if
single file export is requested, which fails if a H-IMAGE is given.
Otherwise, an img tag is used, for which optional parameter IMGALT provides
the text for the alt attribute, while H-IMAGE specifies the height of the
image.
Templates `oer-reveal--svg-div-template' and
`oer-reveal--figure-div-template'specify the general HTML format."
  (let* ((extension (file-name-extension filename))
	 (external (string-match-p "^https?://" filename))
	 (issvg (and (string= "svg" extension) (not external)))
	 (issingle (plist-get (org-export-get-environment 're-reveal)
			      :reveal-single-file)))
    (if (and issvg issingle (not embed-svg))
	(user-error "Cannot produce single file without embedding SVG: %s"
		    filename)
      (if embed-svg
	  ;; Embed SVG's XML directly.
	  (format oer-reveal--svg-div-template
		  filename divclasses
		  (oer-reveal--file-as-string filename t)
		  htmlcaption htmllicense)
	(format oer-reveal--figure-div-template
		filename divclasses
		(if (and issingle (not external))
		    ;; Insert base64 encoded image as single line.
		    (concat "data:image/" extension ";base64,"
			    (with-temp-buffer
			      (insert-file-contents-literally filename)
			      (base64-encode-region 1 (point-max) t)
			      (buffer-string)))
		  filename)
		imgalt h-image htmlcaption htmllicense)))))

(defun oer-reveal--export-no-newline (string backend)
  "Call `org-export-string-as' on STRING, BACKEND, and t;
remove newline characters and, in case of HTML, surrounding p tags,
and return as result."
  (string-trim
   (replace-regexp-in-string "\n\\|<p>\\|</p>" " "
			     (org-export-string-as string backend t))))

(defun oer-reveal--file-as-string (filename &optional no-newlines)
  "Return contents of FILENAME as string.
If optional NO-NEWLINES is non-nil, return result without newlines."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (if no-newlines
	(replace-regexp-in-string
	 "\n" " " (buffer-substring-no-properties (point-min) (point-max)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun oer-reveal--attribute-author
    (attributionname attributionurl copyright backend)
  "Create attribution string with author and copyright information.
If ATTRIBUTIONNAME is non-nil it is the name of the author to which the work
should be attributed.  In that case, ATTRIBUTIONURL can specify a URL to
create a hyperlink to the author.
COPYRIGHT can either be the string `oer-reveal--default-copyright', which
indicates that no copyright is necessary, or an arbitrary Org string.
If ATTRIBUTIONNAME (maybe with ATTRIBUTIONURL) is non-nil, preprend
COPYRIGHT to author information.
If ATTRIBUTIONURL is nil and COPYRIGHT equals `oer-reveal--default-copyright',
return the empty string.
Otherwise, return COPYRIGHT information.
BACKEND must be `org' or `html'."
  (let ((copyright
	 (if (eq backend 'org)
	     copyright
	   (oer-reveal--export-no-newline copyright 'html))))
    (cond ((and attributionname attributionurl)
	   (format (if (eq backend 'org)
		       "%s [[%s][%s]]"
		     "%s <a rel=\"cc:attributionURL dc:creator\" href=\"%s\" property=\"cc:attributionName\">%s</a>")
		   copyright attributionurl attributionname))
	(attributionname
	 (format (if (eq backend 'org)
		     "%s %s"
		   "%s <span property=\"dc:creator cc:attributionName\">%s</span>")
		   copyright attributionname))
	((string= copyright oer-reveal--default-copyright) "")
	(t copyright))))

(defun oer-reveal--attribution-strings
    (metadata &optional caption maxheight divclasses shortlicense embed-svg)
  "Helper function.
See `oer-reveal-export-attribution' and
`oer-reveal--export-attribution-helper' for description of arguments
CAPTION, MAXHEIGHT, DIVCLASSES, SHORTLICENSE, EMBED-SVG.
Return cons cell whose car is the HTML representation for METADATA
and whose cdr is the LaTeX representation."
  (let* ((org-export-with-sub-superscripts nil)
	 (alist (read (oer-reveal--file-as-string metadata)))
	 (filename (alist-get 'filename alist))
	 (texfilename (file-name-sans-extension filename))
	 (licenseurl (alist-get 'licenseurl alist))
	 (licensetext (alist-get 'licensetext alist))
	 (permit (if (alist-get 'permit alist)
		     (format ". %s" (alist-get 'permit alist))
		   ""))
	 (attributionname (alist-get 'cc:attributionName alist))
	 (attributionurl (alist-get 'cc:attributionURL alist))
	 (copyright (alist-get 'copyright alist oer-reveal--default-copyright))
	 (orgauthor (oer-reveal--attribute-author
		     attributionname attributionurl copyright 'org))
	 (htmlauthor (oer-reveal--attribute-author
		      attributionname attributionurl copyright 'html))
	 (title (alist-get 'dc:title alist "Image"))
	 (realcaption (when caption
			(if (stringp caption)
			    caption
			  title)))
	 (htmlcaption (format "<p>%s</p>"
			      (if realcaption
				  (oer-reveal--export-no-newline realcaption 'html)
				"")))
	 (latexcaption (when realcaption
			 (oer-reveal--export-no-newline realcaption 'latex)))
	 (htmltitle (format "<span property=\"dc:title\">%s</span>"
			    (oer-reveal--export-no-newline title 'html)))
	 (imgalt (or (alist-get 'imgalt alist)
		     title))
	 (imgadapted (alist-get 'imgadapted alist "from"))
	 (sourceuri (alist-get 'dc:source alist))
	 (sourcetext (alist-get 'sourcetext alist))
	 (sourcehtml (format "; %s <a rel=\"dc:source\" href=\"%s\">%s</a>"
			     (oer-reveal--export-no-newline imgadapted 'html)
			     sourceuri sourcetext))
	 (divclasses (if divclasses
			 divclasses
		       "figure"))
	 (texwidth (alist-get 'texwidth alist 0.9))
	 (h-image (if maxheight
		      (format " style=\"max-height:%s\"" maxheight)
		    ""))
	 (h-license (if maxheight
			(format " style=\"max-width:%s\"" maxheight)
		      ""))
	 (license (if licensetext
		      (if licenseurl
			  (format " under [[%s][%s]];" licenseurl licensetext)
			(format " under %s" licensetext))
		    ""))
	 (orglicense (cond ((eq shortlicense 'none) "")
			   (shortlicense (format
					  oer-reveal--short-license-template
					  sourceuri licenseurl licensetext))
			   (t (concat
			       (format "“%s” %s" title orgauthor)
			       license
			       (format " %s [[%s][%s]]%s"
				       imgadapted sourceuri sourcetext permit)))))
	 (htmllicense (cond ((eq shortlicense 'none) "")
			    (shortlicense (format
					   "<p%s>%s</p>" h-license
					   (oer-reveal--export-no-newline
					    orglicense 'html)))
			    (t (concat
				(format "<p%s>" h-license)
				;; If title is part of the requested
				;; caption; omit in license.
				(if (and caption (booleanp caption))
				    ""
				  (format "&ldquo;%s&rdquo; " htmltitle))
				htmlauthor
				(if licensetext
				    (if licenseurl
					(format
					 " under <a rel=\"license\" href=\"%s\">%s</a>"
					 licenseurl licensetext)
				      " under %s" licensetext)
				  "")
				(format "%s%s</p>" sourcehtml
					(oer-reveal--export-no-newline
					 permit 'html))))))
	 (texlicense (if (< 0 (length orglicense))
			 (oer-reveal--export-no-newline orglicense 'latex)
		       (oer-reveal--export-no-newline title 'latex)))
	 )
    (if (stringp caption)
	(cons (oer-reveal--export-figure-html
	       filename divclasses htmlcaption htmllicense imgalt h-image
	       embed-svg)
	      (oer-reveal--export-figure-latex
	       filename texwidth texfilename texlicense latexcaption))
      (cons (oer-reveal--export-figure-html
	     filename divclasses htmlcaption
	     htmllicense imgalt h-image embed-svg)
	    (oer-reveal--export-figure-latex
	     filename texwidth texfilename texlicense
	     ;; Similar to above case.  However, a LaTeX caption is always
	     ;; generated via texlicense.
	     ;; Only use latexcaption when shortlicense is t
	     ;; (but not if it is none).
	     (when (and shortlicense (booleanp shortlicense))
	       latexcaption))))))

;;; Function to create a grid of images with license information in HTML.
;; Function oer-reveal-export-image-grid is used in macro in org/config.org.
;; See emacs-reveal-howto for sample use:
;; https://gitlab.com/oer/emacs-reveal-howto
;;;###autoload
(defun oer-reveal-export-image-grid (&rest args)
  "Generate HTML for image grid.
Essentially, this function calls `oer-reveal--export-image-grid-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2."
  ;; The first argument is an integer ID.  If that is a string,
  ;; arguments have been quoted.
  (if (stringp (car args))
      (apply #'oer-reveal--export-image-grid-helper
	     (mapcar #'oer-reveal--read-from-string args))
    (apply #'oer-reveal--export-image-grid-helper args)))

(defun oer-reveal--export-image-grid-helper
    (grid-id grid-images height no-columns no-rows template-areas)
  "Create HTML to display grid with id GRID-ID of GRID-IMAGES.
The grid has a HEIGHT (percentage of viewport height without unit),
NO-COLUMNS columns, NO-ROWS rows; positioning is specified by TEMPLATE-AREAS."
  (let* ((images (read (oer-reveal--file-as-string grid-images)))
	 (no-images (length images))
	 (numbered (cl-mapcar #'cons (number-sequence 1 no-images) images))
	 (row-height (/ (* 0.95 height) no-rows))
	 (image-heights (oer-reveal--compute-image-heights template-areas)))
    (oer-reveal--save-image-grid-css
     grid-id images height no-columns no-rows template-areas)
    (concat (format "#+REVEAL_EXTRA_CSS: %s\n"
		    (format oer-reveal-css-filename-template grid-id))
	    (format "@@html: </p><div class=\"grid%s\">" grid-id)
	    (mapconcat (lambda (pair)
			 (oer-reveal--export-grid-image
			  grid-id row-height image-heights
			  (car pair) (cdr pair)))
		       numbered " ")
	    "</div><p>@@"
	    "\n"
	    "@@latex: Presentation contains image grid.  \\LaTeX{} export not supported.@@")))

(defun oer-reveal--generate-grid-img (grid-id no)
  "Create CSS class assigning grid-area NO to image NO in grid GRID-ID."
  (format oer-reveal--css-grid-img-template grid-id no no))

(defun oer-reveal--generate-grid-imgs (grid-id no-images)
  "Create CSS classes for GRID-ID assigning grid areas for NO-IMAGES images."
  (mapconcat (lambda (no) (oer-reveal--generate-grid-img grid-id no))
	     (number-sequence 1 no-images) "\n"))

(defun oer-reveal--generate-grid
    (grid-id height no-columns no-rows template-areas)
  "Create CSS for grid layout of GRID-ID.
Layout based on `oer-reveal--css-grid-template' requires HEIGHT,
NO-COLUMNS, NO-ROWS, TEMPLATE-AREAS."
  (format oer-reveal--css-grid-template grid-id height
	  (format oer-reveal--css-repeat-template no-columns)
	  (format oer-reveal--css-repeat-template no-rows)
	  template-areas))

(defun oer-reveal--save-image-grid-css
    (grid-id images height no-columns no-rows template-areas)
  "Save CSS for GRID-ID with IMAGES to file.
Helper function for `oer-reveal-export-image-grid', see there for
documentation of arguments HEIGHT, NO-COLUMNS, NO-ROWS, TEMPLATE-AREAS.
Construct name of file in `oer-reveal-export-dir' with
`oer-reveal-css-filename-template', create directories if necessary,
remove possibly previously existing file, write CSS to new file, and
return it's name."
  (let* ((no-images  (length images))
	 (filename (expand-file-name
		    (format oer-reveal-css-filename-template grid-id)
		    oer-reveal-export-dir))
	 (dirname (file-name-directory filename))
	 (css (concat (oer-reveal--generate-grid-imgs grid-id no-images)
		      "\n"
		      (oer-reveal--generate-grid
		       grid-id height no-columns no-rows template-areas)
		      oer-reveal--css-grid-img-all "\n")))
    (mkdir dirname t)
    (when (file-readable-p filename)
      (delete-file filename))
    (append-to-file css nil filename)
    filename))

(defun oer-reveal--compute-image-heights (template-areas)
  "Create hash table with heights of cells in TEMPLATE-AREAS."
  (let ((rows (delete "" (delete " " (split-string template-areas "\""))))
	(result (make-hash-table :test 'equal)))
    (dolist (row rows result)
      (let ((cells (delete-dups (split-string row " "))))
	(dolist (cell cells)
	  (puthash cell (+ 1 (gethash cell result 0)) result))))))

(defun oer-reveal--export-grid-image
    (grid-id row-height image-heights no image)
  "Create HTML for IMAGE number NO in GRID-ID.
The height of the row is ROW-HEIGHT, heights of images are given by
IMAGE-HEIGHTS.
Call `oer-reveal--attribution-strings' with proper metadata."
  (let ((area (format "ga%d" no)))
    (car (oer-reveal--attribution-strings
	  image nil
	  (format "%svh"
		  (* (gethash area image-heights) row-height))
	  (concat "figure grid-img "
		  (format oer-reveal--css-grid-img-class-template
			  grid-id no))))))

;;; Functionality to make org-html-link use org-re-reveal's ID format.
;; This is useful when publishing with org-html-publish-to-html
;; where the HTML file is supposed to link into presentations.
;; Sample use: https://gitlab.com/oer/OS/blob/master/elisp/publish.el
(defun oer-reveal--rewrite-link (old-fun &rest arguments)
  "Combine OLD-FUN on ARGUMENTS with `org-re-reveal--maybe-replace-in-link'."
  (let ((orig (apply old-fun arguments)))
    (org-re-reveal--maybe-replace-in-link orig t)))

(defun oer-reveal--add-advice-link (&rest arguments)
  "Extend `org-html-link' with advice for org-re-reveal's anchor ID format.
ARGUMENTS are unused (but present to allow invocation as preparation
function during Org export, which passes an argument)."
  (ignore arguments) ; Silence byte compiler
  (advice-add #'org-html-link :around #'oer-reveal--rewrite-link))

(defun oer-reveal--remove-advice-link (&rest arguments)
  "Remove advice on `org-html-link'.
ARGUMENTS are unused (but present to allow invocation as completion
function during Org export, which passes an argument)."
  (ignore arguments) ; Silence byte compiler
  (advice-remove #'org-html-link #'oer-reveal--rewrite-link))

(provide 'oer-reveal)
;;; oer-reveal.el ends here
