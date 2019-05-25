;;; org-re-reveal.el --- Org export to reveal.js presentations  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2013-2018 Yujie Wen and contributors to org-reveal, see:
;;                         https://github.com/yjwen/org-reveal/commits/master
;; Copyright (C) 2017-2019 Jens Lechtenbörger
;; Copyright (C) 2019      Naoya Yamashita <conao3@gmail.com>

;; URL: https://gitlab.com/oer/org-re-reveal
;; Version: 1.1.5
;; Package-Requires: ((emacs "24.4") (org "8.3") (htmlize "1.34"))
;; Keywords: tools, outlines, hypermedia, slideshow, presentation, OER

;; This file is not part of GNU Emacs.

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see http://www.gnu.org/licenses/ or write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package provides Org export functionality to generate HTML
;; presentations with the presentation framework reveal.js.
;;
;; Quickstart:
;; 0. Install reveal.js: https://revealjs.com/
;; 1. Activate org-re-reveal.
;;    (a) Place this directory into your load path or install it from MELPA
;;        (https://melpa.org/#/getting-started).
;;    (b) Load package manually ("M-x load-library" followed by
;;        "org-re-reveal") or place "(require 'org-re-reveal)" into your
;;        ~/.emacs and restart or customize org-export-backends by adding
;;        the symbol re-reveal.
;; 2. Load an Org file and export it to HTML.
;;    (a) Make sure that reveal.js is available in your current directory
;;        (e.g., as sub-directory or symbolic link).
;;    (b) Load "Readme.org" (coming with org-re-reveal).
;;    (c) Export to HTML: Press "C-c C-e v v" (write HTML file) or
;;        "C-c C-e v b" (write HTML file and open in browser)
;; See "Readme.org" for introduction, details, and features added to
;; org-reveal.
;;
;; Note that emacs-reveal offers a project that embeds org-re-reveal,
;; reveal.js, and various reveal.js plugins:
;; https://gitlab.com/oer/emacs-reveal
;; Its howto, generated from Org source file in GitLab CI environment:
;; https://oer.gitlab.io/emacs-reveal-howto/howto.html
;;
;; The package org-re-reveal grew out of a forked version of org-reveal
;; when upstream development stopped:
;; https://github.com/yjwen/org-reveal/issues/349
;; https://github.com/yjwen/org-reveal/issues/342

;;; Code:

(require 'ox-html)
(require 'cl-lib)   ; cl-mapcar and autoloads for:
                    ; cl-loop, cl-letf, cl-assert, cl-case, cl-every
(require 'subr-x)   ; string-trim
(require 'url-parse)

(defvar org-re-reveal-keys) ; Silence byte compiler

(defun org-re-reveal-define-backend ()
  "Define the back-end for export as reveal.js presentation."
  (org-export-define-derived-backend 're-reveal 'html

    :menu-entry
    `(,(nth 0 org-re-reveal-keys) "Export to reveal.js HTML Presentation"
      ((,(nth 1 org-re-reveal-keys)
        "To file" org-re-reveal-export-to-html)
       (,(nth 2 org-re-reveal-keys)
        "To file and browse" org-re-reveal-export-to-html-and-browse)
       (,(nth 3 org-re-reveal-keys)
        "Current subtree to file" org-re-reveal-export-current-subtree)))

    :options-alist
    '((:reveal-control nil "reveal_control" org-re-reveal-control t)
      (:reveal-progress nil "reveal_progress" org-re-reveal-progress t)
      (:reveal-history nil  "reveal_history" org-re-reveal-history t)
      (:reveal-center nil "reveal_center" org-re-reveal-center t)
      (:reveal-rolling-links nil "reveal_rolling_links" org-re-reveal-rolling-links t)
      (:reveal-slide-number nil "reveal_slide_number" org-re-reveal-slide-number t)
      (:reveal-keyboard nil "reveal_keyboard" org-re-reveal-keyboard t)
      (:reveal-mousewheel nil "reveal_mousewheel" org-re-reveal-mousewheel t)
      (:reveal-fragmentinurl nil "reveal_fragmentinurl" org-re-reveal-fragmentinurl t)
      (:reveal-hashonebasedindex nil "reveal_hashonebasedindex" org-re-reveal-hashonebasedindex t)
      (:reveal-pdfseparatefragments nil "reveal_pdfseparatefragments" org-re-reveal-pdfseparatefragments t)
      (:reveal-defaulttiming nil "reveal_defaulttiming" org-re-reveal-defaulttiming t)
      (:reveal-generate-ids nil "reveal_generate_ids" org-re-reveal-generate-custom-ids t)
      (:reveal-overview nil "reveal_overview" org-re-reveal-overview t)
      (:reveal-width nil "reveal_width" org-re-reveal-width t)
      (:reveal-height nil "reveal_height" org-re-reveal-height t)
      (:reveal-margin "REVEAL_MARGIN" nil org-re-reveal-margin t)
      (:reveal-min-scale "REVEAL_MIN_SCALE" nil org-re-reveal-min-scale t)
      (:reveal-max-scale "REVEAL_MAX_SCALE" nil org-re-reveal-max-scale t)
      (:reveal-root "REVEAL_ROOT" nil org-re-reveal-root t)
      (:reveal-trans "REVEAL_TRANS" nil org-re-reveal-transition t)
      (:reveal-speed "REVEAL_SPEED" nil org-re-reveal-transition-speed t)
      (:reveal-theme "REVEAL_THEME" nil org-re-reveal-theme t)
      (:reveal-extra-css "REVEAL_EXTRA_CSS" nil org-re-reveal-extra-css newline)
      (:reveal-extra-js "REVEAL_EXTRA_JS" nil org-re-reveal-extra-js nil)
      (:reveal-hlevel "REVEAL_HLEVEL" nil nil t)
      (:reveal-title-slide "REVEAL_TITLE_SLIDE" nil org-re-reveal-title-slide newline)
      (:reveal-academic-title "REVEAL_ACADEMIC_TITLE" nil nil t)
      (:reveal-miscinfo "REVEAL_MISCINFO" nil nil t)
      (:reveal-slide-global-header nil "reveal_global_header" org-re-reveal-global-header t)
      (:reveal-slide-global-footer nil "reveal_global_footer" org-re-reveal-global-footer t)
      (:reveal-slide-toc-footer nil "reveal_toc_footer" org-re-reveal-toc-footer t)
      (:reveal-title-slide-background "REVEAL_TITLE_SLIDE_BACKGROUND" nil nil t)
      (:reveal-title-slide-state "REVEAL_TITLE_SLIDE_STATE" nil nil t)
      (:reveal-title-slide-timing "REVEAL_TITLE_SLIDE_TIMING" nil nil t)
      (:reveal-title-slide-background-size "REVEAL_TITLE_SLIDE_BACKGROUND_SIZE" nil nil t)
      (:reveal-title-slide-background-position "REVEAL_TITLE_SLIDE_BACKGROUND_POSITION" nil nil t)
      (:reveal-title-slide-background-repeat "REVEAL_TITLE_SLIDE_BACKGROUND_REPEAT" nil nil t)
      (:reveal-title-slide-background-transition "REVEAL_TITLE_SLIDE_BACKGROUND_TRANSITION" nil nil t)
      (:reveal-toc-slide-state "REVEAL_TOC_SLIDE_STATE" nil nil t)
      (:reveal-toc-slide-class "REVEAL_TOC_SLIDE_CLASS" nil nil t)
      (:reveal-toc-slide-title "REVEAL_TOC_SLIDE_TITLE" nil org-re-reveal-toc-slide-title t)
      (:reveal-default-slide-background "REVEAL_DEFAULT_SLIDE_BACKGROUND" nil nil t)
      (:reveal-default-slide-background-size "REVEAL_DEFAULT_SLIDE_BACKGROUND_SIZE" nil nil t)
      (:reveal-default-slide-background-position "REVEAL_DEFAULT_SLIDE_BACKGROUND_POSITION" nil nil t)
      (:reveal-default-slide-background-repeat "REVEAL_DEFAULT_SLIDE_BACKGROUND_REPEAT" nil nil t)
      (:reveal-default-slide-background-transition "REVEAL_DEFAULT_SLIDE_BACKGROUND_TRANSITION" nil nil t)
      (:reveal-mathjax-url "REVEAL_MATHJAX_URL" nil org-re-reveal-mathjax-url t)
      (:reveal-preamble "REVEAL_PREAMBLE" nil org-re-reveal-preamble t)
      (:reveal-head-preamble "REVEAL_HEAD_PREAMBLE" nil org-re-reveal-head-preamble newline)
      (:reveal-postamble "REVEAL_POSTAMBLE" nil org-re-reveal-postamble t)
      (:reveal-multiplex-id "REVEAL_MULTIPLEX_ID" nil org-re-reveal-multiplex-id nil)
      (:reveal-multiplex-secret "REVEAL_MULTIPLEX_SECRET" nil org-re-reveal-multiplex-secret nil)
      (:reveal-multiplex-url "REVEAL_MULTIPLEX_URL" nil org-re-reveal-multiplex-url nil)
      (:reveal-multiplex-socketio-url "REVEAL_MULTIPLEX_SOCKETIO_URL" nil org-re-reveal-multiplex-socketio-url nil)
      (:reveal-slide-header "REVEAL_SLIDE_HEADER" nil org-re-reveal-slide-header t)
      (:reveal-slide-footer "REVEAL_SLIDE_FOOTER" nil org-re-reveal-slide-footer t)
      (:reveal-plugins "REVEAL_PLUGINS" nil org-re-reveal-plugins t)
      (:reveal-external-plugins "REVEAL_EXTERNAL_PLUGINS" nil org-re-reveal-external-plugins t)
      (:reveal-default-frag-style "REVEAL_DEFAULT_FRAG_STYLE" nil org-re-reveal-default-frag-style t)
      (:reveal-single-file nil "reveal_single_file" org-re-reveal-single-file t)
      (:reveal-inter-presentation-links nil "reveal_inter_presentation_links" org-re-reveal-inter-presentation-links t)
      (:reveal-init-script "REVEAL_INIT_SCRIPT" nil org-re-reveal-init-script space)
      (:reveal-highlight-css "REVEAL_HIGHLIGHT_CSS" nil org-re-reveal-highlight-css nil))

    :translate-alist
    '((headline . org-re-reveal-headline)
      (inner-template . org-re-reveal-inner-template)
      (item . org-re-reveal-item)
      (keyword . org-re-reveal-keyword)
      (link . org-re-reveal-link)
      (latex-environment . org-re-reveal-latex-environment)
      (latex-fragment . (lambda (frag contents info)
                          (setq info (plist-put info :reveal-mathjax t))
                          (org-html-latex-fragment frag contents info)))
      (plain-list . org-re-reveal-plain-list)
      (quote-block . org-re-reveal-quote-block)
      (section . org-re-reveal-section)
      (src-block . org-re-reveal-src-block)
      (special-block . org-re-reveal-special-block)
      (template . org-re-reveal-template))

    :filters-alist '((:filter-parse-tree . org-re-reveal-filter-parse-tree))))

(defun org-re-reveal-define-menu (symbol value)
  "Define back-end with (new) key bindings.
SYMBOL must be `org-re-reveal-keys' and VALUE its new value."
  (let ((standard (eval (car (get symbol 'standard-value)))))
    (cl-assert
     (eq symbol 'org-re-reveal-keys) nil
     (format "Symbol in org-re-reveal-define-menu unexpected: %s" symbol))
    (cl-assert
     (= (length standard) (length value))
     (format "Value for org-re-reveal-keys must have length %s (same as standard), not %s"
             (length standard) (length value)))
    (set-default symbol value)
    (org-re-reveal-define-backend)))

(defgroup org-export-re-reveal nil
  "Options for exporting Org files to reveal.js HTML pressentations."
  :tag "Org Export Reveal"
  :group 'org-export)

(defcustom org-re-reveal-keys '(?v ?v ?b ?s)
  "Define keys for export with org-re-reveal.
This list must contain four characters: The first one triggers export
with org-re-reveal (after \\<org-mode-map> \\[org-export-dispatch]).
The remaining three charaters each invoke a different export variant.
One of those characters must be typed after the first one; the
variants are, in sequence: Export to file, export to file followed by
browsing that file, subtree export to file."
  :group 'org-export-re-reveal
  :type '(list (character :tag "Key to trigger export with org-re-reveal")
               (character :tag "Key for export to file")
               (character :tag "Key to browse file after export")
               (character :tag "Key for subtree export to file"))
  :set #'org-re-reveal-define-menu)

(defcustom org-re-reveal-root "./reveal.js"
  "Specify root directory of reveal.js containing js/reveal.js."
  :group 'org-export-re-reveal
  :type '(radio (const :tag "Online at https://revealjs.com" "https://revealjs.com")
                (string :tag "Other directory path")))

(defcustom org-re-reveal-script-files '("lib/js/head.min.js" "js/reveal.js")
  "Specify files to initialize reveal.js.
On 2018-10-04, head.min.js was removed on the dev branch of reveal.js.
If you are using a version including that removal, customize this variable
to remove the first file name."
  :group 'org-export-re-reveal
  :type '(repeat string))

(defcustom org-re-reveal-hlevel 1
  "Specify minimum level of headings for grouping into vertical slides."
  :group 'org-export-re-reveal
  :type 'integer)

(defun org-re-reveal--get-hlevel (info)
  "Get HLevel value safely for INFO.
If option \"REVEAL_HLEVEL\" is set, retrieve integer value from it,
else get value from custom variable `org-re-reveal-hlevel'."
  (let ((hlevel-str (plist-get info :reveal-hlevel)))
    (if hlevel-str (string-to-number hlevel-str)
      org-re-reveal-hlevel)))

(defcustom org-re-reveal-title-slide 'auto
  "If nil or empty string, do not insert a title slide.
Otherwise (`auto' or non-empty string), insert title slide.
When `auto', generate automatic title slide.
When set to a string, use this string as format string for the title
slide, where the following escaping elements are allowed:

  %t stands for the title.
  %s stands for the subtitle.
  %a stands for the author's name.
  %A stands for the author's academic title.
  %e stands for the author's email.
  %d stands for the date.
  %m stands for misc information.
  %% stands for a literal %.

Alternatively, the string can also be the name of a file with the title
slide's HTML code (containing the above escape sequences)."
  :group 'org-export-re-reveal
  :type '(choice (const :tag "No title slide" nil)
                 (const :tag "Auto title slide" 'auto)
                 (string :tag "Custom title slide")))

(defcustom org-re-reveal-transition "convex"
  "Reveal transistion style."
  :group 'org-export-re-reveal
  :type '(radio (const "none")
                (const "fade")
                (const "slide")
                (const "convex")
                (const "concave")
                (const "zoom")
                (string :tag "Other transition")))

(defcustom org-re-reveal-transition-speed "default"
  "Reveal transistion speed."
  :group 'org-export-re-reveal
  :type '(radio (const "default")
                (const "fast")
                (const "slow")
                (string :tag "Other transition speed")))

(defcustom org-re-reveal-theme "moon"
  "Reveal theme."
  :group 'org-export-re-reveal
  :type '(radio (const "beige")
                (const "black")
                (const "blood")
                (const "league")
                (const "moon")
                (const "night")
                (const "serif")
                (const "simple")
                (const "sky")
                (const "solarized")
                (const "white")
                (string :tag "Other theme")))

(defcustom org-re-reveal-extra-js ""
  "URL to extra JS file."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-extra-css ""
  "URL to extra css file."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-multiplex-id ""
  "The ID to use for multiplexing.
To enable multiplex, see `org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-multiplex-secret ""
  "The secret to use for master slide.
To enable multiplex, see `org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-multiplex-url ""
  "The url of the socketio server.
To enable multiplex, see `org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-multiplex-socketio-url ""
  "The url of the socketio.js library.
To enable multiplex, see `org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-control t
  "Reveal control applet."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-progress t
  "Reveal progress applet."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-history nil
  "Reveal history applet."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-center t
  "Reveal center applet."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-rolling-links nil
  "Reveal use rolling links."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-slide-number "c"
  "Reveal showing slide numbers."
  :group 'org-export-re-reveal
  :type '(radio (const :tag "horizontal . vertical slide number" "h.v")
                (const :tag "horizontal / vertical slide number" "h/v")
                (const :tag "flattened slide number" "c")
                (const :tag "flattened slide number / total slides" "c/t")
                (string :tag "Other slide number format")))

(defcustom org-re-reveal-keyboard t
  "Reveal use keyboard navigation."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-mousewheel nil
  "Reveal use mousewheel navigation."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-fragmentinurl nil
  "Reveal use fragmentInURL setting."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-hashonebasedindex nil
  "Reveal use fragmentInURL setting."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-pdfseparatefragments t
  "Reveal disable pdfSeparateFragments setting."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-defaulttiming nil
  "Reveal use defaultTiming for speaker notes view."
  :group 'org-export-re-reveal
  :type '(choice integer (const nil)))

(defcustom org-re-reveal-overview t
  "Reveal show overview."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-width nil
  "Slide width as positive integer (pixels) or string (percentage) or nil."
  :group 'org-export-re-reveal
  :type '(choice integer string (const nil))
  :package-version '(org-re-reveal . "1.1.4"))

(defcustom org-re-reveal-height nil
  "Slide height as positive integer (pixels) or string (percentage) or nil."
  :group 'org-export-re-reveal
  :type '(choice integer string (const nil))
  :package-version '(org-re-reveal . "1.1.4"))

(defcustom org-re-reveal-margin "-1"
  "Slide margin (in a string)."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-min-scale "-1"
  "Minimum bound for scaling slide (in a string)."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-max-scale "-1"
  "Maximum bound for scaling slide (in a string)."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-mathjax-url
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  "Default MathJax URL."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-preamble nil
  "Preamble contents."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-head-preamble nil
  "Preamble contents for head part."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-postamble nil
  "Postamble contents."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-body-attrs nil
  "Attribute string to assign to body element.
By default, no attributes are assigned."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-slide-header nil
  "HTML content used as Reveal.js slide header."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-slide-header-html "<div class=\"slide-header\">%s</div>\n"
  "HTML format string to construct slide footer."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-global-header nil
  "If non nil, display slide header also on title and toc slide.
Header is defined by `org-re-reveal-slide-header'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-global-footer nil
  "If non nil, display slide footer also on title and toc slide.
Footer is defined by `org-re-reveal-slide-footer'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-toc-footer nil
  "If non nil, display slide footer also on toc slide.
Footer is defined by `org-re-reveal-slide-footer'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-slide-footer nil
  "Specify HTML content used as Reveal.js slide footer."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-slide-footer-html "<div class=\"slide-footer\">%s</div>\n"
  "HTML format string to construct slide footer.
Must constain exactly one %-sequence \"%s\"."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-toc-slide-title "Table of Contents"
  "String to display as title of toc slide."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-default-frag-style nil
  "Default fragment style."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-plugins
  '(classList markdown zoom notes)
  "Default builtin plugins.

By default,　variables related to multiplex are hidden.
Include 'multiplex in this variable to enable it.

This variable, like any other variable, can be overridden
in the org buffer comments as follows:
  #+REVEAL_PLUGINS: (classList markdown zoom notes multiplex)"
  :group 'org-export-re-reveal
  :type '(set
          (const classList)
          (const markdown)
          (const highlight)
          (const zoom)
          (const notes)
          (const search)
          (const remotes)
          (const multiplex)))

(defcustom org-re-reveal-external-plugins nil
  "Additional third-party plugins to load with reveal.js.
This is either an alist or a filename.
In case of an alist, each entry should contain a name and an expression
of the following form:
\"{src: '%srelative/path/from/reveal/root', async:true/false,
   condition: jscallbackfunction(){}}\"
In case of a file, its lines must be expressions of the above form.
Note that some plugins have dependencies such as jquery; these must be
included here as well, BEFORE the plugins that depend on them."
  :group 'org-export-re-reveal
  :type '(choice alist file))

(defcustom org-re-reveal-single-file nil
  "Export presentation into one single HTML file.
That file embeds JS scripts and pictures."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-inter-presentation-links nil
  "If non nil, try to convert links between presentations."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-init-script nil
  "Custom script to be passed to Reveal.initialize."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-highlight-css "%r/lib/css/zenburn.css"
  "Hightlight.js CSS file."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-note-key-char "n"
  "If not nil, register key for Org structure completion for speaker notes.
When `<' followed by the key character are
typed and then the completion key is pressed, which is usually
`TAB', \"#+BEGIN_NOTES\" and \"#+END_NOTES\" is inserted (maybe in
lower-case).  See \"Readme.org\" how to make this work with Org version
9.2 or later.

The default value is \"n\".  Set the variable to nil to disable
registering the completion."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-klipsify-src nil
  "Set to non-nil to make source code blocks editable in exported presentation."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-klipse-css "https://storage.googleapis.com/app.klipse.tech/css/codemirror.css"
  "Location of the codemirror css file for use with klipse."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-klipse-js "https://storage.googleapis.com/app.klipse.tech/plugin_prod/js/klipse_plugin.min.js"
  "Location of the klipse js source code."
  :group 'org-export-re-reveal
  :type 'string)

(defconst org-re-reveal-klipse-languages
  '("clojure" "html" "javascript" "js" "php" "python" "ruby" "scheme")
  "List of languages supported by org-re-reveal.")

(defcustom org-re-reveal-klipse-height "500px"
  "Height of iframe for klipse."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-klipse-width "100%"
  "Width of iframe for klipse."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-generate-custom-ids t
  "If t, generate CUSTOM_IDs for headings that don't have one.
Set to nil to revert to old behavior, where HTML section elements have
content hashes as \"id\" attributes, which change when slide contents
change.  With the default of t, generate CUSTOM_ID for headlines
missing such a property, by using the value of the headline's number.
This results in more stable URLs when working on presentations and
reloading slides.  You may want to set \"#+OPTIONS: reveal_history:t\"
to see the section identifiers as URL fragments in the address bar,
and you should not disable section numbering (for unnumbered
headlines, hash IDs are used unless a CUSTOM_ID is present).
For CSS code to hide section numbers if necessary, see
URL `https://github.com/yjwen/org-reveal/pull/284'."
  :group 'org-export-re-reveal
  :type 'boolean
  :package-version '(org-re-reveal . "1.1.3"))

(defvar org-re-reveal--slide-id-prefix "slide-"
  "Prefix to use in ID attributes of slide elements.")

(defvar org-re-reveal--href-fragment-prefix
  (concat "/" org-re-reveal--slide-id-prefix)
  "Prefix to use when linking to specific slides.
The default uses a slash between hash sign and slide ID,
which leads to broken links that are not understood outside reveal.js.
See there: https://github.com/hakimel/reveal.js/issues/2276")

(defun org-re-reveal--if-format (fmt val)
  "Apply `format' to FMT and VAL if VAL is not nil.
Otherwise, return empty string."
  (if val (format fmt val) ""))

(defun org-re-reveal--frag-style (frag info)
  "Return fragment string according to FRAG and the default fragment style.
FRAG is the fragment style set on element, INFO is a plist
holding contextual information."
  (cond
   ((string= frag t)
    (let ((default-frag-style (plist-get info :reveal-default-frag-style)))
      (if default-frag-style (format "fragment %s" default-frag-style)
        "fragment")))
   (t (format "fragment %s" frag))))

(defun org-re-reveal--frag-class (frag info)
  "Return proper HTML string description of fragment style.
FRAG is the fragment style set on element, INFO is a plist
holding contextual information."
  (and frag
       (format " class=\"%s\"" (org-re-reveal--frag-style frag info))))

(defun org-re-reveal--frag-index (index)
  "Return attribute string for fragment INDEX if set."
  (and index
       (format " data-fragment-index=\"%s\"" index)))

(defun org-re-reveal-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.

If the block type is 'NOTES' (case-insensitive), transcode the block
into a Reveal.js slide note.  Otherwise, export the block as by the HTML
exporter."
  (let ((block-type (org-element-property :type special-block)))
    (if (string= (downcase block-type) "notes")
        (format "<aside class=\"notes\">\n%s\n</aside>\n" contents)
      (org-html-special-block special-block contents info))))

(defun org-re-reveal--add-class (elem value)
  "Add VALUE as \"class\" attribute in HTML header element ELEM.
Do nothing if \"class\" attribute is alredy present."
  (let ((match (string-match "^<h[1-9][^>]+>" elem)))
    (unless match (error "Element no headline: %s" elem))
    (let ((tag (match-string 0 elem)))
      (if (string-match "class" tag)
          elem
        (replace-regexp-in-string "\\(<h[1-9][^>]+\\)>"
                                  (format "\\1 class=\"%s\">" value)
                                  elem)))))

(defun org-re-reveal--fix-html-headline (headline contents info)
  "Convert HEADLINE with CONTENTS and INFO to HTML.
Call `org-html-headline' to generate initial HTML, remove surrounding
\"div\" tags, and add class attribute to h-element if
\":HTML_HEADLINE_CLASS\" property is present.

Adding a class attribute in ox-reveal.el is a hack which is only
necessary until that functionality has arrived in ox-html.el:
https://lists.gnu.org/archive/html/emacs-orgmode/2018-12/msg00016.html
As that patch has been accepted, the property is called
\":HTML_HEADLINE_CLASS\".  Otherwise, \":REVEAL_HEADLINE_CLASS\" would
have been appropriate..."
  (let* ((class (org-element-property :HTML_HEADLINE_CLASS headline))
         (html (org-html-headline headline contents info))
         (nodiv
          (if (string-prefix-p "<div" html)
              ;; Remove the first <div> and the last </div> tags from html
              (concat "<"
                      (mapconcat 'identity
                                 (butlast (cdr (split-string html "<" t)))
                                 "<"))
            ;; Return the HTML content unchanged
            html)))
    (if class
        (org-re-reveal--add-class nodiv class)
      nodiv)))

(defun org-re-reveal--section-attrs (headline info)
  "Compute attributes for section element of HEADLINE with INFO.
Return empty string or one starting with a space character."
  (let* ((default-slide-background (plist-get info :reveal-default-slide-background))
         (default-slide-background-size (plist-get info :reveal-default-slide-background-size))
         (default-slide-background-position (plist-get info :reveal-default-slide-background-position))
         (default-slide-background-repeat (plist-get info :reveal-default-slide-background-repeat))
         (default-slide-background-transition (plist-get info :reveal-default-slide-background-transition))
         (attrs (org-html--make-attribute-string
                 `(:data-transition ,(org-element-property :REVEAL_DATA_TRANSITION headline)
                                    :data-state ,(org-element-property :REVEAL_DATA_STATE headline)
                                    :data-background ,(or (org-element-property :REVEAL_BACKGROUND headline)
                                                          default-slide-background)
                                    :data-background-size ,(or (org-element-property :REVEAL_BACKGROUND_SIZE headline)
                                                               default-slide-background-size)
                                    :data-background-position ,(or (org-element-property :REVEAL_BACKGROUND_POSITION headline)
                                                                   default-slide-background-position)
                                    :data-background-repeat ,(or (org-element-property :REVEAL_BACKGROUND_REPEAT headline)
                                                                 default-slide-background-repeat)
                                    :data-background-transition ,(or (org-element-property :REVEAL_BACKGROUND_TRANS headline)
                                                                     default-slide-background-transition)))))
    (if (> (length attrs) 0) (format " %s" attrs) "")))

;; Copied from org-html-headline and modified to embed org-re-reveal
;; specific attributes.
(defun org-re-reveal-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (if (org-export-low-level-p headline info)
        ;; This is a deep sub-tree: export it as in ox-html.
        (org-html-headline headline contents info)
      ;; Standard headline.  Export it as a slide
      (let* ((level (org-export-get-relative-level headline info))
             (preferred-id (or (org-element-property :CUSTOM_ID headline)
                                 (org-export-get-reference headline info)
                                 (org-element-property :ID headline)))
             (hlevel (org-re-reveal--get-hlevel info))
             (header (plist-get info :reveal-slide-header))
             (header-div (if header (format org-re-reveal-slide-header-html header) ""))
             (footer (plist-get info :reveal-slide-footer))
             (footer-div (if footer (format org-re-reveal-slide-footer-html footer) ""))
             (first-sibling (org-export-first-sibling-p headline info))
             (attrs (org-re-reveal--section-attrs headline info))
             (extra-attrs (org-element-property :REVEAL_EXTRA_ATTR headline))
             (slide-section-tag (format "<section id=\"%s\"%s%s>\n"
                                        (format "%s%s" org-re-reveal--slide-id-prefix preferred-id)
                                        attrs
                                        (if extra-attrs (format " %s" extra-attrs) "")))
             (ret (concat
                   (if (or (/= level 1) (not first-sibling))
                       ;; Not the first heading. Close previous slide.
                       (concat
                        ;; Slide footer if any.
                        footer-div
                        ;; Close previous slide.
                        "</section>\n"
                        (if (<= level hlevel)
                            ;; Close previous vertical slide group.
                            "</section>\n")))
                   (if (<= level hlevel)
                       ;; Add an extra "<section>" to group following slides
                       ;; into vertical slide group. Transition override
                       ;; attributes are attached at this level, too.
                       (let ((attrs
                              (org-html--make-attribute-string
                               `(:data-transition ,(org-element-property :REVEAL_DATA_TRANSITION headline)))))
                         (if (string= attrs "")
                             "<section>\n"
                           (format "<section %s>\n" attrs))))
                   ;; Start a new slide.
                   slide-section-tag
                   ;; Slide header if any.
                   header-div
                   ;; The HTML content of the headline
                   (org-re-reveal--fix-html-headline headline contents info)
                   (if (and (= level 1)
                            (org-export-last-sibling-p headline info))
                       ;; Last head 1. Close all slides.
                       (concat
                        ;; Slide footer if any
                        footer-div
                        "</section>\n</section>\n")))))
        ret))))

(defun org-re-reveal--read-file (file)
  "Return the content of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun org-re-reveal--file-url-to-path (url)
  "Convert URL that points to local files to file path."
  (replace-regexp-in-string
   (if (string-equal system-type "windows-nt") "^file:///" "^file://")
   "" url))

(defun org-re-reveal--css-label (in-single-file file-name style-id)
  "Generate HTML code to include CSS file FILE-NAME.
If IN-SINGLE-FILE is t, the content of FILE-NAME is embedded;
otherwise, a `<link>' label is generated."
  (when (and file-name (not (string= file-name "")))
    (if in-single-file
        ;; Single-file
        (let ((local-file-name (org-re-reveal--file-url-to-path file-name)))
          (if (file-readable-p local-file-name)
              (concat "<style type=\"text/css\">\n"
                      (org-re-reveal--read-file local-file-name)
                      "\n</style>\n")
            ;; But file is not readable.
            (error "Cannot read %s" file-name)))
      ;; Not in-single-file
      (concat "<link rel=\"stylesheet\" href=\"" file-name "\""
              (if style-id  (format " id=\"%s\"" style-id))
              "/>\n"))))

(defun org-re-reveal-stylesheets (info)
  "Return HTML code for reveal stylesheets using INFO and `org-re-reveal-root'."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
         (reveal-css (concat root-path "css/reveal.css"))
         (theme (plist-get info :reveal-theme))
         (theme-css (concat root-path "css/theme/" theme ".css"))
         (extra-css (plist-get info :reveal-extra-css))
         (in-single-file (plist-get info :reveal-single-file)))
    (concat
     ;; Default embedded style sheets
     "<style type=\"text/css\">
.underline { text-decoration: underline; }
</style>
"
     ;; stylesheets
     (mapconcat (lambda (elem) (org-re-reveal--css-label in-single-file (car elem) (cdr elem)))
                (append (list (cons reveal-css nil)
                              (cons theme-css "theme"))
                        (mapcar (lambda (a) (cons a nil))
                                (split-string extra-css "\n")))
                "\n")

     ;; Include CSS for highlight.js if necessary
     (if (org-re-reveal--using-highlight.js info)
         (format "<link rel=\"stylesheet\" href=\"%s\"/>"
                 (format-spec (plist-get info :reveal-highlight-css)
                              `((?r . ,(directory-file-name root-path))))))
     ;; print-pdf
     (if in-single-file ""
       (format "
<!-- If the query includes 'print-pdf', include the PDF print sheet -->
<script>
    if( window.location.search.match( /print-pdf/gi ) ) {
        var link = document.createElement( 'link' );
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = '%scss/print/pdf.css';
        document.getElementsByTagName( 'head' )[0].appendChild( link );
    }
</script>
"
               root-path)))))

(defun org-re-reveal-mathjax-scripts (info)
  "Return HTML code for declaring MathJax scripts for INFO."
  (if (plist-get info :reveal-mathjax)
      ;; MathJax enabled.
      (format "<script type=\"text/javascript\" src=\"%s\"></script>\n"
              (plist-get info :reveal-mathjax-url))))

(defun org-re-reveal--read-file-as-string (filename)
  "If FILENAME exists as file, return its contents as string.
Otherwise, return nil."
  (when (and (stringp filename)
             (file-readable-p filename)
             (not (file-directory-p filename)))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (buffer-string))))

(defun org-re-reveal--external-plugin-init (info root-path)
  "Build initialization strings for plugins of INFO under ROOT-PATH.
Parameter INFO determines plugins and their initializations
based on `org-re-reveal-external-plugins'."
  (let* ((external-plugins (plist-get info :reveal-external-plugins))
         (file-contents (org-re-reveal--read-file-as-string external-plugins)))
    (if file-contents
        (cl-loop for value in (split-string (string-trim file-contents) "\n")
                 collect (format value root-path))
      (cl-loop for (nil . value) in external-plugins
               collect (format value root-path)))))

(defvar org-re-reveal-client-multiplex nil
  "Used to cause generation of client html file for multiplex.")

(defun org-re-reveal-scripts--external-js (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
         (root-libs (mapcar (lambda (file) (concat root-path file))
                            org-re-reveal-script-files))
         ;; Local files
         (local-root-path (org-re-reveal--file-url-to-path root-path))
         (local-libs (mapcar (lambda (file) (concat local-root-path file))
                             org-re-reveal-script-files))
         (local-libs-exist-p (cl-every #'file-readable-p local-libs))
         (in-single-file (plist-get info :reveal-single-file)))
    (if (and in-single-file
             local-libs-exist-p)
        ;; Embed scripts into HTML
        (concat "<script>\n"
                (mapconcat #'org-re-reveal--read-file local-libs "\n")
                "\n</script>")
      ;; Fall-back to extern script links
      (if in-single-file
          ;; Tried to embed scripts but failed. Print a message about possible errors.
          (error (concat "Cannot read "
                         (mapconcat 'identity
                                    (delq nil (mapcar (lambda (file) (if (not (file-readable-p file)) file))
                                                      local-libs))
                                    ", "))))
      (mapconcat (lambda (file)
                   (concat "<script src=\"" file "\"></script>"))
                 root-libs "\n"))))

(defun org-re-reveal-scripts--reveal-options (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (format "
controls: %s,
progress: %s,
history: %s,
center: %s,
slideNumber: %s,
rollingLinks: %s,
keyboard: %s,
mouseWheel: %s,
fragmentInURL: %s,
hashOneBasedIndex: %s,
pdfSeparateFragments: %s,
%s
overview: %s,
"
          (if (plist-get info :reveal-control) "true" "false")
          (if (plist-get info :reveal-progress) "true" "false")
          (if (plist-get info :reveal-history) "true" "false")
          (if (plist-get info :reveal-center) "true" "false")
          (let ((slide-number (plist-get info :reveal-slide-number)))
            (if slide-number (format "'%s'" slide-number)
              "false"))
          (if (plist-get info :reveal-rolling-links) "true" "false")
          (if (plist-get info :reveal-keyboard) "true" "false")
          (if (plist-get info :reveal-mousewheel) "true" "false")
          (if (plist-get info :reveal-fragmentinurl) "true" "false")
          (if (plist-get info :reveal-hashonebasedindex) "true" "false")
          (if (plist-get info :reveal-pdfseparatefragments) "true" "false")
          (let ((timing (plist-get info :reveal-defaulttiming)))
            (if timing (format "defaultTiming: %s," timing)
              ""))
          (if (plist-get info :reveal-overview) "true" "false")))

(defun org-re-reveal--to-string (option)
  "Return OPTION as string.
If OPTION is an integer > 0, return as string.
If OPTION is a string, embed in quotation marks.
If OPTION is nil, return nil (not the empty string).
Otherwise, raise error."
  (cond ((and (integerp option) (> option 0)) (format "%d" option))
        ((stringp option) (format "\"%s\"" option))
        ((eq option nil) nil)
        (t (error "Option »%s« must be string, positive integer, or nil; not %s"
                  option (type-of option)))))

(defun org-re-reveal-scripts--main-configures (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (concat
   ;; slide width
   (let ((width (plist-get info :reveal-width)))
     (org-re-reveal--if-format "width: %s,\n"
                               (org-re-reveal--to-string width)))

   ;; slide height
   (let ((height (plist-get info :reveal-height)))
     (org-re-reveal--if-format "height: %s,\n"
                               (org-re-reveal--to-string height)))

   ;; slide margin
   (let ((margin (string-to-number (plist-get info :reveal-margin))))
     (if (>= margin 0) (format "margin: %.2f,\n" margin) ""))

   ;; slide minimum scaling factor
   (let ((min-scale (string-to-number (plist-get info :reveal-min-scale))))
     (if (> min-scale 0) (format "minScale: %.2f,\n" min-scale) ""))

   ;; slide maximux scaling factor
   (let ((max-scale (string-to-number (plist-get info :reveal-max-scale))))
     (if (> max-scale 0) (format "maxScale: %.2f,\n" max-scale) ""))

   ;; thems and transitions
   (format "
theme: Reveal.getQueryHash().theme, // available themes are in /css/theme
transition: Reveal.getQueryHash().transition || '%s', // see README of reveal.js for options
transitionSpeed: '%s',\n"
           (plist-get info :reveal-trans)
           (plist-get info :reveal-speed))))

(defun org-re-reveal-scripts--multiplex (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (let* (;; (plist-get info :reveal-plugins) maybe list or string representing list
         (raw-enabled-builtin-plugins (plist-get info :reveal-plugins))
         (enabled-builtin-plugins
          (condition-case err
              (if (listp raw-enabled-builtin-plugins)
                  raw-enabled-builtin-plugins
                (if (listp (read raw-enabled-builtin-plugins))
                    (read raw-enabled-builtin-plugins)
                  (error "#+REVEAL_PLUGINS expect symbol list, like \"#+REVEAL_PLUGINS: (classList markdown zoom notes)\"")))
            (error (signal (car err) (cdr err))))))
    (when (memq 'multiplex enabled-builtin-plugins)
      (format
       "multiplex: {
    secret: %s, // null if client
    id: '%s', // id, obtained from socket.io server
    url: '%s' // Location of socket.io server
},\n"
       (if (eq org-re-reveal-client-multiplex nil)
           (format "'%s'" (plist-get info :reveal-multiplex-secret))
         (format "null"))
       (plist-get info :reveal-multiplex-id)
       (plist-get info :reveal-multiplex-url)))))

(defun org-re-reveal-scripts--dependencies (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))

         (in-single-file (plist-get info :reveal-single-file))

         ;; (plist-get info :reveal-plugins) maybe list or string representing list
         (raw-enabled-builtin-plugins (plist-get info :reveal-plugins))
         (enabled-builtin-plugins
          (condition-case err
              (if (listp raw-enabled-builtin-plugins)
                  raw-enabled-builtin-plugins
                (if (listp (read raw-enabled-builtin-plugins))
                    (read raw-enabled-builtin-plugins)
                  (error "#+REVEAL_PLUGINS expect symbol list, like \"#+REVEAL_PLUGINS: (classList markdown zoom notes)\"")))
            (error (signal (car err) (cdr err)))))
         )
    ;; optional JS library heading
    (if in-single-file ""
      (concat
       "
// Optional libraries used to extend on reveal.js
dependencies: [
"
       ;; JS libraries
       (let* ((builtins
               `(classList ,(format " { src: '%slib/js/classList.js', condition: function() { return !document.body.classList; } }" root-path)
                           markdown ,(format " { src: '%splugin/markdown/marked.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },
 { src: '%splugin/markdown/markdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } }" root-path root-path)
                           highlight ,(format " { src: '%splugin/highlight/highlight.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } }" root-path)
                           zoom ,(format " { src: '%splugin/zoom-js/zoom.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                           notes ,(format " { src: '%splugin/notes/notes.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                           search ,(format " { src: '%splugin/search/search.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                           remotes ,(format " { src: '%splugin/remotes/remotes.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                           multiplex ,(format " { src: '%s', async: true },\n%s"
                                              (plist-get info :reveal-multiplex-socketio-url)
                                              ;; following ensures that either client.js or master.js is included depending on defvar org-re-reveal-client-multiplex value state
                                              (if (not org-re-reveal-client-multiplex)
                                                  (progn
                                                    (if (not (string= "" (plist-get info :reveal-multiplex-secret)))
                                                        (setq org-re-reveal-client-multiplex t))
                                                    (format " { src: '%splugin/multiplex/master.js', async: true }" root-path))
                                                (format " { src: '%splugin/multiplex/client.js', async: true }" root-path)))))
              (builtin-codes
               (mapcar (lambda (p) (plist-get builtins p)) enabled-builtin-plugins))
              (external-plugins
               (org-re-reveal--external-plugin-init info root-path))
              (all-plugins (if external-plugins (append external-plugins builtin-codes) builtin-codes))
              (extra-codes (plist-get info :reveal-extra-js))
              (total-codes
               (if (string= "" extra-codes) all-plugins (append (list extra-codes) all-plugins))                ))
         (mapconcat 'identity total-codes ",\n"))
       "]\n\n"))))

(defun org-re-reveal-scripts--init-script (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (let ((init-script (plist-get info :reveal-init-script))
        (in-single-file (plist-get info :reveal-single-file)))
    (if init-script (concat (if in-single-file "" ",") init-script))))

(defun org-re-reveal-scripts (info)
  "Return necessary scripts to initialize reveal.js.
Use INFO and custom variable `org-re-reveal-root'."
  (concat
   ;; reveal.js/lib/js/head.min.js
   ;; reveal.js/js/reveal.js
   (org-re-reveal-scripts--external-js info)

   ;; start of <script> tag
   "
<script>
// Full list of configuration options available here:
// https://github.com/hakimel/reveal.js#configuration
Reveal.initialize({
"
   ;; plugin configures/frags
   (org-re-reveal-scripts--reveal-options info)

   ;; reveal.js main configures
   (org-re-reveal-scripts--main-configures info)

   ;; multiplexing - depends on defvar 'org-re-reveal-client-multiplex'
   (org-re-reveal-scripts--multiplex info)

   ;; load dependency js
   (org-re-reveal-scripts--dependencies info)

   ;; init-script
   (org-re-reveal-scripts--init-script info)

   ;; end of <script> tag
   "});\n</script>\n"))

(defun org-re-reveal-toc (depth info)
  "Build a slide of table of contents with DEPTH and INFO."
  (let ((toc (org-html-toc depth info)))
    (org-re-reveal-toc-1 toc info)))

(defun org-re-reveal-toc-1 (toc info)
  "Build table of contents with TOC and INFO."
  (when toc
    (let* ((toc-slide-with-header (plist-get info :reveal-slide-global-header))
           (toc-slide-with-footer (or
                                   (plist-get info :reveal-slide-global-footer)
                                   (plist-get info :reveal-slide-toc-footer)))
           (toc-slide-state (plist-get info :reveal-toc-slide-state))
           (toc-slide-class (plist-get info :reveal-toc-slide-class))
           (toc-slide-title (plist-get info :reveal-toc-slide-title))
           (toc (replace-regexp-in-string
                 "<a href=\"#"
                 (concat "<a href=\"#" org-re-reveal--href-fragment-prefix) toc))
           (toc (replace-regexp-in-string
                 (org-html--translate "Table of Contents" info)
                 toc-slide-title toc)))
      (concat "<section id=\"table-of-contents-section\""
              (when toc-slide-state
                (format " data-state=\"%s\"" toc-slide-state))
              ">\n"
              (when toc-slide-with-header
                (let ((header (plist-get info :reveal-slide-header)))
                  (when header (format org-re-reveal-slide-header-html header))))
              (if toc-slide-class
                  (replace-regexp-in-string
                   "<h\\([1-3]\\)>"
                   (format "<h\\1 class=\"%s\">" toc-slide-class)
                   toc)
                toc)
              (when toc-slide-with-footer
                (let ((footer (plist-get info :reveal-slide-footer)))
                  (when footer (format org-re-reveal-slide-footer-html footer))))
              "</section>\n"))))

(defun org-re-reveal-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when (and depth
                (not (plist-get info :reveal-subtree)))
       (org-re-reveal-toc depth info)))
   ;; Document contents.
   contents))

(defun org-re-reveal-parse-keyword-value (value footer keyword info)
  "According to VALUE of KEYWORD and INFO, return HTML tags to split slides.
Currently, only the keyword \"split\" is implemented, and VALUE must
start with \"split\".  Any following text is inserted literally into
the section tag.
The possibly empty FOOTER is inserted at the end of the slide."
  (cl-assert (string-prefix-p "split" value) nil
             (format "Unknown REVEAL keyword.  Expected \"split\", got: %s"
                     value))
  (let* ((headline (org-export-get-parent-headline keyword))
         (split-attrs (substring value 5)) ; Everything after "split"
         (real-attrs (if (< 0 (length split-attrs))
                         split-attrs
                       (org-re-reveal--section-attrs headline info))))
    (format "%s</section>\n<section%s>"
            footer real-attrs)))

;; Copied from org-html-format-list-item. Overwrite HTML class
;; attribute when there is attr_html attributes.
(defun org-re-reveal-format-list-item (contents type checkbox attributes info
                                                &optional term-counter-id
                                                headline)
  "Format a list item into HTML based on INFO.
Item has CONTENTS, TYPE, may be a CHECKBOX, have ATTRIBUTES, and may have
TERM-COUNTER-ID and HEADLINE."
  (let ((attr-html (cond (attributes (format " %s" (org-html--make-attribute-string attributes)))
                         (checkbox (format " class=\"%s\"" (symbol-name checkbox)))
                         (t "")))
        (checkbox (concat (org-html-checkbox checkbox info)
                          (and checkbox " ")))
        (br (org-html-close-tag "br" nil info)))
    (concat
     (cl-case type
       (ordered
        (let* ((counter term-counter-id)
               (extra (if counter (format " value=\"%s\"" counter) "")))
          (concat
           (format "<li%s%s>" attr-html extra)
           (when headline (concat headline br)))))
       (unordered
        (let* ((id term-counter-id)
               (extra (if id (format " id=\"%s\"" id) "")))
          (concat
           (format "<li%s%s>" attr-html extra)
           (when headline (concat headline br)))))
       (descriptive
        (let* ((term term-counter-id))
          (setq term (or term "(no term)"))
          ;; Check-boxes in descriptive lists are associated to tag.
          (concat (format "<dt%s>%s</dt>"
                          attr-html (concat checkbox term))
                  (format "<dd%s>" attr-html)))))
     (unless (eq type 'descriptive) checkbox)
     (and contents (org-trim contents))
     (cl-case type
       (ordered "</li>")
       (unordered "</li>")
       (descriptive "</dd>")))))

;; Copied from org-html-item, changed to call
;; org-re-reveal-format-list-item.
(defun org-re-reveal-item (item contents info)
  "Transcode an ITEM element from Org to Reveal.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (counter (org-element-property :counter item))
         (attributes (org-export-read-attribute :attr_html item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info)))))
    (org-re-reveal-format-list-item
     contents type checkbox attributes info (or tag counter))))

(defun org-re-reveal-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Reveal.
May change custom variables as SIDE EFFECT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((key (org-element-property :key keyword))
         (value (org-element-property :value keyword))
         (footer (plist-get info :reveal-slide-footer))
         (footer-div (if footer
                         (format org-re-reveal-slide-footer-html footer) "")))
    (cl-case (intern key)
      (REVEAL (org-re-reveal-parse-keyword-value value footer-div keyword info))
      (REVEAL_HTML value)
      (HTML value)
      ;; Handling of TOC at arbitrary position is a hack.
      ;; We end the previous section by inserting a closing section tag.
      ;; To avoid unbalanced tags, remove the TOC's closing tag.
      ;; If slide footers are used, insert it before closing the section.
      ;; In any case, if footers are used, the one of the closed section
      ;; is sufficient, and the one contained in the TOC needs to be removed.
      (TOC (concat footer-div
                   "</section>\n"
                   (replace-regexp-in-string
                    (format "</section>\\|%s"
                            (format org-re-reveal-slide-footer-html ".*"))
                    ""
                    (org-re-reveal-toc-1
                     (org-html-keyword keyword contents info) info)))))))

(defun org-re-reveal-embedded-svg (path)
  "Embed the SVG content at PATH into Reveal HTML."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let ((start (re-search-forward "<[ \t\n]*svg[ \t\n]"))
          (end (re-search-forward "<[ \t\n]*/svg[ \t\n]*>")))
      (concat "<svg " (buffer-substring-no-properties start end)))))

(defun org-re-reveal--format-image-data-uri (link path info)
  "Generate the data URI for the image referenced by LINK at PATH with INFO."
  (let* ((ext (downcase (file-name-extension path))))
    (if (string= ext "svg")
        (org-re-reveal-embedded-svg path)
      (org-html-close-tag
       "img"
       (org-html--make-attribute-string
        (org-combine-plists
         (list :src
               (concat
                "data:image/"
                ;; Image type
                ext
                ";base64,"
                ;; Base64 content
                (with-temp-buffer
                  (insert-file-contents-literally path)
                  (base64-encode-region 1 (point-max))
                  (buffer-string))))
         ;; Get attribute list from parent element
         ;; Copied from ox-html.el
         (let* ((parent (org-export-get-parent-element link))
                (link (let ((container (org-export-get-parent link)))
                        (if (and (eq (org-element-type container) 'link)
                                 (org-html-inline-image-p link info))
                            container
                          link))))
           (and (eq (org-element-map parent 'link 'identity info t) link)
                (org-export-read-attribute :attr_html parent)))))
       info))))

(defun org-re-reveal--maybe-replace-in-link (link allow-inter-link)
  "Replace hash sign in LINK, affected by ALLOW-INTER-LINK.

If ALLOW-INTER-LINK is nil, only replace hash signs if URL in LINK starts
with it.  Otherwise, also replace if the URL does not contain a hostname;
such links are assumed to point into other presentations."
  (if (and allow-inter-link
           (string-match "<a href=\"\\([^\"]*\\)\"" link))
      (let* ((url (match-string 1 link))
             (obj (url-generic-parse-url url))
             (host (url-host obj)))
        (if host
            link
          (replace-regexp-in-string
           "<a href=\"\\([^#]*\\)#"
           (concat "<a href=\"\\1#" org-re-reveal--href-fragment-prefix)
           link)))
    (replace-regexp-in-string
     "<a href=\"#"
     (concat "<a href=\"#" org-re-reveal--href-fragment-prefix)
     link)))

(defun org-re-reveal--internal-link-class (link info)
  "Check if LINK is internal, given INFO, and maybe assign class.
The direction of the link is assigned as class attribute to the link
and to its parent via \"attr_html\":
If link points backward (to previous content), class \"backwardlink\"
is assigned, else \"forwardlink\".
Assigning the class to \"attr_html\" of parent is based on a hack in
`org-html-link', while use of \"attr_html\" of the link itself
requires a version of org-mode as of 2018-12-08 or newer."
  (let ((target (or (ignore-errors (org-export-resolve-id-link link info))
                    (ignore-errors (org-export-resolve-fuzzy-link link info)))))
    (when target
      (let* ((lbegin (org-element-property :begin link))
             (tbegin (org-element-property :begin target))
             (direction (if (< tbegin lbegin)
                            "backwardlink"
                          "forwardlink"))
             (parent (org-export-get-parent-element link))
             (attrs (org-combine-plists
                     (org-export-read-attribute :attr_html parent)
                     (org-export-read-attribute :attr_html link)))
             (class (plist-get attrs :class))
             (newclass (if class (concat direction " " class) direction))
             (newattrs (mapconcat (lambda (elem) (format "%s" elem))
                                  (plist-put attrs :class newclass)
                                  " ")))
        (org-element-put-property parent :attr_html (list newattrs))
        (org-element-put-property link :attr_html (list newattrs))))))

(defun org-re-reveal-link (link desc info)
  "Transcode a LINK object with DESC and INFO from Org to Reveal.
The result is identical to ox-html expect for image links.
When `org-re-reveal-single-file' is t,
the result is the Data URI of the referenced image."
  (let* ((want-embed-image (and (plist-get info :reveal-single-file)
                                (plist-get info :html-inline-images)
                                (string= "file" (org-element-property :type link))
                                (org-export-inline-image-p
                                 link (plist-get info :html-inline-image-rules))))
         (allow-inter-link (plist-get info :reveal-inter-presentation-links))
         (raw-path (org-element-property :path link))
         (clean-path (org-re-reveal--file-url-to-path raw-path))
         (can-embed-image (and want-embed-image
                               (file-readable-p clean-path))))
    (if can-embed-image
        (org-re-reveal--format-image-data-uri link clean-path info)
      (if want-embed-image
          (error "Cannot embed image %s" raw-path)
        (org-re-reveal--internal-link-class link info)
        (org-re-reveal--maybe-replace-in-link (org-html-link link desc info)
                                              allow-inter-link)))))

(defun org-re-reveal-latex-environment (latex-env contents info)
  "Transcode a LaTeX environment from Org to Reveal.
LATEX-ENV is the Org element.  CONTENTS is the contents of the environment.
INFO is a plist holding contextual information."
  (setq info (plist-put info :reveal-mathjax t))
  (let ((attrs (org-export-read-attribute :attr_html latex-env)))
    (format "<div%s>\n%s\n</div>\n"
            (if attrs (concat " " (org-html--make-attribute-string attrs)) "")
            (org-html-latex-environment latex-env contents info))))

(defun org-re-reveal-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Reveal.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information.
Extract and set `attr_html' to plain-list tag attributes."
  (ignore info) ; Silence byte compiler
  (let ((tag (cl-case (org-element-property :type plain-list)
               (ordered "ol")
               (unordered "ul")
               (descriptive "dl")))
        (attrs (org-export-read-attribute :attr_html plain-list)))
    (format "%s<%s%s>\n%s\n</%s>%s"
            (if (string= org-html-checkbox-type 'html) "<form>" "")
            tag
            (if attrs (concat " " (org-html--make-attribute-string attrs)) "")
            contents
            tag
            (if (string= org-html-checkbox-type 'html) "</form>" ""))))

(defun org-re-reveal-format-spec (info)
  "Return format specification with INFO.
Formatting extends `org-html-format-spec' with elements for
misc information and academic title."
  (append (org-html-format-spec info)
          `((?A . ,(org-export-data
                    (plist-get info :reveal-academic-title) info))
            (?m . ,(org-export-data
                    (plist-get info :reveal-miscinfo) info)))))

(defun org-re-reveal--build-pre-postamble (type info)
  "Depending on TYPE, return preamble or postamble for INFO as string, or nil."
  (let ((section (plist-get info (intern (format ":reveal-%s" type))))
        (spec (org-re-reveal-format-spec info)))
    (when section
      (let ((section-contents
             (if (functionp (intern section)) (funcall (intern section) info)
               ;; else section is a string.
               (format-spec section spec))))
        (when (org-string-nw-p section-contents)
          (org-element-normalize-string section-contents))))))


(defun org-re-reveal-section (section contents info)
  "Transcode a SECTION element from Org to Reveal.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  ;; Just return the contents. No "<div>" tags.
  (ignore section info) ; Silence byte compiler
  contents)

(defun org-re-reveal--using-highlight.js (info)
  "Check with INFO whether highlight.js plugin is enabled."
  (let ((reveal-plugins (condition-case nil
                            (car (read-from-string (plist-get info :reveal-plugins)))
                          (end-of-file nil)
                          (wrong-type-argument nil))))
    (memq 'highlight (or (and reveal-plugins (listp reveal-plugins) reveal-plugins)
                         org-re-reveal-plugins))))

(defun org-re-reveal--buffer-substring-html-escape (start end)
  "Convert buffer substring characters from plain text to HTML equivalent.
START and END are character positions as used by `buffer-substring'.
Conversion is done by escaping special HTML chars."
  (org-html-encode-plain-text (buffer-substring start end)))

(defun org-re-reveal-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Reveal.
INFO is a plist holding contextual information.  CONTENTS is unused."
  (ignore contents) ; Silence byte compiler
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((use-highlight (org-re-reveal--using-highlight.js info))
           (lang (org-element-property :language src-block))
           (caption (org-export-get-caption src-block))
           (code (if (not use-highlight)
                     (org-html-format-code src-block info)
                   (cl-letf (((symbol-function
                               'org-html-htmlize-region-for-paste)
                              #'org-re-reveal--buffer-substring-html-escape))
                     (org-html-format-code src-block info))))
           (frag (org-export-read-attribute :attr_reveal src-block :frag))
           (findex (org-export-read-attribute :attr_reveal src-block :frag_idx))
           (code-attribs (or (org-export-read-attribute
                              :attr_reveal src-block :code_attribs) ""))
           (label (let ((lbl (org-element-property :name src-block)))
                    (if (not lbl) ""
                      (format " id=\"%s\"" lbl))))
           (klipsify (and (or org-re-reveal-klipsify-src
                              (org-export-read-attribute
                               :attr_reveal src-block :klipsify))
                          (member lang org-re-reveal-klipse-languages)))
           (klipse-height (or (org-export-read-attribute
                               :attr_reveal src-block :klipse-height)
                              org-re-reveal-klipse-height))
           (klipse-width (or (org-export-read-attribute
                              :attr_reveal src-block :klipse-width)
                             org-re-reveal-klipse-width))
           (langselector
            (cond ((or (string= lang "js") (string= lang "javascript"))
                   "selector_eval_js")
                  ((string= lang "clojure") "selector")
                  ((string= lang "html") "selector_eval_html")
                  ((string= lang "php") "selector_eval_php")
                  ((string= lang "python") "selector_eval_python_client")
                  ((string= lang "ruby") "selector_eval_ruby")
                  ((string= lang "scheme") "selector_eval_scheme")
                  )))
      (if (not lang)
          (format "<pre %s%s>\n%s</pre>"
                  (or (org-re-reveal--frag-class frag info) " class=\"example\"")
                  label
                  code)
        (if klipsify
            (concat
             "<iframe style=\"background-color:white;\" height=\""
             klipse-height "\" width=\"" klipse-width
             "\" srcdoc='<html><body><pre><code "
             (if (string= lang "html")
                 "data-editor-type=\"html\" "
               "")
             "class=\"klipse\" " code-attribs ">
"
             (if (string= lang "html")
                 (replace-regexp-in-string
                  "'" "&#39;"
                  (replace-regexp-in-string
                   "&" "&amp;"
                   (replace-regexp-in-string
                    "<" "&lt;"
                    (replace-regexp-in-string
                     ">" "&gt;"
                     (cl-letf (((symbol-function
                                 'org-html-htmlize-region-for-paste)
                                #'buffer-substring))
                       (org-html-format-code src-block info))))))
               (replace-regexp-in-string "'" "&#39;" code))
             "
</code></pre>
<link rel= \"stylesheet\" type= \"text/css\" href=\"" org-re-reveal-klipse-css "\">
<style>
.CodeMirror { font-size: 2em; }
</style>
<script>
window.klipse_settings = { " langselector ": \".klipse\" };
</script>
<script src= \"" org-re-reveal-klipse-js "\"></script></body></html>
'>
</iframe>")
          (format
           "<div class=\"org-src-container\">\n%s%s\n</div>"
           (if (not caption) ""
             (format "<label class=\"org-src-name\">%s</label>"
                     (org-export-data caption info)))
           (if use-highlight
               (format "\n<pre%s%s><code class=\"%s %s\" %s>%s</code></pre>"
                       (or (org-re-reveal--frag-class frag info) "")
                       (or (org-re-reveal--frag-index findex) "")
                       label lang code-attribs code)
             (format "\n<pre %s%s%s>%s</pre>"
                     (or (org-re-reveal--frag-class frag info)
                         (format " class=\"src src-%s\"" lang))
                     (or (org-re-reveal--frag-index findex) "")
                     label code))))))))

(defun org-re-reveal-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the block INFO is a plist holding
contextual information."
  (format "<blockquote%s>\n%s</blockquote>"
          (let ((frag (org-re-reveal--frag-class
                       (org-export-read-attribute
                        :attr_reveal quote-block :frag) info)))
            (if frag
                (concat " " frag)
              ""))
          contents))


(defun org-re-reveal--auto-title-slide-template (info)
  "Generate the automatic title slide template with INFO."
  (let* ((spec (org-re-reveal-format-spec info))
         (title (org-export-data (plist-get info :title) info))
         (author (cdr (assq ?a spec)))
         (email (cdr (assq ?e spec)))
         (date (cdr (assq ?d spec))))
    (concat
     (when (and (plist-get info :with-title)
                (org-string-nw-p title))
       (concat "<h1 class=\"title\">" title "</h1>"))
     (when (and (plist-get info :with-author)
                (org-string-nw-p author))
       (concat "<h2 class=\"author\">" author "</h2>"))
     (when (and (plist-get info :with-email)
                (org-string-nw-p email))
       (concat "<h2 class=\"email\">" email "</h2>"))
     (when (and (plist-get info :with-date)
                (org-string-nw-p date))
       (concat "<h2 class=\"date\">" date "</h2>"))
     (when (plist-get info :time-stamp-file)
       (concat "<p class=\"date\">"
               (org-html--translate "Created" info)
               ": "
               (format-time-string
                (plist-get info :html-metadata-timestamp-format))
               "</p>")))))

(defun org-re-reveal-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (concat
   (format "<!DOCTYPE html>\n<html%s>\n<head>\n"
           (org-re-reveal--if-format " lang=\"%s\"" (plist-get info :language)))
   "<meta charset=\"utf-8\"/>\n"
   (org-re-reveal--if-format "<title>%s</title>\n"
                             (org-export-data (plist-get info :title) info))
   (org-re-reveal--if-format "<meta name=\"author\" content=\"%s\"/>\n"
                             (org-element-interpret-data (plist-get info :author)))
   (org-re-reveal--if-format "<meta name=\"description\" content=\"%s\"/>\n"
                             (plist-get info :description))
   (org-re-reveal--if-format "<meta name=\"keywords\" content=\"%s\"/>\n"
                             (plist-get info :keywords))
   (org-re-reveal-stylesheets info)
   (org-re-reveal-mathjax-scripts info)
   (org-re-reveal--build-pre-postamble 'head-preamble info)
   (org-element-normalize-string (plist-get info :html-head))
   (org-element-normalize-string (plist-get info :html-head-extra))
   "</head>\n<body"
   (org-re-reveal--if-format " %s" org-re-reveal-body-attrs)
   ">\n"
   (org-re-reveal--build-pre-postamble 'preamble info)
   "<div class=\"reveal\">
<div class=\"slides\">\n"
   ;; Title slides
   (let ((title-slide (plist-get info :reveal-title-slide)))
     (when (and (or (eq 'auto title-slide)
                    (and (stringp title-slide) (< 0 (length title-slide))))
                (not (plist-get info :reveal-subtree)))
       (let ((title-slide-background (plist-get info :reveal-title-slide-background))
             (title-slide-background-size (plist-get info :reveal-title-slide-background-size))
             (title-slide-background-position (plist-get info :reveal-title-slide-background-position))
             (title-slide-background-repeat (plist-get info :reveal-title-slide-background-repeat))
             (title-slide-background-transition (plist-get info :reveal-title-slide-background-transition))
             (title-slide-state (plist-get info :reveal-title-slide-state))
             (title-slide-timing (plist-get info :reveal-title-slide-timing))
             (title-slide-with-header (plist-get info :reveal-slide-global-header))
             (title-slide-with-footer (plist-get info :reveal-slide-global-footer)))
         (concat "<section id=\"sec-title-slide\""
                 (when title-slide-background
                   (concat " data-background=\"" title-slide-background "\""))
                 (when title-slide-background-size
                   (concat " data-background-size=\"" title-slide-background-size "\""))
                 (when title-slide-background-position
                   (concat " data-background-position=\"" title-slide-background-position "\""))
                 (when title-slide-background-repeat
                   (concat " data-background-repeat=\"" title-slide-background-repeat "\""))
                 (when title-slide-background-transition
                   (concat " data-background-transition=\"" title-slide-background-transition "\""))
                 (when title-slide-state
                   (concat " data-state=\"" title-slide-state "\""))
                 (when title-slide-timing
                   (concat " data-timing=\"" title-slide-timing "\""))
                 ">\n"
                 (when title-slide-with-header
                   (let ((header (plist-get info :reveal-slide-header)))
                     (when header (format org-re-reveal-slide-header-html header))))
                 (cond ((eq title-slide nil) nil)
                       ((stringp title-slide)
                        (let* ((file-contents
                                (org-re-reveal--read-file-as-string title-slide))
                               (title-string (or file-contents title-slide)))
                          (format-spec title-string
                                       (org-re-reveal-format-spec info))))
                       ((eq title-slide 'auto) (org-re-reveal--auto-title-slide-template info)))
                 "\n"
                 (when title-slide-with-footer
                   (let ((footer (plist-get info :reveal-slide-footer)))
                     (when footer (format org-re-reveal-slide-footer-html footer))))
                 "</section>\n"))))
   contents
   "</div>
</div>\n"
   (org-re-reveal--build-pre-postamble 'postamble info)
   (org-re-reveal-scripts info)
   "</body>
</html>\n"))

(defun org-re-reveal-filter-parse-tree (tree backend info)
  "Do filtering before parsing TREE.
TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist-used as a communication channel.
BACKEND must be `re-reveal'.
Modify the TREE in two ways:
First, map each `attr_reveal' attribute to corresponding
`attr_html' attributes.
Second, if `org-re-reveal-generate-custom-ids' is t (or option
\"reveal_generate_ids\" is t), generate \"CUSTOM_ID\" values for
section headings that do not have one already."
  (cl-assert (org-export-derived-backend-p backend 're-reveal) nil
             (format "Function org-re-reveal-filter-parse-tree called on unexpected backend: %s" backend))
  (let ((default-frag-style (plist-get info :reveal-default-frag-style)))
    (org-element-map tree (remq 'item org-element-all-elements)
      (lambda (elem) (org-re-reveal-append-frag elem default-frag-style))))
  (when (plist-get info :reveal-generate-ids)
    (let ((numbering (org-export--collect-headline-numbering tree info)))
      (dolist (pair numbering nil)
        (let ((headline (car pair))
              (number (cdr pair)))
          (when (org-export-numbered-headline-p headline info)
            (let ((section-number (mapconcat #'number-to-string number "-")))
              (when (and (> (length section-number) 0)
                         (not (org-element-property :CUSTOM_ID headline)))
                (org-element-put-property headline :CUSTOM_ID section-number))))))))
  ;; Return the updated tree.
  tree)

(defun org-re-reveal--update-attr-html (elem frag default-style
                                             &optional frag-index frag-audio)
  "Update ELEM's attr_html attribute with reveal's fragment attributes.
FRAG is the fragment style, a DEFAULT-STYLE may be used;
optional FRAG-INDEX and FRAG-AUDIO may indicate fragment positions
and audio files."
  (let ((attr-html (org-element-property :attr_html elem)))
    (when (and frag (not (string= frag "none")))
      (push (if (string= frag t)
                (if default-style (format ":class fragment %s" default-style)
                  ":class fragment")
              (format ":class fragment %s" frag))
            attr-html)
      (when frag-index
        ;; Index positions should be numbers or the minus sign.
        (cl-assert (or (integerp frag-index)
                       (eq frag-index '-)
                       (and (not (listp frag-index))
                            (not (char-equal
                                  (string-to-char frag-index) ?\())))
                   nil "Index cannot be a list: %s" frag-index)
        (push (format ":data-fragment-index %s" frag-index) attr-html))
      (when (and frag-audio (not (string= frag-audio "none")))
        (push (format ":data-audio-src %s" frag-audio) attr-html)))
    (org-element-put-property elem :attr_html attr-html)))

(defun org-re-reveal-append-frag (elem default-style)
  "Append transformed fragment from ELEM with DEFAULT-STYLE.
Read fragment from ELEM and append transformed fragment attribute to ELEM's
attr_html plist."
  (let ((frag (org-export-read-attribute :attr_reveal elem :frag))
        (frag-index (org-export-read-attribute :attr_reveal elem :frag_idx))
        (frag-audio (org-export-read-attribute :attr_reveal elem :audio)))
    (when frag
      (if (and (string= (org-element-type elem) 'plain-list)
               (char-equal (string-to-char frag) ?\())
          (let* ((items (org-element-contents elem))
                 (frag-list (car (read-from-string frag)))
                 (frag-list (if default-style
                                (mapcar (lambda (s)
                                          "Replace t with default-style"
                                          (if (string= s t) default-style
                                            s))
                                        frag-list)
                              frag-list))
                 (itemno (length items))
                 (style-list (make-list itemno default-style))
                 ;; Make sure that we have enough fragments.  Duplicate the
                 ;; last element of frag-list so that frag-list and items
                 ;; have the same length.
                 (last-frag (car (last frag-list)))
                 (tail-list (make-list
                             (- itemno (length frag-list)) last-frag))
                 (frag-list (append frag-list tail-list))
                 ;; Concerning index positions and audio files, check later
                 ;; that their number is OK.
                 (frag-index (if frag-index
                                 (car (read-from-string frag-index))
                               (make-list itemno nil)))
                 (frag-audio (when frag-audio
                               (car (read-from-string frag-audio)))))
            ;; As we are looking at fragments in lists, we make sure
            ;; that other specs are lists of proper length.
            (cl-assert (listp frag-index) t
                       "Must use list for index positions, not: %s")
            (when frag-index
              (cl-assert (= (length frag-index) itemno) nil
                         "Use one index per item!  %s has %d, need %d"
                         frag-index (length frag-index) (length items)))
            (cl-assert (listp frag-audio) t
                       "Must use list for audio files! %s")
            (when frag-audio
              (cl-assert (= (length frag-audio) itemno) nil
                         "Use one audio file per item!  %s has %d, need %d"
                         frag-audio (length frag-index) itemno))
            (if frag-audio
                (cl-mapcar 'org-re-reveal--update-attr-html
                           items frag-list style-list frag-index frag-audio)
              (cl-mapcar 'org-re-reveal--update-attr-html
                         items frag-list style-list frag-index)))
        (org-re-reveal--update-attr-html
         elem frag default-style frag-index frag-audio))
      elem)))

(defun org-re-reveal-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `org-export-to-file'."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (clientfile (org-export-output-file-name (concat "_client" extension) subtreep))
         (org-html-container-element "div"))

    (setq org-re-reveal-client-multiplex nil)
    (org-export-to-file 're-reveal file
      async subtreep visible-only body-only ext-plist)

    ;; Export the client HTML file if org-re-reveal-client-multiplex is set true
    ;; by previous call to org-export-to-file
    (if org-re-reveal-client-multiplex
        (org-export-to-file 're-reveal clientfile
          async subtreep visible-only body-only ext-plist))
    file))

(defun org-re-reveal-export-to-html-and-browse
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js and browse HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `org-re-reveal-export-to-html'."
  (interactive)
  (browse-url-of-file (expand-file-name (org-re-reveal-export-to-html async subtreep visible-only body-only ext-plist))))

(defun org-re-reveal-export-current-subtree
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current subtree to a Reveal.js HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `org-re-reveal-export-to-html'."
  (interactive)
  (org-narrow-to-subtree)
  (let ((ret (org-re-reveal-export-to-html async subtreep visible-only body-only (plist-put ext-plist :reveal-subtree t))))
    (widen)
    ret))

;;;###autoload
(defun org-re-reveal-publish-to-reveal
    (plist filename pub-dir)
  "Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 're-reveal filename ".html" plist pub-dir))

;; Register auto-completion for speaker notes.
(when org-re-reveal-note-key-char
  (add-to-list 'org-structure-template-alist
               (if (version< org-version "9.2")
                   (list org-re-reveal-note-key-char "#+BEGIN_NOTES\n\?\n#+END_NOTES")
                 (cons org-re-reveal-note-key-char "notes"))))

;;; Extract version string.
;;;###autoload
(defun org-re-reveal-version ()
  "Display version string for org-re-reveal from Lisp file."
  (interactive)
  (let ((lisp-file
         (concat (file-name-sans-extension (locate-library "org-re-reveal"))
                 ".el")))
    (with-temp-buffer
      (insert-file-contents lisp-file)
      (goto-char (point-min))
      (re-search-forward "^;; Version: \\([0-9.]+\\)$")
      (message "org-re-reveal version %s" (match-string 1)))))

(provide 'org-re-reveal)
;;; org-re-reveal.el ends here
