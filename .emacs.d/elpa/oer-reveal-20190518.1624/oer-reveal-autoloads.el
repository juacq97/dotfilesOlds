;;; oer-reveal-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "oer-reveal" "oer-reveal.el" (0 0 0 0))
;;; Generated autoloads from oer-reveal.el

(autoload 'oer-reveal-export-attribution "oer-reveal" "\
Generate HTML and LaTeX code for image with license attribution.
Essentially, this function calls `oer-reveal--export-attribution-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2.

\(fn &rest ARGS)" nil nil)

(autoload 'oer-reveal-export-image-grid "oer-reveal" "\
Generate HTML for image grid.
Essentially, this function calls `oer-reveal--export-image-grid-helper'
\(where arguments ARGS are documented), but makes sure that macro
arguments are properly expanded to work with all Org versions,
also after an incompatible change with Org 9.2.

\(fn &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "oer-reveal" '("oer-reveal-")))

;;;***

;;;### (autoloads nil "oer-reveal-publish" "oer-reveal-publish.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from oer-reveal-publish.el

(autoload 'oer-reveal-publish-setq-defaults "oer-reveal-publish" "\
Change various variables with `setq'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "oer-reveal-publish" '("oer-reveal-publish-")))

;;;***

;;;### (autoloads nil nil ("oer-reveal-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; oer-reveal-autoloads.el ends here
