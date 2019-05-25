;;; citeproc-org-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "citeproc-org" "citeproc-org.el" (0 0 0 0))
;;; Generated autoloads from citeproc-org.el

(autoload 'citeproc-org-render-references "citeproc-org" "\
Render cites and bibliography for export with BACKEND.

\(fn BACKEND)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "citeproc-org" '("citeproc-org-")))

;;;***

;;;### (autoloads nil "citeproc-org-setup" "citeproc-org-setup.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from citeproc-org-setup.el

(autoload 'citeproc-org-setup "citeproc-org-setup" "\
Add citeproc-org rendering to the `org-export-before-parsing-hook' hook.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("citeproc-org-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; citeproc-org-autoloads.el ends here
