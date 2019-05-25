;;; helm-bibtex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bibtex-completion" "bibtex-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from bibtex-completion.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bibtex-completion" '("bibtex-completion-")))

;;;***

;;;### (autoloads nil "helm-bibtex" "helm-bibtex.el" (0 0 0 0))
;;; Generated autoloads from helm-bibtex.el

(autoload 'helm-bibtex "helm-bibtex" "\
Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`helm-bibtex-with-local-bibliography'.

If INPUT is non-nil and a string, that value is going to be used
as a predefined search term.  Can be used to define functions for
frequent searches (e.g. your own publications).

\(fn &optional ARG LOCAL-BIB INPUT)" t nil)

(autoload 'helm-bibtex-with-local-bibliography "helm-bibtex" "\
Search BibTeX entries with local bibliography.

With a prefix ARG the cache is invalidated and the bibliography
reread.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-bibtex" '("helm-")))

;;;***

;;;### (autoloads nil nil ("helm-bibtex-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-bibtex-autoloads.el ends here
