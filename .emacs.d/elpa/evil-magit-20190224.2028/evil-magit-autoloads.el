;;; evil-magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-magit" "evil-magit.el" (0 0 0 0))
;;; Generated autoloads from evil-magit.el

(autoload 'evil-magit-init "evil-magit" "\
This function completes the setup of evil-magit. It is called
automatically when evil-magit is loaded. The only reason to use
this function is if you've called `evil-magit-revert' and wish to
go back to evil-magit behavior.

\(fn)" t nil)

(autoload 'evil-magit-revert "evil-magit" "\
Revert changes by evil-magit that affect default evil+magit behavior.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-magit" '("evil-magit-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-magit-autoloads.el ends here
