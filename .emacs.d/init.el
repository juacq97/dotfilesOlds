(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Repositorio local
(add-to-list 'load-path "~/.emacs.d/modes")

;; Instala use-package en caso de no estar
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-dracula))
 '(custom-safe-themes
   '("e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" default))
 '(org-agenda-files '("/mnt/Data/ORG/Trabajo.org" "/mnt/Data/ORG/Personal.org"))
 '(package-selected-packages
   '(org-super-agenda calfw calfw-org figlet company lua luarocks lua-mode org-superstar org-tree-slide ox-pandoc terminal-here easy-hugo rainbow-delimiters smartparens ewal-doom-themes ewal writeroom-mode heaven-and-hell doom-themes rainbow-mode markdown-mode yasnippet all-the-icons-ivy-rich counsel ivy-prescient ivy-rich ivy which-key doom-modeline evil-magit evil-org evil-collection evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit fixed-pitch))))
 '(org-block-end-line ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit fixed-pitch))))
 '(org-meta-line ((t (:inherit fixed-pitch))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit fixed-pitch)))))
