;=================================================
;
;   █████  ██████████   ██████    █████   ██████
;  ██░░░██░░██░░██░░██ ░░░░░░██  ██░░░██ ██░░░░ 
; ░███████ ░██ ░██ ░██  ███████ ░██  ░░ ░░█████ 
; ░██░░░░  ░██ ░██ ░██ ██░░░░██ ░██   ██ ░░░░░██
; ░░██████ ███ ░██ ░██░░████████░░█████  ██████ 
;  ░░░░░░ ░░░  ░░  ░░  ░░░░░░░░  ░░░░░  ░░░░░░ 
;
;=================================================
;;====================
;; CONFIGURANDO MELPA
;;====================

(package-initialize)
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
(when (< emacs-major-version 24)
;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; Repositorio local
(add-to-list 'load-path "~/.emacs.d/modes")

;; Definir cuando descargar paquetes
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;==================
;; CARGAR CONFIG.ORG
;;==================

;(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;;=================
;; PAQUETES
;;=================

;; Doom modeline
;;=================

(use-package doom-modeline
:ensure t
:hook (after-init . doom-modeline-mode)
:config
;; How tall the mode-line should be (only respected in GUI Emacs).
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be (only respected in GUI Emacs).
(setq doom-modeline-bar-width 4)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether show `all-the-icons' or not (if nil nothing will be showed).
(setq doom-modeline-icon t)

;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display minor modes or not. Non-nil to display in mode-line.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count t)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setq doom-modeline-lsp nil)

;; Whether display github notifications or not. Requires `ghub` package.
(setq doom-modeline-github t)

;; The interval of checking github.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display environment version or not
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python")
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
(setq doom-modeline-mu4e t)

;; Whether display irc notifications or not. Requires `circe' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)
)

;; org-mode 
;;===========

;; org bullets
(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))

;; agenda con C-c a
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-window-setup
      'other-window)
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday nil)
(setq calendar-day-name-array ["domingo" "lunes" "martes" "miércoles"
                                 "jueves" "viernes" "sábado"])
(setq calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo"
                                   "junio" "julio" "agosto" "septiembre"
                                   "octubre" "noviembre" "diciembre"])

;; Agenda flotante
(defun agenda-frame ()
  (interactive)
  (org-agenda nil "n")
  (delete-other-windows))

;; Org capture flotante
(defadvice org-capture-finalize
(after delete-capture-frame activate)
"Advise capture-finalize to close the frame"
(if (equal "capture" (frame-parameter nil 'name))
(delete-frame)))

(defadvice org-capture-destroy
(after delete-capture-frame activate)
"Advise capture-destroy to close the frame"
(if (equal "capture" (frame-parameter nil 'name))
(delete-frame)))

(use-package noflet
:ensure t )
(defun make-capture-frame ()
"Create a new frame and run org-capture."
(interactive)
;(make-frame '((name . "capture")))
(select-frame-by-name "capture")
(delete-other-windows)
(noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
(org-capture)))

;; Parte de un archivo como link de org mode
(global-set-key (kbd "C-c l") 'org-store-link)

;; Plantillas para org capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("i" "Inbox" entry
	 (file "~/Drive/sync/gtd/ai.org")
	 "* %?\n%u" :prepend t)

	("t" "TODO" entry
	 (file+headline "~/Drive/sync/gtd/a.org" "TO-DO")
	 "* TODO %?\n%u" :prepend t)

	("c" "Consulta" entry
	 (file+headline "~/Drive/sync/gtd/a.org" "Consulta")
	 "* INFO %?\n" :prepend t)

	("a" "Agenda"  entry
	 (file+headline "~/Drive/sync/gtd/a.org" "Agenda")
	 "* CITA %?\n SCHEDULED: %t" :prepend t)
	
	("n" "Notas" entry
	 (file+headline "~/Drive/sync/gtd/a.org" "Notas")
	 "* %?" :prepend t)

	("m" "Después" entry
	 (file+headline "~/Drive/sync/gtd/a.org" "Tal vez")
	"* %?" :prepend t) 

	("d" "Diario" entry
	 (file+olp+datetree "~/Drive/sync/cuaderno/diario.org")
	 "* %?" :prepend t)))

;; Soporte para seleccionar con shift 
;;(setq org-support-shift-select t)

;; Flyspell automático
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; referencias en org-mode
(use-package org-ref
  :ensure t)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; Compatibilidad con mas referencias (APA)
(add-to-list 'load-path "/home/juan/.emacs.d/modes/")
(require 'citeproc-org)
  (citeproc-org-setup)
'(citeproc-org-locales-dir "/home/equipo/.emacs.d/csl-locales/")

;; <el TAB abre bloque de código elisp
(add-to-list 'org-structure-template-alist
       '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; Exportar en beamer
(org-beamer-mode)

;; Exportar en pandoc
(use-package ox-pandoc
  :ensure t)
(setq helm-bibtex-format-citation-functions
      '((org-mode . (lambda (x) (insert (concat
                                         "\\cite{"
                                         (mapconcat 'identity x ",")
                                         "}")) ""))))

;; Exportar en reveal.js
(use-package org-re-reveal
       :ensure t
       )
     (use-package org-re-reveal-ref
       :ensure t)
     (use-package oer-reveal
       :ensure t)
     (setq org-re-reveal-root "file:///home/juan/.repos/reveal.js")
;     (setq org-re-reveal-title-slide nil)

;; Clases latex extra
(add-to-list 'org-latex-classes
      '("koma-article"
	"\\documentclass{scrartcl}"
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	("\\paragraph{%s}" . "\\paragraph*{%s}")
	("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
	    
(add-to-list 'org-latex-classes
	     '("doc-recepcional"
	       "\\documentclass{report}"
	       ("\\chapter{%s}" . "\\chapter*{%s}")
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	       )
)

;; Cambiar TODO state con C-ñ
(eval-after-load 'org 
		    '(define-key org-mode-map (kbd "C-ñ") 'org-todo)
		    )

;; Evil-mode
;;==============

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  
;; Make movement keys work respect visual lines
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
;; Make horizontal movement cross lines
(setq-default evil-cross-lines t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-magit
  :ensure t)

;;;; Dashboard ;;;;
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((agenda . 5)
			  (projects . 5)
			  (recents . 5)
			  (registers . 5)))
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Pizza Mozzarella! Pizza Mozzarella! Rella Rella Rella Rella...")
  (setq show-week-agenda-p t)
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)
)

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;;;; Which key ;;;;
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;;;; Ivy ;;;;
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
)

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode 1)
  )

(use-package counsel
   :config
   (counsel-mode 1)
   :bind
   ("M-x" . counsel-M-x)
   )
 (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
 (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

;;;; Yasnippet ;;;;;
(use-package yasnippet
   :ensure t
   :config
   (yas-global-mode)
   (use-package yasnippet-snippets
   :ensure t)
   (yas-reload-all))

;;;; Hydra ;;;;;
(use-package hydra
  :ensure t)

;;;; Projectile ;;;;;
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))
(global-set-key (kbd "C-c p") 'projectile-switch-project)
(global-set-key (kbd "C-c f") 'projectile-find-file)

;;;; Swiper ;;;;;
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  )

;;;; Magit ;;;;;
(use-package magit
  :ensure t
)
(global-set-key (kbd "C-x g") 'magit)

;;;; Markdown-mode ;;;;
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(setq markdown-command "/usr/bin/pandoc")

;;;; Autocomplete ;;;;
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;;;; Ace-window ;;;;;
(use-package ace-window
   :ensure t
   :init (ace-window 1))
(global-set-key (kbd "C-x o") 'ace-window)

;;;; Nov.el ;;;;
(use-package nov
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;;; rainbow-mode ;;;;
(use-package rainbow-mode
   :ensure t
   :init (rainbow-mode 1))

;;;; Neotree ;;;;
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
)

;;;; All the icons ;;;;
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-file-commands '(counsel-dired-jump
                                             counsel-find-file
                                             counsel-file-jump
                                             counsel-find-library
                                             counsel-git
                                             counsel-projectile-find-dir
                                             counsel-projectile-find-file
                                             counsel-recentf))
  :config (all-the-icons-ivy-setup))

;;;; Doom-themes ;;;;
;;  (use-package doom-themes
;;    :ensure t
;;    :config
;;
;;  ;; Global settings (defaults)
;;  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;	doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;
;;  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;;  ;; may have their own settings.
;;  (load-theme 'doom-one t)
;;
;;  ;; Enable flashing mode-line on errors
;;  (doom-themes-visual-bell-config)
;;
;;  ;; Enable custom neotree theme (all-the-icons must be installed!)
;;  (doom-themes-neotree-config)
;;  ;; or for treemacs users
;;  (doom-themes-treemacs-config)
;;
;;  ;; Corrects (and improves) org-mode's native fontification.
;;  (doom-themes-org-config)
;;  )
;;
;;(add-hook 'after-make-frame-functions 'my-frame-config)
;;(add-hook 'after-init-hook 'my-frame-config)

;;;; Beacon ;;;;
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  )

;;;; Flymake-shellcheck
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;;;; HElm ;;;;
;  (use-package helm
;    :ensure t
;    :init
;    (helm-mode 1)
;    :bind
;    (("C-s" . swiper-helm)
;     ("M-x" . helm-M-x)
;     ("C-x C-f" . helm-find-files)
;     ("C-c b" . helm-buffer-list)
;     )
;    )

;;;; Writeroom-mode ;;;;
(use-package writeroom-mode
    :ensure t
    )
(global-set-key [f6] 'writeroom-mode)

;;;; Ewal ;;;;
;(use-package ewal
;  :ensure t
;  )
;
;(when (ewal-load-wal-colors)
;  (custom-set-faces
;   `(line-number ((t (:foreground ,(ewal-get-color 'magenta 4)))))))
;(load-theme 'ewal-spacemacs-modern t)

;;;; Writefreely ;;;;
(use-package writefreely
    :after org
    :ensure ox-gfm
    :config
    (setq
     writefreely-instance-url "https://qua.name"
     writefreely-instance-api-endpoint "https://qua.name/api"
    writefreely-maybe-publish-created-date t
     writefreely-auth-token "c64c8328-bcbf-4626-4587-0a0d9408bba7")
    )

;; Nord theme ;;;;
(use-package nord-theme
  :ensure t
  )
  (load-theme 'nord t)
;; Use `nord4` from Nord's "Snow Storm" palette as background color.
(setq nord-region-highlight "snowstorm")

























;;==================
;; UI
;;==================

;;;;; Responder y/n ;;;;;
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Desplazarse una línea ;;;;;
(setq scroll-conservatively 100)

;;;; Destacar línea del cursor ;;;;
(global-hl-line-mode t)

;;;; Mandar autosave a un directorio específico ;;;;
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backups/")))

;;;; Ocultar elementos ;;;;
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "M-m") 'menu-bar-mode) ; M-m hace aparecer el menú

;;;; Abrir configuración con C-c e ;;;;
(defun config-visit ()
  (interactive)
;  (find-file "~/.emacs.d/config.org"))
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

;;;; Reevaluar config.org ;;;;
  (defun config-reload ()
    (interactive)
    (org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
    )
  (global-set-key (kbd "C-c r") 'config-reload)

;;;; Seguir al buffer nuevo ;;;;;
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;;;; Paréntesis electricos ;;;;
  (setq electric-pair-pairs '(
			      (?\( . ?\))
			      (?\[ . ?\])
			      (?\{ . ?\})
			      ))
  (setq electric-pair-preserve-balance nil)
  (electric-pair-mode)

;;;; Mostrar línea actual en modeline ;;;;;
(line-number-mode 1)
(column-number-mode 1)

;;;; Matar con C-x k ;;;;
(defun kill-curr-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-curr-buffer)

;;;; Visual line mode con C-c v ;;;;
(global-set-key (kbd "C-c v") 'visual-line-mode)

;;;; Flyspell con F5 ;;;;
(global-set-key (kbd "<f5>")  'ispell-word) 

;;;; Mostrar números de líneas ;;;;
(when (version<= "26.0.50" emacs-version )
(global-display-line-numbers-mode))

;;;; eww como navergador por defecto ;;;;;
; (setq browse-url-browser-function 'eww-browse-url)

;;;; Buffer inicial ;;;;
(defun my-frame-config (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    ))

;; Transparencias
;;(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;;(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;;===============================
;; CONFIGURACIONES AUTOMÁTICAS
;;===============================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-file-commands
   (quote
    (counsel-dired-jump counsel-find-file counsel-file-jump counsel-find-library counsel-git counsel-projectile-find-dir counsel-projectile-find-file counsel-recentf)))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(citeproc-org-bibtex-export-use-affixes t)
 '(citeproc-org-suppress-author-cite-link-types (quote ("citet")))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (nord)))
 '(custom-safe-themes
   (quote
    ("a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "f1e97d06df4664396093529be437bac344f2737b60d9d60b70d85455f9c26a7f" "0fb2699a9fdcb5a0eda9f90c002e7c65cd4c6a82096e4ad05deef1a9a2292e49" "cbcfae366ef502108e54223da514f24b9162d8d191d436fdc447f938015f74da" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "43eea8fb35170e00dbe2b4422af5eb26c29c7cff9055ecca511ffae2f3aa51aa" "13325a954fce38bc72d81a93572585e21bdff745892f643a7c9d038486d3516d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "f66ffeadda7b52d40c8d698967ae9e9836f54324445af95610d257fa5e3e1e21" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5" "1e9001d2f6ffb095eafd9514b4d5974b720b275143fbc89ea046495a99c940b0" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(debug-on-error nil)
 '(delete-selection-mode nil)
 '(dired-open-extensions (quote ((".pdf" . "zathura"))))
 '(display-line-numbers (quote visual))
 '(elfeed-goodies/powerline-default-separator (quote wave))
 '(evil-want-C-i-jump nil)
 '(fci-rule-color "#383838")
 '(flyspell-default-dictionary "espanol")
 '(frame-background-mode nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#1c1f26")
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#A5A4AA")
     ("NEXT" . "#A5A4AA")
     ("THEM" . "#FB8B86")
     ("PROG" . "#B0A0A1")
     ("OKAY" . "#B0A0A1")
     ("DONT" . "#9E9DA4")
     ("FAIL" . "#9E9DA4")
     ("DONE" . "#b6b0b6")
     ("NOTE" . "#A5A4AA")
     ("KLUDGE" . "#A5A4AA")
     ("HACK" . "#A5A4AA")
     ("TEMP" . "#A5A4AA")
     ("FIXME" . "#A5A4AA")
     ("XXX+" . "#A5A4AA")
     ("\\?\\?\\?+" . "#A5A4AA"))))
 '(inhibit-startup-screen t)
 '(ivy-prescient-mode t)
 '(ivy-prescient-retain-classic-highlighting t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(magit-diff-use-overlays nil)
 '(neo-theme (quote icons))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(objed-cursor-color "#ff6c6b")
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (todo "TODO" nil))
      nil))))
 '(org-agenda-files (quote ("~/Drive/sync/gtd/a.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "zathura %s"))))
 '(org-re-reveal-script-files (quote ("js/reveal.js")))
 '(package-selected-packages
   (quote
    (writefreely ewal-spacemacs-theme ewal-evil-cursors ewal-spacemacs-themes ewal writeroom-mode writeroom flymake-shellcheck rc-mode ivy-prescient prescient flyspell-correct-ivy oer-reveal org-re-reveal-ref org-re-reveal haskell-mode beacon doom-themes auctex-latexmk doom-modeline dired-open evil-org org-super-agenda evil-collection all-the-icons-ivy all-the-icons-dired all-the-icons smtpmail-multi frames-only-mode flymd yequake noflet evil-magit lua-mode counsel pdf-tools nov powerline solarized-theme magit helm-projectile swiper-helm mu4e-alert citeproc-org ox-word ox-pandoc auctex org-ref neotree spaceline smart-mode-line-atom-one-dark-theme smart-mode-line airline-themes evil rainbow-delimiters rainbow-delimeters expand-region auto-complete try foo 2048-game chess ace-window ztree counsel-projectile projectile org-beamer-mode demo-it latex-math-preview yasnippet-snippets yasnippet markdown-preview-mode markdown-mode+ markdown-mode epresent htmlize ox-reveal company dashboard switch-window avy smex ido-vertical-mode spacemacs-theme org-bullets nord-theme zenburn-theme telephone-line which-key use-package rich-minority python material-theme arjen-grey-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pdf-view-resize-factor 1.05)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(prescient-filter-method (quote (literal regexp initialism fuzzy)))
 '(rainbow-delimiters-max-face-count 5)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(telephone-line-height 20)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(writeroom-fullscreen-effect (quote maximized))
 '(writeroom-global-effects
   (quote
    (writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-vertical-scroll-bars writeroom-set-bottom-divider-width)))
 '(writeroom-mode-line t)
 '(writeroom-width 100)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
 '(yequake-frames
   (quote
    (("mu4e"
      (buffer-fns yequake mu4e)
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters
       (undecorated . t)
       (skip-taskbar . t)
       (quote
	(name . "mu4e"))
       (sticky . t)))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#222D32" :foreground "#D8DEE9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "Fantasque Sans Mono"))))
 '(doom-modeline-evil-emacs-state ((t (:inherit doom-modeline-warning))))
 '(doom-modeline-evil-insert-state ((t (:inherit doom-modeline-urgent :foreground "sea green"))))
 '(doom-modeline-evil-motion-state ((t (:inherit doom-modeline-buffer-path :foreground "dark blue"))))
 '(doom-modeline-evil-normal-state ((t (:inherit doom-modeline-info :foreground "DeepSkyBlue"))))
 '(doom-modeline-evil-operator-state ((t (:inherit doom-modeline-buffer-path :foreground "violet"))))
 '(doom-modeline-evil-replace-state ((t (:inherit doom-modeline-buffer-modified :foreground "black"))))
 '(doom-modeline-evil-visual-state ((t (:inherit doom-modeline-buffer-file :foreground "dark orange"))))
 '(fringe ((t (:background "#222D32" :foreground "#D8DEE9"))))
 '(highlight ((t nil)))
 '(ivy-prompt-match ((t (:background "red"))))
 '(line-number-current-line ((t (:inherit \#D8DEE9))))
 '(mode-line ((t (:background "#222D32" :foreground "#88C0D0"))))
 '(mode-line-inactive ((t (:background "#222D32" :foreground "#D8DEE9"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray45"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "dark violet"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "goldenrod"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan4"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "sky blue"))))
 '(spaceline-evil-normal ((t (:background "#BF616A" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-highlight-face ((t (:background "SkyBlue2" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-modified ((t (:background "#BF616A" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-unmodified ((t (:background "SkyBlue2" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(telephone-line-accent-active ((t (:inherit mode-line :background "#3B4251" :foreground "white"))))
 '(telephone-line-evil-insert ((t (:inherit telephone-line-evil :background "sea green"))))
 '(telephone-line-evil-normal ((t (:inherit telephone-line-evil :background "DeepSkyBlue4")))))

