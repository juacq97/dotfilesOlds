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
(setq doom-modeline-height 25)
(setq doom-modeline-bar-width 4)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-persp-name t)
(setq doom-modeline-lsp nil)
(setq doom-modeline-github t)
(setq doom-modeline-github-interval (* 30 60))
(setq doom-modeline-env-version t)
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)
(setq doom-modeline-env-python-executable "python")
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")
(setq doom-modeline-mu4e t)
(setq doom-modeline-irc t)
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
(setq org-agenda-span 3)
(setq org-agenda-start-on-weekday nil)
(setq calendar-day-name-array ["domingo" "lunes" "martes" "miércoles" "jueves" "viernes" "sábado"])
(setq calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo" "junio" "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre"])

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
      '(
	("i" "Inbox" entry
	 (file "~/Drive/GTD/inbox.org")
	 "* %?\n%u" :prepend t)

;	("t" "TODO" entry
;	 (file "~/Drive/GTD/0gtd.org")
;	 "* TODO %?\n%u" :prepend t)

;	("a" "Agenda"  entry
;	 (file+headline "~/Drive/sync/GTD/0gtd.org" "Agenda")
;	 "* EVENTO %?\n SCHEDULED: %t")
	
	("n" "Notas" entry
	 (file+headline "~/Drive/GTD/referencias.org" "Notas")
	 "* %?" :prepend t)

	("d" "Diario" entry
	 (file+olp+datetree "~/Drive/SEC-ABREOJOS/DIARIO.org")
	 "* %?" :prepend t)))

;; Soporte para seleccionar con shift 
(setq org-support-shift-select t)

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

;; Org-refile GTD
;(setq org-refile-use-outline-path 'file)
;(setq org-refile-targets '((org-agenda-files :level . 3)))

;; Cambiar TODO state con C-ñ
(eval-after-load 'org 
  '(define-key org-mode-map (kbd "C-ñ") 'org-todo))
(eval-after-load 'org 
  '(define-key org-mode-map (kbd "C-c m") 'org-refile))
(eval-after-load 'org-agenda
		    '(define-key org-agenda-mode-map (kbd "ñ") 'org-agenda-todo))


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

;; Macro para tachar líneas
(fset 'Tachar\ lineas\ org-mode
   [?i ?+ escape ?A ?+ escape])
(evil-set-register ?m [?i ?+ escape ?A ?+ escape])

;; Macro para tachar líneas y añadir un salto al final
(fset 'Tachar\ línea\ org-mode\ con\ salto
   [?i ?+ escape ?A ?+ return escape])
(evil-set-register ?a [?i ?+ escape ?A ?+ return escape])

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
  (setq dashboard-items '((projects . 5)
			  (recents . 5)
			  (registers . 5)))
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Pizza Mozzarella! Pizza Mozzarella! Rella Rella Rella Rella...")
  (setq show-week-agenda-p t)
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)
  (defun dashboard-insert-custom (list-size)
    (insert "Custom text"))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
)

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;;;; Which key ;;;;
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;;; Ivy ;;;;
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
   :bind (
	  ("M-x" . counsel-M-x)
	  ("C-x C-f" . counsel-find-file)
	  )
   )
 (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-immediate-done)
 (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
 (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
 (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
 (define-key ivy-minibuffer-map (kbd "C-d") #'ivy-scroll-up-command)
 (define-key ivy-minibuffer-map (kbd "C-u") #'ivy-scroll-down-command)

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
  (projectile-global-mode))
(global-set-key (kbd "C-c p") 'projectile-switch-project)
(global-set-key (kbd "C-c f") 'projectile-find-file)

;;; Swiper ;;;;;
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
;(use-package doom-themes
;  :ensure t
;  :config
;  
;  ;; Global settings (defaults)
;  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;	doom-themes-enable-italic t) ; if nil, italics is universally disabled
;  
;  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;  ;; may have their own settings.
;  (load-theme 'doom-vibrant t)
;
;  ;; Enable flashing mode-line on errors
;  (doom-themes-visual-bell-config)
;
;  ;; Enable custom neotree theme (all-the-icons must be installed!)
;  (doom-themes-neotree-config)
;  ;; or for treemacs users
;  (doom-themes-treemacs-config)
;
;  ;; Corrects (and improves) org-mode's native fontification.
;  (doom-themes-org-config)
;  )
;
;(add-hook 'after-make-frame-functions 'my-frame-config)
;(add-hook 'after-init-hook 'my-frame-config)


;; heaven and hell ;;;;
(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . tango)
          (dark . ewal-spacemacs-classic))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f7>" . heaven-and-hell-load-default-theme)
         ("<f7>" . heaven-and-hell-toggle-theme)))


;;;; Beacon ;;;;
(use-package beacon
  :ensure t
  :config
  (beacon-mode t))

;;;; Writeroom-mode ;;;;
(use-package writeroom-mode
    :ensure t
    )
(global-set-key [f6] 'writeroom-mode)

;;; Ewal ;;;;
(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))

(use-package ewal-spacemacs-themes
  :init (progn
          (setq spacemacs-theme-underline-parens t
                my:rice:font (font-spec
                              :family "Fantasque Sans Mono"
                              :weight 'semi-bold
                              :size 12.0))
          (show-paren-mode +1)
          (global-hl-line-mode)
          (set-frame-font my:rice:font nil t)
          (add-to-list  'default-frame-alist
                        `(font . ,(font-xlfd-name my:rice:font))))
  :config (progn
            (load-theme 'ewal-spacemacs-modern t)
            (enable-theme 'ewal-spacemacs-modern)))

(use-package ewal-evil-cursors
  :after (ewal-spacemacs-themes)
  :config (ewal-evil-cursors-get-colors
           :apply t :spaceline t))
;(use-package ewal
;  :ensure t
;  :config
;  (setq ewal-wal-cache-dir "~/.cache/wal/ewal.json")
;
; :init (setq ewal-use-built-in-always-p nil
;              ewal-use-built-in-on-failure-p t
;              ewal-built-in-palette "sexy-material")
;)
;;(when (ewal-load-wal-colors)
;;  (custom-set-faces
;;   `(line-number ((t (:foreground ,(ewal-get-color 'magenta 4)))))))
;(load-theme 'ewal-spacemacs-modern t)


;; Nord theme ;;;;
;(use-package nord-theme
;  :ensure t
;  )
;  (load-theme 'nord t)
;;; Use `nord4` from Nord's "Snow Storm" palette as background color.
;(setq nord-region-highlight "snowstorm")

;;;; Super Agenda ;;;;
;(use-package org-super-agenda
  ;:ensure t
  ;:config
  ;(org-super-agenda-mode t))
;
;(setq org-super-agenda-groups
       ;'(;; Each group has an implicit boolean OR operator between its selectors.
         ;(:name "Hoy"  ; Optionally specify section name
                ;:time-grid t  ; Items that appear on the time grid
                ;:todo "TODO")  ; Items that have this TODO keyword
         ;(:name "Escuela"
                ;:tag "escuela")
         ;(:name "Casa"
                ;:tag "casa")
         ;(:todo "EPERANDO" :order 8)  ; Set order of this section
         ;))
  ;(org-agenda nil "a")

;;;; TODO-TXT ;;;;
(use-package todotxt
  :ensure t
  :config
  (setq todotxt-file "~/Drive/GTD/todo.txt")
  (add-to-list 'evil-emacs-state-modes 'todotxt-mode))
;; Todo list flotante
(defun todotxt-frame ()
  (interactive)
  (todotxt)
  (delete-other-windows))

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
;(set-frame-parameter (selected-frame) 'alpha '(80 . 80))
;(add-to-list 'default-frame-alist '(alpha . (80 . 80)))

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
 '(beacon-blink-delay 0.3)
 '(beacon-blink-duration 0.5)
 '(beacon-blink-when-focused t)
 '(citeproc-org-bibtex-export-use-affixes t)
 '(citeproc-org-suppress-author-cite-link-types (quote ("citet")))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (ewal-spacemacs-classic)))
 '(custom-safe-themes
   (quote
    ("d13405bf452b05bf05ee12cac44810dcfa16a5bef7ad62116fd86a2ed8a6345b" "018c8326bced5102b4c1b84e1739ba3c7602019c645875459f5e6dfc6b9d9437" "70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "8f2390ac61c567e3576d684467aa6eb616b403f5ddfbd1f9daf9aea866f74b1e" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "fe76f3d5094967034192f6a505085db8db6deb0e135749d9a54dc488d6d3ee2f" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "41098e2f8fa67dc51bbe89cce4fb7109f53a164e3a92356964c72f76d068587e" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "f1e97d06df4664396093529be437bac344f2737b60d9d60b70d85455f9c26a7f" "0fb2699a9fdcb5a0eda9f90c002e7c65cd4c6a82096e4ad05deef1a9a2292e49" "cbcfae366ef502108e54223da514f24b9162d8d191d436fdc447f938015f74da" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "43eea8fb35170e00dbe2b4422af5eb26c29c7cff9055ecca511ffae2f3aa51aa" "13325a954fce38bc72d81a93572585e21bdff745892f643a7c9d038486d3516d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "f66ffeadda7b52d40c8d698967ae9e9836f54324445af95610d257fa5e3e1e21" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5" "1e9001d2f6ffb095eafd9514b4d5974b720b275143fbc89ea046495a99c940b0" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(dashboard-page-separator "

")
 '(dashboard-set-heading-icons nil)
 '(dashboard-set-init-info nil)
 '(debug-on-error nil)
 '(delete-selection-mode nil)
 '(dired-open-extensions (quote ((".pdf" . "zathura"))))
 '(display-line-numbers (quote visual))
 '(elfeed-goodies/powerline-default-separator (quote wave))
 '(evil-want-C-i-jump nil)
 '(ewal-dark-palette-p nil)
 '(fci-rule-color "#383838")
 '(flyspell-default-dictionary "espanol")
 '(frame-background-mode nil)
 '(helm-mode nil)
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
 '(org-agenda-files (quote ("~/Drive/SEC-ABREOJOS/PLANEACIONES/3.org")))
 '(org-archive-default-command (quote org-archive-subtree))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "zathura %s"))))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 3.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file-other-frame)
     (wl . wl-other-frame))))
 '(org-log-note-headings
   (quote
    ((done . "CLOSING NOTE %t")
     (state . "%-12s %d")
     (note . "Nota tomada el %d")
     (reschedule . "Rescheduled from %S on %t")
     (delschedule . "Not scheduled, was %S on %t")
     (redeadline . "New deadline from %S on %t")
     (deldeadline . "Removed deadline, was %S on %t")
     (refile . "Refiled on %t")
     (clock-out . ""))))
 '(org-re-reveal-script-files (quote ("js/reveal.js")))
 '(package-selected-packages
   (quote
    (nyan-mode heaven-and-hell doom-themes todotxt org bug-hunter writefreely ewal-spacemacs-theme ewal-evil-cursors ewal-spacemacs-themes ewal writeroom-mode writeroom flymake-shellcheck rc-mode ivy-prescient prescient flyspell-correct-ivy oer-reveal org-re-reveal-ref org-re-reveal haskell-mode beacon auctex-latexmk doom-modeline dired-open evil-org org-super-agenda evil-collection all-the-icons-ivy all-the-icons-dired all-the-icons smtpmail-multi frames-only-mode flymd yequake noflet evil-magit lua-mode counsel pdf-tools nov powerline solarized-theme magit helm-projectile swiper-helm mu4e-alert citeproc-org ox-word ox-pandoc auctex org-ref neotree spaceline smart-mode-line-atom-one-dark-theme smart-mode-line airline-themes evil rainbow-delimiters rainbow-delimeters expand-region auto-complete try foo 2048-game chess ace-window ztree counsel-projectile projectile org-beamer-mode demo-it latex-math-preview yasnippet-snippets yasnippet markdown-preview-mode markdown-mode+ markdown-mode epresent htmlize ox-reveal company dashboard switch-window avy smex ido-vertical-mode spacemacs-theme org-bullets nord-theme zenburn-theme telephone-line which-key use-package rich-minority python material-theme arjen-grey-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pdf-view-resize-factor 1.05)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(prescient-filter-method (quote (literal regexp initialism fuzzy)))
 '(rainbow-delimiters-max-face-count 5)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t)
 '(spacemacs-theme-keyword-italic nil)
 '(spacemacs-theme-org-height nil)
 '(spacemacs-theme-org-highlight nil)
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
 '(doom-modeline-evil-emacs-state ((t (:inherit doom-modeline-warning))))
 '(doom-modeline-evil-insert-state ((t (:inherit doom-modeline-urgent :foreground "sea green"))))
 '(doom-modeline-evil-motion-state ((t (:inherit doom-modeline-buffer-path :foreground "dark blue"))))
 '(doom-modeline-evil-normal-state ((t (:inherit doom-modeline-info :foreground "DeepSkyBlue"))))
 '(doom-modeline-evil-operator-state ((t (:inherit doom-modeline-buffer-path :foreground "violet"))))
 '(doom-modeline-evil-replace-state ((t (:inherit doom-modeline-buffer-modified :foreground "black"))))
 '(doom-modeline-evil-visual-state ((t (:inherit doom-modeline-buffer-file :foreground "dark orange"))))
 '(fringe ((t (:foreground "#adb2ba"))))
 '(ivy-prompt-match ((t (:background "red"))))
 '(line-number ((t (:foreground "#bb706a"))))
 '(line-number-current-line ((t (:foreground "#973343"))))
 '(org-block ((t (:background "#080913" :foreground "#e19b9d"))))
 '(org-block-begin-line ((t (:background "#080913" :foreground "#bb706b"))))
 '(org-block-end-line ((t (:background "#080913" :foreground "#bb706b"))))
 '(org-done ((t (:inherit bold :background "#006a7ca5" :foreground "#97a7cf"))))
 '(org-todo ((t (:inherit bold :background "#007c8085" :foreground "#92979D"))))
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

