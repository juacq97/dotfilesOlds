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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;=================
;; PAQUETES
;;=================

;; Doom modeline
;;=================

(use-package doom-modeline
:ensure t
:hook (after-init . doom-modeline-mode)
:config
(setq doom-modeline-height 20)
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
(setq doom-modeline-irc-stylize 'identity))
    
;; org-mode 
;;===========

(use-package org-superstar
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-item-bullet-alist t))

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
;(add-to-list 'load-path "/home/juan/.emacs.d/modes/")
;(require 'citeproc-org)
;  (citeproc-org-setup)
;'(citeproc-org-locales-dir "/home/equipo/.emacs.d/csl-locales/")

;; <el TAB abre bloque de código elisp
(add-to-list 'org-structure-template-alist
       '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; Exportar en beamer
(org-beamer-mode)

;; Exportar en pandoc
;(use-package ox-pandoc
;  :ensure t)
;(setq helm-bibtex-format-citation-functions
;      '((org-mode . (lambda (x) (insert (concat
;                                         "\\cite{"
;                                         (mapconcat 'identity x ",")
;                                         "}")) ""))))

;; Exportar en reveal.js
(use-package org-re-reveal
  :ensure t)
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

;; Rainbow delimiters
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Evil-mode
;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
  
;; Make movement keys work respect visual lines
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

;; Algunos atajos basados en SPC como leader key
(define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-current-buffer)
(define-key evil-normal-state-map (kbd "SPC b") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd "SPC s") 'swiper)
(define-key evil-normal-state-map (kbd "SPC p p") 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC p f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC g") 'magit)
(define-key evil-normal-state-map (kbd "SPC v") 'visual-line-mode)

;; Make horizontal movement cross lines
(setq-default evil-cross-lines t)

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
  (evil-collection-init))

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

;; Dashboard 
;;;;;;;;;;;;;;;;;

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((projects . 5)
			  (recents . 5)))
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Pizza Mozzarella! Pizza Mozzarella! Rella Rella Rella Rella...")
  (setq show-week-agenda-p t)
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)
  (defun dashboard-insert-custom (list-size)
    (insert "Custom text"))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  )

(define-key dashboard-mode-map (kbd "SPC SPC") 'counsel-find-file)
(define-key dashboard-mode-map (kbd "SPC k") 'kill-current-buffer)
(define-key dashboard-mode-map (kbd "SPC b") 'switch-to-buffer)
(define-key dashboard-mode-map (kbd "SPC s") 'swiper)
(define-key dashboard-mode-map (kbd "SPC p p") 'projectile-switch-project)
(define-key dashboard-mode-map (kbd "SPC p f") 'projectile-find-file)
(define-key dashboard-mode-map (kbd "SPC g") 'magit)
(define-key dashboard-mode-map (kbd "SPC v") 'visual-line-mode)


;;;; Which key ;;;;
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;;; Ivy ;;;;
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode 1))

(use-package counsel
  :ensure t
   :config
   (counsel-mode 1)
   :bind (
	  ("M-x" . counsel-M-x)
	  ("C-x C-f" . counsel-find-file)))

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
   (yas-global-mode))

   (use-package yasnippet-snippets
   :ensure t)
   (yas-reload-all)

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
  :bind (("C-s" . swiper)))

;;;; Magit ;;;;;
(use-package magit
  :ensure t)
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
    (global-auto-complete-mode t)))

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
  (global-set-key [f8] 'neotree-toggle))

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
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-themes
        '((light . tango)
;          (dark . ewal-spacemacs-modern)))
;          (dark . xresources)))
;          (dark . dracula)))
          (dark . ewal-doom-one)))
;          (dark . gruvbox-dark-hard)))
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
    :ensure t)
(global-set-key [f6] 'writeroom-mode)

;;; Ewal ;;;;
(use-package ewal
  :ensure t
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))

(use-package ewal-doom-themes
  :ensure t)

;; theme sanitync

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

;; theme solarized
(use-package solarized-theme
  :ensure t)

;; theme dracula
;(use-package dracula-theme
;  :ensure t)


;; Smart Parents
(use-package smartparens
  :ensure t
  :config
  (smartparens-mode t))

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
    (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
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

;;;; Mostrar línea actual en modeline ;;;;;
(line-number-mode 1)
(column-number-mode 1)

;;;; Matar con C-x k ;;;;
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;;;; Visual line mode con C-c v ;;;;
(global-set-key (kbd "C-c v") 'visual-line-mode)

;;;; Flyspell con F5 ;;;;
(global-set-key (kbd "<f5>")  'ispell-word) 

;;;; Mostrar números de líneas ;;;;
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;;;; Buffer inicial ;;;;
(defun my-frame-config (&optional frame)
  (with-selected-frame (or frame (selected-frame))))

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
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#161511" "#625a51" "#7b692c" "#786c5b" "#B1994E" "#9C8C6A" "#7B8280" "#c8c4c0"])
 '(beacon-color "#ff9da4")
 '(custom-enabled-themes (quote (ewal-doom-one)))
 '(custom-safe-themes
   (quote
    ("845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "56911bd75304fdb19619c9cb4c7b0511214d93f18e566e5b954416756a20cc80" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "a41b81af6336bd822137d4341f7e16495a49b06c180d6a6417bf9fd1001b6d2b" "ba72dfc6bb260a9d8609136b9166e04ad0292b9760a3e2431cf0cd0679f83c3a" "41098e2f8fa67dc51bbe89cce4fb7109f53a164e3a92356964c72f76d068587e" default)))
 '(fci-rule-color "#003f8e")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(hl-todo-keyword-faces
   (quote
    (("TODO")
     ("NEXT")
     ("THEM")
     ("PROG")
     ("OKAY")
     ("DONT")
     ("FAIL")
     ("DONE")
     ("NOTE")
     ("KLUDGE")
     ("HACK")
     ("TEMP")
     ("FIXME")
     ("XXX+")
     ("\\?\\?\\?+"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#100f0c" "#B1994E"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100f0c" "#7b692c"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100f0c" "#21201c"))
 '(objed-cursor-color "#625a51")
 '(package-selected-packages
   (quote
    (yaml-mode haskell-mode bug-hunter ewal-doom-themes yasnippet-snippets writeroom-mode which-key use-package todotxt solarized-theme smartparens rainbow-mode rainbow-delimiters projectile ox-pandoc org-superstar org-re-reveal-ref oer-reveal nov noflet neotree markdown-mode ivy-prescient heaven-and-hell ewal-spacemacs-themes evil-org evil-magit evil-collection dracula-theme doom-modeline dashboard counsel color-theme-sanityinc-tomorrow beacon auto-complete all-the-icons-ivy all-the-icons-dired ace-window)))
 '(pdf-view-midnight-colors (cons "#c8c4c0" "#161511"))
 '(rustic-ansi-faces
   ["#161511" "#625a51" "#7b692c" "#786c5b" "#B1994E" "#9C8C6A" "#7B8280" "#c8c4c0"])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:box nil)))))
