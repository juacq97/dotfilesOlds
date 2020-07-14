;   █████  ██████████   ██████    █████   ██████
;  ██░░░██░░██░░██░░██ ░░░░░░██  ██░░░██ ██░░░░ 
; ░███████ ░██ ░██ ░██  ███████ ░██  ░░ ░░█████ 
; ░██░░░░  ░██ ░██ ░██ ██░░░░██ ░██   ██ ░░░░░██
; ░░██████ ███ ░██ ░██░░████████░░█████  ██████ 
;  ░░░░░░ ░░░  ░░  ░░  ░░░░░░░░  ░░░░░  ░░░░░░ 
;
;=================================================

;; Configurar MELPA para use-package
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Repositorio local
(add-to-list 'load-path "~/.emacs.d/modes")

;; Instala use-package en caso de no estar
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

;; Evil-mode
;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq-default evil-cross-lines t)
)

;; Movimiento respeta las lineas visuales
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

;; Algunos atajos basados en SPC como leader key
(define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-current-buffer)
(define-key evil-normal-state-map (kbd "SPC b") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC s") 'swiper)
(define-key evil-normal-state-map (kbd "SPC p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC P") 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC g") 'magit)
(define-key evil-normal-state-map (kbd "SPC v") 'visual-line-mode)
(define-key evil-normal-state-map (kbd "SPC t") (lambda () (interactive) (shell-command "st > /dev/null 2>&1 & disown")))

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

;;;; Which key
;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;;;; Ivy-mode
;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))
(setq ivy-extra-directories nil)

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :ensure t
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  :bind (
	  ("M-x" . counsel-M-x)
	  ("C-x C-f" . counsel-find-file)))
(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

(use-package swiper
  :ensure t
  :bind (
	 ("C-s" . swiper)))

;;;; Yasnippet ;;;;;
(use-package yasnippet
   :ensure t
   :config
   (yas-global-mode))

;;;; Magit ;;;;;
(use-package magit
  :ensure t)
(global-set-key (kbd "C-x C-g") 'magit)

;;;; Markdown-mode ;;;;
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(setq markdown-command "/usr/bin/pandoc")

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

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;;;; Doom-themes ;;;;
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )
;(add-hook 'after-make-frame-functions 'my-frame-config)
;(add-hook 'after-init-hook 'my-frame-config)

;; heaven and hell ;;;;
(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-themes
        '((light . doom-one-light)
          (dark . doom-dracula)))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f7>" . heaven-and-hell-load-default-theme)
         ("<f7>" . heaven-and-hell-toggle-theme)))

;;;; Writeroom-mode ;;;;
(use-package writeroom-mode
  :ensure t
  :bind (
	 ("<f6>" . writeroom-mode)))

;;; Focus-mode
(use-package focus
  :ensure t)

;;; Ewal ;;;;
(use-package ewal
  :ensure t
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))

(use-package ewal-doom-themes
  :ensure t)

;;;; Smart Parents ;;;;
(use-package smartparens
  :ensure t
  :config
  (smartparens-mode t))

;;;; Rainbow delimitersa ;;;;
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Projectile ;;;
(use-package projectile
  :ensure t
;  :bind 
;	 ("C-x C-p" . projectile)
  :config
  (projectile-mode 1)) 

;;; Easy hugo ;;;
(use-package easy-hugo
  :ensure t
  :init 
;;; Main blog
  (setq easy-hugo-basedir "/mnt/Data/Blog/")
  (setq easy-hugo-postdir "content/posts/")
  :config
  (add-to-list 'evil-emacs-state-modes 'easy-hugo-mode)
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-org-header t)
 )

(use-package terminal-here
  :ensure t
  :config
  (setq terminal-here-terminal-command "st"))

(use-package ranger
  :ensure t
  )


;;==========
;; Org-mode
;;==========

(setq org-fontify-done-headline nil)
(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;;(font-lock-add-keywords 'org-mode
;;                        '(("^ +\\([-*]\\) "
;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;;
;;(let* (
;;       (base-font-color     (face-foreground 'default nil 'default))
;;       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;
;;(custom-theme-set-faces 'user
;;			`(org-level-8 ((t (,@headline))))
;;			`(org-level-7 ((t (,@headline))))
;;			`(org-level-6 ((t (,@headline))))
;;			`(org-level-5 ((t (,@headline))))
;;			`(org-level-4 ((t (,@headline :height 1.1))))
;;                        `(org-level-3 ((t (,@headline :height 1.25))))
;;			`(org-level-2 ((t (,@headline :height 1.5))))
;;			`(org-level-1 ((t (,@headline :height 1.75))))
;;			`(org-document-title  ((t (, :height 1.5 :underline nil))))))

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

;; Exportar en beamer
(org-beamer-mode)

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
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

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
(column-number-mode 1)

;;;; Matar con C-x k ;;;;
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;;;; Visual line mode con C-c v ;;;;
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-visual-line-mode 1)

;;;; Flyspell con F5 ;;;;
(global-set-key (kbd "<f5>")  'ispell-word) 

;;;; Mostrar números de líneas ;;;;
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;;;; Resaltar el otro parentesis
(show-paren-mode t)

;;; Cursor no parpadeante
(blink-cursor-mode 0)

;; Text scratch como buffer inicial
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode (quote text-mode))
(setq vc-follow-symlinks t)

;; set font for emoji
(set-fontset-font "fontset-default" '(#xF0000 . #xF14FF) "Material Design Icons")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-rich-mode t)
 '(ansi-color-names-vector
   ["#0D0E16" "#D83441" "#79D836" "#D8B941" "#3679D8" "#8041D8" "#36D8BD" "#CEDBE5"])
 '(custom-enabled-themes (quote (doom-dracula)))
 '(custom-safe-themes
   (quote
    ("99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e30e72b10b9c7887ff8adcd1a25b5c6eaa32665e0f8f40994e5b6d51069d3b2a" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "f2b56244ecc6f4b952b2bcb1d7e517f1f4272876a8c873b378f5cf68e904bd59" "9f15d03580b08dae41a1e5c1f00d1f1aa99fea121ca32c28e2abec9563c6e32c" "379a804655efccc13a3d446468992bfdfc30ff27d19cfda6f62c7f9c9e7a8a7d" "a92e9da0fab90cbec4af4a2035602208cebf3d071ea547157b2bfc5d9bd4d48d" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "aa5dee47c85f12d166745ae56c778eb7833df3f6799c2b2d607d5b8da8f5f579" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" default)))
 '(doom-modeline-modal-icon nil)
 '(evil-echo-state nil)
 '(evil-want-minibuffer nil)
 '(fci-rule-color "#767676")
 '(global-display-line-numbers-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#0F1019" "#D85F00"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0F1019" "#79D836"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0F1019" "#767676"))
 '(objed-cursor-color "#D83441")
 '(package-selected-packages
   (quote
    (parse-csv edit-indirect image-dired+ ranger dired-ranger terminal-here projectile zone-nyan nyan-mode selectric-mode landmark fireplace parrot volume easy-hugo htmlize swiper-helm helm-swiper selectrum-prescient selectrum evil-magit evil-org evil-collection amx bug-hunter org-bullets focus smartparens ewal-doom-themes ewal olivetti writeroom-mode heaven-and-hell doom-themes all-the-icons-ivy-rich ivy-rich all-the-icons-ivy all-the-icons-dired neotree rainbow-mode markdown-mode magit yasnippet ivy-prescient counsel ivy helm which-key evil doom-modeline use-package)))
 '(pdf-view-midnight-colors (cons "#CEDBE5" "#0D0E16"))
 '(prescient-persist-mode t)
 '(rustic-ansi-faces
   ["#0D0E16" "#D83441" "#79D836" "#D8B941" "#3679D8" "#8041D8" "#36D8BD" "#CEDBE5"])
 '(vc-annotate-background "#0D0E16")
 '(vc-annotate-color-map
   (list
    (cons 20 "#79D836")
    (cons 40 "#98cd39")
    (cons 60 "#b8c33d")
    (cons 80 "#D8B941")
    (cons 100 "#d89b2b")
    (cons 120 "#d87d15")
    (cons 140 "#D85F00")
    (cons 160 "#ba5548")
    (cons 180 "#9d4b90")
    (cons 200 "#8041D8")
    (cons 220 "#9d3ca5")
    (cons 240 "#ba3873")
    (cons 260 "#D83441")
    (cons 280 "#bf444e")
    (cons 300 "#a7555b")
    (cons 320 "#8e6568")
    (cons 340 "#767676")
    (cons 360 "#767676")))
 '(vc-annotate-very-old-color nil)
 '(writeroom-fullscreen-effect (quote maximized)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "ADBO" :family "JetBrains Mono"))))
 '(org-done ((t (:inherit org-headline-done :background "#282a36" :foreground "#b8bb26" :strike-through nil :weight bold))))
 '(org-headline-done ((t (:foreground "#b8bb26"))))
 '(org-todo ((t (:foreground "#cc241d" :weight bold)))))
