(defalias 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 100)

(global-hl-line-mode t)

(setq make-backup-files nil) ;desactiva todos los backups
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backups/")))
(setq auto-save-list-file-prefix nil)
(setq auto-save-default nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "M-m") 'menu-bar-mode) ; M-m hace aparecer el menú

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

  (defun config-reload ()
    (interactive)
    (org-babel-load-file "~/.repos/dotfiles/.emacs.d/config.org"))
  (global-set-key (kbd "C-c r") 'config-reload)

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

(column-number-mode 1)

(global-set-key (kbd "C-x k") 'kill-current-buffer)

(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-visual-line-mode 1)

(global-set-key (kbd "<f5>")  'ispell-word)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(show-paren-mode t)

(blink-cursor-mode 0)

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode (quote text-mode))
(setq vc-follow-symlinks t)

(set-fontset-font "fontset-default" '(#xF0000 . #xF14FF) "Material Design Icons")

(define-minor-mode my/variable-pitch-mode
  "Toggle `variable-pitch-mode', except for `prog-mode'."
  :init-value nil
  :global nil
  (if my/variable-pitch-mode
      (unless (derived-mode-p 'prog-mode)
	(variable-pitch-mode 1))
    (variable-pitch-mode -1)))

(setq TeX-auto-save t)
 (setq TeX-parse-self t)
 (setq-default TeX-master nil)
(with-eval-after-load 'tex
 (setq TeX-source-correlate-method 'synctex)
 (TeX-source-correlate-mode)
 (setq TeX-source-correlate-start-server t)

 (add-to-list 'TeX-view-program-selection
	      '(output-pdf "Zathura")))
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
		'("XeLaTeX" "xelatex -interaction=nonstopmode %s"
		  TeX-run-command t t :help "Run xelatex") t))

(use-package doom-modeline
:ensure t
:hook (after-init . doom-modeline-mode)
:config
(setq doom-modeline-height 24)
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

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

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

(use-package yasnippet
   :ensure t
   :config
   (yas-global-mode))

(use-package magit
  :ensure t)
(global-set-key (kbd "C-x C-g") 'magit)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(setq markdown-command "/usr/bin/pandoc")

(use-package rainbow-mode
   :ensure t
   :init (rainbow-mode 1))

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
;  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )
;(add-hook 'after-make-frame-functions 'my-frame-config)
;(add-hook 'after-init-hook 'my-frame-config)

(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-themes
        '((light . doom-nord-light)
          (dark . doom-dracula)))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f7>" . heaven-and-hell-load-default-theme)
         ("<f7>" . heaven-and-hell-toggle-theme)))

(use-package writeroom-mode
    :ensure t
    :bind ("<f6>" . writeroom-mode))

(add-hook 'writeroom-mode-hook
	  #'(lambda ()
	    (my/variable-pitch-mode 1)))

(add-hook 'writeroom-mode-disable-hook
	 #'(lambda ()
	   (my/variable-pitch-mode -1)))

(use-package focus
  :ensure t)

(use-package ewal
  :ensure t
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))

(use-package ewal-doom-themes
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-mode t))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package projectile
  :ensure t
;  :bind 
;	 ("C-x C-p" . projectile)
  :config
  (projectile-mode 1))

(use-package easy-hugo
  :ensure t
  :init 
;;; Main blog
  (setq easy-hugo-basedir "/mnt/Data/Blog/")
  (setq easy-hugo-postdir "content/posts/")
  :config
  (add-to-list 'evil-emacs-state-modes 'easy-hugo-mode)
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-org-header t))

(use-package terminal-here
  :ensure t
  :config
  (setq terminal-here-terminal-command "st"))

(use-package org-tree-slide
  :ensure t
  :config
  (setq org-tree-slide-display-header-toggle nil)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-modeline-display nil)
  )

(eval-after-load "org-tree-slide"
  '(progn
     (add-hook 'org-tree-slide-play-hook
	       (lambda ()
		 (org-display-inline-images 1)
		 (my/variable-pitch-mode 1)))
     (add-hook 'org-tree-slide-stop-hook
	       (lambda ()
		 (org-display-inline-images -1)
		 (my/variable-pitch-mode -1)))))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* (
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

(custom-theme-set-faces 'user
			`(org-level-8 ((t (,@headline))))
			`(org-level-7 ((t (,@headline))))
			`(org-level-6 ((t (,@headline))))
			`(org-level-5 ((t (,@headline))))
			`(org-level-4 ((t (,@headline :height 1.1))))
                        `(org-level-3 ((t (,@headline :height 1.25))))
			`(org-level-2 ((t (,@headline :height 1.5))))
			`(org-level-1 ((t (,@headline :height 1.75))))
			`(org-document-title  ((t (, :height 1.5 :underline nil))))))

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

(global-set-key (kbd "C-c l") 'org-store-link)

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

(setq org-support-shift-select t)

(org-beamer-mode)

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

(add-to-list 'org-latex-classes
	     '("moderncv"
	       "\\documentclass{moderncv}"
	       ("\\section{%s}" . "\\section*{%s}}")
	       ("\\subsection{%s}" . "\\subsection*{%s}}")
	       )
	     )

(set-face-attribute 'default nil :font "Source Code Pro-10")
     (set-face-attribute 'fixed-pitch nil :font "Source Code Pro-10")
     (set-face-attribute 'variable-pitch nil :font "Nimbus Sans-12")

     (dolist (face '(default fixed-pitch))
       (set-face-attribute `,face nil :font "Source Code Pro-10"))
       
(custom-theme-set-faces 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit fixed-pitch))))
 '(org-block-end-line ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit fixed-pitch))))
 '(org-meta-line ((t (:inherit fixed-pitch))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit fixed-pitch))))
)
