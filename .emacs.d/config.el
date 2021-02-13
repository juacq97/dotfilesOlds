(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "M-m") 'menu-bar-mode) ; Opens the menu with M-m, very KDE-ish
;; The modeline shows the column number
(column-number-mode 1)
;; Enable line numbers at the left. Relative shows the numbers relative to the focused one
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
;; Match parenthesis
(show-paren-mode t)
;; Disable the blinking
(blink-cursor-mode 0)

;; Press y/n instead of the whole word
 (defalias 'yes-or-no-p 'y-or-n-p)
 ;; Scroll line by line. Cursor doesn't stays at the center of the screen
 (setq scroll-conservatively 100)
 ;; Disable backups. I'm not sure I want this disabled, but opening files it's veeeery slow
 (setq make-backup-files nil) 
 (setq backup-directory-alist
       `((".*" . ,"~/.emacs.d/backups/")))
 (setq auto-save-file-name-transforms
       `((".*" ,"~/.emacs.d/backups/")))
 (setq auto-save-list-file-prefix nil)
 (setq auto-save-default nil)

 ;; This function allows to quicky open this file
 ;; TODO: MOVE THIS TO ANOTHER PLACE
 (defun config-visit ()
   (interactive)
   (find-file "~/.emacs.d/config.org"))
 (global-set-key (kbd "C-c e") 'config-visit)

;; Updates the config fiel with C-c r
 (defun config-reload ()
   (interactive)
   (org-babel-load-file "~/.repos/dotfiles/.emacs.d/config.org"))
 (global-set-key (kbd "C-c r") 'config-reload)

 ;; The text nevers goes outside from the buffer
 (global-visual-line-mode 1)

 ;; Disables the ugly splash screen and the scratch message.
 (setq inhibit-splash-screen t)
 (setq initial-scratch-message nil)
 ;(setq initial-major-mode (quote org-mode))

 ;; With this, emacs will not ask if I want to edit the symlink every time
 (setq vc-follow-symlinks nil)

 ;; This is necessary on +27 to write accents. They say it's a feature... but for who?
 (require 'iso-transl)

(set-face-attribute 'default nil :font "Source Code Pro" :height 95 :width 'regular)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 95 :width 'regular)
(set-face-attribute 'variable-pitch nil :font "Ubuntu" :height 105 :weight 'regular)

(use-package evil
  :ensure t
  :init
  ;; This variable has issues with some commands, example, ~vi~ to append text at the beggining of the lines.
  (setq evil-want-keybinding nil)
  :custom
  ;; This variable needs to be setted by ~customize-group RET evil~. That's why use :custom instead of (setq).
  ;; this is needed to the undo feature
  (evil-undo-system 'undo-tree)
  :config
  (setq-default evil-cross-lines t)
  (evil-mode 1))

(use-package undo-tree
  :ensure t
  :config
(global-undo-tree-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
	 (evil-org-mode . (lambda ()
			    (evil-org-set-key-theme))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; This are keybindings for org-agenda
  (evil-define-key 'motion org-agenda-mode-map
    (kbd "C-p") 'org-agenda-earlier
    (kbd "C-n") 'org-agenda-later))

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "<f5>")  'ispell-word)
(define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-current-buffer)
(define-key evil-normal-state-map (kbd "SPC b") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC s") 'swiper)
(define-key evil-normal-state-map (kbd "SPC p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC P") 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC g") 'magit)
(define-key evil-normal-state-map (kbd "SPC v") 'visual-line-mode)
(define-key evil-normal-state-map (kbd "SPC c") 'org-capture)
(define-key evil-normal-state-map (kbd "SPC RET") (lambda () (interactive) (shell-command "alacritty > /dev/null 2>&1 & disown")))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil)
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
(doom-modeline-mode 1)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x C-g") 'magit))

(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode 1))

(use-package smartparens
  :ensure t
  :config
  (smartparens-mode t))

(use-package smartparens
  :ensure t
  :config
  (smartparens-mode t))

(use-package lua-mode
  :ensure t)

(use-package luarocks
  :ensure t)

(use-package company
  :ensure t
  :config
  (company-mode 1))

(use-package rg
  :ensure t)

(use-package writeroom-mode
    :ensure t
    :bind ("<f6>" . writeroom-mode))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
				(t . ivy--regex-fuzzy)))
  (setq ivy-extra-directories nil)
  (ivy-mode 1))

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
  :bind (
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/.repos" "/mnt/Data/Drive/CIMB/PLANEACIONES")))

(use-package fortune-cookie
  :ensure t
  :custom
  (fortune-dir "/usr/share/fortunes"))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-set-footer t)
  (dashboard-footer (with-temp-buffer
		      (let ((fortune-buffer-name (current-buffer)))
			(fortune-in-buffer t nil)
			(buffer-string))))
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info nil)
  (setq dashboard-items '(
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-page-separator "\n\n")
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package modus-vivendi-theme
  :ensure t)
(use-package modus-operandi-theme
  :ensure t
  :config
  (setq modus-operandi-theme-slanted-constructs t)
  (setq modus-operandi-theme-syntax 'alt-syntax))

(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-load-theme-no-confirm t)
  (setq heaven-and-hell-themes
	'((light . doom-one-light)
	  (dark . doom-one)))
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f7>" . heaven-and-hell-load-default-theme)
	 ("<f7>" . heaven-and-hell-toggle-theme)))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(setq markdown-command "/usr/bin/pandoc")

(use-package dired
  :ensure nil ; it's a built-in package
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump) ; To quickly open a dired buffer on the file path
	 ("C-<return>" . (lambda () (interactive) (shell-command "alacritty > /dev/null 2>&1 & disown")))) ; To quickly open a Terminal window
  :hook (
	 (dired-mode . dired-hide-details-mode)
	 (dired-mode . hl-line-mode))
  :config
  (setq dired-listing-switches "-AgGhovF --group-directories-first") ; man ls to details
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t) ;It uses the trash bin
  (setq dired-dwim-target 'dired-dwim-target-next-visible) ; If I have two buffers or frames open and I try to copy a file from one buffer, it understand that I want to copy it to the other buffer.

  ;; Some keybindings. It makes use of the ~evil-collection~ key-map and (maybe) replaces some default keybindings.
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-open-file
    "nd" 'dired-create-directory
    "nf" 'dired-create-empty-file
    "/" 'swiper
    "gj" 'counsel-bookmark))

(use-package dired-single
  :ensure t)

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions '(
				;; Images
				("png" . "rifle_sxiv.sh")
				("jpg" . "rifle_sxiv.sh")
				;; Multimedia
				("mp4" . "mpv")
				("mkv" . "mpv")
				("mp3" . "mpv")
				("aac" . "mpv")
				("ogg" . "mpv")
				("avi" . "mpv")
				("mov" . "mpv")
				("flac" . "mpv")
				;; libreoffice
				("odt" . "libreoffice")
				("odf" . "libreoffice")
				("ods" . "libreoffice")
				("odp" . "libreoffice")
				;; Otros
				("pdf" . "zathura")
				)))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "zh" 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :ensure t
  :config
  (setq dired-subtree-use-backgrounds nil)
  ;; this snippet adds icons from all-the-icons to the subtree
  (advice-add 'dired-subtree-toggle :after (lambda ()
					     (interactive)
					     (when all-the-icons-dired-mode
					       (revert-buffer)))))

(defun dired-frame ()
  (interactive)
  (dired)
  (delete-other-windows))

(use-package all-the-icons
  :ensure t)

;; Icons for dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda ()
			(interactive)
			(unless (file-remote-p default-directory)
			  (all-the-icons-dired-mode)))))

;; Icons for ivy
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package easy-hugo
  :ensure t
  :init 
  ;;; Main blog. you can have more if you want
  (setq easy-hugo-basedir "/mnt/Data/Blog/")
  (setq easy-hugo-postdir "content/posts/")
  :config
  (add-to-list 'evil-emacs-state-modes 'easy-hugo-mode)
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-org-header t))

(use-package ledger-mode
  :ensure t
  :config
  (evil-define-key 'normal ledger-mode-map (kbd "SPC r") 'ledger-report)
  (evil-define-key 'normal ledger-mode-map (kbd "SPC i") 'ledger-add-transaction))

(use-package evil-ledger
  :ensure t
  :after ledger-mode
  :hook
  (ledger-mode . evil-ledger-mode))

(use-package org
  :ensure nil
  :hook
  (org-mode . variable-pitch-mode)
  :config
  ;; Removes the ellipsis at the end and replaces it with a string
  (setq org-ellipsis " ●")

  ;; If you have many subtask, when you mark it as DONE, the main task remain unchaged. With this function, if all the subtask are marked as DONE, the main task is marked as well.
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "PROJ"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  ;; This keybinding uses org-store-link to store a postition on a document, so you can link it on other document.
  (global-set-key (kbd "C-c l") 'org-store-link)

  ;; Change TODO states with SPC t. It uses evil-collection key-map.
  (evil-define-key 'normal org-mode-map
    (kbd "SPC t") 'org-todo)

  ;; You can add blocks pressing C-, and then the corresponding key.
(require 'org-tempo)
(setq org-structure-template-alist
   '(("el" . "src emacs-lisp")
     ("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("x" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))))

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(fixed-pitch))
(set-face-attribute 'org-table nil :inherit '(fixed-pitch))
;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;; Titles and Sections
(setq org-hidden-keywords '(title))
;; set basic title font
(set-face-attribute 'org-level-8 nil :weight 'bold :inherit 'default)
;; Low levels are unimportant => no scaling
(set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
(set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
(set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
(set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
(set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.1) ;\large
(set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.3) ;\Large
(set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.5) ;\LARGE
;; Only use the first 4 styles and do not cycle.
(setq org-cycle-level-faces nil)
(setq org-n-level-faces 4)
;; Document Title, (\huge)
(set-face-attribute 'org-document-title nil
		    :height 2.074
		    :foreground 'unspecified
		    :inherit 'org-level-8)

;; Pandoc support
(use-package ox-pandoc
  :ensure t)

;; LaTeX beamer support
(org-beamer-mode)

(use-package org-superstar
   :ensure t
   :config
   (setq superstar-special-todo-items t))

 (defun my/org-enable-prettify ()
   (setq prettify-symbols-alist
	 '(("TODO" . ?❗)
	   ("DONE" . ?✔)
	   ("PROJ" . ?✎)
	   ("WAIT" . ?⌛)
	   ("NEXT" . ?➠)
	   ("EVENTO" . ?)
	   ("DROP" . ?✖)
	   ("EMISION" . ?✒)
	   ("FINALIZADO" . ?✔)
	   ("LIKE" . ?❤)
	   ("CANCELADO" . ?✘)))
   (prettify-symbols-mode 1))
 (add-hook 'org-mode-hook 'my/org-enable-prettify)

;; This hook enables org-superstar 
 (add-hook 'org-mode-hook
	   (lambda ()
	     (org-superstar-mode 1)))

(use-package org-tree-slide
  :ensure t
  :config
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  )

;; This packages hides the modeline because I don't know how to hide it without it.
(use-package hide-mode-line
  :ensure t)

;; This keys are to go to the next or previous slide. It uses ~evil-collection key-map
(evil-define-key 'normal 'org-tree-slide-mode-map
  "{"  'org-tree-slide-move-previous-tree
  "}"  'org-tree-slide-move-next-tree)

(eval-after-load "org-tree-slide"
  '(progn
     (add-hook 'org-tree-slide-play-hook
	       (lambda ()
		 (org-display-inline-images 1)
		 (hide-mode-line-mode 1)
		 (display-line-numbers-mode -1)
		 (variable-pitch-mode 1)))
     (add-hook 'org-tree-slide-stop-hook
	       (lambda ()
		 (org-display-inline-images -1)
		 (hide-mode-line-mode -1)
		 (display-line-numbers-mode 1)
		 (variable-pitch-mode -1)))))

(setq org-directory "/mnt/Data/ORG") ; The directory of your files
(setq org-agenda-files '("/mnt/Data/ORG/TODO.org"))
(global-set-key (kbd "C-c a") 'org-agenda) ; Keybinding to open the agenda buffer

;; by default the agenda takes the current buffer. With this it'll create its own buffer
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-span 3) ; Only shows next 3 days
(setq org-agenda-start-on-weekday nil) ;;Agenda start on monday
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Since I speak spanish as my mother language, I want the days and months in spanish. Without this it'll remain on english.
(setq calendar-day-name-array ["domingo" "lunes" "martes" "miércoles" "jueves" "viernes" "sábado"])
(setq calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo" "junio" "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre"])

;; Activate hl-line-mode on agenda buffers
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; Removes the ~======~ between blocks. It's ugly IMO
(setq org-agenda-block-separator (string-to-char " "))

;;Remove ths strings ~SCHEDULED:~ and ~DEADLINE:~ 
(setq org-agenda-scheduled-leaders '(" " " "))
(setq org-agenda-deadline-leaders '(" " "En %d días: " "Hace %d días: "))

;; Custom fonts! I'm using Ubuntu fonts here... I'm not sure why.
(custom-theme-set-faces 'user
			  '(org-agenda-date-today ((t (:weight ultra-bold :height 130 :family "Ubuntu")))) ; Today
			  '(org-agenda-structure ((t (:underline t :weight bold :height 200 :width normal :family "Ubuntu")))) ; Titles
			  '(org-agenda-calendar-event ((t (:family "Ubuntu" :inherit (default))))));Rest of the text

(setq org-agenda-custom-commands
	'(("o" "My Agenda"
	   ((todo "TODO" (
			  (org-agenda-overriding-header " Tareas:\n")
			  (tags-todo "TODO")
			  (org-agenda-remove-tags t)
			  (org-agenda-prefix-format "%T %?-s")
			  (org-agenda-todo-keyword-format "")))
	    (agenda "" (
			(org-agenda-overriding-header "  Eventos:\n")
			(org-agenda-skip-scheduled-if-done t)
			(org-agenda-skip-timestamp-if-done t)
			(org-agenda-skip-deadline-if-done t)
			(org-agenda-skip-deadline-prewarning-if-scheduled t)
			(org-agenda-start-day "+0d")
			(org-agenda-span 3)
			(org-agenda-prefix-format "  %?-t %T %?-5s")
			(org-agenda-repeating-timestamp-show-all nil)
			(org-agenda-remove-tags t)
			;;(concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
)
		    (org-agenda-todo-keyword-format " -> ")
		    (org-agenda-time)
		    (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ ahora")
		    (org-agenda-deadline-leaders '("" ""))
		    (org-agenda-time-grid (quote ((today require-timed) (800 1000 1200 1400 1600 1800 2000 2200) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈"))))
	    ))))

(defun agenda-frame ()
  (interactive)
  (org-agenda nil "o")
  (delete-other-windows))

(use-package calfw
  :ensure t)
(use-package calfw-org
  :ensure t)

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(
	("t" "Entradas del trabajo")
	("tt" "TODO" entry
	 (file "~/mnt/DATA/ORG/Trabajo.org")
	 "* TODO %?\n%u" :prepend t)
	("ta" "Agenda"  entry
	 (file "~/mnt/DATA/ORG/Trabajo.org")
	 "* %?\n SCHEDULED: %t")
	("p" "Entradas personales")
	("pt" "TODO" entry
	 (file "~/mnt/DATA/ORG/Trabajo.org")
	 "* TODO %?\n%u" :prepend t)
	("pa" "Agenda"  entry
	 (file "~/mnt/DATA/ORG/Trabajo.org")
	 "* %?\n SCHEDULED: %t")))

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

(add-to-list 'org-latex-classes
      '("koma-article"
	"\\documentclass{scrartcl}"
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	("\\paragraph{%s}" . "\\paragraph*{%s}")
	("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

      '("doc-recepcional"
	"\\documentclass{report}"
	("\\chapter{%s}" . "\\chapter*{%s}")
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	("\\paragraph{%s}" . "\\paragraph*{%s}")
	("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))

(use-package eshell-git-prompt
    :ensure t)
    (use-package fish-completion
    :ensure t
  :hook (eshell-mode . fish-completion-mode))
  
  (use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))
  
(use-package esh-autosuggest
:ensure t
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

  (use-package eshell-toggle
  :ensure t
  :bind ("<f4>" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))
