(defalias 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 100)

(setq make-backup-files nil) ;desactiva todos los backups
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backups/")))
(setq auto-save-list-file-prefix nil)
(setq auto-save-default nil)

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

(global-visual-line-mode 1)

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode (quote org-mode))
(setq vc-follow-symlinks nil)

(setq heaven-and-hell-themes
      '((light . doom-nord-light)
        (dark . doom-dracula)))

(set-face-attribute 'default nil :font "Source Code Pro-9.7")
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro-9.7")
(set-face-attribute 'variable-pitch nil :font "Nimbus Sans-12")

(set-face-attribute 'default nil :background "#272A34")

(dolist (face '(default fixed-pitch))
  (set-face-attribute `,face nil :font "Source Code Pro-9.7"))


(define-minor-mode my/variable-pitch-mode
  "Toggle `variable-pitch-mode', except for `prog-mode'."
  :init-value nil
  :global nil
  (if my/variable-pitch-mode
      (unless (derived-mode-p 'prog-mode)
	(variable-pitch-mode 1))
    (variable-pitch-mode -1)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "M-m") 'menu-bar-mode) ; M-m hace aparecer el menú

(global-hl-line-mode t)

(column-number-mode 1)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(show-paren-mode t)

(blink-cursor-mode 0)

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq-default evil-cross-lines t))

(use-package evil-collection
  :after evil
  :ensure t)
  (evil-collection-init)

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

(evil-define-key 'motion org-agenda-mode-map
  (kbd "C-p") 'org-agenda-earlier
  (kbd "C-n") 'org-agenda-later)

(use-package evil-magit
  :ensure t)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

(global-set-key (kbd "C-x k") 'kill-current-buffer)

(global-set-key (kbd "C-c v") 'visual-line-mode) ; visual line

(global-set-key (kbd "<f5>")  'ispell-word) ;activar ispell

(define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-current-buffer)
(define-key evil-normal-state-map (kbd "SPC b") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC s") 'swiper)
(define-key evil-normal-state-map (kbd "SPC p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC P") 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "SPC g") 'magit)
(define-key evil-normal-state-map (kbd "SPC v") 'visual-line-mode)
(define-key evil-normal-state-map (kbd "SPC c") 'org-capture)
(define-key evil-normal-state-map (kbd "SPC RET") (lambda () (interactive) (shell-command "st > /dev/null 2>&1 & disown")))

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
(setq doom-modeline-irc-stylize 'identity))
(doom-modeline-mode 1)

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

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

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
   :config
   (rainbow-mode 1))

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f7>" . heaven-and-hell-load-default-theme)
         ("<f7>" . heaven-and-hell-toggle-theme)))

(use-package writeroom-mode
    :ensure t
    :bind ("<f6>" . writeroom-mode))

;  (add-hook 'writeroom-enable-mode-hook
;  	    (variable-pitch-mode 1))
;  
;  (add-hook 'writeroom-mode-disable-hook
;  	   (variable-pitch-mode -1))
  ;(add-hook 'writeroom-local-effects 'my/variable-pitch-mode)

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
  (setq terminal-here-terminal-command "alacritty"))

(use-package ox-pandoc
  :ensure t)

(use-package org-tree-slide
  :ensure t
  :config
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  )

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
		 (my/variable-pitch-mode 1)))
     (add-hook 'org-tree-slide-stop-hook
	       (lambda ()
		 (org-display-inline-images -1)
		 (hide-mode-line-mode -1)
		 (display-line-numbers-mode 1)
		 (my/variable-pitch-mode -1)))))

(use-package org-superstar
  :ensure t)

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
(set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.2) ;\large
(set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.44) ;\Large
(set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.728) ;\LARGE
;; Only use the first 4 styles and do not cycle.
(setq org-cycle-level-faces nil)
(setq org-n-level-faces 4)
;; Document Title, (\huge)
(set-face-attribute 'org-document-title nil
                    :height 2.074
                    :foreground 'unspecified
                    :inherit 'org-level-8)

(add-hook 'org-mode-hook
          (lambda ()
            (org-superstar-mode 1)))

(setq org-directory "/mnt/ORG/")

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-window-setup
      'other-window)

(setq org-agenda-span 3)

(setq org-agenda-start-on-weekday nil)

(setq calendar-day-name-array ["domingo" "lunes" "martes" "miércoles" "jueves" "viernes" "sábado"])
(setq calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo" "junio" "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre"])

(setq org-agenda-block-separator (string-to-char " "))

(setq org-agenda-scheduled-leaders 
      '("" " "))
(setq org-agenda-deadline-leaders 
      '("Fecha límite:  " "En %d días: " "Hace %d días: "))

(custom-theme-set-faces 'user
			'(org-agenda-date-today ((t (:foreground "#d7befb" :weight ultra-bold :height 130 :family "Ubuntu")))) ;El día actual
			'(org-agenda-structure ((t (:foreground "#ffffff" :underline t :weight bold :height 200 :width normal :family "Ubuntu")))) ;Los títulos
			'(org-agenda-calendar-event ((t (:family "Ubuntu" :inherit (default))))) ;El texto
			)

(setq org-agenda-custom-commands
	'(("o" "My Agenda"
	   ((todo "TODO" (
			(org-agenda-overriding-header " Tareas por hacer:\n")
			(tags-todo "TODO")
			(org-agenda-remove-tags t)
			(org-agenda-prefix-format "%T %?-s")
			(org-agenda-todo-keyword-format "")))
	    (agenda "" (
			(org-agenda-overriding-header " Eventos para hoy:\n")
			(org-agenda-skip-scheduled-if-done t)
			(org-agenda-skip-timestamp-if-done t)
			(org-agenda-skip-deadline-if-done t)
			(org-agenda-skip-deadline-prewarning-if-scheduled t)
			(org-agenda-start-day "+0d")
			(org-agenda-span 3)
			(org-agenda-prefix-format "  %?-t %T %?-5s")
			(org-agenda-repeating-timestamp-show-all nil)
			(org-agenda-remove-tags t)
			 ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
			(org-agenda-todo-keyword-format " -> ")
			(org-agenda-time)
			(org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
			;; (org-agenda-scheduled-leaders '("" ""))
			;; (org-agenda-deadline-leaders '("" ""))
			(org-agenda-time-grid (quote ((today require-timed) (800 1000 1200 1400 1600 1800 2000 2200) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈"))))
)))))

;; Agenda flotante
(defun agenda-frame ()
  (interactive)
  (org-agenda nil "o")
  (delete-other-windows))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "PROJ"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(evil-define-key 'normal org-mode-map
  (kbd "SPC t") 'org-todo)

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
;	("i" "Inbox" entry
;	 (file "~/Drive/GTD/inbox.org")
;	 "* %?\n%u" :prepend t)

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
	 "* %?\n SCHEDULED: %t")
;;	("n" "Notas" entry
;;	 (file+headline "~/Drive/GTD/referencias.org" "Notas")
;;	 "* %?" :prepend t)
;;
;;	("d" "Diario" entry
;;	 (file+olp+datetree "~/Drive/SEC-ABREOJOS/DIARIO.org")
;;	 "* %?" :prepend t)
))

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
     ("v" . "verse")))

(use-package org-super-agenda
:ensure t)
(let ((org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
	 (:name "Para hoy"  ; Optionally specify section name
		:scheduled today)  ; Items that appear on the time grid
	 (:name "Tareas pendientes"
		;; Single arguments given alone
		:todo "TODO")
	 (:name "Eventos para los próximos 3 días"
		:scheduled t
		:deadline t)
	 ;; Set order of multiple groups at once
	 (:order-multi (2 (:name "Shopping in town"
				 ;; Boolean AND group matches items that match all subgroups
				 :and (:tag "shopping" :tag "@town"))
			  (:name "Food-related"
				 ;; Multiple args given in list with implicit OR
				 :tag ("food" "dinner"))
			  (:name "Personal"
				 :habit t
				 :tag "personal")
			  (:name "Space-related (non-moon-or-planet-related)"
				 ;; Regexps match case-insensitively on the entire entry
				 :and (:regexp ("space" "NASA")
					       ;; Boolean NOT also has implicit OR between selectors
					       :not (:regexp "moon" :tag "planet")))))
	 ;; Groups supply their own section names when none are given
	 (:todo "WAITING" :order 8)  ; Set order of this section
	 (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
		;; Show this group at the end of the agenda (since it has the
		;; highest number). If you specified this group last, items
		;; with these todo keywords that e.g. have priority A would be
		;; displayed in that group instead, because items are grouped
		;; out in the order the groups are listed.
		:order 9)
	 (:priority<= "B"
		      ;; Show this section after "Today" and "Important", because
		      ;; their order is unspecified, defaulting to 0. Sections
		      ;; are displayed lowest-number-first.
		      :order 1)
	 ;; After the last group, the agenda will display items that didn't
	 ;; match any of these groups, with the default order position of 99
	 )))
  (org-agenda nil "a"))

(use-package lua-mode
  :ensure t)

(use-package luarocks
  :ensure t)

(use-package company
  :ensure t
  :config
  (company-mode 1))

;;(setq use-default-font-for-symbols nil)
(set-fontset-font t '(#xF01C9 . #xF0A88) "Material Design Icons")
;; Add Apple Color Emoji to the default symbol fontset used by Emacs

(use-package ranger
  :ensure t)
(setq ranger-cleanup-on-disable t)
(setq ranger-cleanup-eagerly t)
(setq ranger-parent-depth 0)
(setq ranger-width-preview 0.30)
(setq ranger-show-literal nil)
(setq ranger-modify-header nil)
(setq ranger-excluded-extensions '("mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav" "mov" "flv" "ogm" "ogg" "mkv" "doc" "xls" "ppt" "odt" "ods" "odg" "odp" "docx" "xlsx" "odtx" "pdf" "ps" "ps.gz" "dvi"))

(use-package openwith
  :ensure t)
  (when (require 'openwith nil 'noerror)
    (setq openwith-associations
	  (list
	   (list (openwith-make-extension-regexp
		  '("mpg" "mpeg" "mp3" "mp4"
		    "avi" "wmv" "wav" "mov" "flv"
		    "ogm" "ogg" "mkv"))
		 "mpv"
		 '(file))
	   (list (openwith-make-extension-regexp
		  '("doc" "xls" "ppt" "odt" "ods" "odg" "odp" "docx" "xlsx" "odtx"))
		 "libreoffice"
		 '(file))
	   (list (openwith-make-extension-regexp
		  '("pdf" "ps" "ps.gz" "dvi"))
		 "zathura"
		 '(file))
	   ))
    (openwith-mode 1))

(use-package all-the-icons-dired
  :ensure t)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq dired-hide-details-mode t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-listing-switches "-lFaGh1 --group-directories-first")

;;  (use-package dired-open
;;    :ensure t)
;;
;;  (setq dired-open-extensions
;;	'(("pdf" . "zathura")
;;	  ("mkv" . "mpv")
;;	  ("mp4" . "mpv")
;;	  ("avi" . "mpv")
;;	  ("html" . "firefox")
;;	  ("mp3" . "mpv")
;;	  ("ogg" . "mpv")
;;	  ("flac" . "mpv")
;;	  ("aac" . "mpv")
;;	  ("jpg" . "rifle_sxiv.sh")
;;	  ("png" . "rifle_sxiv.sh")
;;	  ("gif" . "sxiv -a")
;;	  ("doc" . "libreoffice")
;;	  ("docx" . "libreoffice")
;;	  ("odt" . "libreoffice")
;;	  ("ppt" . "libreoffice")
;;	  ("pptx" . "libreoffice")
;;	  ("odp" . "libreoffice")
;;	  ("xls" . "libreoffice")
;;	  ("xlsx" . "libreoffice")
;;	  ("ods" . "libreoffice")
;;  ))

(use-package dired-hide-dotfiles
  :ensure t)
  
(defun my-dired-mode-hook ()
  "My `dired' mode hook."
  ;; To hide dot-files by default
  (dired-hide-dotfiles-mode)

  ;; To toggle hiding
  (define-key dired-mode-map "." #'dired-hide-dotfiles-mode))

(add-hook 'dired-mode-hook #'my-dired-mode-hook)
