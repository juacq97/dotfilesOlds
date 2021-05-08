(require 'package)
  ;; Allows to install packages from melpa
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; this is a test

  (package-initialize)

  (add-to-list 'load-path "~/.emacs.d/modes/")
  (add-to-list 'load-path "~/.repos/nano-emacs")
  ;; If not here, install use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; Automatically download all packages. To prevent this, add ~:ensure nil~
  (setq use-package-always-ensure t)
;;  (setq use-package-verbose t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "M-m") 'menu-bar-mode) ; Opens the menu with M-m, very KDE-ish
(column-number-mode 1) ; The modeline shows the column number at the end

(setq  cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
       select-enable-clipboard t              ; Merge system's and Emacs' clipboard
        x-stretch-cursor t)                    ; Stretch cursor to the glyph width

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(set-face-attribute 'line-number nil :inherit nil)
(set-face-attribute 'line-number-current-line nil :inherit nil)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                elpher-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(show-paren-mode t) ; Highlight the matching parenthesis
     (blink-cursor-mode 0) ; Disable the blinking
     ;; Press y/n instead of the whole word
     (defalias 'yes-or-no-p 'y-or-n-p)
     ;; Scroll line by line. Cursor doesn't stays at the center of the screen. Can be laggy
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
       ;(org-babel-load-file "~/.repos/dotfiles/.emacs.d/config.org"))
       (load-file user-init-file))
     (global-set-key (kbd "C-c r") 'config-reload)

     (global-visual-line-mode 1) ; wrap lines to the size of the buffer

     ;; Disables the ugly splash screen 
     (setq inhibit-splash-screen t)
     (setq initial-scratch-message nil) ; Disable the scratch mesage
     (setq initial-major-mode (quote org-mode)) ; Change the mode of the scratch buffer

     ;; With this, emacs will not ask if I want to edit the symlink every time
     (setq vc-follow-symlinks nil)

     ;; This is necessary on 27+ to write accents (needed to write spanish). They say it's a feature... not for me!
     (require 'iso-transl)

;; When a split is done, follow it.
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

(set-face-attribute 'default nil :family "Fira Code" :height 102)
(set-face-attribute 'fixed-pitch nil :family "Fira Code")
(set-face-attribute 'variable-pitch nil :family "Open Sans")

(use-package emojify
  :config
  (global-emojify-mode))

(use-package undo-tree
  :ensure t
  :config
(global-undo-tree-mode 1))

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

(global-unset-key (kbd "C-z"))
  (use-package general
    :config
    (general-create-definer my/leader-keys
       :keymaps '(normal insert visual emacs)
       :prefix "SPC"
      :global-prefix "C-SPC")

    (my/leader-keys
     "SPC" '(counsel-find-file :which-key "Open a file")
     "k" '(kill-current-buffer :which-key "Kill buffer")
     "b" '(counsel-switch-buffer :which-key "Switch buffer")
     "s" '(swiper :which-key "Swiper search")
     "p" '(counsel-projectile-find-file :which-key "Projectile, find file")
     "P" '(counsel-projectile-switch-project :which-key "Projectile, switch project")
     "g" '(magit :which-key "Magit")
     "v" '(visual-line-mode :which-key "Activate visual-line-mode")
     "c" '(org-capture :which-key "Capture with org")
     "u" '(winner-undo :which-key "Undo layout")
     "r" '(winner-redo :which-key "Redo layout")
     "RET" '((lambda () (interactive) (shell-command "alacritty > /dev/null 2>&1 & disown")))))

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "<f5>")  'ispell-word)

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
				(t . ivy--regex-fuzzy)))
  (setq ivy-extra-directories nil)
  (ivy-mode 1))

;; (use-package ivy-rich 
;;   :ensure t
;;   :config
;;   (ivy-rich-mode 1))

(use-package ivy-prescient
  :ensure t
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package counsel
  :ensure t
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :bind (
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer))

  :config
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package which-key
  :defer 0
  :ensure t
  :init
  (which-key-mode))

(use-package magit
  :commands magit-status
  :ensure t
  :config
  (global-set-key (kbd "C-x C-g") 'magit))

(use-package rainbow-mode
  :defer t
  :ensure t
  :config
  (rainbow-mode 1))

;(use-package smartparens
;  :hook (prog-mode . smartparents-mode)
;  :ensure t
;  :config
;  (smartparens-mode t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package projectile
    :bind (("C-c p" . projectile-find-file) ("C-c P" . projectile-switch-projects))
  :ensure t
:config (setq projectile-project-search-path '("~/.repos" "/mnt/Data/Drive/CIMB/PLANEACIONES")))

(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode 1))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package rg
  :defer 0
  :ensure t)

(use-package writeroom-mode
    :ensure t
    :bind ("<f6>" . writeroom-mode))

(use-package switch-window
  :bind ("C-x o" . switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?z))

(use-package elfeed
  :config
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-show-unique-buffers t))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.repos/dotfiles/.emacs.d/feeds.org")))

(use-package elfeed-protocol
  :config
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate
  ;; setup extra protocol feeds

  (defadvice elfeed (after configure-elfeed-feeds activate)
    "Make elfeed-org autotags rules works with elfeed-protocol."
    (setq elfeed-protocol-tags elfeed-feeds)
    (setq elfeed-feeds '(
                         ;; format 6, for password in pass(1), using password-store.el
                         ("owncloud+https://admin@cloud.juancastro.xyz"
                          :password (password-store-get "nextcloud/admin")
                          :autotags elfeed-protocol-tags))))

    ;; use autotags

    ;; enable elfeed-protocol
    (elfeed-protocol-enable))

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
        "gj" 'counsel-bookmark)
)

(use-package dired-single
  :after dired
  :ensure t)

(use-package dired-open
  :after dired
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
                                ("xlsx" . "libreoffice")
                                ("odp" . "libreoffice")
                                ;; Otros
                                ("pdf" . "zathura")
                                )))

(use-package dired-hide-dotfiles
    :ensure t
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "zh" 'dired-hide-dotfiles-mode)
)

(use-package dired-subtree
  :after dired
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

(use-package doom-modeline
    :ensure t
    :config
    (add-hook 'window-selection-change-functions #'doom-modeline-set-selected-window)
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
    (setq doom-modeline-github nil)
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

(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-load-theme-no-confirm t)
  (setq heaven-and-hell-themes
	'((light . doom-one-light)
	  (dark . doom-horizon)))
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f7>" . heaven-and-hell-load-default-theme)
	 ("<f7>" . heaven-and-hell-toggle-theme)))

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

; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;;  (use-package fortune-cookie
;;    :ensure t
;;    :custom
;;    (fortune-dir "/usr/share/fortunes"))
;;
;;  (use-package dashboard
;;    :ensure t
;;    :config
;;    (dashboard-setup-startup-hook)
;;    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;;    (setq dashboard-banner-logo-title "Welcome to Emacs")
;;    (setq dashboard-startup-banner 'logo)
;;    (setq dashboard-show-shortcuts nil)
;;    (setq dashboard-set-init-info nil)
;;    (setq dashboard-footer-messages nil)
;;    (setq dashboard-banner-logo-title nil)
;;    (setq dashboard-items '(
;;                            (bookmarks . 5)
;;                            (projects . 5)
;;                            (agenda . 5)))
;;    (setq dashboard-center-content t)
;;    (setq dashboard-page-separator "\n\n")
;;    (setq dashboard-set-heading-icons t)
;;    (setq dashboard-set-file-icons t))

(use-package all-the-icons
    :ensure t)

  ;; Icons for dired
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . (lambda ()
                          (interactive)
                          (unless (file-remote-p default-directory)
                            (all-the-icons-dired-mode)))))
(use-package all-the-icons-ivy
:init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

  ;; Icons for ivy
  (use-package all-the-icons-ivy-rich
    :ensure t
    :after ivy-rich
    :config
    (all-the-icons-ivy-rich-mode 1))

(defun my/org-font-setup ()
  (require 'org-faces) 
  (set-face-attribute 'org-block nil :foreground nil :inherit '(fixed-pitch))
  (set-face-attribute 'org-code nil :inherit '(fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit '(fixed-pitch))
  ;; THIS defun CONTINUES BELOW

;;; Remove the word #+TITLE:
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
) ;; <=== org-font-setup ends here

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

  (use-package org
    :ensure nil
    :hook (
           (org-mode . my/org-mode-setup)
           (org-mode . my/org-font-setup))

    :config
    ;;(add-hook 'org-mode-hook 'my/org-font-setup)
    ;; Removes the ellipsis at the end and replaces it with a string
    (setq org-ellipsis " ⤾")


    ;; If you have many subtask, when you mark it as DONE, the main task remain unchaged. With this function, if all the subtask are marked as DONE, the main task is marked as well.
    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states)   ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "PROJ"))))
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
    ;; This keybinding uses org-store-link to store a postition on a document, so you can link it on other document.
    (global-set-key (kbd "C-c l") 'org-store-link)

    ;; Change TODO states with SPC t. It uses evil-collection key-map.
    ;;(evil-define-key 'normal org-mode-map
    ;;  (kbd "SPC t") 'org-todo)

;; Activate org-beamer
  (org-beamer-mode)

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
       ("v" . "verse")))

;; NOTE: THE USE PACKAGE MACRO CONTINUES

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
) ;; <=== The use-package org ends here

(defun my/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

;    (use-package visual-fill-column
;      :hook (
;      (org-agenda-mode . my/org-mode-visual-fill)
;      (org-mode . my/org-mode-visual-fill)
;      (elpher-mode . my/org-mode-visual-fill)))

;; Pandoc support
;(use-package ox-pandoc
;  :after org
;  :ensure t)

(use-package org-superstar
   :ensure t
   :config
   (setq superstar-special-todo-items t))

 (defun my/org-enable-prettify ()
   (setq prettify-symbols-alist
         '(("DROP" . ?✖)
           ("EMISION" . ?✒)
           ("FINALIZADO" . ?✔)
           ("LIKE" . ?❤)))
   (prettify-symbols-mode 1))
 (add-hook 'org-mode-hook 'my/org-enable-prettify)

;; This hook enables org-superstar 
 (add-hook 'org-mode-hook
           (lambda ()
             (org-superstar-mode 1)))

(use-package org-tree-slide
  :ensure t
  :defer t
  :config
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  )

;; This packages hides the modeline because I don't know how to hide it without it.
(use-package hide-mode-line
  :defer t
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
(setq org-agenda-files '(
                         "/mnt/data/Nextcloud/ORG/sync/TODO.org"
                         "~/testing-orgfiles.org"))
(global-set-key (kbd "C-c a") 'org-agenda) ; Keybinding to open the agenda buffer

;; by default the agenda takes the current buffer. With this it'll create its own buffer
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-span 7) ; Only shows next 3 days
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
(setq org-agenda-window-setup 'current-window)

;;Remove ths strings ~SCHEDULED:~ and ~DEADLINE:~ 
(setq org-agenda-scheduled-leaders '("" ""))
(setq org-agenda-deadline-leaders '("🕓" "En %d días:" "Hace %d días:"))

;; Custom fonts! I'm using Ubuntu fonts here... I'm not sure why.
(custom-theme-set-faces 'user
                        '(org-agenda-date-today ((t (:weight bold :height 130)))) ; Today
                        '(org-agenda-structure ((t (:underline nil :weight bold :height 150 :width normal)))) ; Titles
                        '(org-agenda-calendar-event ((t (:inherit (default)))))
                        '(org-agenda-calendar-sexp ((t (:inherit (default))))));Rest of the text

(setq org-agenda-custom-commands
      '(("o" "My Agenda"
         ((agenda "" (
                      (org-agenda-files '("/mnt/data/Nextcloud/ORG/sync/TODO.org"))
                      (org-agenda-overriding-header "📅 Calendario\n")
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-skip-deadline-prewarning-if-scheduled nil)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 7)
                      (org-agenda-prefix-format "  %?-t %T %?5s")
                      (org-agenda-repeating-timestamp-show-all t)
                      ;;(concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator)
                      (org-agenda-remove-tags t)
                 (org-agenda-todo-keyword-format " ")
                  (org-agenda-time)
                  (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈ ahora")
                  ;(org-agenda-deadline-leaders '("" ""))
                  (org-agenda-time-grid (quote ((today require-timed) (800 1000 1200 1400 1600 1800 2000 2200) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

          (todo "NEXT" (
                        (org-agenda-files '("/mnt/data/Nextcloud/ORG/sync/TODO.org"))
                        (org-agenda-overriding-header "⭐ Siguientes\n")
                        (org-agenda-remove-tags nil)
                        (org-agenda-hide-tags-regexp "^\@")
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-prefix-format "%T %?-s")
                        (org-agenda-todo-keyword-format "")))

          (todo "ESPERANDO" (
                        (org-agenda-files '("/mnt/data/Nextcloud/ORG/sync/TODO.org"))
                        (org-agenda-overriding-header "🕘 Esperando\n")
                        (org-agenda-remove-tags nil)
                        (org-agenda-hide-tags-regexp "^\@")
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-prefix-format "%T %?-s")
                        (org-agenda-todo-keyword-format "")))


          (todo "PROJ" (
                        (org-agenda-files '("/mnt/data/Nextcloud/ORG/sync/TODO.org"))
                        (org-agenda-overriding-header "✈ Proyectos\n")
                        (org-agenda-remove-tags nil)
                        (org-agenda-hide-tags-regexp "^\@")
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-prefix-format "%T %?-s")
                        (org-agenda-todo-keyword-format "")))

          (todo "" (
                        (org-agenda-files '("/mnt/data/Nextcloud/ORG/sync/TODO.org"))
                        (org-agenda-overriding-header "☑ Tareas\n")
                        (org-agenda-remove-tags nil)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-prefix-format "%?-s")
                        (org-agenda-todo-keyword-format "%-1s")))))))

(defun agenda-frame ()
  (interactive)
  (org-agenda nil "o")
  (delete-other-windows))

(setq org-refile-targets
      '(("../DONE.org" :maxlevel . 1)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))

;; Automatically tangle our Emacs.org config file when we save it
(defun my/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

(use-package org-appear
  :config
  ;; This is needed to org-appear
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-link-descriptive t)
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)

  :hook (org-mode . org-appear-mode))

(use-package ox-gemini
  :config
  (require 'ox-gemini))

(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ("http_website"
         :base-directory "/mnt/data/www/source/"
         :base-extension "org"
         :publishing-directory "/mnt/data/www/site/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :exclude "GEM_.*"
         :with-date t
         :html-head "<link rel=stylesheet type=text/css href=https://juancastro.xyz/assets/style.css />"
         :html-head-include-default-style nil
         :with-toc nil
         :html-postamble t
         :html-postamble-format (("en" "<footer id=footer class=footer> <p><a rel=license href=http://creativecommons.org/licenses/by-sa/4.0/>CC-BY-SA</a> Juan Castro | Made with Emacs 27 (Org-mode 9.4.4) <a rel=homepage href=https://juancastro.xyz>Home page </a></p> </footer>"))
        :section-numbers nil
         ;:html-postable nil
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )
        ("gemini_capsule"
         :base-directory "/mnt/data/www/source/"
         :base-extension "org"
         :publishing-directory "/mnt/data/www/capsule/"
         :recursive t
         :publishing-function org-gemini-publish-to-gemini
         :exclude "index"
         :with-date t
         :with-toc nil
         :section-numbers nil
         ;:html-postable nil
         :headline-levels 4             ; Just the default for this project.
         ;:auto-preamble t
         )))

(add-to-list 'load-path "~/.repos/org-caldav")
(setq org-icalendar-include-todo 'all
      org-caldav-sync-todo t
      org-icalendar-categories '(local-tags)
      org-caldav-url "https://cloud.juancastro.xyz/remote.php/dav/calendars/admin/"
      org-caldav-calendar-id "prueba"
      org-caldav-files '("~/ywy.org")
      org-caldav-inbox "~/testing-caldav.org")
(require 'org-caldav)

(use-package vterm)

(use-package fish-completion
     :after esh-mode
     :ensure t
   :hook (eshell-mode . fish-completion-mode))

   (use-package eshell-syntax-highlighting
   :ensure t
   :after esh-mode
   :config
   (eshell-syntax-highlighting-global-mode +1))

 (use-package esh-autosuggest
 :ensure t
   :hook (eshell-mode . esh-autosuggest-mode))

   (use-package eshell-toggle
   :ensure t
   :bind ("<f4>" . eshell-toggle)
   :custom
   (eshell-toggle-size-fraction 3)
   (eshell-toggle-run-command nil))

(use-package eshell
  :ensure nil
  :config
  (setq eshell-banner-message (concat (shell-command-to-string "fortune-es") "\n\n")))

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "/mnt/data/.mail/juancastro.xyz")

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-refile-folder "/All Mail")
  (setq mu4e-trash-folder  "/Trash")

  (setq mu4e-maildir-shortcuts
        '((:maildir "/Inbox"     :key ?i)
          (:maildir "/Sent"      :key ?s)
          (:maildir "/Trash"     :key ?t)
          (:maildir "/Drafts"    :key ?d)
          (:maildir "/All Mail"  :key ?a)))

  (setq smtpmail-smtp-server "mail.juancastro.xyz"
        smtpmail-smtp-service 587
        smtpmail-stream-type  'starttls)

  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-compose-format-flowed t)
  (setq user-mail-address "juan@juancastro.xyz")
  (setq user-full-name "Juan Adrián Castro Quintana")
  (setq mu4e-compose-signature "Juan Adrián Castro Quintana")

  (mu4e t))

(use-package lua-mode
  :mode "\\.lua\\'"
  :ensure t)

(use-package luarocks
  :after (lua)
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(setq markdown-command "/usr/bin/pandoc")

(use-package easy-hugo
  :ensure t
  :commands easy-hugo
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
  :mode "\\.lgr\\'"
  :config
  (evil-define-key 'normal ledger-mode-map (kbd "SPC r") 'ledger-report)
  (evil-define-key 'normal ledger-mode-map (kbd "SPC i") 'ledger-add-transaction))

(use-package evil-ledger
  :ensure t
  :after ledger-mode
  :hook
  (ledger-mode . evil-ledger-mode))

(use-package kdeconnect
  :ensure t
  :config
  (setq kdeconnect-devices "7843123afa92d0a8")
  (setq kdeconnect-active-device "7843123afa92d0a8"))

(use-package pinentry
  :init
  (pinentry-start))

(use-package gemini-mode)

;  (use-package pdf-tools
;    :ensure t)

;;(use-package org-noter
;;  :config
;;  ;; Your org-noter config ........
;;  (require 'org-noter-pdftools))
;;
;;(use-package org-pdftools
;;  :hook (org-mode . org-pdftools-setup-link))
;;
;;(use-package org-noter-pdftools
;;  :after org-noter
;;  :config
;;  ;; Add a function to ensure precise note is inserted
;;  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;;    (interactive "P")
;;    (org-noter--with-valid-session
;;     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;;                                                   (not org-noter-insert-note-no-questions)
;;                                                 org-noter-insert-note-no-questions))
;;           (org-pdftools-use-isearch-link t)
;;           (org-pdftools-use-freestyle-annot t))
;;       (org-noter-insert-note (org-noter--get-precise-info)))))
;;
;;  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;;  (defun org-noter-set-start-location (&optional arg)
;;    "When opening a session with this document, go to the current location.
;;With a prefix ARG, remove start location."
;;    (interactive "P")
;;    (org-noter--with-valid-session
;;     (let ((inhibit-read-only t)
;;           (ast (org-noter--parse-root))
;;           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;;       (with-current-buffer (org-noter--session-notes-buffer session)
;;         (org-with-wide-buffer
;;          (goto-char (org-element-property :begin ast))
;;          (if arg
;;              (org-entry-delete nil org-noter-property-note-location)
;;            (org-entry-put nil org-noter-property-note-location
;;                           (org-noter--pretty-print-location location))))))))
;;  (with-eval-after-load 'pdf-annot
;;    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-horizon))
 '(custom-safe-themes
   '("0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" default))
 '(org-agenda-files '("/mnt/data/Nextcloud/ORG/sync/TODO.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-calendar-event ((t (:inherit (default)))))
 '(org-agenda-calendar-sexp ((t (:inherit (default)))))
 '(org-agenda-date-today ((t (:weight bold :height 130))))
 '(org-agenda-structure ((t (:underline nil :weight bold :height 150 :width normal)))))
