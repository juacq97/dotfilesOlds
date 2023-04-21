;; Profile emacs startup
    ;; The default is 800 kilobytes.  Measured in bytes. (setq gc-cons-threshold (* 50 1000 1000))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (message "*** Emacs loaded in %s with %d garbage collections."
                         (format "%.2f seconds"
                                 (float-time
                                  (time-subtract after-init-time before-init-time)))
                         gcs-done)))
(setq comp-deferred-compilation t)
  (setq comp-async-report-warnings-errors nil)

(require 'package)
;; Allows to install packages from melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

  ;;(setq package-archives
  ;;      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
  ;;        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
  ;;        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))
      (package-initialize)
      (add-to-list 'load-path "~/.emacs.d/external/")

      ;; If not here, install use-package
      (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

      ;; Automatically download all packages. To prevent this, add ~:ensure nil~
      (setq use-package-always-ensure t)
    ;;  (setq use-package-verbose t)

(require 'frame)
;;  (defun set-cursor-hook (frame)
;;    (modify-frame-parameters
;;     frame (list (cons 'cursor-color "#ffffff"))))

;; (add-hook 'after-make-frame-functions 'set-cursor-hook)
    (tool-bar-mode -1)
   

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

(use-package paren
  :ensure nil
  :config
  (set-face-attribute 'show-paren-match-expression nil :inherit nil :weight 'semibold :background "snow4")
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  (show-paren-mode t)) ; Highlight the matching parenthesis

(blink-cursor-mode 1) ; Disable the blinking
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

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

;; Updates the config fiel with C-c r
(defun config-reload ()
  (interactive)
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
(global-set-key (kbd "C-c i") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-c o") 'split-and-follow-vertically)

(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 100 :weight 'semibold)
(set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font" :height 100 :weight 'semibold)
(set-face-attribute 'variable-pitch nil :family "Fira Sans" :height 102 :weight 'medium)

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
  ;;; This variable has issues with some commands, example, ~vi~ to append text at the beggining of the lines.
  (setq evil-want-keybinding nil)
  :custom
  ;;; This variable needs to be setted by ~customize-group RET evil~. That's why use :custom instead of (setq).
  ;;; this is needed to the undo feature
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
     "SPC" '(find-file :which-key "Open a file")
     "k" '(kill-current-buffer :which-key "Kill buffer")
     "b" '(consult-buffer :which-key "Switch buffer")
     "s" '(consult-line :which-key "Search")
     "p" '(projectile-find-file :which-key "Projectile, find file")
     "P" '(projectile-switch-project :which-key "Projectile, switch project")
     "g" '(magit :which-key "Magit")
     "v" '(visual-line-mode :which-key "Activate visual-line-mode")
     "c" '(org-capture :which-key "Capture with org")
     "u" '(winner-undo :which-key "Undo layout")
     "r" '(winner-redo :which-key "Redo layout")
     "RET" '((lambda () (interactive) (shell-command "alacritty > /dev/null 2>&1 & disown")))))

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "<f5>")  'ispell-word)

(use-package selectrum
  :ensure t
  :init
  (selectrum-mode +1))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))
;;
(use-package marginalia
  :after selectrum
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))
;;
(use-package consult
  :ensure t
  :bind (
         ("C-s" . consult-line)
         ("C-x b" . consult-buffer)))

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
  :init 
  (rainbow-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(use-package helpful
  :ensure t
  :custom
  (describe-function-function #'helpful-callable)
  (describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package writeroom-mode
  :ensure t
  :bind ("<f6>" . writeroom-mode)
  :config
  (setq writeroom-fullscreen-effect 'fullboth))

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
  (dirvish-dired)
  (delete-other-windows))

;    (use-package doom-modeline
;      :ensure t
;      :config
;      (add-hook 'window-selection-change-functions #'doom-modeline-set-selected-window)
;      (setq doom-modeline-height 25)
;      (setq doom-modeline-bar-width 4)
;      (setq doom-modeline-buffer-file-name-style 'relative-from-project)
;      (setq doom-modeline-icon t)
;      (setq doom-modeline-major-mode-icon t)
;      (setq doom-modeline-modal-icon t)
;      (setq doom-modeline-major-mode-color-icon t)
;      (setq doom-modeline-minor-modes nil)
;      (setq doom-modeline-buffer-encoding nil)
;      (setq doom-modeline-enable-word-count t)
;      (setq doom-modeline-checker-simple-format t)
;      (setq doom-modeline-persp-name t)
;      (setq doom-modeline-lsp nil)
;      (setq doom-modeline-github nil)
;      (setq doom-modeline-env-version t)
;      (setq doom-modeline-env-enable-python t)
;      (setq doom-modeline-env-enable-ruby t)
;      (setq doom-modeline-env-enable-perl t)
;      (setq doom-modeline-env-enable-go t)
;      (setq doom-modeline-env-enable-elixir t)
;      (setq doom-modeline-env-enable-rust t)
;      (setq doom-modeline-env-python-executable "python")
;      (setq doom-modeline-env-ruby-executable "ruby")
;      (setq doom-modeline-env-perl-executable "perl")
;      (setq doom-modeline-env-go-executable "go")
;      (setq doom-modeline-env-elixir-executable "iex")
;      (setq doom-modeline-env-rust-executable "rustc")
;      (setq doom-modeline-mu4e t)
;      (setq doom-modeline-irc t)
;      (setq doom-modeline-irc-stylize 'identity))
;  (doom-modeline-mode 1)

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)

  (setq-default mode-line-format
                '(" "
                  mode-line-front-space
                  mode-line-client
                  mode-line-frame-identification
                  mode-line-buffer-identification " " mode-line-position
                  (vc-mode vc-mode)
                  (multiple-cursors-mode mc/mode-line)
                  " " mode-line-modes
                  mode-line-end-spaces))

  (use-package minions
    :ensure t
    :config
    (minions-mode +1))

  ;; Disabled: Trying out clock outside Emacs
  ;; (use-package time
  ;;   :validate-custom
  ;;   (display-time-24hr-format t)
  ;;   (display-time-day-and-date t)
  ;;   (display-time-world-list '(("Europe/Paris" "Paris")
  ;;                              ("Europe/London" "London")
  ;;                              ("America/Los_Angeles" "Los Angeles")))
  ;;   (display-time-string-forms
  ;;    '((format "%s %s %s, %s:%s"
  ;;              dayname
  ;;              monthname day
  ;;              24-hours minutes)))
  ;;   :config
  ;;   (display-time))

  (setq global-mode-string (remove 'display-time-string global-mode-string))

  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (setq doom-gruvbox-dark-variant "hard")

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(defun my-demo-modus-vivendi ()
  (modus-themes-with-colors
    (custom-set-faces
     `(default ((,class :background ,"#242424")))
     `(fringe ((,class :background ,"#242424")))
     `(cursor ((,class :background ,"#6C605A")))
     `(hl-line ((,class :background ,"#303030")))
     `(show-paren-match-expression ((,class :background ,"#191a1b")))
     `(selectrum-current-candidate ((,class :background ,"#191a1b")))
     `(mode-line ((,class :background ,"#303030")))
     `(org-block ((,class :background ,"#303030")))
     `(org-block-end-line ((,class :background ,"#303030")))
     `(org-block-begin-line ((,class :background ,"#303030"))))))

(defun my-demo-modus-operandi ()
  (modus-themes-with-colors
    (custom-set-faces
     `(default ((,class :background ,"#fafafa")))
     `(fringe ((,class :background ,"#fafafa")))
     `(cursor ((,class :background ,"#6C605A")))
     `(hl-line ((,class :background ,"#d4d4d4")))
     `(show-paren-match-expression ((,class :background ,"#e4e4e4")))
     `(selectrum-current-candidate ((,class :background ,"#D4D4D4")))
     `(mode-line ((,class :background ,"#ebebeb")))
     `(org-block ((,class :background ,"#ebebeb" :foreground ,"#3A3A3A")))
     `(org-block-end-line ((,class :background ,"#ebebeb")))
     `(org-block-begin-line ((,class :background ,"#ebebeb"))))))

(defun load-vivendi ()
  (interactive)
  (load-theme 'modus-vivendi t)
  (my-demo-modus-vivendi))

(defun load-operandi ()
  (interactive)
  (load-theme 'modus-operandi t)
  (my-demo-modus-operandi))

(defun my-demo-modus-themes-toggle ()
(interactive)
(if (eq (car custom-enabled-themes) 'modus-operandi)
    (load-vivendi)
  (load-operandi)))

(use-package modus-themes
  :ensure t
  :config
(setq modus-themes-mode-line 'moody)
  (setq modus-themes-org-blocks 'gray-background)
  (setq modus-themes-subtle-line-numbers t)
  (setq modus-themes-vivendi-color-overrides
        '((bg-main . "#303030")))
  (setq modus-themes-operandi-color-overrides
        '((bg-main . "#ebebeb"))))

;;  (load-vivendi)
  (global-set-key (kbd "<f7>") 'my-demo-modus-themes-toggle)

;          (use-package heaven-and-hell
;            :ensure t
;            :init
;            (setq heaven-and-hell-theme-type 'light)
;            (setq heaven-and-hell-load-theme-no-confirm t)
;            (setq heaven-and-hell-themes
;                  '((light . modus-operandi)
;                    (dark . modus-vivendi)))
;            :hook (after-init . heaven-and-hell-init-hook)
;            :bind (("C-c <f7>" . heaven-and-hell-load-default-theme)
;                   ("<f7>" . heaven-and-hell-toggle-theme)))

;; (use-package dbus
;;   :demand t
;;   :init
;;   (defun theme-switcher (value)
;;      (pcase value
;;           ;; No Preference
;;       (0 (shell-command "gsettings set org.gnome.desktop.interface gtk-theme \"adw-gtk3\"")
;;          (load-operandi))
;;            ;; Prefers dark
;;        (1 (shell-command "gsettings set org.gnome.desktop.interface gtk-theme \"adw-gtk3-dark\"")
;;          (load-vivendi))
;;            ;; Prefers light. Not currently used by Gnome
;;        (2 (shell-command "gsettings set org.gnome.desktop.interface gtk-theme \"adw-gtk3\"")
;;          (load-operandi))
;;            (_ (message "Invalid key value"))))

;;   (defun handler (value)
;;     (theme-switcher (car (car value))))

;;   (defun signal-handler (namespace key value)
;;     (if (and
;;          (string-equal namespace "org.freedesktop.appearance")
;;          (string-equal key "color-scheme"))
;;         (theme-switcher (car value))))
;;   :config
;;   (dbus-call-method-asynchronously
;;    :session
;;    "org.freedesktop.portal.Desktop"
;;    "/org/freedesktop/portal/desktop"
;;    "org.freedesktop.portal.Settings"
;;    "Read"
;;    #'handler
;;    "org.freedesktop.appearance"
;;    "color-scheme")

;;   (dbus-register-signal
;;    :session
;;    "org.freedesktop.portal.Desktop"
;;    "/org/freedesktop/portal/desktop"
;;    "org.freedesktop.portal.Settings"
;;    "SettingChanged"
;;    #'signal-handler))

(use-package all-the-icons
  :ensure t)

;; Icons for dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda ()
                        (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))

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
  (set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.2) ;\Large
  (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.3) ;\LARGE
  ;; Only use the first 4 styles and do not cycle.
  (setq org-cycle-level-faces nil)
  (setq org-n-level-faces 4)
  ;; Document Title, (\huge)
  (set-face-attribute 'org-document-title nil
                      :height 1.3
                      :foreground 'unspecified
                      :inherit 'org-level-8)
) ;; <=== org-font-setup ends here

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

  (use-package org
    :ensure nil
    :hook ((org-mode . my/org-mode-setup)
           (org-mode . my/org-font-setup))
    :config
    ;;(add-hook 'org-mode-hook 'my/org-font-setup)
    ;; Removes the ellipsis at the end and replaces it with a string
    (setq org-ellipsis " ⤾")
    (add-to-list 'org-file-apps '("\\.pdf" . "evince %s"))
    ;; Now you can put [[color:red][red text]] when export to html
    (org-add-link-type
      "color"
      (lambda (path)
        (message (concat "color "
                         (progn (add-text-properties
                                 0 (length path)
                                 (list 'face `((t (:foreground ,path))))
                                 path) path))))
      (lambda (path desc format)
        (cond
         ((eq format 'html)
          (format "<span style=\"color:%s;\">%s</span>" path desc))
         ((eq format 'latex)
          (format "{\\color{%s}%s}" path desc)))))

    ;; If you have many subtask, when you mark it as DONE, the main task remain unchaged. With this function, if all the subtask are marked as DONE, the main task is marked as well.
    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states)   ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "PROJ"))))
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
    ;; This keybinding uses org-store-link to store a postition on a document, so you can link it on other document.
    (global-set-key (kbd "C-c l") 'org-store-link)

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

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(setq org-format-latex-header "\\documentclass{article} \\usepackage[usenames]{color} \\usepackage[default]{cantarell} \\pagestyle{empty} \\setlength{\\textwidth}{\\paperwidth} \\addtolength{\\textwidth}{-3cm} \\setlength{\\oddsidemargin}{1.5cm} \\addtolength{\\oddsidemargin}{-2.54cm} \\setlength{\\evensidemargin}{\\oddsidemargin} \\setlength{\\textheight}{\\paperheight} \\addtolength{\\textheight}{-\\headheight} \\addtolength{\\textheight}{-\\headsep} \\addtolength{\\textheight}{-\\footskip} \\addtolength{\\textheight}{-3cm} \\setlength{\\topmargin}{1.5cm} \\addtolength{\\topmargin}{-2.54cm}")

(use-package ox-latex
  :ensure nil
  :config
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
  ;; (setq org-latex-prefer-user-labels t)

  ;; deleted unwanted file extensions after latexMK
  (setq org-latex-logfiles-extensions
        (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil)))

(add-to-list 'org-latex-classes
      '("koma-article"
        "\\documentclass{scrartcl}"
        "\\usepackage[left=3cm,right=4.5cm,bottom=3cm,top=10cm]{geometry}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

  ;; https://github.com/philipphoman
      '("mybeamerposter"
        "\\documentclass[final]{beamer}
                 \\usepackage[orientation=portrait,size=letter]
                 \\usepackage[absolute,overlay]{textpos}
                       \\usepackage[authoryear]{natbib}
                       [NO-DEFAULT-PACKAGES]"))


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
(use-package ox-pandoc
  :after org
  :ensure t)

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
;;
;; This hook enables org-superstar 
(add-hook 'org-mode-hook
          (lambda ()
            (org-superstar-mode 1)))

(use-package org-tree-slide
  :ensure t
  :defer t
  :config
  (setq org-tree-slide-header t)
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

;; (setq org-directory "/mnt/Data/ORG") ; The directory of your files
;; (setq org-agenda-files '(
;;                          ("/mnt/data/Nextcloud/ORG/sync/TODO.org")
;;                          ("/mnt/data/Nextcloud/ORG/escuela.org")))
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
                        (org-agenda-todo-keyword-format "%-1s")))))

        ("h" "Horario escolar"
         ((agenda "" (
                      (org-agenda-files '("/mnt/data/CIMB/horario.org"))
                      (org-agenda-overriding-header "🗓 Horario\n")
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-skip-deadline-prewarning-if-scheduled nil)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 1)
                      (org-agenda-prefix-format "  %?-t %T %?5s")
                      (org-agenda-repeating-timestamp-show-all t)
                      ;;(concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator)
                      (org-agenda-remove-tags t)
                 (org-agenda-todo-keyword-format " ")
                  (org-agenda-time)
                  (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈ ahora")
                  ;(org-agenda-deadline-leaders '("" ""))
                  (org-agenda-time-grid (quote ((today require-timed) (800 1000 1200 1400 1600 ) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))))))

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
           ("i" "Inbox" entry
            (file "/mnt/data/Nextcloud/ORG/sync/Inbox.org"))
           ("d" "Diario de clase")
           ("de" "Tercero")
           ("dea" "Bitácora de 3A" plain
            (file+olp+datetree "/mnt/data/Nextcloud/ORG/journal.org")
             "**** %? :3A:\n")
           ("deb" "Bitácora de 3B" plain
            (file+olp+datetree "/mnt/data/Nextcloud/ORG/journal.org")
             "**** %? :3B:\n")
           ("dec" "Bitácora de 3C" plain
            (file+olp+datetree "/mnt/data/Nextcloud/ORG/journal.org")
             "**** %? :3C:\n")
           ("ded" "Bitácora de 3D" plain
            (file+olp+datetree "/mnt/data/Nextcloud/ORG/journal.org")
             "**** %? :3D:\n")))

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

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame))) 

(defun capture-frame ()
  (interactive)
  (org-capture)
  (delete-other-windows))

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
  (setq org-appear-autoentities t)
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))

(use-package ox-reveal
  :ensure t
  :config
  ;(setq org-re-reveal-center t)
  (setq org-reveal-root "file:///home/juan/.repos/reveal.js"))

(set-face-attribute 'markdown-blockquote-face nil :foreground nil :inherit '(fixed-pitch))
(set-face-attribute 'markdown-code-face nil :inherit '(fixed-pitch))
(set-face-attribute 'markdown-table-face nil :inherit '(fixed-pitch))
(set-face-attribute 'markdown-italic-face nil :slant 'italic :weight 'medium :inherit 'default)
;; set basic title font
(set-face-attribute 'markdown-header-face-6 nil :weight 'bold :inherit 'default)
;; Low levels are unimportant => no scaling
(set-face-attribute 'markdown-header-face-5 nil :inherit 'markdown-header-face-6)
(set-face-attribute 'markdown-header-face-4 nil :inherit 'markdown-header-face-6)
;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
(set-face-attribute 'markdown-header-face-3 nil :inherit 'markdown-header-face-6 :height 1.1) ;\large
(set-face-attribute 'markdown-header-face-2 nil :inherit 'markdown-header-face-6 :height 1.3) ;\Large
(set-face-attribute 'markdown-header-face-1 nil :inherit 'markdown-header-face-6 :height 1.5) ;\LARGE
(setq markdown-hide-markup t)
(setq markdown-header-scaling t)

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

; (add-to-list 'load-path "~/.emacs.d/packages/")
; (require 'beancount)
; (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

(use-package kdeconnect
  :ensure t
  :config
  (setq kdeconnect-devices "7843123afa92d0a8")
  (setq kdeconnect-active-device "7843123afa92d0a8"))

(use-package gemini-mode)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-plus-contrib gemini-mode kdeconnect evil-ledger ledger-mode markdown-mode luarocks lua-mode ox-reveal org-appear calfw-org calfw hide-mode-line org-tree-slide org-superstar ox-pandoc all-the-icons-dired all-the-icons modus-themes doom-themes minions moody dired-subtree dired-hide-dotfiles dired-open dired-single writeroom-mode helpful company yasnippet rainbow-delimiters rainbow-mode magit which-key consult marginalia orderless selectrum general evil-org evil-collection evil undo-tree emojify use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-calendar-event ((t (:inherit (default)))))
 '(org-agenda-calendar-sexp ((t (:inherit (default)))))
 '(org-agenda-date-today ((t (:weight bold :height 130))))
 '(org-agenda-structure ((t (:underline nil :weight bold :height 150 :width normal)))))
