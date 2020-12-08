(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Repositorio local
(add-to-list 'load-path "~/.emacs.d/modes")

;; Instala use-package en caso de no estar
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file "~/.repos/dotfiles/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(beacon-color "#c82829")
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("7e22a8dcf2adcd8b330eab2ed6023fa20ba3b17704d4b186fa9c53f1fab3d4d2" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "587938eeeaefd2b2f68a0970e02985246a28c02c1c140cb0943d2b6909c47261" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" default))
 '(evil-undo-system 'undo-fu)
 '(fci-rule-color "#383a42")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(frame-background-mode 'light)
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#80d200")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(org-agenda-files '("/mnt/Data/ORG/Personal.org" "/mnt/Data/ORG/Trabajo.org"))
 '(org-icalendar-use-deadline
   '(event-if-not-todo event-if-todo event-if-todo-not-done todo-due))
 '(org-icalendar-use-scheduled
   '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start))
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow color-theme-sanitync-tomorrow winnow modus-operandi-theme modus-vivendi-theme sml-modeline burly auto-complete hide-mode-line dired-single terminal-toggle vterm ivy-posframe rg ripgrep fzf nord-theme magit undo-fu undo-tree org-gnome yaml-mode dired-hide-dotfiles dired-open all-the-icons-dired openwith ranger dired-ranger org-super-agenda figlet company lua luarocks lua-mode org-superstar org-tree-slide ox-pandoc terminal-here easy-hugo rainbow-delimiters smartparens ewal-doom-themes ewal writeroom-mode heaven-and-hell doom-themes rainbow-mode markdown-mode yasnippet all-the-icons-ivy-rich counsel ivy-prescient ivy-rich ivy which-key doom-modeline evil-magit evil-org evil-collection evil use-package))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(terminal-here-terminal-command 'alacritty)
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "#a8a8a8"])
 '(xterm-color-names-bright
   ["#181a20" "#f4923b" "#80d200" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t nil)))
 '(org-agenda-calendar-event ((t (:family "Ubuntu" :inherit (default)))))
 '(org-agenda-date-today ((t (:foreground "#d7befb" :weight ultra-bold :height 130 :family "Ubuntu"))))
 '(org-agenda-structure ((t (:foreground "#ffffff" :underline t :weight bold :height 200 :width normal :family "Ubuntu"))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit fixed-pitch))))
 '(org-block-end-line ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit fixed-pitch))))
 '(org-document-info-keyword ((t (:inherit fixed-pitch))))
 '(org-meta-line ((t (:inherit fixed-pitch))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit fixed-pitch))))
 '(remind-conf-keyword-face ((t (:inherit font-lock-builtin-face :weight bold))))
 '(remind-conf-substitutes-face ((t (:inherit compilation-info :weight bold))))
 '(remind-time-face ((t (:inherit compilation-info :weight bold)))))
