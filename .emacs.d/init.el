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

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


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
    ("3c7eef027f94956ea194aafa537c78098ab4cd907a2bb11b0e6c5f42e8a95750" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e30e72b10b9c7887ff8adcd1a25b5c6eaa32665e0f8f40994e5b6d51069d3b2a" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "f2b56244ecc6f4b952b2bcb1d7e517f1f4272876a8c873b378f5cf68e904bd59" "9f15d03580b08dae41a1e5c1f00d1f1aa99fea121ca32c28e2abec9563c6e32c" "379a804655efccc13a3d446468992bfdfc30ff27d19cfda6f62c7f9c9e7a8a7d" "a92e9da0fab90cbec4af4a2035602208cebf3d071ea547157b2bfc5d9bd4d48d" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "aa5dee47c85f12d166745ae56c778eb7833df3f6799c2b2d607d5b8da8f5f579" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" default)))
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
