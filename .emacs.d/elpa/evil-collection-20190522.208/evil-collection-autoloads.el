;;; evil-collection-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-collection" "evil-collection.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from evil-collection.el

(autoload 'evil-collection-translate-key "evil-collection" "\
Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap symbols. Like `evil-define-key', when a keymap does not exist,
the keybindings will be deferred until the keymap is defined, so
`with-eval-after-load' is not neccessary. TRANSLATIONS corresponds to a list of
key replacement pairs. For example, specifying \"a\" \"b\" will bind \"a\" to
\"b\"'s definition in the keymap. Specifying nil as a replacement will unbind a
key. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the initial
invocation, and future invocations will always look up keys in the backup
keymap. When no TRANSLATIONS are given, this function will only create the
backup keymap without making any translations. On the other hand, if DESTRUCTIVE
is non-nil, the keymap will be destructively altered without creating a backup.
For example, calling this function multiple times with \"a\" \"b\" \"b\" \"a\"
would continue to swap and unswap the definitions of these keys. This means that
when DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation.

\(fn STATES KEYMAPS &rest TRANSLATIONS &key DESTRUCTIVE &allow-other-keys)" nil nil)

(function-put 'evil-collection-translate-key 'lisp-indent-function 'defun)

(autoload 'evil-collection-swap-key "evil-collection" "\
Wrapper around `evil-collection-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `evil-collection-translate-key'. ARGS
should consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\"
\"a\" with `evil-collection-translate-key') and optionally keyword arguments for
`evil-collection-translate-key'.

\(fn STATES KEYMAPS &rest ARGS)" nil t)

(function-put 'evil-collection-swap-key 'lisp-indent-function 'defun)

(autoload 'evil-collection-init "evil-collection" "\
Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load 'calendar
    (require 'evil-collection-calendar)
    (evil-collection-calendar-setup))

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'.

\(fn &optional MODES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection" '("evil-collection-")))

;;;***

;;;### (autoloads nil "evil-collection-ag" "evil-collection-ag.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ag.el

(autoload 'evil-collection-ag-setup "evil-collection-ag" "\
Set up `evil' bindings for `ag'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ag" '("evil-collection-ag-maps")))

;;;***

;;;### (autoloads nil "evil-collection-alchemist" "evil-collection-alchemist.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-alchemist.el

(autoload 'evil-collection-alchemist-setup "evil-collection-alchemist" "\
Set up `evil' bindings for `alchemist'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-alchemist" '("evil-collection-alchemist-maps")))

;;;***

;;;### (autoloads nil "evil-collection-anaconda-mode" "evil-collection-anaconda-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-anaconda-mode.el

(autoload 'evil-collection-anaconda-mode-setup "evil-collection-anaconda-mode" "\
Set up `evil' bindings for `anaconda-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-anaconda-mode" '("evil-collection-anaconda-mode-maps")))

;;;***

;;;### (autoloads nil "evil-collection-arc-mode" "evil-collection-arc-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-arc-mode.el

(autoload 'evil-collection-arc-mode-setup "evil-collection-arc-mode" "\
Set up `evil' bindings for `arc-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-arc-mode" '("evil-collection-arc-mode-maps")))

;;;***

;;;### (autoloads nil "evil-collection-bookmark" "evil-collection-bookmark.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-bookmark.el

(autoload 'evil-collection-bookmark-setup "evil-collection-bookmark" "\
Set up `evil' bindings for `bookmark'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-bookmark" '("evil-collection-bookmark-maps")))

;;;***

;;;### (autoloads nil "evil-collection-buff-menu" "evil-collection-buff-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-buff-menu.el

(autoload 'evil-collection-buff-menu-setup "evil-collection-buff-menu" "\
Set up `evil' bindings for `buff-menu'..

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-buff-menu" '("evil-collection-buff-menu-")))

;;;***

;;;### (autoloads nil "evil-collection-calc" "evil-collection-calc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-calc.el

(autoload 'evil-collection-calc-setup "evil-collection-calc" "\
Set up `evil' bindings for `calc'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-calc" '("evil-collection-calc-maps")))

;;;***

;;;### (autoloads nil "evil-collection-calendar" "evil-collection-calendar.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-calendar.el

(autoload 'evil-collection-calendar-setup "evil-collection-calendar" "\
Set up `evil' bindings for `calendar'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-calendar" '("evil-collection-calendar-maps")))

;;;***

;;;### (autoloads nil "evil-collection-cider" "evil-collection-cider.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-cider.el

(autoload 'evil-collection-cider-setup "evil-collection-cider" "\
Set up `evil' bindings for `cider'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-cider" '("evil-collection-cider-")))

;;;***

;;;### (autoloads nil "evil-collection-cmake-mode" "evil-collection-cmake-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-cmake-mode.el

(autoload 'evil-collection-cmake-mode-setup "evil-collection-cmake-mode" "\
Set up `evil' bindings for `cmake-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-cmake-mode" '("evil-collection-cmake-mode-set-evil-shift-width")))

;;;***

;;;### (autoloads nil "evil-collection-comint" "evil-collection-comint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-comint.el

(autoload 'evil-collection-comint-setup "evil-collection-comint" "\
Set up `evil' bindings for `comint'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-comint" '("evil-collection-comint-maps")))

;;;***

;;;### (autoloads nil "evil-collection-company" "evil-collection-company.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-company.el

(autoload 'evil-collection-company-setup "evil-collection-company" "\
Set up `evil' bindings for `company'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-company" '("evil-collection-company-")))

;;;***

;;;### (autoloads nil "evil-collection-compile" "evil-collection-compile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-compile.el

(autoload 'evil-collection-compile-setup "evil-collection-compile" "\
Set up `evil' bindings for `compile'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-compile" '("evil-collection-compile-maps")))

;;;***

;;;### (autoloads nil "evil-collection-cus-theme" "evil-collection-cus-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-cus-theme.el

(autoload 'evil-collection-cus-theme-setup "evil-collection-cus-theme" "\
Set up `evil' bindings for `cus-theme'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-cus-theme" '("evil-collection-cus-theme-maps")))

;;;***

;;;### (autoloads nil "evil-collection-custom" "evil-collection-custom.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-custom.el

(autoload 'evil-collection-custom-setup "evil-collection-custom" "\
Set up `evil' bindings for `Custom-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-custom" '("evil-collection-custom-maps")))

;;;***

;;;### (autoloads nil "evil-collection-daemons" "evil-collection-daemons.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-daemons.el

(autoload 'evil-collection-daemons-setup "evil-collection-daemons" "\
Set up `evil' bindings for `daemons'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-daemons" '("evil-collection-daemons-maps")))

;;;***

;;;### (autoloads nil "evil-collection-deadgrep" "evil-collection-deadgrep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-deadgrep.el

(autoload 'evil-collection-deadgrep-setup "evil-collection-deadgrep" "\
Set up `evil' bindings for deadgrep..

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-deadgrep" '("evil-collection-deadgrep-maps")))

;;;***

;;;### (autoloads nil "evil-collection-debbugs" "evil-collection-debbugs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-debbugs.el

(autoload 'evil-collection-debbugs-setup "evil-collection-debbugs" "\
Set up `evil' bindings for `debbugs-gnu-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-debbugs" '("evil-collection-debbugs-maps")))

;;;***

;;;### (autoloads nil "evil-collection-debug" "evil-collection-debug.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-debug.el

(autoload 'evil-collection-debug-setup "evil-collection-debug" "\
Set up `evil' bindings for `debug'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-debug" '("evil-collection-debug-maps")))

;;;***

;;;### (autoloads nil "evil-collection-diff-mode" "evil-collection-diff-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-diff-mode.el

(autoload 'evil-collection-diff-toggle-setup "evil-collection-diff-mode" "\
Toggle visiting diff buffers in motion state.

\(fn)" t nil)

(autoload 'evil-collection-diff-mode-setup "evil-collection-diff-mode" "\
Set up `evil' bindings for `diff-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-diff-mode" '("evil-collection-diff-")))

;;;***

;;;### (autoloads nil "evil-collection-dired" "evil-collection-dired.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-dired.el

(autoload 'evil-collection-dired-setup "evil-collection-dired" "\
Set up `evil' bindings for `dired'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-dired" '("evil-collection-dired-maps")))

;;;***

;;;### (autoloads nil "evil-collection-disk-usage" "evil-collection-disk-usage.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-disk-usage.el

(autoload 'evil-collection-disk-usage-setup "evil-collection-disk-usage" "\
Set up `evil' bindings for `disk-usage'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-disk-usage" '("evil-collection-disk-usage-maps")))

;;;***

;;;### (autoloads nil "evil-collection-doc-view" "evil-collection-doc-view.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-doc-view.el

(autoload 'evil-collection-doc-view-setup "evil-collection-doc-view" "\
Set up `evil' bindings for `doc-view'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-doc-view" '("evil-collection-doc-view-maps")))

;;;***

;;;### (autoloads nil "evil-collection-ebib" "evil-collection-ebib.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ebib.el

(autoload 'evil-collection-ebib-setup "evil-collection-ebib" "\
Set up `evil' bindings for `ebib'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ebib" '("evil-collection-ebib-maps")))

;;;***

;;;### (autoloads nil "evil-collection-edbi" "evil-collection-edbi.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-edbi.el

(autoload 'evil-collection-edbi-setup "evil-collection-edbi" "\
Set up `evil' bindings for EDBI.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-edbi" '("evil-collection-edbi-")))

;;;***

;;;### (autoloads nil "evil-collection-edebug" "evil-collection-edebug.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-edebug.el

(autoload 'evil-collection-edebug-setup "evil-collection-edebug" "\
Set up `evil' bindings for `edebug'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-edebug" '("evil-collection-edebug-maps")))

;;;***

;;;### (autoloads nil "evil-collection-ediff" "evil-collection-ediff.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ediff.el

(autoload 'evil-collection-ediff-setup "evil-collection-ediff" "\
Initialize evil-ediff.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ediff" '("evil-collection-ediff-")))

;;;***

;;;### (autoloads nil "evil-collection-eglot" "evil-collection-eglot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-eglot.el

(autoload 'evil-collection-eglot-setup "evil-collection-eglot" "\
Set up `evil' bindings for `eglot'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-eglot" '("evil-collection-eglot-maps")))

;;;***

;;;### (autoloads nil "evil-collection-elfeed" "evil-collection-elfeed.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-elfeed.el

(autoload 'evil-collection-elfeed-setup "evil-collection-elfeed" "\
Set up `evil' bindings for `elfeed'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-elfeed" '("evil-collection-elfeed-maps")))

;;;***

;;;### (autoloads nil "evil-collection-elisp-mode" "evil-collection-elisp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-elisp-mode.el

(autoload 'evil-collection-elisp-mode-setup "evil-collection-elisp-mode" "\
Set up `evil' bindings for `elisp-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-elisp-mode" '("evil-collection-elisp-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-elisp-refs" "evil-collection-elisp-refs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-elisp-refs.el

(autoload 'evil-collection-elisp-refs-setup "evil-collection-elisp-refs" "\
Set up `evil' bindings for `elisp-refs'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-elisp-refs" '("evil-collection-elisp-refs-maps")))

;;;***

;;;### (autoloads nil "evil-collection-emms" "evil-collection-emms.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-emms.el

(autoload 'evil-collection-emms-browser-setup "evil-collection-emms" "\
Set up `evil' bindings for `emms-browser'.

\(fn)" nil nil)

(autoload 'evil-collection-emms-playlist-setup "evil-collection-emms" "\
Set up `evil' bindings for `emms-playlist'.

\(fn)" nil nil)

(autoload 'evil-collection-emms-setup "evil-collection-emms" "\
Set up `evil' bindings for `emms'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-emms" '("evil-collection-emms-")))

;;;***

;;;### (autoloads nil "evil-collection-epa" "evil-collection-epa.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-epa.el

(autoload 'evil-collection-epa-setup "evil-collection-epa" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-epa" '("evil-collection-epa-maps")))

;;;***

;;;### (autoloads nil "evil-collection-ert" "evil-collection-ert.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ert.el

(autoload 'evil-collection-ert-setup "evil-collection-ert" "\
Set up `evil' bindings for `ert'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ert" '("evil-collection-ert-maps")))

;;;***

;;;### (autoloads nil "evil-collection-eshell" "evil-collection-eshell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-eshell.el

(autoload 'evil-collection-eshell-setup "evil-collection-eshell" "\
Set up `evil' bindings for `eshell'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-eshell" '("evil-collection-eshell-")))

;;;***

;;;### (autoloads nil "evil-collection-eval-sexp-fu" "evil-collection-eval-sexp-fu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-eval-sexp-fu.el

(autoload 'evil-collection-eval-sexp-fu-setup "evil-collection-eval-sexp-fu" "\
Set up `evil' with `eval-sexp-fu'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-eval-sexp-fu" '("evil-collection-eval-sexp-fu-")))

;;;***

;;;### (autoloads nil "evil-collection-evil-mc" "evil-collection-evil-mc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-evil-mc.el

(autoload 'evil-collection-evil-mc-setup "evil-collection-evil-mc" "\
Set up `evil' bindings for evil-mc.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-evil-mc" '("evil-collection-evil-mc-")))

;;;***

;;;### (autoloads nil "evil-collection-eww" "evil-collection-eww.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-eww.el

(autoload 'evil-collection-eww-setup "evil-collection-eww" "\
Set up `evil' bindings for `eww'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-eww" '("evil-collection-eww-maps")))

;;;***

;;;### (autoloads nil "evil-collection-flycheck" "evil-collection-flycheck.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-flycheck.el

(autoload 'evil-collection-flycheck-setup "evil-collection-flycheck" "\
Set up `evil' bindings for `flycheck'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-flycheck" '("evil-collection-flycheck-maps")))

;;;***

;;;### (autoloads nil "evil-collection-flymake" "evil-collection-flymake.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-flymake.el

(autoload 'evil-collection-flymake-setup "evil-collection-flymake" "\
Set up `evil' bindings for `flymake'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-flymake" '("evil-collection-flymake-maps")))

;;;***

;;;### (autoloads nil "evil-collection-free-keys" "evil-collection-free-keys.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-free-keys.el

(autoload 'evil-collection-free-keys-setup "evil-collection-free-keys" "\
Set up `evil' bindings for `free-keys'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-free-keys" '("evil-collection-free-keys-")))

;;;***

;;;### (autoloads nil "evil-collection-geiser" "evil-collection-geiser.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-geiser.el

(autoload 'evil-collection-geiser-setup "evil-collection-geiser" "\
Set up bindings for `geiser'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-geiser" '("evil-collection-geiser-")))

;;;***

;;;### (autoloads nil "evil-collection-ggtags" "evil-collection-ggtags.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ggtags.el

(autoload 'evil-collection-ggtags-setup "evil-collection-ggtags" "\
Set up `evil' bindings for `ggtags'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ggtags" '("evil-collection-ggtags-maps")))

;;;***

;;;### (autoloads nil "evil-collection-git-timemachine" "evil-collection-git-timemachine.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-git-timemachine.el

(autoload 'evil-collection-git-timemachine-setup "evil-collection-git-timemachine" "\
Setup `evil' keybindings for `git-timemachine'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-git-timemachine" '("evil-collection-git-timemachine-map")))

;;;***

;;;### (autoloads nil "evil-collection-go-mode" "evil-collection-go-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-go-mode.el

(autoload 'evil-collection-go-mode-setup "evil-collection-go-mode" "\
Set up `evil' bindings for `go-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-go-mode" '("evil-collection-go-mode-maps")))

;;;***

;;;### (autoloads nil "evil-collection-grep" "evil-collection-grep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-grep.el

(autoload 'evil-collection-grep-setup "evil-collection-grep" "\
Set up `evil' bindings for `grep'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-grep" '("evil-collection-grep-maps")))

;;;***

;;;### (autoloads nil "evil-collection-guix" "evil-collection-guix.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-guix.el

(autoload 'evil-collection-guix-setup "evil-collection-guix" "\
Set up `evil' bindings for `guix'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-guix" '("evil-collection-guix-")))

;;;***

;;;### (autoloads nil "evil-collection-hackernews" "evil-collection-hackernews.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-hackernews.el

(autoload 'evil-collection-hackernews-setup "evil-collection-hackernews" "\
Set up `evil' bindings for `hackernews-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-hackernews" '("evil-collection-hackernews-maps")))

;;;***

;;;### (autoloads nil "evil-collection-helm" "evil-collection-helm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-helm.el

(autoload 'evil-collection-helm-setup "evil-collection-helm" "\
Set up `evil' bindings for `helm'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-helm" '("evil-collection-helm-")))

;;;***

;;;### (autoloads nil "evil-collection-help" "evil-collection-help.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-help.el

(autoload 'evil-collection-help-setup "evil-collection-help" "\
Set up `evil' bindings for `help'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-help" '("evil-collection-help-maps")))

;;;***

;;;### (autoloads nil "evil-collection-helpful" "evil-collection-helpful.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-helpful.el

(autoload 'evil-collection-helpful-setup "evil-collection-helpful" "\
Set up `evil' bindings for `helpful'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-helpful" '("evil-collection-helpful-maps")))

;;;***

;;;### (autoloads nil "evil-collection-ibuffer" "evil-collection-ibuffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ibuffer.el

(autoload 'evil-collection-ibuffer-setup "evil-collection-ibuffer" "\
Set up `evil' bindings for `ibuffer'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ibuffer" '("evil-collection-ibuffer-maps")))

;;;***

;;;### (autoloads nil "evil-collection-image" "evil-collection-image.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-image.el

(autoload 'evil-collection-image-setup "evil-collection-image" "\
Set up `evil' bindings for `image-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-image" '("evil-collection-image-maps")))

;;;***

;;;### (autoloads nil "evil-collection-image+" "evil-collection-image+.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-image+.el

(autoload 'evil-collection-image+-setup "evil-collection-image+" "\
Set up `evil' bindings for `image+'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-image+" '("evil-collection-image+-maps")))

;;;***

;;;### (autoloads nil "evil-collection-image-dired" "evil-collection-image-dired.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-image-dired.el

(autoload 'evil-collection-image-dired-setup "evil-collection-image-dired" "\
Set up `evil' bindings for `image-dired-thumbnail-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-image-dired" '("evil-collection-image-dired-maps")))

;;;***

;;;### (autoloads nil "evil-collection-imenu-list" "evil-collection-imenu-list.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-imenu-list.el

(autoload 'evil-collection-imenu-list-setup "evil-collection-imenu-list" "\
Set up `evil' bindings for `imenu-list'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-imenu-list" '("evil-collection-imenu-list-maps")))

;;;***

;;;### (autoloads nil "evil-collection-indium" "evil-collection-indium.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-indium.el

(autoload 'evil-collection-indium-setup "evil-collection-indium" "\
Set up `evil' bindings for `indium'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-indium" '("evil-collection-indium-maps")))

;;;***

;;;### (autoloads nil "evil-collection-info" "evil-collection-info.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-info.el

(autoload 'evil-collection-info-setup "evil-collection-info" "\
Set up `evil' bindings for `info-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-info" '("evil-collection-info-maps")))

;;;***

;;;### (autoloads nil "evil-collection-ivy" "evil-collection-ivy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ivy.el

(autoload 'evil-collection-ivy-setup "evil-collection-ivy" "\
Set up `evil' bindings for `ivy-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ivy" '("evil-collection-ivy-maps")))

;;;***

;;;### (autoloads nil "evil-collection-js2-mode" "evil-collection-js2-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-js2-mode.el

(autoload 'evil-collection-js2-mode-setup "evil-collection-js2-mode" "\
Set up `evil' bindings for `js2-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-js2-mode" '("evil-collection-js2-set-evil-shift-width")))

;;;***

;;;### (autoloads nil "evil-collection-kotlin-mode" "evil-collection-kotlin-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-kotlin-mode.el

(autoload 'evil-collection-kotlin-mode-setup "evil-collection-kotlin-mode" "\
Set up `evil' bindings for `kotlin-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-kotlin-mode" '("evil-collection-kotlin-maps")))

;;;***

;;;### (autoloads nil "evil-collection-lispy" "evil-collection-lispy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-lispy.el

(autoload 'evil-collection-lispy-setup "evil-collection-lispy" "\
Set up `evil' bindings for `lispy'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-lispy" '("evil-collection-lispy-" "lispy-tab-hydra" "g-knight")))

;;;***

;;;### (autoloads nil "evil-collection-log-edit" "evil-collection-log-edit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-log-edit.el

(autoload 'evil-collection-log-edit-setup "evil-collection-log-edit" "\
Set up `evil' bindings for `log-edit'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-log-edit" '("evil-collection-log-edit-maps")))

;;;***

;;;### (autoloads nil "evil-collection-log-view" "evil-collection-log-view.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-log-view.el

(autoload 'evil-collection-log-view-setup "evil-collection-log-view" "\
Set up `evil' bindings for `log-view'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-log-view" '("evil-collection-log-view-maps")))

;;;***

;;;### (autoloads nil "evil-collection-lsp-ui-imenu" "evil-collection-lsp-ui-imenu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-lsp-ui-imenu.el

(autoload 'evil-collection-lsp-ui-imenu-setup "evil-collection-lsp-ui-imenu" "\
Set up `evil' bindings for `lsp-ui-imenu'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-lsp-ui-imenu" '("evil-collection-lsp-ui-imenu-mode-maps")))

;;;***

;;;### (autoloads nil "evil-collection-lua-mode" "evil-collection-lua-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-lua-mode.el

(autoload 'evil-collection-lua-mode-setup "evil-collection-lua-mode" "\
Set up `evil' bindings for `lua-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-lua-mode" '("evil-collection-lua-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-macrostep" "evil-collection-macrostep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-macrostep.el

(autoload 'evil-collection-macrostep-setup "evil-collection-macrostep" "\
Set up `evil' bindings for `macrostep'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-macrostep" '("evil-collection-macrostep-maps")))

;;;***

;;;### (autoloads nil "evil-collection-magit" "evil-collection-magit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-magit.el

(autoload 'evil-collection-magit-setup "evil-collection-magit" "\
Set up `evil' bindings for `magit'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-magit" '("evil-collection-magit-maps")))

;;;***

;;;### (autoloads nil "evil-collection-magit-todos" "evil-collection-magit-todos.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-magit-todos.el

(autoload 'evil-collection-magit-todos-setup "evil-collection-magit-todos" "\
Set up `evil' bindings for `magit-todos'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-magit-todos" '("evil-collection-magit-todos-")))

;;;***

;;;### (autoloads nil "evil-collection-man" "evil-collection-man.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-man.el

(autoload 'evil-collection-man-setup "evil-collection-man" "\
Set up `evil' bindings for `man'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-man" '("evil-collection-man-maps")))

;;;***

;;;### (autoloads nil "evil-collection-minibuffer" "evil-collection-minibuffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-minibuffer.el

(autoload 'evil-collection-minibuffer-setup "evil-collection-minibuffer" "\
Initialize minibuffer for `evil'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-minibuffer" '("evil-collection-minibuffer-")))

;;;***

;;;### (autoloads nil "evil-collection-mu4e" "evil-collection-mu4e.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-mu4e.el

(autoload 'evil-collection-mu4e-setup "evil-collection-mu4e" "\
Initialize evil-mu4e if necessary.
If mu4e-main-mode is in evil-state-motion-modes, initialization
is already done earlier.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-mu4e" '("evil-collection-mu4e-")))

;;;***

;;;### (autoloads nil "evil-collection-mu4e-conversation" "evil-collection-mu4e-conversation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-mu4e-conversation.el

(autoload 'evil-collection-mu4e-conversation-setup "evil-collection-mu4e-conversation" "\
Set up `evil' bindings for `mu4e-conversation'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-mu4e-conversation" '("evil-collection-mu4e-conversation-")))

;;;***

;;;### (autoloads nil "evil-collection-neotree" "evil-collection-neotree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-neotree.el

(autoload 'evil-collection-neotree-setup "evil-collection-neotree" "\
Set up `evil' bindings for `neotree'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-neotree" '("evil-collection-neotree-maps")))

;;;***

;;;### (autoloads nil "evil-collection-notmuch" "evil-collection-notmuch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-notmuch.el

(autoload 'evil-collection-notmuch-setup "evil-collection-notmuch" "\
Set up `evil' bindings for `notmuch'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-notmuch" '("evil-collection-notmuch-")))

;;;***

;;;### (autoloads nil "evil-collection-nov" "evil-collection-nov.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-nov.el

(autoload 'evil-collection-nov-setup "evil-collection-nov" "\
Set up `evil' bindings for `nov'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-nov" '("evil-collection-nov-maps")))

;;;***

;;;### (autoloads nil "evil-collection-occur" "evil-collection-occur.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-occur.el

(autoload 'evil-collection-occur-setup "evil-collection-occur" "\
Set up `evil' bindings for `occur'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-occur" '("evil-collection-occur-maps")))

;;;***

;;;### (autoloads nil "evil-collection-omnisharp" "evil-collection-omnisharp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-omnisharp.el

(autoload 'evil-collection-omnisharp-setup "evil-collection-omnisharp" "\
Set up `evil' bindings for `omnisharp'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-omnisharp" '("evil-collection-omnisharp-maps")))

;;;***

;;;### (autoloads nil "evil-collection-outline" "evil-collection-outline.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-outline.el

(autoload 'evil-collection-outline-setup "evil-collection-outline" "\
Set up `evil' bindings for `outline'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-outline" '("evil-collection-outline-")))

;;;***

;;;### (autoloads nil "evil-collection-p4" "evil-collection-p4.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-p4.el

(autoload 'evil-collection-p4-setup "evil-collection-p4" "\
Set up `evil' bindings for `p4'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-p4" '("evil-collection-p4-maps")))

;;;***

;;;### (autoloads nil "evil-collection-package-menu" "evil-collection-package-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-package-menu.el

(autoload 'evil-collection-package-menu-setup "evil-collection-package-menu" "\
Set up `evil' bindings for `package-menu'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-package-menu" '("evil-collection-package-menu-maps")))

;;;***

;;;### (autoloads nil "evil-collection-pass" "evil-collection-pass.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-pass.el

(autoload 'evil-collection-pass-setup "evil-collection-pass" "\
Set up `evil' bindings for `pass-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-pass" '("evil-collection-pass-maps")))

;;;***

;;;### (autoloads nil "evil-collection-pdf" "evil-collection-pdf.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-pdf.el

(autoload 'evil-collection-pdf-setup "evil-collection-pdf" "\
Set up `evil' bindings for `pdf-view'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-pdf" '("evil-collection-pdf-")))

;;;***

;;;### (autoloads nil "evil-collection-popup" "evil-collection-popup.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-popup.el

(autoload 'evil-collection-popup-setup "evil-collection-popup" "\
Set up `evil' bindings for `popup'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-popup" '("evil-collection-popup-maps")))

;;;***

;;;### (autoloads nil "evil-collection-proced" "evil-collection-proced.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-proced.el

(autoload 'evil-collection-proced-setup "evil-collection-proced" "\
Set up `evil' bindings for `proced'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-proced" '("evil-collection-proced-maps")))

;;;***

;;;### (autoloads nil "evil-collection-process-menu" "evil-collection-process-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-process-menu.el

(autoload 'evil-collection-process-menu-setup "evil-collection-process-menu" "\
Set up `evil' bindings for `list-processes'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-process-menu" '("evil-collection-process-menu-maps")))

;;;***

;;;### (autoloads nil "evil-collection-prodigy" "evil-collection-prodigy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-prodigy.el

(autoload 'evil-collection-prodigy-setup "evil-collection-prodigy" "\
Set up `evil' bindings for `prodigy'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-prodigy" '("evil-collection-prodigy-maps")))

;;;***

;;;### (autoloads nil "evil-collection-profiler" "evil-collection-profiler.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-profiler.el

(autoload 'evil-collection-profiler-setup "evil-collection-profiler" "\
Set up `evil' bindings for `profiler'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-profiler" '("evil-collection-profiler-maps")))

;;;***

;;;### (autoloads nil "evil-collection-python" "evil-collection-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-python.el

(autoload 'evil-collection-python-setup "evil-collection-python" "\
Set up `evil' bindings for `python'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-python" '("evil-collection-python-")))

;;;***

;;;### (autoloads nil "evil-collection-quickrun" "evil-collection-quickrun.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-quickrun.el

(autoload 'evil-collection-quickrun-setup "evil-collection-quickrun" "\
Set up `evil' bindings for `quickrun'..

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-quickrun" '("evil-collection-quickrun-maps")))

;;;***

;;;### (autoloads nil "evil-collection-racer" "evil-collection-racer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-racer.el

(autoload 'evil-collection-racer-setup "evil-collection-racer" "\
Set up `evil' bindings for `racer'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-racer" '("evil-collection-racer-maps")))

;;;***

;;;### (autoloads nil "evil-collection-realgud" "evil-collection-realgud.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-realgud.el

(autoload 'evil-collection-realgud-setup "evil-collection-realgud" "\
Set up `evil' bindings for `realgud'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-realgud" '("evil-collection-realgud-maps")))

;;;***

;;;### (autoloads nil "evil-collection-reftex" "evil-collection-reftex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-reftex.el

(autoload 'evil-collection-reftex-setup "evil-collection-reftex" "\
Set up `evil' bindings for `reftex'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-reftex" '("evil-collection-reftex-maps")))

;;;***

;;;### (autoloads nil "evil-collection-restclient" "evil-collection-restclient.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-restclient.el

(autoload 'evil-collection-restclient-setup "evil-collection-restclient" "\
Set up `evil' bindings for `restclient'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-restclient" '("evil-collection-restclient-")))

;;;***

;;;### (autoloads nil "evil-collection-rjsx-mode" "evil-collection-rjsx-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-rjsx-mode.el

(autoload 'evil-collection-rjsx-mode-setup "evil-collection-rjsx-mode" "\
Set up `evil' bindings for `rjsx-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-rjsx-mode" '("evil-collection-rjsx-maps")))

;;;***

;;;### (autoloads nil "evil-collection-robe" "evil-collection-robe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-robe.el

(autoload 'evil-collection-robe-setup "evil-collection-robe" "\
Set up `evil' bindings for `robe'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-robe" '("evil-collection-robe-maps")))

;;;***

;;;### (autoloads nil "evil-collection-rtags" "evil-collection-rtags.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-rtags.el

(autoload 'evil-collection-rtags-setup "evil-collection-rtags" "\
Set up `evil' bindings for `rtags'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-rtags" '("evil-collection-rtags-maps")))

;;;***

;;;### (autoloads nil "evil-collection-ruby-mode" "evil-collection-ruby-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ruby-mode.el

(autoload 'evil-collection-ruby-mode-setup "evil-collection-ruby-mode" "\
Set up `evil' bindings for `ruby'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ruby-mode" '("evil-collection-ruby-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-settings" "evil-collection-settings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-settings.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-settings" '("evil-collection-settings-setup-debugger-keys")))

;;;***

;;;### (autoloads nil "evil-collection-simple" "evil-collection-simple.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-simple.el

(autoload 'evil-collection-simple-setup "evil-collection-simple" "\
Set up `evil' bindings for `simple'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-simple" '("evil-collection-simple-maps")))

;;;***

;;;### (autoloads nil "evil-collection-slime" "evil-collection-slime.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-slime.el

(autoload 'evil-collection-slime-setup "evil-collection-slime" "\
Set up `evil' bindings for `slime'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-slime" '("evil-collection-slime-")))

;;;***

;;;### (autoloads nil "evil-collection-term" "evil-collection-term.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-term.el

(autoload 'evil-collection-term-setup "evil-collection-term" "\
Set up `evil' bindings for `term'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-term" '("evil-collection-term-")))

;;;***

;;;### (autoloads nil "evil-collection-tetris" "evil-collection-tetris.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-tetris.el

(autoload 'evil-collection-tetris-setup "evil-collection-tetris" "\
Set up `evil' bindings for `tetris'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-tetris" '("evil-collection-tetris-maps")))

;;;***

;;;### (autoloads nil "evil-collection-tide" "evil-collection-tide.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-tide.el

(autoload 'evil-collection-tide-setup "evil-collection-tide" "\
Set up `evil' bindings for `tide'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-tide" '("evil-collection-tide-maps")))

;;;***

;;;### (autoloads nil "evil-collection-transmission" "evil-collection-transmission.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-transmission.el

(autoload 'evil-collection-transmission-setup "evil-collection-transmission" "\
Set up `evil' bindings for `transmission'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-transmission" '("evil-collection-transmission-maps")))

;;;***

;;;### (autoloads nil "evil-collection-typescript-mode" "evil-collection-typescript-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-typescript-mode.el

(autoload 'evil-collection-typescript-mode-setup "evil-collection-typescript-mode" "\
Set up `evil' bindings for `typescript-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-typescript-mode" '("evil-collection-typescript-mode-set-evil-shift-width")))

;;;***

;;;### (autoloads nil "evil-collection-vc-annotate" "evil-collection-vc-annotate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vc-annotate.el

(autoload 'evil-collection-vc-annotate-setup "evil-collection-vc-annotate" "\
Set up `evil' bindings for `vc-annotate'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-vc-annotate" '("evil-collection-vc-annotate-maps")))

;;;***

;;;### (autoloads nil "evil-collection-vc-dir" "evil-collection-vc-dir.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vc-dir.el

(autoload 'evil-collection-vc-dir-setup "evil-collection-vc-dir" "\
Set up `evil' bindings for `vc-dir'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-vc-dir" '("evil-collection-vc-dir-maps")))

;;;***

;;;### (autoloads nil "evil-collection-vc-git" "evil-collection-vc-git.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vc-git.el

(autoload 'evil-collection-vc-git-setup "evil-collection-vc-git" "\
Set up `evil' bindings for `vc-git'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-vc-git" '("evil-collection-vc-git-maps")))

;;;***

;;;### (autoloads nil "evil-collection-vdiff" "evil-collection-vdiff.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vdiff.el

(autoload 'evil-collection-vdiff-setup "evil-collection-vdiff" "\
Set up `evil' bindings for `vdiff-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "evil-collection-view" "evil-collection-view.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-view.el

(autoload 'evil-collection-view-setup "evil-collection-view" "\
Set up `evil' bindings for `view'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-view" '("evil-collection-view-maps")))

;;;***

;;;### (autoloads nil "evil-collection-vlf" "evil-collection-vlf.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vlf.el

(autoload 'evil-collection-vlf-setup "evil-collection-vlf" "\
Set up `evil' bindings for `vlf'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-vlf" '("evil-collection-vlf-")))

;;;***

;;;### (autoloads nil "evil-collection-vterm" "evil-collection-vterm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vterm.el

(autoload 'evil-collection-vterm-setup "evil-collection-vterm" "\
Set up `evil' bindings for `vterm'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-vterm" '("evil-collection-vterm-")))

;;;***

;;;### (autoloads nil "evil-collection-w3m" "evil-collection-w3m.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-w3m.el

(autoload 'evil-collection-w3m-setup "evil-collection-w3m" "\
Set up `evil' bindings for `w3m'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-w3m" '("evil-collection-w3m-maps")))

;;;***

;;;### (autoloads nil "evil-collection-wdired" "evil-collection-wdired.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-wdired.el

(autoload 'evil-collection-wdired-setup "evil-collection-wdired" "\
Set up `evil' bindings for `wdired'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-wdired" '("evil-collection-wdired-maps")))

;;;***

;;;### (autoloads nil "evil-collection-wgrep" "evil-collection-wgrep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-wgrep.el

(autoload 'evil-collection-wgrep-setup "evil-collection-wgrep" "\
Set up `evil' bindings for `wgrep'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-wgrep" '("evil-collection-wgrep-maps")))

;;;***

;;;### (autoloads nil "evil-collection-which-key" "evil-collection-which-key.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-which-key.el

(autoload 'evil-collection-which-key-setup "evil-collection-which-key" "\
Set up `evil' bindings for `which-key'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-which-key" '("evil-collection-which-key-maps")))

;;;***

;;;### (autoloads nil "evil-collection-woman" "evil-collection-woman.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-woman.el

(autoload 'evil-collection-woman-setup "evil-collection-woman" "\
Set up `evil' bindings for `woman'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-woman" '("evil-collection-woman-maps")))

;;;***

;;;### (autoloads nil "evil-collection-xref" "evil-collection-xref.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-xref.el

(autoload 'evil-collection-xref-setup "evil-collection-xref" "\
Set up `evil' bindings for `xref'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-xref" '("evil-collection-xref-maps")))

;;;***

;;;### (autoloads nil "evil-collection-youtube-dl" "evil-collection-youtube-dl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-youtube-dl.el

(autoload 'evil-collection-youtube-dl-setup "evil-collection-youtube-dl" "\
Set up `evil' bindings for `youtube-dl'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-youtube-dl" '("evil-collection-youtube-dl-maps")))

;;;***

;;;### (autoloads nil "evil-collection-ztree" "evil-collection-ztree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ztree.el

(autoload 'evil-collection-ztree-setup "evil-collection-ztree" "\
Set up `evil' bindings for `ztree'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ztree" '("evil-collection-ztree-maps")))

;;;***

;;;### (autoloads nil nil ("evil-collection-integration.el" "evil-collection-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-collection-autoloads.el ends here
