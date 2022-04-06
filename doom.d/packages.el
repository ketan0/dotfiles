;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
(package! centered-window-mode)
(package! citeproc)
(package! benchmark-init)
(package! evil-extra-operator)
(package! leuven-theme)
(package! org-super-agenda)
(package! org-journal)
(package! org-roam :recipe (:host github :repo "org-roam/org-roam"))
(package! org-pomodoro)
(package! ssh-config-mode)
(package! org-ql)
(package! dash)
(package! s)
(package! f)
(package! web-mode)
(package! evil-matchit)
;; (package! smsn-mode :recipe (:host github :repo "synchrony/smsn-mode"))
;; (package! indent-guide)
;; (package! latex-math-preview)
(package! request)
(package! org-ml :recipe (:host github :repo "ndwarshuis/org-ml"))
(package! multifiles :recipe (:host github :repo "magnars/multifiles.el"))
(package! json-pointer :recipe (:host github :repo "syohex/emacs-json-pointer"))
(package! poet :recipe (:host github :repo "kunalb/poet"))
(package! texfrag :recipe (:host github :repo "TobiasZawada/texfrag"))
(package! language-detection)
(package! counsel-spotify :recipe (:host github :repo "Lautaro-Garcia/counsel-spotify"))
(package! apples-mode :recipe (:host github :repo "tequilasunset/apples-mode"))
(package! ob-applescript :recipe (:host github :repo "stig/ob-applescript.el"))
;; (package! spotify :recipe (:host github :repo "danielfm/spotify.el"))
(package! oauth2)
(package! gif-screencast :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
(package! org-fragtog)
(package! hydra)
(package! aio)
;; (package! prettier)
;; (package! lsp-mode :recipe (:host github :repo "emacs-lsp/lsp-mode"))
;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
