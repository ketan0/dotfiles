;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ketan Agrawal"
      user-mail-address "agrawalk@stanford.edu")

(defvar ketan0/dotfiles-dir (file-name-as-directory "~/.dotfiles")
     "Personal dotfiles directory.")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-rouge)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(use-package! org-super-agenda
  :defer t
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-header-separator "\n")
  (setq org-super-agenda-groups '((:auto-category t)))
  (setq org-super-agenda-header-map (make-sparse-keymap))) ;;the header keymaps conflict w/ evil-org keymaps

(use-package! org-journal
  :defer t
  :init
  (map! :map doom-leader-map "j" 'org-journal-new-entry)
  (setq org-journal-find-file 'find-file
        org-journal-dir "~/org/"
        org-journal-carryover-items nil
        org-journal-date-format "%A, %d %B %Y"))

(use-package! org-roam
  :init
  (map! :map doom-leader-map "r" 'org-roam-find-file)
  (org-roam-mode)
  :diminish org-roam-mode
  :config
  (setq org-roam-graphviz-executable "/usr/local/bin/dot")
  (setq org-roam-graph-viewer "/Applications/Google Chrome.app/")
  (setq org-roam-directory "~/org/"))

(use-package! company-math)

(defun ketan0/org-mode-setup ()
  (message "Running org-mode hook!")
  (setq-local company-backends
              (push '(company-math-symbols-unicode company-org-roam)
                    company-backends)))

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-ellipsis "…")
  (setq org-directory "~/org")
  (setq org-return-follows-link t)

  (setq org-emphasis-alist ;;different ways to emphasize text
        '(("!"  (:foreground "red") )
          ("*" (bold :foreground "Orange" ))
          ("/" italic "<i>" "</i>")
          ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
          ("-" (:overline t) "<span style=\"text-decoration:overline;\">" "</span>")
          ("~" org-code "<code>" "</code>" verbatim)
          ("=" org-verbatim "<code>" "</code>" verbatim)
          ("+" (:strike-through t) "<del>" "</del>")))

  ;;stores changes from dropbox
  (setq org-mobile-inbox-for-pull "~/org/flagged.org")
  ;;Organ (my app)'s store
  (setq org-mobile-directory "~/Dropbox/Apps/Organ/")

  ;;settings for TODOs
  (setq org-log-done 'time) ;;record time a task is done

  (setq org-agenda-files '("~/org/"))
  (setq org-agenda-block-separator nil)
  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-format-date (lambda (date) (concat "\n"
                                                 (make-string (/ (window-width) 2) 9472)
                                                 "\n"
                                                 (org-agenda-format-date-aligned date))))
  (setq org-agenda-window-setup 'only-window) ;;agenda take up whole frame
  ;;don't show warnings for deadlines
  (setq org-deadline-warning-days 0) ;;don't show upcoming tasks in today view

  (setq org-edit-src-content-indentation 0) ;;don't indent src blocks further

  ;;refile headlines to any other agenda files
  (setq org-refile-use-cache t) ;;speeds up loading refile targets
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-refile-use-outline-path 'file) ;;see whole path (not just headline)
  (setq org-outline-path-complete-in-steps nil) ;;easy to complete in one go w/ helm

  (setq org-archive-location (concat (file-name-as-directory org-directory) "archive.org::datetree/")) ;;archive done tasks to datetree in archive.org

  (setq org-catch-invisible-edits (quote show-and-error)) ;;avoid accidental edits in folded areas, links, etc.
  (setq org-default-notes-file (concat (file-name-as-directory org-directory) "capture.org"))

  (setq org-capture-templates
        '(;; other entries
          ("t" "todo" entry
           (file "~/org/capture.org")
           "* TODO %?")
          ("c" "coronavirus" entry (file+datetree
                                    "~/org/20200314210447_coronavirus.org")
           "* %^{Heading}")
          ("k" "CS 520: Knowledge Graphs" entry (file+datetree
                                                 "~/org/20200331194240-cs520_knowledge_graphs.org")
           "* %^{Heading}")
          ("l" "Linguist 167: Languages of the World" entry (file+datetree
                                                             "~/org/20200406225041-linguist_167_languages_of_the_world.org")
           "* %^{Heading}")
          ("m" "CS 229: Machine Learning" entry (file+datetree
                                                 "~/org/20200403043734-cs229_machine_learning.org")
           "* %^{Heading}")
          ("p" "CS 110: Principles of Computer Systems" entry (file+datetree
                                                               "~/org/20200403044116-cs110_principles_of_computer_systems.org")
           "* %^{Heading}")
          ("u" "new package" entry (file+headline
                                    "~/.emacs.d/init.org" "Packages")
           "* %^{package name} \n#+begin_src emacs-lisp\n(use-package %\\1)\n#+end_src\n"))))

(defun ketan0/latex-mode-setup ()
  (setq-local company-backends
              (push '(company-math-symbols-latex company-latex-commands)
                    company-backends)))

(use-package! tex
  :defer t
  :config
  (setq-default TeX-master nil)
  ;; Use pdf-tools to open PDF files
  (setq TeX-save-query nil)
  (setq TeX-auto-save t)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(defun tangle-karabiner ()
  "If the current buffer is 'karabiner.org' the code-blocks are
   tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat ketan0/dotfiles-dir "karabiner.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle-file
       (expand-file-name (concat ketan0/dotfiles-dir "karabiner.org"))
       (expand-file-name (concat ketan0/dotfiles-dir "karabiner.edn"))))
    (message (concat "Goku output: " (shell-command-to-string "goku")))))

(defun source-yabairc ()
  "If the current buffer is 'karabiner.org' the code-blocks are
   tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat ketan0/dotfiles-dir "yabairc")))
    ;; Avoid running hooks when tangling.
    (message (concat "yabairc has been sourced"
                     (shell-command-to-string
                      "launchctl kickstart -k \"gui/${UID}/homebrew.mxcl.yabai\"")))))

(add-hook 'after-save-hook 'tangle-karabiner)
(add-hook 'after-save-hook 'source-yabairc)
