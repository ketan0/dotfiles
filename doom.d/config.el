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
(setq doom-theme 'doom-horizon)

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

;; use visual lines + relative numbering
(setq vc-handled-backends '(Git))

(setq display-line-numbers-type t)

(map! :map prog-mode-map :nm "<tab>" '+fold/toggle)
(defvar ketan0/fold-state nil
  "HACK: keep track of whether everything in the buffer is folded")
(defun ketan0/fold-toggle-all ()
  (interactive)
  (if ketan0/fold-state (+fold/open-all) (+fold/close-all))
  (setq-local ketan0/fold-state (not ketan0/fold-state)))
(map! :map prog-mode-map :nm "<S-tab>" 'ketan0/fold-toggle-all)

(map! :map evil-motion-state-map "gj" 'evil-next-line)
(map! :map evil-motion-state-map "gk" 'evil-previous-line)

(map! :map evil-motion-state-map "k" 'evil-previous-visual-line)

(map! :map evil-motion-state-map "j" 'evil-next-visual-line)
(map! :map evil-motion-state-map "k" 'evil-previous-visual-line)
(map! :map evil-visual-state-map "j" 'evil-next-visual-line)
(map! :map evil-visual-state-map "k" 'evil-previous-visual-line)

(map! :map evil-normal-state-map "Q" (kbd "@q"))

;; (use-package! org-super-agenda
;;   :defer t
;;   :config
;;   (org-super-agenda-mode t)
;;   (setq org-super-agenda-header-separator "\n")
;;   (setq org-super-agenda-groups '((:auto-category t)))
;;   (setq org-super-agenda-header-map (make-sparse-keymap))) ;;the header keymaps conflict w/ evil-org keymaps

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
  ;; (message "Running org-mode hook!")
  (setq-local company-backends
              (push '(company-math-symbols-unicode company-org-roam)
                    company-backends)))

(use-package org
  :defer nil
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-ellipsis "…")
  (setq org-directory "~/org/")
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
  (setq org-mobile-directory "~/Library/Mobile Documents/iCloud\~com\~appsonthemove\~beorg/Documents/org/")
  (setq org-mobile-checksum-files "~/Library/Mobile Documents/iCloud\~com\~appsonthemove\~beorg/Documents/org/checksums.dat")
  ;;settings for TODOs
  (setq org-log-done 'time) ;;record time a task is done

  (setq org-agenda-files '("~/org/capture.org"
                           "~/org/todos.org"
                           "~/org/20200514213715-the_document_2.org"))

  (setq org-agenda-span 'day)

  ;;https://github.com/jethrokuan/.emacs.d/blob/master/init.el
  (setq ketan0/org-agenda-todo-view
        `(" " "Ketan's Custom Agenda"
          ((agenda ""
                   ((org-agenda-span 'day)
                    (org-deadline-warning-days 10)))
           (todo "TODO"
                 ((org-agenda-overriding-header "To Refile")
                  (org-agenda-files '("~/org/capture.org"))))
           (todo "STRT"
                 ((org-agenda-overriding-header "Queue")
                  (org-agenda-files '("~/org/todos.org"))))
           nil)))


  (setq ketan0/org-agenda-todo-view-remote
        `("r" "Ketan's Custom Agenda"
          ((agenda ""
                   ((org-agenda-span 'day)
                    (org-deadline-warning-days 10)
                    (org-agenda-files '("/ssh:ketanmba:~/org/capture.org"
                                        "/ssh:ketanmba:~/org/todos.org"
                                        "/ssh:ketanmba:~/org/20200514213715-the_document_2.org"))))
           (todo "TODO"
                 ((org-agenda-overriding-header "To Refile")
                  (org-agenda-files '("/ssh:ketanmba:~/org/capture.org"))))
           (todo "STRT"
                 ((org-agenda-overriding-header "Queue")
                  (org-agenda-files '("/ssh:ketanmba:~/org/todos.org"))))
           nil)))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
          ((agenda "")
           (alltodo "")))))
  (add-to-list 'org-agenda-custom-commands `,ketan0/org-agenda-todo-view)
  (add-to-list 'org-agenda-custom-commands `,ketan0/org-agenda-todo-view-remote)

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
  (setq ketan0/org-files (file-expand-wildcards "~/org/*org"))
  (setq org-refile-targets '((ketan0/org-files :maxlevel . 3)))
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
                    company-backends))
  (push '(?$ . ("$ " . " $")) evil-surround-pairs-alist))

(use-package! tex
  :defer t

  :config
  (setq-default TeX-master nil)
  ;; Use pdf-tools to open PDF files
  (setq latex-run-command "pdflatex")
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

(use-package! lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l")
  :config
  (setq lsp-auto-require-clients t)
  (setq lsp-auto-configure t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-eldoc-hook nil)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd-10")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (c++-mode . lsp)
         ;; (c-mode . lsp)
         (python-mode . lsp)
         (tex-mode . lsp)
         (latex-mode . lsp)
         )
  :commands lsp)

;; optionally
(use-package! lsp-ui
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package! org-pomodoro)

(use-package! ein
  :config
  (map! :map ein:notebook-mode-map "<S-return>" 'ein:worksheet-execute-cell-and-goto-next-km)
  ;;TODO make below thing work
  (map! :map ein:notebook-mode-map "<C-return>" 'ein:worksheet-execute-cell-km)
  (map! :map ein:notebook-mode-map :nm "<down>" 'ein:worksheet-goto-next-input-km)
  (map! :map ein:notebook-mode-map :nm "<down>" 'ein:worksheet-goto-next-input-km)
  (map! :map ein:notebook-mode-map :nm "<up>" 'ein:worksheet-goto-prev-input-km))

(use-package! evil-extra-operator
  :init
  (map! :m "gz" 'evil-operator-google-search))

(use-package! rtags
  :config
  (setq rtags-tramp-enabled t))
