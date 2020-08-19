;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ketan Agrawal"
      user-mail-address "agrawalk@stanford.edu")

(defvar ketan0/dotfiles-dir (file-name-as-directory "~/.dotfiles")
  "Personal dotfiles directory.")
(defvar ketan0/fold-state nil
  "HACK: keep track of whether everything in the buffer is folded")
(defun ketan0/fold-toggle-all ()
  (interactive)
  (if ketan0/fold-state (+fold/open-all) (+fold/close-all))
  (setq-local ketan0/fold-state (not ketan0/fold-state)))
;; (defvar ketan0/tramp-prefix
;;   (if (string= (shell-command-to-string "hostname") "ketanmba.local\n")
;;       "" "/ssh:ketanmba:")
;;   "append tramp prefix if on remote machine")
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
(setq doom-theme 'doom-dark+)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

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

(setq auto-revert-remote-files t)
(setq global-auto-revert-mode t)

;; from https://www.emacswiki.org/emacs/RevertBuffer
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )
(map! :map evil-motion-state-map "gb" 'revert-all-buffers)

(setq display-line-numbers-type t)

(setq max-specpdl-size (* 30 1600))
(setq max-lisp-eval-depth (* 60 800))

(map! :map prog-mode-map :nm "<tab>" '+fold/toggle)

(map! :map prog-mode-map :nm "<S-tab>" 'ketan0/fold-toggle-all)

(map! :map evil-motion-state-map "gj" 'evil-next-line)
(map! :map evil-motion-state-map "gk" 'evil-previous-line)

(map! :map evil-motion-state-map "k" 'evil-previous-visual-line)

(map! :map evil-motion-state-map "j" 'evil-next-visual-line)
(map! :map evil-motion-state-map "k" 'evil-previous-visual-line)
(map! :map evil-visual-state-map "j" 'evil-next-visual-line)
(map! :map evil-visual-state-map "k" 'evil-previous-visual-line)

(map! :map evil-normal-state-map "Q" (kbd "@q"))

(use-package! company-math)


(defun ketan0/org-mode-setup ()
  ;; (message "Running org-mode hook!")
  (setq-local company-backends
              (push '(company-math-symbols-unicode company-org-roam)
                    company-backends)))

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-ellipsis "…")
  (setq org-directory  "~/Sync/org/")
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
  (setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))
  ;;Organ (my app)'s store
  (setq org-mobile-directory "~/Library/Mobile Documents/iCloud\~com\~appsonthemove\~beorg/Documents/org/")

  (setq org-mobile-checksum-files "~/Library/Mobile Documents/iCloud\~com\~appsonthemove\~beorg/Documents/org/checksums.dat")
  ;;settings for TODOs
  (setq org-log-done 'time) ;;record time a task is done
  (setq org-log-into-drawer t)
  (setq org-treat-insert-todo-heading-as-state-change t)

  (setq org-habit-show-all-today t)


  (setq org-default-notes-file (concat org-directory "capture.org"))
  (setq org-agenda-files `(,org-default-notes-file
                           ,(concat org-directory "todos.org")))

  (setq org-agenda-span 'day)
  (setq org-agenda-start-day "+0d")
  (add-hook 'org-agenda-mode-hook #'doom-mark-buffer-as-real-h)

  (defun ketan0/create-gtd-project-block (project-name)
    `(org-ql-block '(and (todo "STRT")
                         (path "todos.org")
                         (ancestors ,project-name))
                   ((org-ql-block-header ,project-name))))
  ;; initial inspiration for custom agenda https://github.com/jethrokuan/.emacs.d/blob/master/init.el
  (setq ketan0/org-agenda-todo-view
        `(" " "Ketan's Custom Agenda"
          ((agenda ""
                   ((org-agenda-span 'day)
                    (org-deadline-warning-days 10)))
           (org-ql-block '(and (todo "DONE")
                               (path "todos.org")
                               (closed :on today))
                         ((org-ql-block-header "Finished Today")))
           ;; my definition of a 'stuck' project:
           ;; todo state PROJ, has TODOs within, but no next (STRT) actions
           (org-ql-block '(and (todo "PROJ")
                               (not (done))
                               (descendants (todo "TODO"))
                               (not (descendants (todo "STRT")))
                               (not (descendants (scheduled))))
                         ((org-ql-block-header "Stuck Projects")))
           (org-ql-block '(path "capture.org")
                         ((org-ql-block-header "To Refile")))
           ,(ketan0/create-gtd-project-block "Amazon")
           ,(ketan0/create-gtd-project-block "Langcog")
           ,(ketan0/create-gtd-project-block "PAC")
           ,(ketan0/create-gtd-project-block "Emacs")
           ,(ketan0/create-gtd-project-block "Neo4j backend")
           ,(ketan0/create-gtd-project-block "Knowledge graph")
           ,(ketan0/create-gtd-project-block "GTD")
           ,(ketan0/create-gtd-project-block "Shortcuts")
           ,(ketan0/create-gtd-project-block "Academics")
           ,(ketan0/create-gtd-project-block "Grad school")
           ,(ketan0/create-gtd-project-block "Misc")
           nil)))
  (setq ketan0/org-agenda-amzn-view
        `("z" "Amazon TODO"
          (,(ketan0/create-gtd-project-block "General/Management")
           ,(ketan0/create-gtd-project-block "Login / Access Control")
           ,(ketan0/create-gtd-project-block "Add Page - Add DSN")
           ,(ketan0/create-gtd-project-block "Admin Verification Page - New DSN")
           ,(ketan0/create-gtd-project-block "View/Modify/Delete Page - View status of requests")
           ,(ketan0/create-gtd-project-block "JSON Edits Page")
           nil)))
  (setq org-agenda-custom-commands `(,ketan0/org-agenda-todo-view ,ketan0/org-agenda-amzn-view))

  ;;TODO: why isn't this going into evil mode
  (defun ketan0/gtd-daily-review ()
    (interactive)
    (org-ql-search (cons (concat org-directory "archive.org") (org-agenda-files))
      '(and (ts :from -14 :to today) (done))
      :title "Recent Items"
      :sort '(date priority todo)
      :super-groups '((:auto-ts t)))
    (end-of-buffer))
  ;;thanks jethro
  (defun ketan0/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  (defun ketan0/switch-to-amazon-na ()
    (interactive)
    (org-agenda nil "z"))
  (map! "<f5>" #'ketan0/gtd-daily-review)
  (map! "<f4>" #'ketan0/switch-to-agenda)
  (map! "<f6>" #'ketan0/switch-to-amazon-na)

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
  (setq ketan0/org-files (file-expand-wildcards (concat org-directory "*org")))
  (setq org-refile-targets '((ketan0/org-files :maxlevel . 3)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)


  (setq org-refile-use-outline-path 'file) ;;see whole path (not just headline)
  (setq org-outline-path-complete-in-steps nil) ;;easy to complete in one go w/ helm

  (setq org-archive-location (concat org-directory "archive.org::datetree/")) ;;archive done tasks to datetree in archive.org

  (setq org-catch-invisible-edits (quote show-and-error)) ;;avoid accidental edits in folded areas, links, etc.

  (defun my/org-checkbox-todo ()
    "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
        (save-excursion
          (org-back-to-heading t)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (goto-char beg)
          (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                                 end t)
              (if (match-end 1)
                  (if (equal (match-string 1) "100%")
                      (unless (string-equal todo-state "DONE")
                        (org-todo 'done))
                    (unless (string-equal todo-state "TODO")
                      (org-todo 'todo)))
                (if (and (> (match-end 2) (match-beginning 2))
                         (equal (match-string 2) (match-string 3)))
                    (unless (string-equal todo-state "DONE")
                      (org-todo 'done))
                  (unless (string-equal todo-state "TODO")
                    (org-todo 'todo)))))))))
  (add-hook 'org-checkbox-statistics-hook 'my/org-checkbox-todo)

  (org-set-modules 'org-modules '(ol-bibtex org-habit))

  (setq org-capture-templates
        `(;; other entries
          ("t" "todo" entry
           (file org-default-notes-file)
           "* TODO %?")
          ("s" "strt" entry
           (file org-default-notes-file)
           "* STRT %?")
          ("d" "done" entry
           (file org-default-notes-file)
           "* DONE %?\nCLOSED: %U") ;;TODO: put CLOSED + timestamp
          ("i" "idea" entry
           (file ,(concat org-directory "ideas.org"))
           "* %?") ;;TODO: put CLOSED + timestamp
          ("c" "coronavirus" entry (file+datetree
                                    ,(concat org-directory "20200314210447_coronavirus.org"))
           "* %^{Heading}")
          ("w" "Review: Weekly Review" entry (file+datetree ,(concat org-directory "reviews.org"))
           (file ,(concat org-directory "20200816223343-weekly_review.org")))))

  (add-hook 'after-save-hook 'org-save-all-org-buffers))

(use-package! org-super-agenda)
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
        org-journal-dir org-directory
        org-journal-carryover-items nil
        org-journal-file-format "%Y%m%d.org"
        org-journal-date-format "%A, %d %B %Y"))

(use-package! org-ql)
(use-package! org-ml)
(use-package! json-pointer)

(use-package! org-roam
  :init
  (map! :map doom-leader-map "r" 'org-roam-find-file)
  (org-roam-mode)
  :diminish org-roam-mode
  :config
  (setq org-roam-graphviz-executable "/usr/local/bin/dot")
  (setq org-roam-graph-viewer nil)
  (setq org-roam-directory org-directory))

(setq ketan0/org-thoughtset-package-path "/Users/ketanagrawal/emacs-packages/org-thoughtset")

(use-package! org-thoughtset
  :load-path ketan0/org-thoughtset-package-path
  :config
  (add-hook 'org-cycle-hook 'org-thoughtset-grab-headline-children)
  ;; (remove-hook 'org-cycle-hook 'org-thoughtset-grab-headline-children)
  )


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


;; shamelessly taken from magnars
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(use-package! request)
(use-package! dash)
(use-package! s)
(use-package! f)

(use-package! multifiles
  :init
  (map! :map doom-leader-map "e" 'mf/mirror-region-in-multifile))

(use-package! evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package! tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  ;; (remove-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))


(require 'web-mode)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
