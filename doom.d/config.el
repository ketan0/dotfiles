;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ketan Agrawal"
      user-mail-address "agrawalk@stanford.edu")
(add-hook 'eww-mode-hook 'visual-line-mode)
(require 'mailcap)
(add-to-list 'mailcap-user-mime-data
               '((type . "application/pdf")
                 (viewer . pdf-view-mode)))
(add-to-list 'mailcap-user-mime-data
               '((type . "application/markdown")
                 (viewer . markdown-mode)))
(setq browse-url-browser-function 'eww-browse-url)

;; BEGIN eww syntax highlighting
(require 'cl-lib)
(defun eww-tag-pre (dom)
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (eww-fontify-pre dom))
    (shr-ensure-newline)))

(defun eww-fontify-pre (dom)
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (eww-buffer-auto-detect-mode)))
      (when mode
        (eww-fontify-buffer mode)))
    (buffer-string)))

(defun eww-fontify-buffer (mode)
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun eww-buffer-auto-detect-mode ()
  (let* ((map '((ada ada-mode)
                (awk awk-mode)
                (c c-mode)
                (cpp c++-mode)
                (clojure clojure-mode lisp-mode)
                (csharp csharp-mode java-mode)
                (css css-mode)
                (dart dart-mode)
                (delphi delphi-mode)
                (emacslisp emacs-lisp-mode)
                (erlang erlang-mode)
                (fortran fortran-mode)
                (fsharp fsharp-mode)
                (go go-mode)
                (groovy groovy-mode)
                (haskell haskell-mode)
                (html html-mode)
                (java java-mode)
                (javascript javascript-mode)
                (json json-mode javascript-mode)
                (latex latex-mode)
                (lisp lisp-mode)
                (lua lua-mode)
                (matlab matlab-mode octave-mode)
                (objc objc-mode c-mode)
                (perl perl-mode)
                (php php-mode)
                (prolog prolog-mode)
                (python python-mode)
                (r r-mode)
                (ruby ruby-mode)
                (rust rust-mode)
                (scala scala-mode)
                (shell shell-script-mode)
                (smalltalk smalltalk-mode)
                (sql sql-mode)
                (swift swift-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes (cdr (assoc language map)))
         (mode (cl-loop for mode in modes
                        when (fboundp mode)
                        return mode)))
    (message (format "%s" language))
    (when (fboundp mode)
      mode)))

(setq shr-external-rendering-functions
      '((pre . eww-tag-pre)))
;; END eww syntax highlighting

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
(mac-auto-operator-composition-mode)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord-light)

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

(map! :map evil-motion-state-map "j" 'evil-next-visual-line)
(map! :map evil-motion-state-map "k" 'evil-previous-visual-line)
(map! :map evil-visual-state-map "j" 'evil-next-visual-line)
(map! :map evil-visual-state-map "k" 'evil-previous-visual-line)

(map! :map evil-normal-state-map "Q" (kbd "@q"))

(use-package! company-math)


;; (defun ketan0/org-mode-setup ()
;;   ;; (message "Running org-mode hook!")
;;   (setq-local company-backends
;;               (push '(company-math-symbols-unicode company-org-roam)
;;                     company-backends)))

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-directory  "~/Sync/org/")
  :config
  (setq org-ellipsis "…")
  (setq org-return-follows-link t)
  ;; (setq org-emphasis-alist ;;different ways to emphasize text
  ;;       '(("!"  (:foreground "red") )
  ;;         ("*" (bold :foreground "Orange" ))
  ;;         ("/" italic "<i>" "</i>")
  ;;         ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
  ;;         ("-" (:overline t) "<span style=\"text-decoration:overline;\">" "</span>")
  ;;         ("~" org-code "<code>" "</code>" verbatim)
  ;;         ("=" org-verbatim "<code>" "</code>" verbatim)
  ;;         ("+" (:strike-through t) "<del>" "</del>")))
  (setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))
  (setq org-mobile-directory "~/Library/Mobile Documents/iCloud\~com\~appsonthemove\~beorg/Documents/org/")
  (setq org-mobile-checksum-files "~/Library/Mobile Documents/iCloud\~com\~appsonthemove\~beorg/Documents/org/checksums.dat")
  (setq org-log-done 'time) ;;record time a task is done
  (setq org-log-into-drawer t)
  (setq org-treat-insert-todo-heading-as-state-change t)
  (setq org-habit-show-all-today t)
  (setq org-default-notes-file (concat org-directory "capture.org"))
  (setq ketan0/org-todos-file (concat org-directory "todos.org"))
  (setq org-agenda-files `(,org-default-notes-file ,ketan0/org-todos-file))
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day "+0d")
  (add-hook 'org-agenda-mode-hook #'doom-mark-buffer-as-real-h)

  (defun ketan0/create-gtd-project-block (tag-name)
    `(org-ql-block '(and (todo "STRT")
                         (path "todos.org")
                         (tags ,tag-name))
                   ((org-ql-block-header ,tag-name))))
  ;; initial inspiration for custom agenda https://github.com/jethrokuan/.emacs.d/blob/master/init.el
  ;; main projects + stuff scheduled for today + stuff I finished
  (setq ketan0/main-agenda
        `(" " "Ketan's Focused Agenda"
          ,(append '((agenda ""
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
                                   ((org-ql-block-header "To Refile"))))
                   (mapcar 'ketan0/create-gtd-project-block
                           '("academic_zettel" "pac" "100_blocks" "org_spotify")) nil)))
  ;; (setq ketan0/tinkering-agenda
  ;;       `("o" "Ketan's Emacs tinkering Agenda"
  ;;         ,(append (mapcar 'ketan0/create-gtd-project-block '("kg" "emacs" "shortcuts")) nil)))

  ;; Create an agenda view for the PARA "area" represented by the given tag
  (defun ketan0/area-agenda (area-tag)
    (org-ql-search
      org-agenda-files
      `(and (tags ,area-tag)
            (ancestors (todo "PROJ"))
            (todo "STRT"))
      :super-groups (list '(:auto-outline-path))
      :sort 'priority
      :title (format "%s agenda" area-tag)))

  (setq org-agenda-custom-commands (list ketan0/main-agenda))
  ;;TODO: why isn't this going into evil mode
  ;;thanks jethro
  (defun ketan0/switch-to-main-agenda ()
    (interactive)
    (org-agenda nil " "))
  ;; (defun ketan0/switch-to-tinkering-agenda ()
  ;;   (interactive)
  ;;   (org-agenda nil "o"))

  (map! "<f4>" #'ketan0/switch-to-main-agenda)
  (map! "<f5> p" (lambda () (interactive) (ketan0/area-agenda "projects")))
  (map! "<f5> a" (lambda () (interactive) (ketan0/area-agenda "academic")))
  (map! "<f5> k" (lambda () (interactive) (ketan0/area-agenda "knowledge")))
  (map! "<f5> t" (lambda () (interactive) (ketan0/area-agenda "tinker")))
  (map! "<f5> r" (lambda () (interactive) (ketan0/area-agenda "research")))
  (map! "<f5> b" (lambda () (interactive) (ketan0/area-agenda "body")))
  (map! "<f5> g" (lambda () (interactive) (ketan0/area-agenda "process")))
  (map! "<f5> u" (lambda () (interactive) (ketan0/area-agenda "utility")))
  (map! "<f5> w" (lambda () (interactive) (ketan0/area-agenda "writing")))
  (map! "<f5> s" (lambda () (interactive) (ketan0/area-agenda "social")))

  (defun ketan0/gtd-daily-review ()
    (interactive)
    (org-ql-search (cons (concat org-directory "archive.org") (org-agenda-files))
      '(and (ts :from -1 :to today) (done))
      :title "Recent Items"
      :sort '(date priority todo)
      :super-groups '((:auto-ts t)))
    (goto-char (point-max)))
  (map! "<f6>" #'ketan0/gtd-daily-review)

  (map! :map doom-leader-map "a" 'counsel-org-goto-all)

  (setq org-agenda-block-separator nil)
  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-format-date
        (lambda (date)
          (concat "\n"
                  (make-string (/ (window-width) 2) 9472)
                  "\n"
                  (org-agenda-format-date-aligned date))))
  (setq org-agenda-window-setup 'current-window) ;;agenda take up whole frame
  ;;don't show warnings for deadlines
  (setq org-deadline-warning-days 0) ;;don't show upcoming tasks in today view
  (setq org-edit-src-content-indentation 0) ;;don't indent src blocks further

  ;;refile headlines to any other agenda files
  (setq org-refile-use-cache t) ;;speeds up loading refile targets
  (setq ketan0/org-files (file-expand-wildcards (concat org-directory "*org")))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
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

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
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
           (file ,(concat org-directory "20200816223343-weekly_review.org")))
          ("p" "Protocol" entry (file ,org-default-notes-file)
           "* TODO [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n"
           :immediate-finish t)
          ("L" "Protocol Link" entry (file ,org-default-notes-file)
           "* TODO [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \n\n"
           :immediate-finish t)))

  (add-hook 'after-save-hook 'org-save-all-org-buffers))

(use-package! ox-hugo
  :config
  ;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:"
                     "%?\n")          ;Place the cursor here finally
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"                ;`org-capture' binding + h
                   "Hugo post"
                   entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Blog Ideas" heading. It can even be a
                   ;; symlink pointing to the actual location of all-posts.org!
                   (file+olp "~/blog/content-org/blog.org" "Blog Posts")
                   (function org-hugo-new-subtree-post-capture-template)))))

(use-package! org-journal
  :after org
  :init
  (map! :map doom-leader-map "j" 'org-journal-new-entry)
  (setq org-journal-find-file 'find-file
        org-journal-date-prefix "* "
        org-journal-dir org-directory
        org-journal-carryover-items nil
        org-journal-file-format "%Y%m%d.org"
        org-journal-date-format "%A, %d %B %Y"))

(use-package org-ql
  :config
  (defun ketan0/next-actions ()
    (interactive)
    (org-ql-search
      org-agenda-files
      '(or (and (path "capture.org") (parent))
           (and (todo "STRT")
                (path "todos.org")
                (ancestors (todo "PROJ"))))
      :super-groups (list
                     '(:auto-outline-path)
                     '(:name "Today"  ; Optionally specify section name
                      :time-grid t
                      )  ; Items that have this TODO keyword
                     )
      :sort 'priority
      :title "Next Actions"))
  ;; (map! "<f4>" #'ketan0/next-actions)

  ;;display org-ql-search/view in the same window
  (setq org-ql-view-display-buffer-action (cons #'display-buffer-same-window nil)))
  ;;the headers in org-ql views should inherit evil-org-agenda keybinds, not standard org-agenda keybinds.)

(use-package! org-super-agenda
  :config
  (require 'evil-org-agenda)
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)))

(use-package! org-ml)
(use-package! json-pointer)

(use-package! org-roam
  :init
  :hook
  (after-init . org-roam-mode)
  :config
  (map! :leader :nm
        "r r" #'org-roam-find-file
        "r i" #'org-roam-insert-immediate
        "r l" #'org-roam-insert
        "r u" #'org-roam-unlinked-references
        "r b" #'org-roam-buffer-activate
        "r d" #'org-roam-buffer-deactivate)
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

;; (use-package! tex
;;   :config
;;   (setq-default TeX-master nil)
;;   ;; Use pdf-tools to open PDF files
;;   (setq latex-run-command "pdflatex")
;;   (setq TeX-save-query nil)
;;   (setq TeX-auto-save t)
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;         TeX-source-correlate-start-server t)
;;   ;; Update PDF buffers after successful LaTeX runs
;;   (add-hook 'TeX-after-compilation-finished-functions
;;             #'TeX-revert-document-buffer))

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
    (message (concat "Sourcing yabairc..."
                     (shell-command-to-string
                      "launchctl kickstart -k \"gui/${UID}/homebrew.mxcl.yabai\"")))))

(add-hook 'after-save-hook 'tangle-karabiner)
(add-hook 'after-save-hook 'source-yabairc)

;; (use-package! lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "s-l")
;;   :config
;;   (setq lsp-auto-require-clients t)
;;   (setq lsp-auto-configure t)
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-eldoc-hook nil)
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-tramp-connection "clangd-10")
;;                     :major-modes '(c-mode c++-mode)
;;                     :remote? t
;;                     :server-id 'clangd-remote))
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          ;; (c++-mode . lsp)
;;          ;; (c-mode . lsp)
;;          (python-mode . lsp)
;;          (tex-mode . lsp)
;;          (latex-mode . lsp)
;;          )
;;   :commands lsp)

;; optionally
;; (use-package! lsp-ui
;;   :config
;;   (setq lsp-ui-sideline-ignore-duplicate t)
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package! ein
;;   :config
;;   (map! :map ein:notebook-mode-map "<S-return>" 'ein:worksheet-execute-cell-and-goto-next-km)
;;   ;;TODO make below thing work
;;   (map! :map ein:notebook-mode-map "<C-return>" 'ein:worksheet-execute-cell-km)
;;   (map! :map ein:notebook-mode-map :nm "<down>" 'ein:worksheet-goto-next-input-km)
;;   (map! :map ein:notebook-mode-map :nm "<down>" 'ein:worksheet-goto-next-input-km)
;;   (map! :map ein:notebook-mode-map :nm "<up>" 'ein:worksheet-goto-prev-input-km))

(use-package! evil-extra-operator
  :init
  (map! :m "gz" 'evil-operator-google-search))

;; taken from magnars
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
(map! :map evil-window-map "f" 'toggle-window-split)

(use-package! multifiles
  :init
  (map! :map doom-leader-map "e" 'mf/mirror-region-in-multifile))

(use-package! evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; (use-package! tide
;;   :config
;;   (defun setup-tide-mode ()
;;     (interactive)
;;     (tide-setup)
;;     (flycheck-mode +1)
;;     (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1)
;;     ;; company is an optional dependency. You have to
;;     ;; install it separately via package-install
;;     ;; `M-x package-install [ret] company`
;;     (company-mode +1))

;;   ;; aligns annotation to the right hand side
;;   (setq company-tooltip-align-annotations t)

;;   ;; formats the buffer before saving
;;   (add-hook 'before-save-hook 'tide-format-before-save)
;;   ;; (remove-hook 'before-save-hook 'tide-format-before-save)

;;   (add-hook 'typescript-mode-hook #'setup-tide-mode))


;; (require 'web-mode)
;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2))
;; (add-hook 'web-mode-hook  'my-web-mode-hook)
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

(defun org-peek-link-activate-func (start end path bracketp)
  (remove-overlays start end)
  (when (file-exists-p path)
    (let ((overlay (make-overlay start end nil t)))
      (overlay-put overlay 'display
                   (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string)))
      (overlay-put overlay 'before-string (concat "PEEK: " path "\n"))
      (overlay-put overlay 'face 'org-quote)
      (overlay-put overlay 'evaporate t))))

(defun org-peek-link-complete (&optional arg)
  "Create a peek link using completion.
Modified version of `org-file-complete-link'."
  (let ((file (read-file-name "File: "))
        (pwd (file-name-as-directory (expand-file-name ".")))
        (pwd1 (file-name-as-directory (abbreviate-file-name
                                       (expand-file-name ".")))))
    (cond ((equal arg '(16))
           (concat "peek:"
                   (abbreviate-file-name (expand-file-name file))))
          ((string-match
            (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
           (concat "peek:" (match-string 1 file)))
          ((string-match
            (concat "^" (regexp-quote pwd) "\\(.+\\)")
            (expand-file-name file))
           (concat "peek:"
                   (match-string 1 (expand-file-name file))))
          (t (concat "peek:" file)))))

(org-link-set-parameters "peek"
                         :activate-func 'org-peek-link-activate-func
                         :complete 'org-peek-link-complete)

(use-package! evil-snipe
  :config
  ;; don't use s/S to repeat (sometimes I like to mess around and do multiple searches)
  (setq evil-snipe-repeat-keys nil)
  ;; search the whole buffer, not just the line
  (setq evil-snipe-scope 'buffer))

;; see Mathjax fragments in eww
(use-package! texfrag)
