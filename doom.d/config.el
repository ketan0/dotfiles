;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ketan Agrawal"
      user-mail-address "ketanjayagrawal@gmail.com")

(defvar ketan0/dotfiles-dir (file-name-as-directory "~/.dotfiles")
  "Personal dotfiles directory.")
(defvar ketan0/goodnotes-dir (file-name-as-directory "~/Dropbox/Apps/GoodNotes 5/GoodNotes")
  "Dropbox-synced iPad notes from Goodnotes")
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
(when (boundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

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

;; don't need the /g on the end of :s/old/new
(setq evil-ex-substitute-global t)

(map! :map evil-motion-state-map "gb" 'revert-all-buffers)

(map! :map prog-mode-map :nm "<tab>" '+fold/toggle)

(map! :map prog-mode-map :nm "<S-tab>" 'ketan0/fold-toggle-all)

(map! :map emacs-lisp-mode-map :nm "<return>" 'helpful-at-point)

(map! :map evil-motion-state-map "gj" 'evil-next-line)
(map! :map evil-motion-state-map "gk" 'evil-previous-line)

(map! :map evil-motion-state-map "j" 'evil-next-visual-line)
(map! :map evil-motion-state-map "k" 'evil-previous-visual-line)
(map! :map evil-visual-state-map "j" 'evil-next-visual-line)
(map! :map evil-visual-state-map "k" 'evil-previous-visual-line)

(map! :map evil-normal-state-map "Q" (kbd "@q"))

(define-key evil-visual-state-map "." 'evil-a-paren)

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-directory  "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
  :config
  (setq org-babel-default-header-args
        (cons '(:exports . "both") ;; export code and results by default
        (cons '(:eval . "no-export") ;; don't evaluate src blocks when exporting
              (assq-delete-all :exports
              (assq-delete-all :eval org-babel-default-header-args)))))
  (setq org-ellipsis "…")
  (setq org-hide-emphasis-markers t)

  ;; org HTML export settings
  (setq org-html-htmlize-output-type 'css)
  ;; taken from https://gitlab.com/ngm/commonplace/-/blob/master/publish.el
  (defun ketan0/org-roam--backlinks-list (file)
    (message "Processing file: %s" file)
    ;; (message "(org-roam--org-roam-file-p %s): %s" file (org-roam--org-roam-file-p file))
    ;; (message "(org-roam--org-roam-file-p):" (org-roam--org-roam-file-p))
    (if (org-roam--org-roam-file-p file)
        ;; (org-roam-buffer--insert-backlinks)
        (--reduce-from
         (let ((note-title (org-roam-db--get-title (car it))))
           (concat acc (if (or (string= note-title "sitemap") ;; exclude from backlinks
                               (string= note-title "Hello"))
                           ""
                         (format "- [[file:%s][%s]]\n"
                                 (file-relative-name (car it) org-roam-directory)
                                 note-title))))
         "" (org-roam-db-query [:select [source] :from links :where (= dest $s1)] file))
      (progn (message "it's not a file!") "")))

  (defun ketan0/org-export-preprocessor (backend)
    ;; (message "buffer-file-name is \"%s\"" buffer-file-name)
    (let ((links (ketan0/org-roam--backlinks-list (buffer-file-name))))
      (message "links is \"%s\"" links)
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "
* Links to this note
:PROPERTIES:
:CUSTOM_ID: backlinks
:END:
") links)))))

  (add-hook 'org-export-before-processing-hook 'ketan0/org-export-preprocessor)

  (setq org-publish-project-alist
        '(("digital laboratory"
           :base-directory "~/garden-simple/org"
           :publishing-function org-html-publish-to-html
           :publishing-directory "~/garden-simple/html"
           :auto-sitemap t
           :sitemap-title "sitemap"
           ;; :html-head-include-default-style nil
           :section-numbers nil
           :with-toc nil
           :preserve-breaks t
           :html-preamble t
           :html-preamble-format (("en" "<a style=\"color: inherit; text-decoration: none\" href=\"/\"><h2>Ketan's Digital Laboratory &#129514;</h2></a>"))
           :html-postamble t

           :html-postamble-format (("en" "<p>Made with <span class=\"heart\">♥</span> using <a href=\"https://orgmode.org/\">org-mode</a>. Source code is available <a href=\"https://github.com/ketan0/digital-laboratory\">here</a>.</p>
<script src=\"https://unpkg.com/axios/dist/axios.min.js\"></script>
<script src=\"https://unpkg.com/@popperjs/core@2\"></script>
<script src=\"https://unpkg.com/tippy.js@6\"></script>
<script src=\"tooltips.js\"></script>"))
           :html-link-home ""
           :html-link-up ""
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"syntax.css\" />
<link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\" />"
           :html-head-extra "<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"/apple-touch-icon.png\" />
<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"/favicon-32x32.png\" />
<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"/favicon-16x16.png\" />
<link rel=\"manifest\" href=\"/site.webmanifest\" />")))

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
  (setq org-mobile-directory org-directory)
  (setq org-mobile-checksum-files (concat org-directory "checksums.dat"))
  (setq org-log-done 'time) ;;record time a task is done
  (setq org-log-into-drawer t)
  (setq org-treat-insert-todo-heading-as-state-change t)
  (setq org-habit-show-all-today t)
  (setq org-default-notes-file (concat org-directory "capture.org"))
  (setq ketan0/org-todos-file (concat org-directory "todos.org"))
  (setq org-agenda-files (list org-default-notes-file ketan0/org-todos-file))
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day "+0d")
  (add-hook 'org-mode-hook #'org-fragtog-mode)

  (defvar org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")
  (defun org-set-created-property (&optional active NAME)
    "Set a property on the entry giving the creation time.
By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified."
    (interactive)
    (let* ((created (or NAME org-created-property-name))
           (fmt (if active "<%s>" "[%s]"))
           (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
      (unless (org-entry-get (point) created nil)
        (org-set-property created now)
        )))
  (add-hook 'org-journal-after-entry-create-hook #'org-set-created-property)

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
                              (org-deadline-warning-days 2)
                              (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                          'nottodo '("TODO")))
                             ))
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
                     (org-ql-block '(and (ts :to 0) (todo "STRT"))
                                   ((org-ql-block-header "Top Priority")))
                     )
                   ;; (mapcar 'ketan0/create-gtd-project-block
                   ;;         '("projects" "academic" "knowledge" "research"))
                   ;;
                   nil)))
  (setq org-agenda-custom-commands (list ketan0/main-agenda))

  (defun ketan0/switch-to-main-agenda ()
    (interactive)
    (org-agenda nil " "))
  ;; Create an agenda view for each PARA "area"
  (defun ketan0/area-agenda (area-tag)
    (org-ql-search
      org-agenda-files
      `(and (tags ,area-tag)
            (ancestors (todo "PROJ"))
            (todo "STRT"))
      :super-groups (list '(:auto-outline-path))
      :sort 'priority
      :title (format "%s agenda" area-tag)))

  (map! "<f4>" #'ketan0/switch-to-main-agenda
        "<f5> p" (lambda () (interactive) (ketan0/area-agenda "projects"))
        "<f5> a" (lambda () (interactive) (ketan0/area-agenda "academic"))
        "<f5> k" (lambda () (interactive) (ketan0/area-agenda "knowledge"))
        "<f5> t" (lambda () (interactive) (ketan0/area-agenda "tinker"))
        "<f5> r" (lambda () (interactive) (ketan0/area-agenda "research"))
        "<f5> b" (lambda () (interactive) (ketan0/area-agenda "body"))
        "<f5> g" (lambda () (interactive) (ketan0/area-agenda "process"))
        "<f5> u" (lambda () (interactive) (ketan0/area-agenda "utility"))
        "<f5> w" (lambda () (interactive) (ketan0/area-agenda "writing"))
        "<f5> s" (lambda () (interactive) (ketan0/area-agenda "social")))

  ;; review completed tasks in the last week
  (defun ketan0/weekly-review ()
    (interactive)
    (org-ql-search (cons (concat org-directory "archive.org") (org-agenda-files))
      '(and (ts :from -7 :to today) (done))
      :title "Recent Items"
      :sort '(date priority todo)
      :super-groups '((:auto-ts t)))
    (goto-char (point-max)))
  (map! "<f6>" #'ketan0/weekly-review)

  (defun ketan0/look-ahead ()
    (interactive)
    (let ((days-ahead (read-number "How many days to look ahead: " 7)))
      (org-ql-search (cons (concat org-directory "archive.org") (org-agenda-files))
        `(and (ts :from today :to ,days-ahead) (not (todo "DONE")))
        :title "Week Ahead"
        :sort '(date priority todo)
        :super-groups '((:auto-ts t)))))
  (map! "<f3>" #'ketan0/look-ahead)

  (map! :map doom-leader-map "a" 'counsel-org-goto-all)

  (setq org-agenda-block-separator nil)
  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-window-setup 'current-window) ;;agenda take up whole frame
  ;;don't show warnings for deadlines
  (setq org-deadline-warning-days 0) ;;don't show upcoming tasks in today view
  (setq org-edit-src-content-indentation 0) ;;don't indent src blocks further

  ;;refile headlines to any other agenda files
  (setq org-refile-use-cache t) ;;speeds up loading refile targets
  (setq ketan0/org-files (file-expand-wildcards (concat org-directory "*.org")))
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

  (plist-put org-format-latex-options :scale 1.0) ;; scale for inline latex fragments

  (setq org-structure-template-alist
        '(("p" . "src python :results output\n")
          ("b" . "src bash\n")
          ("a" . "export ascii\n")
          ("c" . "center\n")
          ("C" . "comment\n")
          ("e" . "example\n")
          ("E" . "export\n")
          ("h" . "export html\n")
          ("l" . "export latex\n")
          ("q" . "quote\n")
          ("s" . "src ") ;; no newline so we can enter the programming language
          ("v" . "verse\n")))

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

  (setq +org-capture-frame-parameters
    `((name . "doom-capture")
      (width . (text-pixels . 720))
      (height . (text-pixels . 180))
      (left . 360)
      (top . 360)
      (transient . t)
      ,(when (and IS-LINUX (not (getenv "DISPLAY")))
         `(display . ":0"))
      ,(if IS-MAC '(menu-bar-lines . 1))))

  (setq org-capture-templates
        `(("t" "todo" entry
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
          ("c" "coronavirus" entry
           (file+olpdatetree
            ,(concat org-directory "20200314210447_coronavirus.org"))
           "* %^{Heading}")
          ("H" "HCI lab notebook" entry
           (file+olp+datetree
            ,(concat org-directory "20210401093501-hci_counterfactual_reasoning.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("E" "EConsults lab notebook" entry
           (file+olp+datetree
            ,(concat org-directory "20210714164344-econsults_prediction.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("T" "org-twitter" entry
           (file+olp+datetree
            ,(concat org-directory "20201013012647-org_twitter.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("G" "raspi-glove" entry
           (file+olp+datetree
            ,(concat org-directory "20201113182201-raspberry_pi_glove.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("D" "Digital Garden" entry
           (file+olp+datetree
            ,(concat org-directory "20201111010429-digital_laboratory.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("P" "PAC lab notebook" entry
           (file+olp+datetree
            ,(concat org-directory "20200313153429_pac.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("o" "Org-spotify lab notebook" entry
           (file+olp+datetree
            ,(concat org-directory "20201006205609-org_spotify.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t)
          ("w" "Review: Weekly Review" entry
           (file+olp+datetree
            ,(concat org-directory "reviews.org"))
           (file ,(concat org-directory "20200816223343-weekly_review.org"))
           :jump-to-captured t)
          ("p" "Protocol" entry
           (file ,org-default-notes-file)
           "* STRT [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n"
           :immediate-finish t)
          ("L" "Protocol Link" entry
           (file ,org-default-notes-file)
           "* STRT [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \n\n"
           :immediate-finish t)))
  (defadvice! open-org-capture-in-current-window (oldfun &rest args)
    :around #'org-protocol-capture
    (let (display-buffer-alist)
      (apply oldfun args))))




;; drag-n-drop images
(after! org-download
  (setq org-download-method 'attach)
  (setq org-download-backend "curl \"%s\" -o \"%s\""))

;; automatically publish org file upon save
(defun ketan0/org-html-export-after-save ()
  "Function for `after-save-hook' to run `org-hugo-export-wim-to-md'.
The exporting happens only when Org Capture is not in progress."
  (save-excursion (org-publish-current-file)))

(define-minor-mode ketan0/org-html-auto-export-mode
  "Toggle auto exporting the Org file using `ox-html'."
  :global nil
  :lighter ""
  (if ketan0/org-html-auto-export-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'ketan0/org-html-export-after-save :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'ketan0/org-html-export-after-save :local)))
(provide 'ketan0/org-html-auto-export-mode)

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

(use-package! org-ql
  :after org
  :config
  ;; (defun ketan0/next-actions ()
  ;;   (interactive)
  ;;   (org-ql-search
  ;;     org-agenda-files
  ;;     '(or (and (path "capture.org") (parent))
  ;;          (and (todo "STRT")
  ;;               (path "todos.org")
  ;;               (ancestors (todo "PROJ"))))
  ;;     :super-groups (list
  ;;                    '(:auto-outline-path)
  ;;                    '(:name "Today"  ; Optionally specify section name
  ;;                      :time-grid t
  ;;                      )  ; Items that have this TODO keyword
  ;;                    )
  ;;     :sort 'priority
  ;;     :title "Next Actions"))
  ;; (map! "<f4>" #'ketan0/next-actions)
  ;; display org-ql-search/view in the same window
  (setq org-ql-view-display-buffer-action (cons #'display-buffer-same-window nil)))

(use-package! org-super-agenda
  :after org
  :config
  (require 'evil-org-agenda)
  ;; the headers in org-super-agenda  should inherit evil-org-agenda keybinds, not standard org-agenda keybinds.
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)))

(use-package! org-roam
  :init
  (map! :leader :nm
        "r r" #'org-roam-find-file
        "r i" #'org-roam-insert-immediate
        "r l" #'org-roam-insert
        "r t" '(lambda () (interactive) (find-file (concat org-directory "todos.org")))
        "r v" '(lambda () (interactive) (find-file (concat ketan0/goodnotes-dir "vision.pdf")))
        "r u" #'org-roam-unlinked-references
        "r b" #'org-roam-buffer-activate
        "r d" #'org-roam-buffer-deactivate)
  :after org
  ;; :init
  ;; :hook
  ;; (after-init . org-roam-mode)
  :config
  (require 'org-roam-protocol)

  (setq org-roam-graphviz-executable "/usr/local/bin/dot")
  (setq org-roam-graph-viewer nil)
  (setq org-roam-directory org-directory))

(use-package! apples-mode
  :init (add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . apples-mode)))
(use-package! ob-applescript)

(defun ketan0/tangle-karabiner ()
  "If the current buffer is 'karabiner.org' the code-blocks are
   tangled, and the tangled file karabiner.edn is compiled with Goku."
  (when (equal (buffer-file-name)
               (expand-file-name (concat ketan0/dotfiles-dir "karabiner.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle-file
       (expand-file-name (concat ketan0/dotfiles-dir "karabiner.org"))
       (expand-file-name (concat ketan0/dotfiles-dir "karabiner.edn"))))
    (message (concat "Goku output: " (shell-command-to-string "goku")))))
(add-hook 'after-save-hook 'ketan0/tangle-karabiner)

(defun ketan0/source-yabairc ()
  "If the current buffer is 'yabairc' then yabai is relaunched."
  (when (equal (buffer-file-name)
               (expand-file-name (concat ketan0/dotfiles-dir "yabairc")))
    ;; Avoid running hooks when tangling.
    (message (concat "Sourcing yabairc..."
                     (shell-command-to-string
                      "launchctl kickstart -k \"gui/${UID}/homebrew.mxcl.yabai\"")))))
(add-hook 'after-save-hook 'ketan0/source-yabairc)

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

(use-package! evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; (use-package! tide
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save))
;;   :config
;;   (map! :localleader
;;         :map tide-mode-map
;;         "k" #'flycheck-previous-error
;;         "j" #'flycheck-next-error
;;         "x" #'tide-fix)

;;   (setq tide-format-options
;;         '(:indentSize 2
;;           :tabSize 2
;;           :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces nil
;;           :placeOpenBraceOnNewLineForFunctions nil
;;           :placeOpenBraceOnNewLineForControlBlocks nil))

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
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)

;;   (with-eval-after-load 'web-mode
;;     (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;     (add-hook 'web-mode-hook
;;               (lambda ()
;;                 (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                   (setup-tide-mode)))))
;;   ;; enable typescript-tslint checker
;;   (with-eval-after-load 'flycheck
;;     (flycheck-add-mode 'typescript-tslint 'web-mode)))


;; ;; (require 'web-mode)
;; ;; (defun my-web-mode-hook ()
;; ;;   "Hooks for Web mode."
;; ;;   (setq web-mode-markup-indent-offset 2))
;; ;; (add-hook 'web-mode-hook  'my-web-mode-hook)
;; ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; ;; (add-hook 'web-mode-hook
;; ;;           (lambda ()
;; ;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;; ;;               (setup-tide-mode))))

;; ;; enable typescript-tslint checker
;; ;; (flycheck-add-mode 'typescript-tslint 'web-mode)

(use-package! evil-snipe
  :config
  ;; don't use s/S to repeat (sometimes I like to mess around and do multiple searches)
  (setq evil-snipe-repeat-keys nil)
  ;; search the whole buffer, not just the line
  (setq evil-snipe-scope 'buffer))

;; ;; secret stuff that I'm not publishing on github
;; (load-file "~/.doom.d/ketan0-secrets.el")

;; ;; (use-package! counsel-spotify
;; ;;   :config
;; ;;   (setq counsel-spotify-client-id ketan0-secrets/spotify-client-id)
;; ;;   (setq counsel-spotify-client-secret ketan0-secrets/spotify-client-secret))

;; ;; (use-package! spotify
;; ;;   :config
;; ;;   (setq spotify-oauth2-client-id ketan0-secrets/spotify-client-id)
;; ;;   (setq spotify-oauth2-client-secret ketan0-secrets/spotify-client-secret))

;; ;; (use-package! elfeed
;; ;;   :config
;; ;;   (setq-default elfeed-search-filter "@emacs")
;; ;;   (map! :leader :nm
;; ;;         "o e" #'elfeed))

;; ;; (use-package! elfeed-org
;; ;;   :config
;; ;;   (elfeed-org)
;; ;;   (setq rmh-elfeed-org-files (list (concat org-directory "elfeed.org"))))

;; ;; (setq ketan0/org-twitter-package-path "/Users/ketanagrawal/emacs-packages/org-twitter")
;; ;; (use-package! org-twitter
;; ;;   :load-path ketan0/org-twitter-package-path
;; ;;   :config
;; ;;   ;; TODO: is this right?? idk if this is right
;; ;;   (map! :map org-mode-map
;; ;;         :leader
;; ;;         :desc "tweet headline" "t h" #'org-twitter-tweet-this-headline
;; ;;         :desc "tweet selection" "t v" #'org-twitter-tweet-selection
;; ;;         :desc "tweet subheadlines" "t t" #'org-twitter-tweet-subheadlines-as-thread)
;; ;;   (with-eval-after-load 'org-capture
;; ;;     (add-to-list 'org-capture-templates
;; ;;                  `("S" "tweet" entry ;; for use with org-twitter
;; ;;                    (file ,(concat org-directory "20200406054034-twitter.org"))
;; ;;                    "* %?"))
;; ;;     (defun ketan0/org-twitter-finalize ()
;; ;;       (let ((key  (plist-get org-capture-plist :key)))
;; ;;         (when (and (string= key "S") (not org-note-abort))
;; ;;           (aio-wait-for (call-interactively 'org-twitter-tweet-this-headline)))))
;; ;;     (add-hook 'org-capture-prepare-finalize-hook 'ketan0/org-twitter-finalize)))

;; ;; (setq ketan0/org-spotify-package-path "/Users/ketanagrawal/emacs-packages/org-spotify")
;; ;; (use-package! org-spotify
;; ;;   :load-path ketan0/org-spotify-package-path
;; ;;   :config
;; ;;   (setq org-spotify-oauth2-client-id ketan0-secrets/spotify-client-id)
;; ;;   (setq org-spotify-oauth2-client-secret ketan0-secrets/spotify-client-secret))

;; ;; (use-package! gif-screencast
;; ;;   :config
;; ;;   ;; (map! "<f3>" 'gif-screencast-start-or-stop)
;; ;;   (setq gif-screencast-args '("-v" "-x"))
;; ;;   (setq gif-screencast-cropping-program "mogrify")
;; ;;   (setq gif-screencast-capture-format "ppm"))

;; (use-package! conda
;;   :config
;;   (setq conda-anaconda-home "/Users/ketanagrawal/miniconda3")
;;   (setq conda-env-home-directory "/Users/ketanagrawal/miniconda3/"))

;; (defun ketan0/parse-csv-file (file sep)
;;   "parse FILE representing a CSV table into a list of lists."
;;   (interactive
;;    (list (read-file-name "CSV file: ")))
;;   (let ((buf (find-file-noselect file))
;;         (result nil))
;;     (with-current-buffer buf
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (let ((line (buffer-substring-no-properties
;;                      (line-beginning-position) (line-end-position))))
;;           (push (split-string line sep) result))
;;         (forward-line 1)))
;;     (reverse result)))

;; (defvar ketan0/chrome-history-tsv-file "~/Downloads/history_autobackup_20201108_full.tsv"
;;   "Location of the TSV exported by History Trends Unlimited Chrome extension"
;;   )

;; (defun ketan0/insert-chrome-history-org-link ()
;;   (interactive)
;;   ;; TODO: caching
;;   (-let ((history-items
;;           (->> (-slice (ketan0/parse-csv-file ketan0/chrome-history-tsv-file "\t") 1)
;;                (seq-sort-by #'(lambda (x) (string-to-number (substring (nth 1 x) 1))) #'>)
;;                (-select-columns '(3 0)))))
;;     (ivy-read "Chrome History: " history-items
;;               :action (lambda (x) (interactive) (insert (format "[[%s][%s]]" (cadr x) (car x))))
;;               :re-builder '+ivy-prescient-non-fuzzy
;;               :sort nil)))
