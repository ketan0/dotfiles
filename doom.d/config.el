;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ketan Agrawal"
      user-mail-address "ketanjayagrawal@gmail.com")
;; (setq default-directory "~/")


;; shortcut to go from org file => browser view of generated HTML file
;; (using my open_url_smart Applescript -- see karabiner.org for details)
(defun ketan0/launch-nebula-note-locally-in-browser ()
  (interactive)
  (->> (file-name-base (buffer-file-name))
       (format "osascript ~/.dotfiles/open_url_smart.scpt \
'http://localhost:3000/%s.html'")
       (shell-command)))

(defun ketan0/launch-nebula-note-in-browser ()
  (interactive)
  (->> (file-name-base (buffer-file-name))
       (format "osascript ~/.dotfiles/open_url_smart.scpt \
'http://nebula.ketan.me/%s.html'")
       (shell-command)))

(map! :leader
      (:prefix ("k" . "Ketan custom keybinds")
       :desc "Launch nebula note locally in browser" "l" #'ketan0/launch-nebula-note-locally-in-browser
       :desc "Launch nebula note in browser" "n" #'ketan0/launch-nebula-note-in-browser
       ))

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

(setq doom-font (font-spec :family "Fira Code" :size 14))
(setq doom-font-increment 1)
(setq doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14))

(setq +latex-viewers '(skim))
(setq reftex-default-bibliography "/Users/ketanagrawal/zoterocitations.bib")

;; needed for font ligatures to work (e.g. in Fira Code)
(when (boundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(defun ketan0/dark-mode-active ()
  (string=
   (shell-command-to-string
    "printf %s \"$( osascript -e \'tell application \"System Events\"
tell appearance preferences to return dark mode
end tell\')\"")
   "true"))
(defun ketan0/responsive-theme ()
  (if (ketan0/dark-mode-active) 'doom-spacegrey 'doom-ayu-light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme (ketan0/responsive-theme))

;; responsive theme

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

(setq-default frame-title-format
              '(:eval (format "%s%s"
                              (buffer-name)
                              (if-let ((current-branch (car (vc-git-branches))))
                                  (format " (%s)" current-branch) ""))))


;; use visual lines + relative numbering
(setq vc-handled-backends '(Git))

(setq auto-revert-remote-files t)
(setq global-auto-revert-mode t)

(setq enable-local-variables t)

;; now can change compile-command on a per-directory basis
(make-variable-buffer-local 'compile-command)

(setq org-crypt-key "ketanjayagrawal@gmail.com")
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.
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

(map! :nm "H" 'evil-first-non-blank)
(map! :nm "L" 'evil-last-non-blank)

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

(map! :map doom-leader-toggle-map "d" 'toggle-debug-on-error)

(define-key evil-visual-state-map "." 'evil-a-paren)

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-directory  "~/org/")
  (setq ketan0/org-directory-private  (concat org-directory "private/"))
  :config
  (require 'evil-org-agenda)
  ;; hides property drawers
  ;; https://commonplace.doubleloop.net/preparing-for-org-roam-v2
  ;; ok not using this for now
  ;; (defun ketan0/org-hide-properties ()
  ;;   "Hide org headline's properties using overlay."
  ;;   (save-excursion
  ;;     (goto-char (point-min))
  ;;     (while (re-search-forward
  ;;             "^ *:PROPERTIES:\n\\( *:.+?:.*\n\\)+ *:END:\n" nil t)
  ;;       (overlay-put (make-overlay
  ;;                     (match-beginning 0) (match-end 0))
  ;;                    'display ""))))

  ;; (add-hook 'org-mode-hook #'ketan0/org-hide-properties)
  ;; (remove-hook 'org-mode-hook #'ketan0/org-hide-properties)
  (setq org-babel-default-header-args
        (cons '(:exports . "both") ;; export code and results by default
              (cons '(:eval . "no-export") ;; don't evaluate src blocks when exporting
                    (assq-delete-all :exports
                                     (assq-delete-all :eval org-babel-default-header-args)))))
  (setq org-ellipsis "…")
  (setq org-hide-emphasis-markers t)

  ;; org HTML export settings
  ;; patch from https://gist.github.com/jethrokuan/d6f80caaec7f49dedffac7c4fe41d132
  ;; makes links to headlines work properly
  (defun org-html--reference (datum info &optional named-only)
    "Return an appropriate reference for DATUM.
DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.
When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
    (let* ((type (org-element-type datum))
           (user-label
            (org-element-property
             (pcase type
               ((or `headline `inlinetask) :CUSTOM_ID)
               ((or `radio-target `target) :value)
               (_ :name))
             datum))
           (user-label (or user-label
                           (when-let ((path (org-element-property :ID datum)))
                             (concat "ID-" path)))))
      (cond
       ((and user-label
             (or (plist-get info :html-prefer-user-labels)
                 ;; Used CUSTOM_ID property unconditionally.
                 (memq type '(headline inlinetask))))
        user-label)
       ((and named-only
             (not (memq type '(headline inlinetask radio-target target)))
             (not user-label))
        nil)
       (t
        (org-export-get-reference datum info)))))
  (defun org-html-src-block (src-block _contents info)
    "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
    (if (org-export-read-attribute :attr_html src-block :textarea)
        (org-html--textarea-block src-block)
      (let* ((lang (org-element-property :language src-block))
             (code (org-html-format-code src-block info))
             (label (let ((lbl (org-html--reference src-block info t)))
                      (if lbl (format " id=\"%s\"" lbl) "")))
             (klipsify  (and  (plist-get info :html-klipsify-src)
                              (member lang '("javascript" "js"
                                             "ruby" "scheme" "clojure" "php" "html")))))
        (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
          (format "<div class=\"org-src-container\">\n%s%s\n</div>"
                  ;; Build caption.
                  (let ((caption (org-export-get-caption src-block)))
                    (if (not caption) ""
                      (let ((listing-number
                             (format
                              "<span class=\"listing-number\">%s </span>"
                              (format
                               (org-html--translate "Listing %d:" info)
                               (org-export-get-ordinal
                                src-block info nil #'org-html--has-caption-p)))))
                        (format "<label class=\"org-src-name\">%s%s</label>"
                                listing-number
                                (org-trim (org-export-data caption info))))))
                  ;; Contents.
                  (if klipsify
                      (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
                              lang
                              label
                              (if (string= lang "html")
                                  " data-editor-type=\"html\""
                                "")
                              code)
                    (format "<pre class=\"src src-%s\" data-language=\"%s\"%s>%s</pre>"
                            lang lang label code)))))))
  (require 'ox-html)
  ;; (load-file "~/garden-simple/publish-utils.el")

  ;; export youtube links to html as iframes of the video
  (defvar yt-iframe-format
    ;; You may want to change your width and height.
    (concat "<iframe width=\"440\""
            " height=\"335\""
            " src=\"https://www.youtube.com/embed/%s\""
            " frameborder=\"0\""
            " allowfullscreen>%s</iframe>"))
  (org-add-link-type
   "yt"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
              handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format yt-iframe-format
                     path (or desc "")))
       (latex (format "\href{%s}{%s}"
                      path (or desc "video"))))))

  (defun ketan0/org-html-export-after-save ()
    "Function for `after-save-hook' to run `org-publish-current-file'.
The exporting happens only when Org Capture is not in progress."
    (when (org-publish-get-project-from-filename (buffer-file-name (buffer-base-buffer)))
      ;; (let ((org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$")))
      (save-excursion (org-publish-current-file))
      ;; )
      ))
  (add-hook 'after-save-hook #'ketan0/org-html-export-after-save :append)
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
  (setq org-default-notes-file (concat ketan0/org-directory-private "capture.org"))
  (setq ketan0/org-todos-file (concat ketan0/org-directory-private "todos.org"))
  (setq ketan0/org-archive-file (concat ketan0/org-directory-private "archive.org"))
  ;;archive done tasks to datetree in archive.org
  (setq org-archive-location (concat ketan0/org-archive-file "::datetree/"))
  (setq org-agenda-files (list org-default-notes-file
                               ketan0/org-todos-file
                               ketan0/org-archive-file))
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day "+0d")
  ;; don't create random IDs when I call org-capture
  (setq org-id-link-to-org-use-id 'create-if-interactive)

  (add-hook 'org-mode-hook #'org-fragtog-mode)
  (setq display-line-numbers t)
  ;; (defun turn-off-line-numbers ()
  ;;   (display-line-numbers-mode 0))
  ;; (add-hook 'org-mode-hook 'turn-off-line-numbers)
  (add-hook 'org-mode-hook 'mixed-pitch-mode)

  ;;   (defvar org-created-property-name "CREATED"
  ;;     "The name of the org-mode property that stores the creation date of the entry")
  ;;   (defun org-set-created-property (&optional active NAME)
  ;;     "Set a property on the entry giving the creation time.
  ;; By default the property is called CREATED. If given the `NAME'
  ;; argument will be used instead. If the property already exists, it
  ;; will not be modified."
  ;;     (interactive)
  ;;     (let* ((created (or NAME org-created-property-name))
  ;;            (fmt (if active "<%s>" "[%s]"))
  ;;            (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
  ;;       (unless (org-entry-get (point) created nil)
  ;;         (org-set-property created now)
  ;;         )))
  ;;   (add-hook 'org-journal-after-entry-create-hook #'org-set-created-property)

  (defun ketan0/create-gtd-project-block (tag-name)
    `(org-ql-block '(and (todo "STRT")
                         (path "todos.org")
                         (tags ,tag-name))
                   ((org-ql-block-header ,tag-name))))
  ;; initial inspiration for custom agenda https://github.com/jethrokuan/.emacs.d/blob/master/init.el
  ;; main projects + stuff scheduled for today + stuff I finished
  ;; NOTE(2022): not actively using this anymore. Instead, see ketan0/new-agenda
  (setq ketan0/main-agenda
        `(" " "Ketan's Focused Agenda"
          ,(append '((agenda ""
                             ((org-agenda-span 'day)
                              (org-deadline-warning-days 2)
                              (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                          'nottodo '("TODO")))
                              ))
                     (org-ql-block '(and (ts :to 0) (todo "STRT"))
                                   ((org-ql-block-header "Top Priority")))
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
                     )
                   ;; (mapcar 'ketan0/create-gtd-project-block
                   ;;         '("projects" "academic" "knowledge" "research"))
                   ;;
                   nil)))
  (setq org-agenda-custom-commands (list ketan0/main-agenda))

  (defun ketan0/switch-to-main-agenda ()
    (interactive)
    (org-agenda nil " "))

  (defun ketan0/look-ahead (arg)
    (interactive "P")
    (let ((days-ahead (if arg (read-number "How many days to look ahead: " 7) 7)))
      (org-ql-search org-agenda-files
        `(and (not (done)) (ts-active :from today :to ,days-ahead))
        :title "Week Ahead"
        :sort '(date priority todo)
        :super-groups '((:auto-ts t)))))
  ;; (map! "<f3>" #'ketan0/look-ahead)


  (defun ketan0/repeating-task-last-24-hours (item)
    (-some->> (get-text-property 0 'LAST_REPEAT item)
      (s-match org-ql-regexp-ts-inactive) (car)
      (ts-parse)
      (ts< (ts-adjust 'hour -24 (ts-now)))))

  (defun ketan0/new-agenda ()
    (interactive)
    (org-ql-search
      org-agenda-files
      `(or (path "capture.org")
           (and (not (done)) (ts-active :to 0))
           (ts-inactive :from ,(ts-adjust 'hour -24 (ts-now))))
      :super-groups `((:name "🗂 To Refile" :file-path "capture.org" :order 0)
                      (:name "✅ Finished Today" :todo "DONE"
                       :pred ketan0/repeating-task-last-24-hours
                       :face (:foreground "#00AA00")
                       :order 4 :forward)
                      ;; :forward keyword passes along matches, using the patch in
                      ;; https://github.com/alphapapa/org-super-agenda/issues/153
                      ;; discard everything except today agenda
                      (:discard (:todo "DONE" :scheduled future :deadline future))
                      (:name "⚠️ Overdue" :deadline past :scheduled past :order 1 :forward)
                      (:name "❗️ Important" :priority>= "B" :face (:foreground "#0096FF") :order 2 :forward)
                      (:auto-outline-path t :order 3))
      :sort 'date
      :title "My Agenda for today"))
  ;; (map! "<f4>" #'ketan0/new-agenda)

  ;; (defun ketan0/ts-inactive-last-7-days (item)
  ;;   (message item)
  ;;   (-when-let* (((ts-string) (s-match org-ql-regexp-ts-inactive item))
  ;;                (ts (ts-parse ts-string))
  ;;                (now (ts-now)))
  ;;     (ts>= ts (ts-adjust 'day -7 now))))

  ;; (defun ketan0/ts-inactive-group (item)
  ;;   (message item)
  ;;   (-when-let* (((ts-string) (s-match org-ql-regexp-ts-inactive item))
  ;;                (ts (ts-parse ts-string))
  ;;                (now (ts-now)))
  ;;     (ts>= ts (ts-adjust 'day -7 now))))

  ;; review completed tasks in the last week

  (defun ketan0/weekly-review (arg)
    (interactive "P")
    (let ((days-back (if arg (read-number "How many days to look back: " 7) 7)))
      (org-ql-search org-agenda-files
        `(and (ts-inactive :from ,(+ (- days-back) 1) today) )
        :title "Week in Review"
        :sort '(date priority todo)
        :super-groups '((:auto-ts-inactive t)))
      (goto-char (point-max))))
  ;; (map! "<f5>" #'ketan0/weekly-review)

  ;; Create an agenda view for each PARA "area"
  (defun ketan0/area-agenda (area-tag)
    (org-ql-search
      org-agenda-files
      `(and (tags ,area-tag)
            (ancestors (todo "PROJ"))
            (todo "STRT"))
      :super-groups '((:auto-outline-path))
      :sort 'priority
      :title (format "%s agenda" area-tag)))

  (map! "<f6> p" (lambda () (interactive) (ketan0/area-agenda "projects"))
        "<f6> a" (lambda () (interactive) (ketan0/area-agenda "academic"))
        "<f6> k" (lambda () (interactive) (ketan0/area-agenda "knowledge"))
        "<f6> t" (lambda () (interactive) (ketan0/area-agenda "tinker"))
        "<f6> r" (lambda () (interactive) (ketan0/area-agenda "research"))
        "<f6> b" (lambda () (interactive) (ketan0/area-agenda "body"))
        "<f6> g" (lambda () (interactive) (ketan0/area-agenda "process"))
        "<f6> u" (lambda () (interactive) (ketan0/area-agenda "utility"))
        "<f6> w" (lambda () (interactive) (ketan0/area-agenda "writing"))
        "<f6> s" (lambda () (interactive) (ketan0/area-agenda "social")))

  (map! :map doom-leader-map "a" 'counsel-org-goto-all)

  (setq org-agenda-block-separator nil)
  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-window-setup 'current-window) ;;agenda take up whole frame
  ;; set priority in bulk
  (setq org-agenda-bulk-custom-functions '((?P (lambda nil (org-agenda-priority 'set)))))

  ;;don't show warnings for deadlines
  (setq org-deadline-warning-days 0) ;;don't show upcoming tasks in today view


  ;; shortcut to schedule items to today
  (defun ketan0/org-schedule-today ()
    (interactive)
    (org-schedule nil "+0d"))
  (defun ketan0/org-agenda-schedule-today ()
    (interactive)
    (org-agenda-schedule nil "+0d"))

  (map! :map org-mode-map
        :localleader
        (:prefix ("d" . "date/deadline")
         "t" #'ketan0/org-schedule-today
         "S" #'org-time-stamp))
  (map! :after org-agenda
        :map org-agenda-mode-map
        :localleader
        (:prefix ("d" . "date/deadline")
         "t" #'ketan0/org-agenda-schedule-today))

  (setq org-edit-src-content-indentation 0) ;;don't indent src blocks further


  ;;refile settings
  (setq org-refile-use-cache t) ;;speeds up loading refile targets
  (setq org-refile-targets '((nil :maxlevel . 9) ;; at most can refile 9 levels deep in the hierarchy
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file) ;;see whole path (not just headline)
  (setq org-outline-path-complete-in-steps nil) ;;easy to complete in one go w/ helm


  (setq org-catch-invisible-edits (quote show-and-error)) ;;avoid accidental edits in folded areas, links, etc.
  (defun my/org-checkbox-todo ()
    "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
    (unless (org-before-first-heading-p)
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
                      (org-todo 'todo))))))))))
  (add-hook 'org-checkbox-statistics-hook 'my/org-checkbox-todo)

  (org-set-modules 'org-modules '(ol-bibtex org-habit))

  (plist-put org-format-latex-options :scale 1.0) ;; scale for inline latex fragments
  (setq org-preview-latex-default-process 'dvisvgm) ;; svg is crisper than png

  (setq org-startup-with-inline-images t) ;; show inline images by default

  (setq org-structure-template-alist
        '(("p" . "src python :results output\n")
          ("b" . "src bash\n")
          ("s" . "src")
          ("q" . "quote\n")
          ("a" . "export ascii\n")
          ("c" . "center\n")
          ("C" . "comment\n")
          ("e" . "src emacs-lisp\n")
          ("E" . "export\n")
          ("h" . "export html\n")
          ("l" . "export latex\n")
          ("v" . "verse\n")
          ("x" . "example\n")))

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

  (setq +org-capture-frame-parameters
        `((name . "doom-capture")
          (width . (text-pixels . 720))
          (height . (text-pixels . 220))
          (left . 504)
          (top . 340)
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
           (file ,(concat ketan0/org-directory-private "ideas.org"))
           "* %?") ;;TODO: put CLOSED + timestamp
          ("c" "coronavirus" entry
           (file+olpdatetree
            ,(concat ketan0/org-directory-private "20200314210447_coronavirus.org"))
           "* %^{Heading}")
          ("H" "HCI lab notebook" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "20210401093501-hci_counterfactual_reasoning.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("E" "EConsults lab notebook" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "20210714164344-econsults_prediction.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("T" "org-twitter" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "20201013012647-org_twitter.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("G" "raspi-glove" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "20201113182201-raspberry_pi_glove.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("n" "nice thing" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "nice_things_people_have_said_to_me.org") "Tree")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("D" "Digital Garden" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "20201111010429-digital_laboratory.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("P" "PAC lab notebook" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "20200313153429_pac.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t :jump-to-captured t)
          ("o" "Org-spotify lab notebook" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "20201006205609-org_spotify.org") "Lab Notebook")
           "* %?" :tree-type week :unnarrowed t)
          ("w" "Weekly Review" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "reviews.org"))
           (file ,(concat ketan0/org-directory-private "20200816223343-weekly_review.org"))
           :jump-to-captured t)
          ("m" "Monthly Review" entry
           (file+olp+datetree
            ,(concat ketan0/org-directory-private "reviews.org"))
           (file ,(concat ketan0/org-directory-private "20201025201109-monthly_review.org"))
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

;; citations completions
(use-package! citar
  :after oc
  :config
  (map! :map org-mode-map
        :localleader
        "R" #'citar-refresh)
  (map! :map LaTeX-mode-map
        :localleader
        "R" #'citar-refresh)
  (setq citar-bibliography org-cite-global-bibliography)
  ;; (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook))
  )


;; drag-n-drop images
(after! org-download
  (setq org-download-method 'attach)
  (setq org-download-backend "curl \"%s\" -o \"%s\""))

(use-package! find-lisp)

;; automatically publish org file upon save
;; (defun ketan0/org-html-export-after-save ()
;;   "Function for `after-save-hook' to run `org-publish-current-file'.
;; The exporting happens only when Org Capture is not in progress."
;;   (when (org-publish-get-project-from-filename (buffer-file-name (buffer-base-buffer)))
;;     ;; (let ((org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$")))
;;     (save-excursion (org-publish-current-file))
;;     ;; )
;;     ))
;; (add-hook 'after-save-hook #'ketan0/org-html-export-after-save :append :local)


;; (define-minor-mode ketan0/org-html-auto-export-mode
;;   "Toggle auto exporting the Org file using `ox-html'."
;;   :global nil
;;   :lighter ""
;;   (if ketan0/org-html-auto-export-mode
;;       ;; When the mode is enabled
;;       (require 'ox-html)
;;       (add-hook 'after-save-hook #'ketan0/org-html-export-after-save :append :local)
;;     ;; When the mode is disabled
;;     (remove-hook 'after-save-hook #'ketan0/org-html-export-after-save :local)))
;; (provide 'ketan0/org-html-auto-export-mode)

(use-package! org-journal
  :after org
  :init
  (map! :map doom-leader-map
        "j j" 'org-journal-new-entry
        "j d" 'org-decrypt-entry)
  (setq org-journal-find-file 'find-file
        org-journal-file-header "#+title: %Y%m%d"
        org-journal-date-prefix "* "
        org-journal-dir ketan0/org-directory-private
        org-journal-carryover-items nil
        org-journal-file-format "%Y%m%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-encryption nil) ;; not using for now
  (defadvice! ketan0/org-journal-new-entry-append (prefix)
    :after #'org-journal-new-entry
    ;; start journal in insert mode
    (unless prefix (evil-append 1)))
  :config
  ;; COMMENTED OUT: code for storing / restoring cursor position in an encrypted block
  ;; (not using atm, because I'm not using org encryption that much)
  ;; store point before encryption; restore point after decryption
  ;; (defvar-local ketan0/last-cursor-position nil)
  ;; (defun ketan0/store-cursor-position ()
  ;;   (setq-local ketan0/last-cursor-position (point)))

  ;; (defun ketan0/restore-cursor-position ()
  ;;   (when ketan0/last-cursor-position
  ;;     (goto-char ketan0/last-cursor-position)))
  ;; (advice-add 'org-encrypt-entry :before #'ketan0/store-cursor-position)
  ;; (advice-remove 'org-decrypt-entries #'ketan0/restore-cursor-position)
  ;; (advice-add 'org-encrypt-entries :before #'ketan0/store-cursor-position)
  ;; TODO: this should probably be happening after org-decrypt-entries,
  ;; but that messes up org-journal-new-entry's positioning of a new header
  ;; (advice-add 'org-decrypt-entry :after #'ketan0/restore-cursor-position)

  (defun org-journal-encryption-hook ())
  (defun ketan0/add-id-to-journal-entry ()
    (save-excursion
      (goto-char (point-min))
      (org-id-get-create)))
  (add-hook 'org-journal-after-header-create-hook #'ketan0/add-id-to-journal-entry))

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
  :init

  :config
  (defun ketan0/inactive-sort-fn (a b)
    (ts< (get-text-property 0 'org-super-agenda-ts a)
         (get-text-property 0 'org-super-agenda-ts b)))
  (org-super-agenda--def-auto-group ts-inactive
    "the date of their latest inactive timestamp anywhere in the entry (formatted according to `org-super-agenda-date-format', which see)"
    :keyword :auto-ts-inactive
    :key-form
    (org-super-agenda--when-with-marker-buffer
      (org-super-agenda--get-marker item)
      (let* ((limit (org-entry-end-position))
             (latest-ts (->> (cl-loop for next-ts =
                                      (when (re-search-forward org-ql-regexp-ts-inactive limit t)
                                        (ts-parse-org (match-string 0)))
                                      while next-ts
                                      collect next-ts)
                             (-max-by #'ts>))))
        (when latest-ts
          (propertize (ts-format org-super-agenda-date-format latest-ts)
                      'org-super-agenda-ts latest-ts))))
    :key-sort-fn ketan0/inactive-sort-fn)
  (defconst org-super-agenda-special-selectors
    '(:name :order :face :transformer :forward)
    "Special, non-grouping selectors.")

  ;; adds :forward selector for org-super-agenda groups
  ;; https://github.com/alphapapa/org-super-agenda/issues/153
  (defun org-super-agenda--group-dispatch (items group)
    "Group ITEMS with the appropriate grouping functions for GROUP.
Grouping functions are listed in `org-super-agenda-group-types', which
see."
    (cl-loop for (selector args) on group by 'cddr  ; plist access
             for fn = (org-super-agenda--get-selector-fn selector)
             ;; This double "when fn" is an ugly hack, but it lets us
             ;; use the destructuring-bind; otherwise we'd have to put
             ;; all the collection logic in a progn, or do the
             ;; destructuring ourselves, which would be uglier.
             when fn
             for (auto-section-name non-matching matching) = (funcall fn items args)
             when fn
             ;; This is the implicit OR
             append matching into all-matches
             and collect auto-section-name into names
             and do (setq items (append non-matching (and (memq :forward group)
                                                          ;; my addition
                                                          (-map #'concat matching))))
             for name = (if (stringp (car names))
                            (s-join " and " (-non-nil names))
                          ;; Probably an :auto-group
                          (car names))
             finally return (list name items all-matches)))
  ;; the headers in org-super-agenda  should inherit evil-org-agenda keybinds, not standard org-agenda keybinds.
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map))
  (map! :map org-super-agenda-header-map "q" #'bury-buffer)

  (setq org-super-agenda-date-format "%A %B %e")
  )

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  ;; https://www.orgroam.com/manual.html#Configuring-the-Org_002droam-buffer-display
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (map! :leader :nm
        "r r" #'org-roam-node-find
        "r a" #'org-roam-node-random
        "r i" #'org-roam-node-insert
        "r u" #'org-roam-unlinked-references-section
        "r b" #'org-roam-buffer-toggle)
  :after org
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-directory org-directory)
  (setq org-roam-db-location (concat org-roam-directory "org-roam.db"))
  (setq org-id-extra-files (append (find-lisp-find-files org-roam-directory "\.org$")
                                   (find-lisp-find-files ketan0/org-directory-private "\.org$")))
  (setq org-roam-list-files-commands '(find)) ;; rg and fd don't seem to work for private/ subdir
  (org-roam-db-autosync-mode))

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

;; source: https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/
(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer "*doom:vterm-popup:main*"
    ;; (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

(defun ketan0/source-shortcuts ()
  "If the current buffer is 'yabairc' then yabai is relaunched with the new config."
  (when (equal (buffer-file-name)
               (expand-file-name (concat ketan0/dotfiles-dir "work-shortcuts.sh")))
    ;; Avoid running hooks when tangling.
    (message "Sourcing shell shortcuts...")
    (run-in-vterm "source ~/.dotfiles/work-shortcuts.sh")))
(add-hook 'after-save-hook 'ketan0/source-shortcuts)

(defun ketan0/source-yabairc ()
  "If the current buffer is 'yabairc' then yabai is relaunched with the new config."
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

;; (use-package! counsel-spotify
;;   :config
;;   (setq counsel-spotify-client-id ketan0-secrets/spotify-client-id)
;;   (setq counsel-spotify-client-secret ketan0-secrets/spotify-client-secret))

;; ;; ;; (use-package! spotify
;; ;; ;;   :config
;; ;; ;;   (setq spotify-oauth2-client-id ketan0-secrets/spotify-client-id)
;; ;; ;;   (setq spotify-oauth2-client-secret ketan0-secrets/spotify-client-secret))


;; (setq ketan0/org-spotify-package-path "/Users/ketanagrawal/org-spotify")
;; (use-package! org-spotify
;;   :after org
;;   :load-path ketan0/org-spotify-package-path
;;   :config
;;   (map! :map org-mode-map
;;         :localleader
;;         (:prefix ("S" . "Org Spotify")
;;          :desc "Update playlist at point" "u" #'org-spotify-push-playlist-at-point
;;          :desc "Insert Spotify track" "t" #'org-spotify-insert-track
;;          :desc "Insert Spotify album" "a" #'org-spotify-insert-album
;;          :desc "Insert Spotify artist" "r" #'org-spotify-insert-artist
;;          :desc "Insert Spotify playlist" "p" #'org-spotify-insert-playlist))
;;   (setq org-spotify-user-id ketan0-secrets/spotify-user-id)
;;   (setq org-spotify-oauth2-client-id ketan0-secrets/spotify-client-id)
;;   (setq org-spotify-oauth2-client-secret ketan0-secrets/spotify-client-secret))

;; ;; (use-package! gif-screencast
;; ;;   :config
;; ;;   ;; (map! "<f3>" 'gif-screencast-start-or-stop)
;; ;;   (setq gif-screencast-args '("-v" "-x"))
;; ;;   (setq gif-screencast-cropping-program "mogrify")
;; ;;   (setq gif-screencast-capture-format "ppm"))

;; (use-package! conda
;;   :config
;;   (setq conda-anaconda-home "/Users/ketanagrawal/miniconda3")
;;   (setq conda-env-home-directory "/Users/ketanagrawal/miniconda3"))

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
;; (use-package! pdf-tools
;;       :config
;;       (custom-set-variables
;;         '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
;;       (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))

;; (use-package! conda
;;   :config
;;   (map! :map doom-leader-code-map
;;         :desc "Activate conda env" "A" #'conda-env-activate))

(use-package! python
  :config
  (setq python-shell-completion-native-enable nil)
  ;; show a vertical line at the line length limit
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode))

(use-package! pyvenv
  :config
  (pyvenv-activate (expand-file-name "~/rime/python/rime/.venv")))

(use-package! py-isort
  :config
  ;; HACK -- more mature approach would be to add a var to py-isort package where one can manually specify the settings path
  (defun py-isort--find-settings-path ()
    (expand-file-name "~/isort-config/"))
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package! flycheck
  :config
  ;; so I can set pylintrc per-directory, in a .dir-locals.el file fo
  (make-variable-buffer-local 'flycheck-pylintrc)
  (map! :map doom-leader-code-map
        :desc "List Flycheck errors" "x" #'flycheck-list-errors))

(use-package! flycheck-projectile)

(use-package! doom-modeline-core
  :config
  (setq doom-modeline-vcs-max-length 40))

(use-package! lsp
  :config
  (setq lsp-file-watch-threshold 10000)
  (defvar-local ketan0/flycheck-local-cache nil)

  (defun ketan0/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker ketan0/flycheck-local-cache))
	(funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'ketan0/flycheck-checker-get)

  (add-hook 'lsp-managed-mode-hook
	    (lambda ()
	      (when (derived-mode-p 'python-mode)
		(setq ketan0/flycheck-local-cache
                      '((lsp . ((next-checkers . (python-pylint python-mypy)))))))))
  (map! :map doom-leader-code-map
        :desc "Restart LSP workspace" "R" #'lsp-restart-workspace))

(use-package! life
  :config
  (setq life-default-sleeptime 0.1)
  (setq life-preferred-pattern '(" @@"
                                 "@@ "
                                 " @ "))
  (defun life-insert-random-pattern ()
    (insert-rectangle
     (if (boundp 'life-preferred-pattern)
         life-preferred-pattern
       (elt life-patterns (random (length life-patterns)))))
    (insert ?\n))
  (defun life (&optional sleeptime)
    "Run Conway's Life simulation.
The starting pattern is randomly selected.  Prefix arg (optional first
arg non-nil from a program) is the number of seconds to sleep between
generations (this defaults to 1)."
    (interactive "p")
    ;; (message "sleeptime is %s" sleeptime)
    ;; (or sleeptime (setq sleeptime life-default-sleeptime))
    (life-setup)
    (catch 'life-exit
      (while t
        (let ((inhibit-quit t)
              (inhibit-read-only t))
          (life-display-generation life-default-sleeptime)
          (life-grim-reaper)
          (life-expand-plane-if-needed)
          (life-increment-generation))))))

;; (use-package outline
;;   :after org-super-agenda
;;   :config

;;   (defvar ketan0/outline-agenda-hidden nil)
;;   (defun ketan0/outline-toggle-all ()
;;     (interactive)
;;     (if ketan0/outline-agenda-hidden (outline-show-all) (outline-hide-body))
;;     (setq ketan0/outline-agenda-hidden (not ketan0/outline-agenda-hidden)))
;;   (map! :map evil-org-agenda-mode-map "<S-tab>" #'ketan0/outline-toggle-all)
;;   (map! :map org-super-agenda-header-map "<tab>" #'outline-toggle-children)
;;   (defvar org-super-agenda-auto-fold-groups '("Stuck Tasks" "Other items" "Notice"))

;;   ;; function borrowed from new fork of origami.el https://github.com/emacs-origami/origami.el/blob/master/origami.el#L1024
;;   (defun outline-auto-agenda (pattern-or-patterns function)
;;     "Search buffer and apply the FUNCTION on each line.
;; PATTERN-OR-PATTERNS is a string or a list of strings to search"
;;     (interactive)
;;     (let ((patterns (if (listp pattern-or-patterns) pattern-or-patterns (list pattern-or-patterns))))
;;       (save-excursion
;;         (dolist (pattern patterns)
;;           (goto-char (point-min))
;;           (while (re-search-forward pattern nil t 1)
;;             (unless (outline-invisible-p)
;;               (funcall function)))))))
;;   (setq org-super-agenda-header-prefix "* ️")

;;   (defun outline-agenda-setup ()
;;     (setq-local outline-regexp org-super-agenda-header-prefix)
;;     (setq-local outline-level #'outline-level)
;;     (setq-local outline-heading-alist
;;                 `((,org-super-agenda-header-prefix . 1)))
;;     (outline-auto-agenda org-super-agenda-auto-fold-groups #'outline-hide-subtree))

;;   :hook ((org-agenda-mode . outline-minor-mode)
;;          (org-agenda-finalize . outline-agenda-setup)))

(defun codex (&optional b e)
  (interactive "r")
  (goto-char e)
  (insert (with-output-to-string
            (shell-command-on-region
             b e "codex.py" standard-output nil))))

(defun my/center (width)
  (interactive "nBuffer width: ")
  (let* ((adj          (- (window-text-width)
                          width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer)))


(map! :map evil-window-map
      "SPC" #'centered-window-mode)
(centered-window-mode)

(use-package! request)

(use-package! undo-fu-session
  :config
  (push "/Users/ketanagrawal/org/thoughts.org" undo-fu-session-incompatible-files))

(use-package! magit
  :config
  (setq magit-list-refs-sortby "-creatordate"))
