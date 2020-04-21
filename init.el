(defvar ketan0/dotfiles-dir (file-name-as-directory "~/.dotfiles")
  "Personal dotfiles directory.")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
       (goto-char (point-max))
       (eval-print-last-sexp)))
   (load bootstrap-file nil 'nomessage))
   (straight-use-package 'use-package)
   (setq straight-use-package-by-default t)

(defun tangle-init ()
        "If the current buffer is 'init.org' the code-blocks are
    tangled, and the tangled file is compiled."
        (when (equal (buffer-file-name)
                    (expand-file-name (concat ketan0/dotfiles-dir "init.org")))
        ;; Avoid running hooks when tangling.
        (let ((prog-mode-hook nil))
            (org-babel-tangle-file
            (expand-file-name (concat ketan0/dotfiles-dir "init.org"))
            (expand-file-name (concat ketan0/dotfiles-dir "init.el")) 
            "emacs-lisp")
            (byte-compile-file (concat user-emacs-directory "init.el")))))
    ;;TODO: add dotfiles variable and stuffs

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
;;TODO: add dotfiles variable and stuffs

(defun source-yabairc ()
  "If the current buffer is 'karabiner.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat ketan0/dotfiles-dir "yabairc")))
    ;; Avoid running hooks when tangling.
       (message (concat "yabairc has been sourced" (shell-command-to-string "launchctl kickstart -k \"gui/${UID}/homebrew.mxcl.yabai\"")))))
;;TODO: add dotfiles variable and stuffs

(add-hook 'after-save-hook 'tangle-init)
(add-hook 'after-save-hook 'tangle-karabiner)
(add-hook 'after-save-hook 'source-yabairc)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (if (string-equal system-type "darwin")
                (do-applescript
                 (get-string-from-file "~/Documents/rightalign.txt")))))

(setq custom-safe-themes t)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(define-key key-translation-map (kbd "<left>") (kbd "C-h")) 
(define-key key-translation-map (kbd "<down>") (kbd "C-j")) 
(define-key key-translation-map (kbd "<up>") (kbd "C-k")) 
(define-key key-translation-map (kbd "<right>") (kbd "C-l"))

(global-set-key (kbd "A-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-m") 'suspend-frame)
(global-set-key (kbd "M-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "˙") 'switch-to-prev-buffer) ;; A-h
(global-set-key (kbd "¬") 'switch-to-next-buffer) ;; A-l

;;TODO: work on making this work
;; (defun ketan0/find-certain-file (filepath)
;;   "here's a function"
;;   `(lambda (filepath)
;;      (interactive)
;;      (find-file filepath)))

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(defun find-todo-file ()
  "Edit the todo.org file, in *this* window."
  (interactive)
  (find-file (concat org-directory "/todo.org")))

(defun find-vision-file ()
  "Edit the vision.org file, in *this* window."
  (interactive)
  (find-file (concat org-directory "/20200407061957-vision.org")))

(defun er-find-user-init-file ()
  "Edit the `user-init-file', in *this* window."
  (interactive)
  (find-file (concat ketan0/dotfiles-dir "init.org")))

(defun open-dir-in-finder ()
  "Open a new Finder window to the path of the current buffer"
  (interactive)
  (start-process "mai-open-dir-process" nil "open" "."))

(defun open-dir-in-iterm ()
  "Open the current directory of the buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/1.0.0/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path
                       iterm-brew-path)))
    (start-process "mai-open-dir-process" nil "open" "-a" iterm-path ".")))

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
          New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

          It returns the buffer (for elisp programing).

          URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
          Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer-other-window $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

(set-frame-font "Fira Code 12" nil t)
;;Fira Code ligatures
(if (string-equal system-type "darwin")
    (mac-auto-operator-composition-mode t))

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (load-theme 'airline-luna t)))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(use-package doom-themes
  :defer t
  :no-require t)

(use-package apropospriate-theme
  :defer t
  :no-require t)

(use-package cyberpunk-theme
  :defer t
  :no-require t)

(use-package oldlace-theme
  :defer t
  :no-require t)

(use-package spacemacs-theme
  :defer t
  :no-require t)

(use-package leuven-theme
  :defer t
  :no-require t)

(use-package constant-theme
  :defer t
  :no-require t)

(use-package cherry-blossom-theme
  :defer t
  :no-require t)

(use-package gruvbox-theme
  :defer t
  :no-require t)

(use-package dracula-theme
  :defer t
  :no-require t)

(use-package bubbleberry-theme
  :defer t
  :no-require t)

(use-package airline-themes
  :config (load-theme 'airline-luna))


(switch-theme 'doom-acario-dark)

(setq gc-cons-threshold 100000000) ;;100mb; default setting is too low for lsp-mode et al.
(setq read-process-output-max (* 1024 1024)) ;; 1mb


(setq frame-title-format "%b") ; show buffer name in title bar
(setq inhibit-splash-screen t) ;don't show default emacs startup screen
(setq visible-bell t) ;Instead of shell bell, visual flash
(setq ring-bell-function ; don't ring (flash) the bell on C-g
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))
(electric-pair-mode t) ;;auto-pairs, eg () [] {}
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\( . ?\))
        (?\[ . ?\])
        (?\$ . ?\$)
        (?\{ . ?\})))

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))
(global-visual-line-mode t)

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))


(setq vc-follow-symlinks 'ask)
;; TRAMP: disable version control to avoid delays:
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Turn on the blinking cursor
(blink-cursor-mode t)

(setq-default indent-tabs-mode nil)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

(show-paren-mode t)
(column-number-mode t)

(setq uniquify-buffer-name-style 'forward)

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(use-package centered-window
  :config 
  (centered-window-mode t))

(require 'bind-key)

(use-package diminish
  :config
  (diminish 'auto-revert-mode))

;;______________________________________________________________________
     ;;;;  Installing Org with straight.el
(require 'subr-x)
(use-package git)

(defun org-git-version ()
  "The Git version of 'org-mode'.
                      Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of 'org-mode'.
                      Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)


(use-package org
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
           "* %^{package name} \n#+begin_src emacs-lisp\n(use-package %\\1)\n#+end_src\n")))

  ;;open links in same window
  (setq org-link-frame-setup '((file . find-file)))
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

(use-package org-roam
  :after org
  :diminish org-roam-mode
  :hook 
  (after-init . org-roam-mode)
  :straight (:host github :repo "jethrokuan/org-roam" :branch "master")
  :config
  (setq org-roam-graphviz-executable "/usr/local/bin/dot")
  (setq org-roam-graph-viewer "/Applications/Google Chrome.app/")
  (setq org-roam-directory "~/org/"))

(use-package org-journal
  :custom
  (org-journal-find-file 'find-file)
  (org-journal-dir "~/org/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-header-separator "\n")
  (setq org-super-agenda-groups '((:auto-category t)))
  (setq org-super-agenda-header-map (make-sparse-keymap))) ;;the header keymaps conflict w/ evil-org keymaps

(use-package org-ql)

(use-package org-pdftools
  :init
  (setq org-pdftools-root-dir "~/Dropbox/Apps/GoodNotes 5/GoodNotes/"
        org-pdftools-search-string-separator "??")
  :after org
  :config
  (org-link-set-parameters "pdftools"
                           :follow #'org-pdftools-open
                           :complete #'org-pdftools-complete-link
                           :store #'org-pdftools-store-link
                           :export #'org-pdftools-export)
  (add-hook 'org-store-link-functions 'org-pdftools-store-link))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode t))))

(use-package ox-pandoc)

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config 
  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (evil-mode t)
  (define-key evil-normal-state-map "Q" (kbd "@q"))
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after org
  :diminish evil-org-mode
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  (define-key evil-normal-state-map (kbd "0") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "$") 'evil-end-of-line)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
;; (setq evil-want-C-i-jump nil) ;; C-i and TAB are same in terminal

(use-package evil-magit
  :after evil
  :config
  (evil-magit-init))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package evil-commentary
  :after evil
  :config 
  (evil-commentary-mode t))

(use-package evil-leader
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key ;active in all modes
    "<SPC>" 'helm-M-x
    ";" 'bookmark-jump
    "a" 'org-agenda
    "b" 'switch-to-buffer
    "c" 'org-capture
    "e" 'eshell
    "f" 'helm-find-files
    "g" 'magit-status
    "h i" 'info
    "h k" 'describe-key
    "h m" 'describe-mode
    "h o" 'describe-symbol
    "h v" 'describe-variable
    "h w" 'where-is
    "i" 'er-find-user-init-file
    "j" 'org-journal-new-entry
    "k" 'kill-this-buffer
    "K" 'kill-buffer-and-window
    "l" 'link-hint-open-link
    ;; "n" 'switch-to-next-buffer
    ;; "o" 'xah-new-empty-buffer
    "o f" 'open-dir-in-finder
    "o i" 'open-dir-in-iterm
    ;; "p" 'switch-to-prev-buffer
    "p" 'org-pomodoro
    "q" 'delete-other-windows
    "s h" 'evil-window-left
    "s j" 'evil-window-down
    "s k" 'evil-window-up
    "s l" 'evil-window-right
    "s s" 'helm-projectile-rg
    "s f" 'helm-org-rifle-current-buffer
    "s t" 'window-toggle-split-direction 
    "t l" 'load-theme
    "t s" 'switch-theme
    "t d" 'disable-theme
    "w" 'save-buffer
    "'" 'org-edit-special
    "r f" 'org-roam-find-file
    "r g" 'org-roam-show-graph
    "r i" 'org-roam-insert
    "r l" 'org-roam
    "r o" 'org-open-at-point
    "v" 'find-vision-file)
  (evil-leader/set-key-for-mode 'LaTeX-mode
    "c a" 'LaTeX-command-run-all 
    "c c" 'LaTeX-command-master
    "c e" 'LaTeX-environment)
  (global-evil-leader-mode t))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package helm
  :diminish helm-mode
  :bind
  (:map helm-map
   ("C-j" . helm-next-line)
   ("C-k" . helm-previous-line))
  (:map helm-find-files-map
   ("C-h" . hem-find-files-up-one-level)
   ("C-l" . helm-execute-persistent-action))
  (:map helm-M-x-map
   ("C-d" . help-message))
  :init
  (setq helm-completion-style 'emacs)
  (setq completion-styles '(helm-flex))
  :config 
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-mode t))

;; (use-package helm-files
;;   :bind
;;   (:map helm-find-files-map

(use-package helm-projectile
  :after helm-mode
  :commands helm-projectile
  :bind ("C-c p h" . helm-projectile))

(use-package helm-org)

(use-package helm-ag
  :after helm-mode)

(use-package helm-rg
  :after helm-mode)

(use-package projectile
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package elpy
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs")) ;; use conda envs
  (pyvenv-mode 1))

(use-package ein)

(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
        ("C-w" . 'evil-delete-backward-word)
        ("<RET>" . company-complete-selection))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :config 
  (setq lsp-auto-require-clients t)
  (setq lsp-auto-configure t)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "<insert your LS's binary name or path here>")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         (python-mode . lsp)
         )
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(defun ketan0/prog-mode-setup ()
  (push 'company-lsp company-backends))

(use-package company-lsp
  :commands company-lsp
  :config
  (add-hook 'prog-mode-hook #'ketan0/prog-mode-setup))
;; Company completions for org-roam
(use-package company-org-roam
  :straight (:host github :repo "jethrokuan/company-org-roam" :branch "master"))

(use-package clojure-mode)

(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  (setq flycheck-global-modes '(not org-src-mode)) ;; no flycheck when doing code snippets
  (setq flycheck-indication-mode nil))

(use-package google-this
  :diminish google-this-mode
  :config
  (google-this-mode t))

(use-package auctex
  :defer t
  :config
  (setq-default TeX-master nil)
  (setq TeX-save-query nil)
  (setq TeX-auto-save t)

  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")) 
        TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
            TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))

(defun ketan0/latex-mode-setup ()
  (setq-local company-backends
              (push '(company-math-symbols-latex company-latex-commands)
                    company-backends)))

;;TODO: let's clean this up
(defun ketan0/org-mode-setup ()
  (setq-local company-backends
              (push '(company-math-symbols-unicode company-org-roam)
                    company-backends)))

(use-package company-math
  :init
  (add-hook 'LaTeX-mode-hook 'ketan0/latex-mode-setup)
  (add-hook 'org-mode-hook 'ketan0/org-mode-setup))

(use-package pdf-tools
  :config
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package magit)

(use-package mac-pseudo-daemon
  :straight (mac-pseudo-daemon :type git :host github :repo "DarwinAwardWinner/mac-pseudo-daemon")
  :config
  (mac-pseudo-daemon-mode t))

(use-package ssh-config-mode)

(use-package link-hint)

(use-package adaptive-wrap
  :diminish adaptive-wrap-prefix-mode
  :init (adaptive-wrap-prefix-mode))

(use-package helm-org-rifle)

(use-package dash)

(use-package f)

(use-package s)

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package org-pomodoro)