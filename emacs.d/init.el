(defvar ketan0/dotfiles-dir (file-name-as-directory "~/.dotfiles")
  "Personal dotfiles directory.")

(setq user-emacs-directory (file-truename "~/.emacs.d/"))

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
                (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle-file
       (expand-file-name (concat user-emacs-directory "init.org"))
       (expand-file-name (concat user-emacs-directory "init.el")) 
       "emacs-lisp")
      (byte-compile-file (concat user-emacs-directory "init.el")))))
;;TODO: add dotfiles variable and stuffs

(add-hook 'after-save-hook 'tangle-init)

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
  (find-file (concat user-emacs-directory "init.org")))

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

(setq inhibit-splash-screen t) ;don't show default emacs startup screen
(setq visible-bell t) ;Instead of shell bell, visual flash
(setq ring-bell-function ; don't ring (flash) the bell on C-g
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))
(electric-pair-mode t) ;;auto-pairs, eg () [] {}
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

(use-package diminish)

;;______________________________________________________________________
          ;;;;  Installing Org with straight.el
          ;;; https://github.com/raxod502/straight.el/blob/develop/README.md#installing-org-with-straightel
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

(when (version<= "9.2" org-version)
  (require 'org-tempo))

;; (straight-use-package 'org) ; or org-plus-contrib if desired

(use-package org
  :config
  (setq org-ellipsis "…")
  (setq org-log-done t)
  (setq org-directory "~/org")


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

  (setq org-agenda-files '("~/org/"))
  (setq org-agenda-block-separator nil)
  (setq org-agenda-format-date (lambda (date) (concat "\n"
                                                      (make-string (/ (window-width) 2) 9472)
                                                      "\n"
                                                      (org-agenda-format-date-aligned date))))
  (setq org-agenda-start-with-follow-mode t)
  (setq org-agenda-window-setup 'only-window)
  ;;don't show warnings for deadlines
  (setq org-deadline-warning-days 0)

  ;;refile headlines to any other agenda files
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-catch-invisible-edits (quote show-and-error))
  (setq org-default-notes-file (concat org-directory "/capture.org"))
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
           "* %^{Heading}")))
  ;;open links in same window
  (delete '(file . find-file-other-window) org-link-frame-setup)
  (add-to-list 'org-link-frame-setup '(file . find-file))
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
  (setq org-roam-directory "~/org/"))


;; Company completions for org-roam
(use-package company-org-roam
  :straight (:host github :repo "jethrokuan/company-org-roam" :branch "master")
  :config
  (push '(company-org-roam company-capf company-files) company-backends))

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
    "n" 'switch-to-next-buffer
    ;; "o" 'xah-new-empty-buffer
    "o f" 'open-dir-in-finder
    "o i" 'open-dir-in-iterm
    "p" 'switch-to-prev-buffer
    "q" 'delete-other-windows
    "s h" 'evil-window-left
    "s j" 'evil-window-down
    "s k" 'evil-window-up
    "s l" 'evil-window-right
    "s s" 'helm-projectile-ag
    "t l" 'load-theme
    "t s" 'switch-theme
    "t d" 'disable-theme
    "w" 'save-buffer;;)
  ;; (evil-leader/set-key-for-mode 'org-mode ;just for org-mode, normal state
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
  :init
  (setq helm-completion-style 'emacs)
  (setq completion-styles '(helm-flex))
  :config 
  (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-mode t))

(use-package helm-projectile
  :after helm-mode
  :commands helm-projectile
  :bind ("C-c p h" . helm-projectile))

(use-package helm-org)

(use-package helm-ag
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
  :config
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  (global-company-mode t))

(use-package clojure-mode)

(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  (setq flycheck-indication-mode nil))

(use-package google-this
  :diminish google-this-mode
  :config
  (google-this-mode t))

(use-package auctex
  :defer t
  :config
  (defvar TeX-auto-save)
  (defvar TeX-command-list)
  (setq TeX-auto-save t)
  (setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
            TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))

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
