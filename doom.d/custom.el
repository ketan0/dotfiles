(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((ketan0/org-html-auto-export-mode . t))))
(custom-theme-set-faces!
 'leuven
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block :extend t :background "#EAEAFF" :foreground "#000088")
 '(org-block-begin-line :extend t :background "#EAEAFF" :foreground "#008ED1")
 '(org-block-end-line :extend t :background "#EAEAFF" :foreground "#008ED1"))

(custom-theme-set-faces!
  'doom-ayu-light
 '(org-scheduled-previously :foreground "#575f66")
 '(org-scheduled-today :foreground "#575f66")
 '(lsp-face-highlight-textual :foreground "#575f66" :background "#f0f8fd")
;;        Foreground: #d9c2c6
;; DistantForeground: #fcfcfc
;;        Background: #f0f8fd
 )

(custom-theme-set-faces!
  'doom-ayu-mirage
 '(org-scheduled-previously :foreground "#cbccc6")
 '(org-scheduled-today :foreground "#cbccc6"))

(custom-theme-set-faces!
  'user
  `(outline-8 :weight semibold)
  `(outline-7 :weight semibold)
  `(outline-6 :weight semibold)
  `(outline-5 :weight semibold)
  `(outline-4 :weight semibold :height 1.1)
  `(outline-3 :weight semibold :height 1.25)
  `(outline-2 :weight semibold :height 1.5)
  `(outline-1 :weight semibold :height 1.75)
  `(org-superstar-header-bullet :height 1)
  `(org-document-title :weight bold :height 2.0))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-structure ((t (:height 1.5))))
 '(org-document-title ((t (:weight bold :height 3.0))))
 '(org-superstar-header-bullet ((t (:height 1))))
 '(outline-1 ((t (:weight semibold :height 1.75))))
 '(outline-2 ((t (:weight semibold :height 1.5))))
 '(outline-3 ((t (:weight semibold :height 1.25))))
 '(outline-4 ((t (:weight semibold :height 1.1))))
 '(outline-5 ((t (:weight semibold))))
 '(outline-6 ((t (:weight semibold))))
 '(outline-7 ((t (:weight semibold))))
 '(outline-8 ((t (:weight semibold)))))
