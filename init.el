;; set standard indent
(setq standard-indent 4)
;; set default indent to use spaces
(setq indent-tabs-mode nil)
;; show line number
(setq line-number-mode t)
;; show column number
(setq column-number-mode t)
;; syntax highlight mode
(setq global-font-lock-mode t)
;; search highlight
(setq search-highlight t)
;; highlight parentheses
(setq show-paren-mode t)
;; no tab
(setq-default indent-tabs-mode nil)

;; let backup files saved in emacs.d/backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying 1    ; Don't delink hardlinks
  version-control 1      ; Use version numbers on backups
  delete-old-versions 1  ; Automatically delete excess backups
  kept-new-versions 10   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
)

;; add MELPA, Marmalade into package management
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/")) ; gnu
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; for org mode
  (package-initialize)
)

;; add ~/.emacs.d/ into load-path
(add-to-list 'load-path 
  (expand-file-name "lisp" user-emacs-directory))

;; define default packages
(defvar default-packages '(async
                           bash-completion
                           company
                           company-c-headers
                           exec-path-from-shell
                           expand-region
                           find-file-in-project
                           go-mode
                           helm
                           json-mode
                           markdown-mode
                           org
                           powerline
                           projectile
                           smex
                           xcscope
                           yaml-mode)
  "Default packages")

;; check and install default packages
(defun check-default-packages ()
  (setq res t)
  (dolist (pkg default-packages)
    (when (not (require pkg nil :noerror)) (setq res nil)))
  res)

(unless (check-default-packages)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg default-packages)
    (when (not (require pkg nil :noerror))
      (package-install pkg))))

;; start company-mode (auto-complete)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; smex
;(require 'smex)
;(smex-initialize)
;(global-set-key (kbd "M-x") 'smex)

;; helm
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

;; add expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; add powerline
(require 'powerline)
(powerline-default-theme)

;; add xcscope
(require 'xcscope)
(cscope-setup)

;; add find-file-in-project
(require 'find-file-in-project)
(global-set-key (kbd "C-x p") 'find-file-in-project)

;; add projectile
(projectile-global-mode)

;; bash-completion
(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook"
)
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete
)

;; go-mode
(require 'go-mode-autoloads)

;; markdown-mode
(autoload 'markdown-mode
  "markdown-mode"
  "Major mode for editing Markdown files"
  t
)
(add-to-list 'auto-mode-alist
  '("\\.markdown\\'" . markdown-mode)
  '("\\.md\\'" . markdown-mode)
)

;; org-mode
(add-to-list 'auto-mode-alist
  '("\\.org\\'" . org-mode)
  '("\\.om\\'" . org-mode)
)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist
  '("\\.yaml$" . yaml-mode)
  '("\\.yml$" . yaml-mode)
)

;; company-c-headers
(add-hook 'c-mode-hook
  (add-to-list 'company-backends 'company-c-headers)
)

;; ## set theme for OS X Emacs.app
(defun set-osx-theme ()
  ;; get env variables from shell
  (exec-path-from-shell-copy-env "HOME")
  (exec-path-from-shell-copy-env "USER")
  (exec-path-from-shell-initialize)

  ;; set theme
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(custom-enabled-themes (quote (wombat)))
   '(custom-safe-themes (quote ("3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" "282606e51ef2811142af5068bd6694b7cf643b27d63666868bc97d04422318c1" default)))
   '(fci-rule-color "#383838")
   '(vc-annotate-background "#2B2B2B")
   '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
   '(vc-annotate-very-old-color "#DC8CC3"))

  ;; set font faces
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Droid_Sans_Mono_for_Powerline")))))

  ;; set pwd
  (setq command-line-default-directory "~/")
)

;; set theme for Emacs in X
(defun set-x-theme ()
  ;; set frame size
  (set-frame-size (selected-frame) 80 30)

  ;; set theme
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(custom-enabled-themes (quote (wombat))))

  ;; set faces
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 121 :width normal :foundry "monotype" :family "Monospace")))))
)

;; set theme for terminal
(defun set-term-theme ()  
  ;; set font faces for OS X
  (case system-type 
    ('darwin (set-frame-font "Courier 12" nil t))
  )
)

;; set env by runtime
(case window-system
  ('ns (set-osx-theme))
  ('x (set-x-theme))
  ('nil (set-term-theme))
)

;; set eshell default
(setq explicit-shell-file-name "/usr/local/bin/fish")
(setq shell-file-name "fish")
(setq explicit-fish-args '("--login"))
(setenv "SHELL" shell-file-name)
