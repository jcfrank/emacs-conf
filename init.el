
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
  (add-to-list
   'package-archives
   '("elpa" . "http://elpa.gnu.org/packages/")) ; gnu
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t)
  (add-to-list
   'package-archives
   '("marmalade" . "http://marmalade-repo.org/packages/")
   t)
  (add-to-list
   'package-archives
   '("org" . "http://orgmode.org/elpa/")
   t) ; for org mode
  (package-initialize)
)

;; add ~/.emacs.d/ into load-path
(add-to-list 'load-path 
  (expand-file-name "lisp" user-emacs-directory))

;; define default packages
(defvar default-packages 
  '(
    async
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
    yaml-mode
    flx-ido
    erlang
  )
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

;; helm
(require 'helm)
(setq helm-M-x-fuzzy-match t)

;; add expand-region
(require 'expand-region)

;; add powerline
(require 'powerline)
(powerline-default-theme)

;; add xcscope
(require 'xcscope)
(cscope-setup)

;; add find-file-in-project
;; key bindings were moved to a general area
(require 'find-file-in-project)

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
  '("\\.markdown$" . markdown-mode)
  '("\\.md$" . markdown-mode)
)

;; org-mode
(require 'org)
(add-to-list 'auto-mode-alist
  '("\\.org$" . org-mode)
  '("\\.om$" . org-mode)
)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist
  '("\\.yaml$" . yaml-mode)
  '("\\.yml$" . yaml-mode)
)

;; company-c-headers
(require 'company-c-headers)
(add-hook 'c-mode-common-hook
  (lambda ()
    (add-to-list 'company-backends 'company-c-headers)
  )
)

;; flx-ido for fuzzy ido
(require 'flx-ido)
(ido-mode 1)
;(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; erlang mode
(add-hook 'erlang-mode-hook
  (lambda ()
    (require 'erlang-start)
  )
)

;; auto pair
(electric-pair-mode t)

;; key-bindings
;(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c s") 'helm-occur)
(global-set-key (kbd "C-=") 'er/expand-region)

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
   '(ansi-color-faces-vector
     [default bold shadow italic underline bold bold-italic bold])
   '(custom-enabled-themes (quote (misterioso)))
   '(custom-safe-themes
     (quote
      ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
   '(fci-rule-color "#eee8d5")
   '(vc-annotate-background nil)
   '(vc-annotate-color-map
     (quote
      ((20 . "#dc322f")
       (40 . "#cb4b16")
       (60 . "#b58900")
       (80 . "#859900")
       (100 . "#2aa198")
       (120 . "#268bd2")
       (140 . "#d33682")
       (160 . "#6c71c4")
       (180 . "#dc322f")
       (200 . "#cb4b16")
       (220 . "#b58900")
       (240 . "#859900")
       (260 . "#2aa198")
       (280 . "#268bd2")
       (300 . "#d33682")
       (320 . "#6c71c4")
       (340 . "#dc322f")
       (360 . "#cb4b16"))))
   '(vc-annotate-very-old-color nil))

  ;; set font faces
  (custom-set-faces
   ;; custom-set-faces added by Custom.
   '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Droid_Sans_Mono_for_Powerline")))))

  ;; set pwd
  (setq command-line-default-directory "~/")
)

;; set theme for Emacs in X
(defun set-x-theme ()
  ;; set frame size
  (set-frame-size (selected-frame) 80 35)

  ;; set theme
  (custom-set-variables
   ;; custom-set-variables added by Custom.
   '(custom-enabled-themes (quote (wombat))))
)

;; set theme for terminal
(defun set-term-theme ()  
  ;; set font faces for OS X
  (pcase system-type
    (`darwin (set-frame-font "Courier 12" nil t))
  )
)

;; set env by runtime
(pcase window-system
  (`ns (set-osx-theme))
  (`x (set-x-theme))
  (`nil (set-term-theme))
  (unknown (message "Unknonw window system %s!\n" unknown))
)
