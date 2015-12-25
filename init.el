;; == set standard indent ==
(setq standard-indent 2)
;; == set default indent to use spaces ==
(setq indent-tabs-mode nil)
;; == show line number ==
(line-number-mode 1)
;; == show column number ==
(column-number-mode 1)
;; == syntax highlight mode ==
(global-font-lock-mode t)
;; == search highlight ==
(setq search-highlight t)
;; == highlight parentheses ==
(show-paren-mode 1)
;; == no tab ==
(setq-default indent-tabs-mode nil)

;; == let backup files saved in emacs.d/backup ==
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying 1    ; Don't delink hardlinks
  version-control 1      ; Use version numbers on backups
  delete-old-versions 1  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
)

;; == add ELPA, MELPA package management ==
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
  (add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize)
)

;; == add ~/.emacs.d/ into load-path
(add-to-list 'load-path 
  (expand-file-name "lisp" user-emacs-directory))

;; == start company-mode (auto-complete) ==
(when (require 'company nil :noerror)
  (global-company-mode t))
;; == start auto-complete-mode
(when (require 'auto-complete nil :noerror)
  (global-auto-complete-mode t))

;; == define default packages ==
(defvar default-packages '(org
                           find-file-in-project
                           auto-complete
                           company
                           smex
                           powerline
                           ac-c-headers
                           json-mode
                           yaml-mode
                           markdown-mode
                           ac-c-headers
                           json-mode
                           yaml-mode
                           markdown-mode
                           go-mode
                           bash-completion
                           exec-path-from-shell
                           expand-region
                           erlang
                           xcscope)
  "Default packages")

;; == check and install default packages ==
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

;;set font family for osx
(if (eq system-type 'darwin)
  (set-frame-font "Courier 12" nil t)
)

;;copy PATH from SHELL to exec-path when in osx
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;;add smex
(when (require 'smex nil :noerror)
    (smex-initialize)
    (global-set-key (kbd "M-x") 'smex))

;;add expand-region
(when (require 'expand-region nil :noerror)
    (global-set-key (kbd "C-=") 'er/expand-region))

;;add powerline
(when (require 'powerline nil :noerror)
    (powerline-default-theme))

;;add xcscope
(when (require 'xcscope)
    (cscope-setup))

;; erlang mode
(require 'erlang-start)

;; find-file-in-project
(when (require 'find-file-in-project nil :noerror)
    (global-set-key (kbd "C-x p") 'find-file-in-project))

;; set only for OS X Emacs.app
(defun set-osx-theme ()
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
  (setq command-line-default-directory "/Users/jcfrank7")
)

;; set for Emacs in X
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

;; set env by runtime
(case window-system
  ('ns (set-osx-theme))
  ('x (set-x-theme))
)
