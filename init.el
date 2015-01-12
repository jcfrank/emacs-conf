;; == set standard indent ==
(setq standard-indent 4)
;; == set default indent to use spaces ==
(setq indent-tabs-mode nil)
;; == show line number ==
(setq line-number-mode 1)
;; == show column number ==
(setq column-number-mode 1)
;; == syntax highlight mode ==
(setq font-lock-mode 1)
;; == search highlight ==
(setq search-highlight 1)
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
  (package-initialize)
)
;; == start company-mode (auto-complete) ==
(when (require 'company nil :noerror)
  (global-company-mode t))
;; == start auto-complete-mode
(when (require 'auto-complete nil :noerror)
  (global-auto-complete-mode t))
;; == define default packages ==
(defvar jcfrank/packages '(ac-c-headers
                           company
                           erlang
                           edts
                           json-mode
                           yaml-mode
                           markdown-mode
                           auto-complete
                           go-mode
                           bash-completion
                           exec-path-from-shell)
  "Default packages")
;; == check and install default packages ==
(defun jcfrank/packages-installed-p ()
  (setq res t)
  (dolist (pkg jcfrank/packages)
    (when (not (require pkg nil :noerror)) (setq res nil)))
  res)

(unless (jcfrank/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg jcfrank/packages)
    (when (not (require pkg nil :noerror))
      (package-install pkg))))

;;set font family for osx
(if (eq system-type 'darwin)
  (set-frame-font "Courier 14" nil t)
)

;;add after-init-hook to init edts
;;EDTS has a weird bug. If it is just installed, we need to go to
;;"~/.emacs.d/elpa/edts-..../" and run "make".
;;Otherwise there would be server not started error.
(add-hook 'after-init-hook 'init-modes)
(defun init-modes ()
  (require 'edts-start))

;;copy PATH from SHELL to exec-path when in osx
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" default)))
 '(edts-man-root "/Users/jcfrank7/.emacs.d/edts/doc/17.4"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

