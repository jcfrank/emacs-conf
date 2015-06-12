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
;;== highlight parentheses ==
(show-paren-mode 1)
;;== no tab ==
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
(defvar default-packages '(ac-c-headers
                           company
                           json-mode
                           yaml-mode
                           markdown-mode
                           auto-complete
                           go-mode
                           bash-completion
                           exec-path-from-shell
                           smex
                           expand-region
                           powerline
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

