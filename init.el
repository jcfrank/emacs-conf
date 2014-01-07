;; == set standard indent ==
(setq standard-indent 4)
;; == set default indent to use spaces ==
(setq indent-tabs-mode nil)
;; == show line number ==
(setq line-number-mode 1)
;; == show column number ==
;; (column-number-mode 1)
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
;; == add ELPA package management ==
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
)
;; == start company-mode (auto-complete) ==
(when (require 'company nil :noerror)
  (global-company-mode t))
;; == start auto-complete-mode
(when (require 'auto-complete nil :noerror)
  (global-auto-complete-mode t))
;; == define default packages ==
(defvar jcfrank/packages '(starter-kit
                           starter-kit-bindings
                           starter-kit-ruby
                           ac-c-headers
                           company
                           company-go
                           erlang
                           json-mode
                           scala-mode2
                           yaml-mode
                           markdown-mode
                           auto-complete
                           go-mode) 
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
