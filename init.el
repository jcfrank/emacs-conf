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
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )
