;;; init.el --- Emacs config
;;; Commentary:
;;; Markus' Emacs config file
;;; Code:

;;;; stuff to do before everything else
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;;; check that a few select packages are always available, the others are installed by 'use-package' (https://github.com/jwiegley/use-package)

;;;; having common lisp is always good, no?
(require 'cl)

(add-to-list 'load-path "/home/markus/.emacs.d/lisp/use-package/")
(require 'use-package)
(require 'bind-key)

;;;; General config

;; personal details
(setq user-full-name "Markus Binsteiner"
      user-mail-address "m@ilmark.us")

(desktop-save-mode 1)

;; exec-path: leiningen
(add-to-list 'exec-path "/opt/lein")

;; store backups in a common folder, do version control, don't delete anything
(setq backup-directory-alist '(("." . "~/.backups/emacs")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; disable graphical help and stuff when on X
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; never suspend 
(global-set-key "\C-x\C-z" nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key "\C-z" nil)
(global-set-key (kbd "C-z") nil)
(put 'suspend-frame 'disabled t)

;; columns and line numbers
(column-number-mode 1)

;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; use Ctrl-i to cycle windows
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(global-set-key (kbd "H-i") 'other-window)


;; In-/decrease text size
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

;; tab-width
(setq-default tab-width 2)

;; dired
(use-package dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;; use target in other buffer, if available
(setq dired-dwim-target t)
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")
(setq dired-listing-switches "--group-directories-first -alh")
;; show symlinks
(setq dired-details-hide-link-targets nil)
(setq-default dired-omit-mode t)
(define-key dired-mode-map (kbd "C-x o") 'dired-omit-mode)

;; spelling(setq-default ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/config/flydict/dict"
      ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

;; show paren by default
(show-paren-mode 1)

;; rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;; Package configuration

;; smart-mode-line
(use-package smart-mode-line
	:ensure smart-mode-line
	:init
	(sml/setup)
	(sml/apply-theme 'automatic)
	)

;; expand-region
(use-package expand-region
	:ensure expand-region
	:init
	(global-set-key (kbd "C-=") 'er/expand-region)
	)

(use-package change-inner
	:ensure change-inner
	:init
	(global-set-key (kbd "M-i") 'change-inner)
	(global-set-key (kbd "M-o") 'change-outer)
	)

;; org-mode
(use-package org
  :init
  (setq org-startup-folded nil))

;; flycheck
(use-package flycheck
	:ensure flycheck
	:init
	(add-hook 'after-init-hook #'global-flycheck-mode)
	)

;; gnus
(setq gnus-select-method '(nntp "tweaknet" 
																(nntp-address "news.tweaknews.eu")))
(setq gnus-secondary-select-methods '((nnimap "localhost"
																							(nnimap-stream shell)))
      nnimap-shell-program "/usr/lib/dovecot/imap")

;; company-mode
(use-package company
  :ensure company
  :init
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
		(setq company-global-modes
					'(not python-mode))
		(setq company-minimum-prefix-length 2)
		(setq company-idle-delay 0)
		(setq company-show-numbers t))
 	:config
	(progn
	  ;; having to use M-p/M-n is annoying
		(define-key company-active-map (kbd "C-p") 'company-select-previous) 
		(define-key company-active-map (kbd "C-n") 'company-select-next) 
		)
	)

;; we need auto-complete for some modes (jedi, mostly)
(use-package auto-complete
	:ensure auto-complete
	:init
	(progn
		(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
		(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)))

;; jedi
(use-package jedi
	:ensure jedi
	:init
	(progn
		(setq jedi:setup-keys t)
		(autoload 'jedi:setup "jedi" nil t)
		(add-hook 'python-mode-hook 'jedi:setup)
		(setq jedi:complete-on-dot t)))

;; dired+
(use-package dired+
	:ensure dired+
	:init
  (progn
		(toggle-diredp-find-file-reuse-dir t)
		(add-hook 'dired-mode-hook
							(lambda ()
								(define-key dired-mode-map (kbd "^")
									(lambda () (interactive) (find-alternate-file "..")))
								))
		)
	)

(use-package dired-details
	:ensure dired-details
	)

(use-package dired-details+
	:ensure dired-details+)

;; clojure-mode
(use-package clojure-mode
  :ensure clojure-mode
	;;  :init
	;;  (add-hook 'clojure-mode-hook 'paredit-mode)
	)

;; paredit
;;(use-package paredit
;;	:ensure paredit
;;	:config
;;	(define-key paredit-mode-map (kbd "S-<return>") 'paredit-newline)
;;)

;; smartparens
;; doc: https://github.com/Fuco1/smartparens/wiki
(use-package smartparens
  :ensure smartparens
  :init 
  (smartparens-global-mode)
	)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :init
  (require 'smartparens-config)
	)

;; rainbow-mode
(use-package rainbow-mode
  :ensure rainbow-mode
	)

;; flyspell
(eval-after-load "ispell"
  '(add-to-list 'ispell-local-dictionary-alist
                '("deutsch8"
									"[a-zA-ZäöüßÄÖÜ]" "[^a-zA-ZäöüßÄÖÜ]" "[']" t
                  ("-C" "-d" "de_DE-neu.multi")
                  "~latin1" iso-8859-1)))

(defun fd-switch-dictionary()
	(interactive)
	(let* ((dic ispell-current-dictionary)
				 (change (if (string= dic "deutsch8") "english" "deutsch8")))
		(ispell-change-dictionary change)
		(message "Dictionary switched from %s to %s" dic change)
		))

(use-package flyspell
  :defer t
  :config
  (define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
  (define-key flyspell-mode-map (kbd "M-.") 'ispell-word)
	(define-key flyspell-mode-map (kbd "<f8>") 'fd-switch-dictionary)
  :init
  (progn
    (setq-default ispell-program-name "aspell")
    (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
    (add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))))


;; helm
(defun my-helm ()
  (interactive)
  (helm-other-buffer
   (append

		(if (projectile-project-p)
				'(helm-source-projectile-buffers-list)
			'())

		'(helm-c-source-buffers-list
			helm-c-source-recentf)

		(if (projectile-project-p)
				'(helm-source-projectile-files-list
					helm-source-projectile-projects)
			'())

		'(helm-c-source-locate
			helm-c-source-buffer-not-found
			helm-c-source-file-name-history
			helm-c-source-info-pages)
		)
	 helm-buffer ; " *my-helm*"
	 ))

(use-package helm
  :ensure helm
  :init
  (progn 
    (require 'helm-config) 
		(require 'helm-grep)
		;; use C-c h instead of C-x c
		(global-set-key (kbd "C-c h") 'helm-command-prefix)
		(global-unset-key (kbd "C-x c"))

		(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
		(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
		(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (setq helm-candidate-number-limit 10)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-quick-update t ; do not display invisible candidates
					helm-split-window-in-side-p t ; open helm buffer inside current window, not occupy whole other window
					helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non--nil
					helm-move-to-line-cycle-in-source nil ; move to end or beginning of source when reaching top or bottom of source.
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode 1)
		(global-set-key (kbd "M-x") 'helm-M-x)
		)
  :bind 
	("C-\\" . my-helm)
	)

;; helm-swoop
(use-package helm-swoop
  :ensure helm-swoop
  :bind (("C-s" . helm-swoop)))

;; projectile
(use-package projectile
	:ensure projectile
	:init
	(progn
		(setq projectile-remember-window-configs t)
		(projectile-global-mode)))

(use-package helm-projectile
	:ensure helm-projectile
	)

;; move between windows
(use-package windmove
  :bind
  (("<f2> l" . windmove-right)
   ("<f2> h" . windmove-left)
   ("<f2> k" . windmove-up)
   ("<f2> j" . windmove-down)))

;; undo-tree
(use-package undo-tree
  :ensure undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; guide-key
(use-package guide-key
  :ensure guide-key
  :init
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1))  ; Enable guide-key-mode

;; cider
(use-package cider
	:ensure cider
  :config
  (progn
	  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
		(add-to-list 'same-window-buffer-names "<em>nrepl</em>")
		;; don't really need to navigate in that buffer
		(define-key cider-repl-mode-map (kbd "C-p") 'cider-repl-previous-input) 
		(define-key cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input) 
		)
	)

;;;; themes
;;(use-package gruvbox-theme
;;	:ensure gruvbox-theme
;;)
;;(use-package zenburn-theme
;;	:ensure zenburn-theme
;;)
;; (use-package anti-zenburn-theme
;;  	:ensure anti-zenburn-theme
;;  )
																				;(use-package noctilux-theme
																				;  :ensure noctilux-theme
																				;	)
(use-package color-theme
	:ensure color-theme)
(use-package color-theme-solarized
	:ensure color-theme-solarized
	:init
	(color-theme-solarized-light)
	)
;;(use-package leuven-theme
;;	:ensure leuven-theme)

(provide 'init)
;;; init.el ends here


