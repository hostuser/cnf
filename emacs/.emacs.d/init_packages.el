;;; package --- Summary

;;; Commentary:
;;; External packages & their configuration.

;;; Code:

;;;; helm
(load-user-file "init_helm.el")


;;;; swiper
(use-package swiper
	:ensure swiper
	:init
	(progn
		(ivy-mode t)
		(setq ivy-use-virtual-buffers t)
		(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
		(global-set-key (kbd "C-s") 'swiper)
		(global-set-key (kbd "C-r") 'swiper)
		(global-set-key (kbd "C-c C-r") 'ivy-resume)
		(global-set-key [f6] 'ivy-resume)
		(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-kill-word) ;; do it like helm does it
		))

;;;; projectile
(use-package projectile
	:ensure projectile
	:init
	(progn
		(setq projectile-completion-system 'ivy)
		(projectile-global-mode))
	)


;;;; helm-projectile
(use-package helm-projectile
  :ensure helm-projectile
	)


;;;; magit
(use-package magit
  :ensure magit
	:init
	(progn
		(setq magit-last-seen-setup-instructions "1.4.0")
		(global-set-key (kbd "C-c g s") 'magit-status))
	)

;;;; iedit
(use-package iedit
  :ensure iedit
	:init
	(global-set-key (kbd "C-(") 'iedit-mode)
	;; (progn
	;; (define-key global-map (kbd "C-;") (kbd "C-("))
	;; (define-key isearch-mode-map (kbd "C-;") (kbd "C-("))
	;; (define-key helm-map (kbd "C-;") (kbd "C-("))
	;; (define-key esc-map (kbd "C-;") (kbd "C-("))
	)

;;;; lispy
;; (use-package lispy
  ;; :ensure lispy
	;; :init
	;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))


;;;; yasnippet

(use-package yasnippet
	:ensure yasnippet
	:diminish yas-minor-mode
	:defer t
	:init (yas-global-mode t))



;;;; hydra
(use-package hydra
	:ensure hydra
	:init
	(load-user-file "init_hydra.el")
 )

;;;; whole-line-or-region
;; using "C-w" to kill a whole line if there is no active region
(use-package whole-line-or-region
	:ensure whole-line-or-region
	:diminish whole-line-or-region-minor-mode
	:init (whole-line-or-region-mode)
)


;; (use-package eyebrowse
  ;; :ensure eyebrowse
	;; :init (eyebrowse-mode t)
	;; )


;;;; workgroups2
;; (use-package workgroups2
  ;; :ensure workgroups2
	;; :init (workgroups-mode 1)
	;; )



;;;; clean-aindent-mode
(use-package clean-aindent-mode
  :ensure clean-aindent-mode
	:init (add-hook 'prog-mode-hook 'clean-aindent-mode)
	)


;;;; undo-tree
(use-package undo-tree
  :ensure undo-tree
	:init (global-undo-tree-mode)
	)

;;;; elpy
(use-package elpy
  :ensure elpy
	:init	
	(progn
		(elpy-enable)
		(elpy-use-ipython)
		;; (setq elpy-rpc-backend "jedi")
		(setq python-shell-interpreter "ipython"
					python-shell-interpreter-args "--profile=dev")))


;;;; anaconda-mode
(use-package anaconda-mode
  :ensure anaconda-mode
	:init (add-hook 'python-mode-hook 'anaconda-mode)
	)

;;;; company
(use-package company
  :ensure company
	:init
	(progn
		(add-hook 'after-init-hook 'global-company-mode)
    (setq company-global-modes
          '(not python-mode))
    (setq company-minimum-prefix-length 2)
    (setq company-idle-delay 0)
    (setq company-show-numbers t)
)
	:config
	(progn
		;; having to use M-p/M-n is annoying
		(define-key company-active-map (kbd "C-p") 'company-select-previous)
		(define-key company-active-map (kbd "C-n") 'company-select-next)
		) 
	)

;;;; company-anaconda
(use-package company-anaconda
  :ensure company-anaconda
	:init
	(add-to-list 'company-backends 'company-anaconda)
	)


;;;; dockerfile-mode
(use-package dockerfile-mode
  :ensure dockerfile-mode
	)
;;;; yaml-mode

(use-package yaml-mode
  :ensure yaml-mode
	)


;;;; flycheck
(use-package flycheck
  :ensure flycheck
	:init (add-hook 'after-init-hook #'global-flycheck-mode)
	)

;;;; flyspell
;; (eval-after-load "ispell"
;;   '(add-to-list 'ispell-local-dictionary-alist
;;                 '("deutsch8"
;;                   "[a-zA-ZäöüßÄÖÜ]" "[^a-zA-ZäöüßÄÖÜ]" "[']" t
;;                   ("-C" "-d" "de_DE-neu.multi")
;;                   "~latin1" iso-8859-1)))
;; (use-package flyspell
;;   :config
;;   :init
;;   (progn
;;     (setq-default ispell-program-name "aspell")
;;     (define-key flyspell-mode-map (kbd "C-.") 'nil)
;;     (define-key flyspell-mode-map (kbd "C-;") 'nil)
;;     (define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
;;     (define-key flyspell-mode-map (kbd "M-.") 'ispell-word)
;;     (define-key flyspell-mode-map (kbd "<f8>") 'fd-switch-dictionary)
;;     (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
;;     (add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))))


;;;; flycheck-tip
;; (use-package flycheck-tip
  ;; :ensure flycheck-tip
	;; :init (flycheck-tip-use-timer 'verbose)
	;; )


;;;; golden-ratio
;; (use-package golden-ratio
;;   :ensure golden-ratio
;; 	:init
;; 	(progn
;; 		(require 'golden-ratio)

;; 		(add-to-list 'golden-ratio-exclude-modes "ediff-mode")
;; 		(add-to-list 'golden-ratio-exclude-modes "helm-mode")
;; 		(add-to-list 'golden-ratio-exclude-modes "dired-mode")
;; 		(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; 		(defun pl/helm-alive-p ()
;; 			(if (boundp 'helm-alive-p)
;; 					(symbol-value 'helm-alive-p)))

;; 		;; do not enable golden-raio in thses modes
;; 		(setq golden-ratio-exclude-modes '("ediff-mode"
;; 																			 "gud-mode"
;; 																			 "gdb-locals-mode"
;; 																			 "gdb-registers-mode"
;; 																			 "gdb-breakpoints-mode"
;; 																			 "gdb-threads-mode"
;; 																			 "gdb-frames-mode"
;; 																			 "gdb-inferior-io-mode"
;; 																			 "gud-mode"
;; 																			 "gdb-inferior-io-mode"
;; 																			 "gdb-disassembly-mode"
;; 																			 "gdb-memory-mode"
;; 																			 "magit-log-mode"
;; 																			 "magit-reflog-mode"
;; 																			 "magit-status-mode"
;; 																			 "IELM"
;; 																			 "eshell-mode" "dired-mode"))
;; 		(golden-ratio-mode))
;; 	)

;;;; csv-mode
(use-package csv-mode
  :ensure csv-mode
	)

;;;; jedi
;; (use-package jedi
  ;; :ensure jedi
	;; :init
	;; (progn
		;; (add-hook 'python-mode-hook 'jedi:setup)
		;; (setq jedi:complete-on-dot t)
		;; ))


;;;; multiple-cursors
(use-package multiple-cursors
  :ensure multiple-cursors
	:init 
	)


;;;; Avy
(use-package avy
	:ensure avy
	:init
	(progn
		(global-set-key (kbd "C-c c") 'avy-goto-char)
		(global-set-key (kbd "C-c l") 'avy-goto-line)
		(global-set-key (kbd "C-c w") 'avy-goto-word-0)
		(global-set-key (kbd "C-;") 'avy-goto-char)
		(global-set-key (kbd "C-,") 'avy-goto-line)
		;; (global-set-key (kbd "C-z") 'avy-goto-word-0) -> done in init_keys.el
		))

;;;; ace-window
(use-package ace-window
	:ensure ace-window
	:init
	(progn
		(global-set-key (kbd "M-[") 'ace-window)
		(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
		))

;;;; rainbow-delimiters
(use-package rainbow-delimiters
	 :ensure rainbow-delimiters
	 :init
	 (progn
		 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
		 ;; use more bold colours
		 (require 'cl-lib)
		 (require 'color)
		 ;; (cl-loop for index from 1 to
			;; rainbow-delimiters-max-face-count do
			;; (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
				;; (cl-callf color-saturate-name (face-foreground face) 30)))
		 ;; display unmatched parens in red
		 ;; (require 'paren) ; show-paren-mismatch is defined in paren.el
		 ;; (set-face-attribute 'rainbow-delimiters-unmatched-face nil
												 ;; :foreground 'unspecified
		 ;; :inherit 'show-paren-mismatch)))
		 ))

;;;; smartparens
(use-package smartparens
	:ensure smartparens
	:init
	(progn
		(require 'smartparens-config)
		(smartparens-global-mode t)
		(setq sp-base-key-bindings 'smartparens)
		(setq sp-autoskip-closing-pair 'always)
		(setq sp-hybrid-kill-entire-symbol nil)
		;; custom keys
		(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
		(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

		(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
		(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
		(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
		(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

		(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
		(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
		(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

		(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
		(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

		(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
		(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

		(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
		(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
		(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
		(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)))
	
;;;; theme
	(use-package color-theme-sanityinc-solarized
		:ensure color-theme-sanityinc-solarized
		:init
		(load-theme 'sanityinc-solarized-light t)
		)


(provide 'init_packages)
;;; init_packages.el ends here
