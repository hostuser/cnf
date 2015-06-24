;;; init.el --- Emacs config
;;; Commentary:
;;; Markus' Emacs config file
;;; Code:


(add-to-list 'load-path "/home/markus/.emacs.d/lisp/use-package/")
(add-to-list 'load-path "C:/Users/markus/.emacs.d/lisp/use-package/")

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; first time startup needs to init package list
(when (not package-archive-contents) (package-refresh-contents))

;; helper function to load external init code
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))
;;;; having common lisp is always good, no?
(require 'cl)

(require 'use-package)
(require 'bind-key)

;;;; customize changes
(setq custom-file "~/.emacs.d/init_custom.el")

;;(load custom-file)
;;;; load custom functions
(load-user-file "init_functions.el")

;;;; load general settings
(load-user-file "init_general.el")

;;;; dired settings
(load-user-file "init_dired.el")

;;;; packages
(load-user-file "init_packages.el")

;;;; load custom keybindings
(load-user-file "init_keys.el")



(provide 'init)
;;; init.el ends here
