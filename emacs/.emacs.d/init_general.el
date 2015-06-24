; package --- Summary

;;; Commentary:
;;; General settings and config

;;; Code:

;; personal details
(desktop-save-mode 1)

;; store backups in a common folder, do version control, don't delete anything
(setq backup-directory-alist '(("." . "~/.backups/emacs")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; disable graphical help and stuff when on X
(tooltip-mode -1)
;;(tool-bar-mode -1)
;;(menu-bar-mode -1)
;;(scroll-bar-mode -1)

;; tooltips
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; font
(set-default-font "Inconsolata-11")

;; show-paren-mode
(show-paren-mode t)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; columns and line numbers
(column-number-mode 1)

;; always add final new-line
(setq require-final-newline t)

;; auto compress/uncompress comressed files
(auto-compression-mode t)

;; don't need no blinking cursor
(blink-cursor-mode -1)

;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; http://www.masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

(setq delete-by-moving-to-trash t)

;; enable system clipboard
(setq x-select-enable-clipboard t)

;; transient-mark-mode
(transient-mark-mode t)

;; line numbers
(add-hook 'prog-mode-hook 'linum-mode)

;; winner-mode
(winner-mode t)

;; world time zones I'm interested in (helm-world-time)
(setq display-time-world-list '(("Europe/Berlin" "Munich")
                                ("Australia/Melbourne" "Melbourne")
                                ("Asia/Shanghai" "China")
                                ))

;; tab-width
(setq-default tab-width 2)

;; imenu
(setq imenu-auto-rescan t)
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;; buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;; to investigate

;; does give an error when doing "C-x b" otherwise
(setq ido-use-faces nil)

(provide 'init_general)
;;; init_general.el ends here
