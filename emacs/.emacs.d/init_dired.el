; package --- Summary

;;; Commentary:
;;; Dired settings

;;; Code:

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

;; spelling, setq-default ispell-program-name "aspell"
(setq ispell-personal-dictionary "~/config/flydict/dict"
      ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))


(provide 'init_dired)
;;; init_dired.el ends here
