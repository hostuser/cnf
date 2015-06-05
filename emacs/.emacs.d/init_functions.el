; package --- Summary

;;; Commentary:
;;; custom functions

;;; Code:

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(defun org-mode-is-intrusive ()
	;; Make something work in org-mode:
	;; (local-unset-key (kbd "something I use"))
	(local-unset-key (kbd "C-'"))
	)
(add-hook 'org-mode-hook 'org-mode-is-intrusive)

(provide 'init_functions)
;;; init_functions.el ends here
