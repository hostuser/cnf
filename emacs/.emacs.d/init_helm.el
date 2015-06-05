; package --- Summary

;;; Commentary:
;;; helm config

;;; Code:
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
		(global-set-key (kbd "M-y") 'helm-show-kill-ring)

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
    (helm-mode 0)
;;    (global-set-key (kbd "M-x") 'helm-M-x)
		(image-dired-display-image-mode)
    )
  :bind
  ("C-\\" . my-helm)
  )


(provide 'init_helm)
;;; init_helm.el ends here
