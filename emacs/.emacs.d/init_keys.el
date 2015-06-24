; package --- Summary

;;; Commentary:
;;; Custom keybindings

;;; Code:

;; other window functions
(global-set-key (kbd "C-q") 'delete-other-windows)

;; never suspend
(global-set-key "\C-x\C-z" nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key "\C-z" nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-.") 'repeat)
(put 'suspend-frame 'disabled t)
(global-set-key (kbd "C-z") 'avy-goto-word-0)

;; imenu
(global-set-key (kbd "M-i") 'imenu)

;; error navigation
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; search & replace
(global-set-key (kbd "M-C-s") 'query-replace)

;; hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; comment or ucomment line
(global-set-key (kbd "C-M-c") 'comment-or-uncomment-region-or-line)

;; use Ctrl-i to cycle windows
(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'other-window)

;; (keyboard-translate ?\C-m ?\H-m)
;; (global-set-key [?\H-m] 'avy-goto-line))))

;; use M-i to cycle windows
;;(global-set-key (kbd "M-i") 'other-window)
(global-set-key (kbd "C-<tab>") 'other-window)

(provide 'init_keys)
;;; init_keys.el ends here
