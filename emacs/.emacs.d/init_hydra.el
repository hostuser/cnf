; package --- Summary

;;; Commentary:
;;; Hydra functions & keybindings

(require 'hydra-examples)

;;; Code:
(defhydra hydra-errors-navigation()
	"navigate errors"
	("n" next-error "next error")
	("p" previous-error "previous error"))


(defhydra hydra-scroll ()
	"scroll"
	("j" (lambda () (interactive) (scroll-up-command 4)) "down")
	("k" (lambda () (interactive) (scroll-down-command 4)) "up")
	("g" beginning-of-buffer "beginning of buffer")
	("G" end-of-buffer "end of buffer")
	("n" next-line "next line")
	("p" previous-line "previous line")
	("q" nil "quit")
)

(defhydra hydra-strings ()
	"string manipulation"
	("r" query-replace "query replace"))

(defun split-vertically-and-move ()
		 "split window vertically and move cursor to newly created window"
		 (interactive)
		 (split-window-vertically)
		 (other-window 1))

(defun split-horizontally-and-move ()
		 "split window horizontally and move cursor to newly created window"
		 (interactive)
		 (split-window-horizontally)
		 (other-window 1))

(defun scroll-other-window-up-line ()
  "Scroll the other window one line up."
  (interactive)
  (scroll-other-window -1))

(defun scroll-other-window-down-line ()
  "Scroll the other window one line down."
  (interactive)
  (scroll-other-window 1))

(defun scroll-window-down-line ()
  "Scroll the window one line down."
  (interactive)
  (scroll-up-command 2))

(defun scroll-window-up-line ()
  "Scroll the window one line up."
  (interactive)
  (scroll-down-command 2))

(defhydra hydra-window ()
	("v" split-vertically-and-move "split vertically and move cursor to new window")
	("y" split-horizontally-and-move "split horizontally and move cursor to new window")
	("C-n" scroll-other-window-down-line "scroll down other window one line")
	("C-p" scroll-other-window-up-line "scroll up other window one line")
	("p" (lambda () (interactive) (scroll-down-command 2)) "scroll down window one line")
	("n" (lambda () (interactive) (scroll-up-command 2)) "scroll up window one line")
	("g" beginning-of-buffer "beginning of buffer")
	("G" end-of-buffer "end of buffer")
	("h" windmove-left "move window left")
	("j" windmove-down "move window down")
	("k" windmove-up "move window up")
	("l" windmove-right "move window right")
	("o" delete-other-windows "delete other windows")
	(";" hydra-move-splitter-left "splitter left")
	("'" hydra-move-splitter-right "splitter right")
	("." hydra-move-splitter-down "splitter down")
	("/" hydra-move-splitter-up "splitter up")
	("i" other-window "other window")
	("M-i" other-window "other window")
  ("\\" helm-mini "helm-mini" :exit t)
  ("f" find-file "find file" :exit t)
	("d" delete-window "delete window")
	("b" winner-undo "winner undo")
	("f" winner-redo "winner redo")
	("C-g" nil "quit")
	("q" nil "quit")
	("SPC" nil "quit")
	("RET" nil "quit")
	)


(defhydra hydra-helm (:hint nil :color pink)
        "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
        ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
        ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
        ("<tab>" helm-keyboard-quit "back" :exit t)
        ("<escape>" nil "quit")
        ("\\" (insert "\\") "\\" :color blue)
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("n" helm-next-source)
        ("p" helm-previous-source)
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ("c" helm-recenter-top-bottom-other-window)
        ("m" helm-toggle-visible-mark)
        ("t" helm-toggle-all-marks)
        ("u" helm-unmark-all)
        ("H" helm-help)
        ("s" helm-buffer-help)
        ("v" helm-execute-persistent-action)
        ("d" helm-persistent-delete-marked)
        ("y" helm-yank-selection)
        ("w" helm-toggle-resplit-and-swap-windows)
        ("f" helm-follow-mode))


;;(global-set-key (kbd "C-M-SPC") 'hydra-scroll/body)
(global-unset-key (kbd "C-;"))
(global-set-key (kbd "C-;") 'hydra-window/body)
(global-set-key (kbd "M-;") 'hydra-window/body)
(global-set-key (kbd "C-c s") 'hydra-strings/body)
;;(global-set-key (kbd "M-g") 'hydra-errors-navigation)

(provide 'init_hydra)
;;; init_hydra.el ends here
