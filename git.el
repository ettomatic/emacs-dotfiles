;;; gutter

(global-git-gutter-mode t)
(custom-set-variables
 '(git-gutter:modified-sign "~")
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-"))
(custom-set-variables
 '(git-gutter:hide-gutter t))
(custom-set-variables '(git-gutter:lighter " GG"))
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

(global-set-key (kbd "C-x <down>") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x <up>") 'git-gutter:previous-hunk)

;;; magit

;; Disable the `highlight` face that Magit uses to highlight diffs. It's unreadable with my color scheme.
;; An unreadable highlight face is a common issue on the Magit tracker.'
(defun disable-magit-highlight-in-buffer () (face-remap-add-relative 'magit-item-highlight '()))
(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)
(add-hook 'magit-commit-mode-hook 'disable-magit-highlight-in-buffer)
(add-hook 'magit-diff-mode-hook 'disable-magit-highlight-in-buffer)

(defun git-pull ()
  (interactive)
  (with-magit-output-buffer 'magit-pull))
(defun git-push ()
  (interactive)
  (with-magit-output-buffer 'magit-push))

;; Have Magit open in the current window, not a new split.
(setq magit-status-buffer-switch-function 'switch-to-buffer)

(setq magit-last-seen-setup-instructions "1.4.0")
