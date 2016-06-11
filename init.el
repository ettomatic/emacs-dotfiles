(setq prelude-guru nil)
(guru-global-mode -1)
(setq guru-warn-only nil)

(menu-bar-mode -1)
;; nice themes:
;; cyberpunk (better on console)
(load-theme 'dracula t)

(global-linum-mode t)

;; When opening a new buffer, don't show the scratch message.
(setq initial-scratch-message "")

;; Require typing only "y" or"n" instead of the full "yes" to confirm destructive actions.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable the prompt we get when killing a buffer with a process. This affects clojure mode in particular,
;; when we want to restart the nrepl process.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Fix for shift up = <select> is undefined for windmove
(define-key input-decode-map "\e[1;2A" [S-up])

;; kill whole line
(global-set-key [remap kill-whole-line] 'crux-kill-whole-line)
(global-set-key [f13] 'crux-kill-whole-line)
(global-set-key (kbd "C-c , k") 'crux-kill-whole-line)

;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. remove to end of the line.
   2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<M-RET>") 'newline-without-break-of-line)

(display-time-mode t)

;;smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; Swiper search
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key [f6] 'ivy-resume)

;; ace window
(global-set-key (kbd "M-p") 'ace-window)

;; Drag stuff
(drag-stuff-mode t)
(drag-stuff-global-mode t)

;; helm
(require 'prelude-helm-everywhere)
(global-set-key (kbd "C-c h") 'helm-mini)
(setq helm-autoresize-max-height 10)

;; list all methods in file using Helm
(global-set-key (kbd "C-c , i") 'helm-imenu)

;; Golden ratio
(require 'golden-ratio)
(golden-ratio-mode 1)
(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-extra-commands 'avy-goto-word-1)
(add-to-list 'golden-ratio-extra-commands 'avy-goto-char)

;; inhibit golden ratio on Helm
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; HideShow mode
(add-hook 'prog-mode-hook 'hs-minor-mode)
(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                 `(ruby-mode
                   ,(rx (or "def" "class" "module" "{" "[")) ; Block start
                   ,(rx (or "}" "]" "end"))                  ; Block end
                   ,(rx (or "#" "=begin"))                   ; Comment start
                   ruby-forward-sexp nil)))

;; Projectile
(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "C-c p c") 'projectile-persp-switch-project)

;; Deft
(setq deft-extension "txt")
(setq deft-directory (getenv "DEFT"))
(setq deft-current-sort-method 'title)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . restclient-mode))

;; mysql
(setq sql-mysql-login-params
      '((user :default "root")
        (database :default "")
        (server :default "localhost")
        (port :default 3306)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; I don't want any delay in suggestions
(setq company-idle-delay 0)

;; hide minor modes from mode line
(rich-minority-mode 1)
(setq rm-whitelist "ruby")

;; a nicer mode line
(setq sml/theme 'respectful)
(sml/setup)

;; code folding
(require 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-c , f") #'vimish-fold)
(global-set-key (kbd "C-c , u") #'vimish-fold-delete)

;; Spelling correction
(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
    With prefix P, create local abbrev. Otherwise it will
    be global.
    If there's nothing wrong with the word at point, keep-lines
    looking for a typo until the beginning of buffer. You cancel-change-group
    skip typos you don't want to fix with `SPC', and you cancel-change-group
    abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (thing-at-point 'word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking-at
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word))
        (setq aft (thing-at-point 'word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev            (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point"))))
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Emacs please try to keep my code always indented
;(global-aggressive-indent-mode 1)


;; Jump to definition
(dumb-jump-mode)
(global-set-key (kbd "C-c , g") 'dumb-jump-go)


;; (set-face-background 'vertical-border "gray")
;; (set-face-foreground 'vertical-border (face-background 'vertical-border))
