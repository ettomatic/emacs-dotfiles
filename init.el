(setq prelude-guru nil)
(load-theme 'cyberpunk t)

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
(global-set-key [remap kill-whole-line] 'prelude-kill-whole-line)
(global-set-key [f13] 'prelude-kill-whole-line)
(global-set-key (kbd "C-c , k") 'prelude-kill-whole-line)

;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. remove to end of the line.
   2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<M-RET>") 'newline-without-break-of-line)


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

;; Golden ratio
(require 'golden-ratio)
(golden-ratio-mode 1)

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

;;projectile
(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "C-c p c") 'projectile-persp-switch-project)


(setq deft-extension "txt")
(setq deft-directory (getenv "DEFT"))

;; mysql
(setq sql-mysql-login-params
      '((user :default "root")
        (database :default "")
        (server :default "localhost")
        (port :default 3306)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
