(if (equal system-type'darwin)
    (progn
       ;;(setq mac-command-modifier 'super)
       (setq mac-option-modifier 'alt)
       (setq mac-command-modifier 'meta)
       (setq multi-term-program "/usr/local/bin/zsh")
       (setq ns-use-srgb-colorspace t)
       ;; Fix for shift up = <select> is undefined for windmove
       ;(define-key input-decode-map "\e[1;2A" [S-up])
       (define-key input-decode-map "\e[1;4A" [M-up])
       (define-key input-decode-map "\e[1;4B" [M-down])
       ))
