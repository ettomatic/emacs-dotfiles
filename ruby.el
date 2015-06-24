(setq ruby-insert-encoding-magic-comment nil)

(global-set-key (kbd "C-c , '") 'ruby-tools-to-single-quote-string)
(global-set-key (kbd "C-c , \"") 'ruby-tools-to-double-quote-string)

;; rbenv
(global-rbenv-mode)

;; rspec
(setq rspec-use-rake-when-possible nil)

;; The default indentation system attempts to align the arguments of a function
;; with the opening bracket vertically.
(setq ruby-deep-indent-paren nil)

;; Scroll to the first test failure
(setq compilation-scroll-output 'first-error)

;; Easily toggle ruby's hash syntax
(require 'ruby-hash-syntax)