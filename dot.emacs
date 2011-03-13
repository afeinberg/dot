;; -*-emacs-lisp-*-

(require 'cl)

(defun disable (symbols)
  (mapcar (lambda (symbol)
	    (when (fboundp symbol)
	      (funcall symbol 'nil)))
	  symbols))

(disable '(tool-bar-mode scroll-bar-mode))

(setq user-mail-address "alex@strlen.net"
      browse-url-mozilla-program "firefox"
    
      line-number-mode 1
      column-number-mode 1
      
      backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      
      visible-bell t)

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)

(show-paren-mode t)

(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode 't))

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'tramp)
(setq tramp-default-method "scpx")

(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(require 'ido)
(ido-mode)

(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/magit")

(require 'magit)

(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "~/elisp/slime")
(require 'slime)

(eval-after-load "slime"
  '(progn
     (add-to-list 'load-path "~/elisp/slime/contrib")
     (require 'slime-fancy)
     (require 'slime-banner)
     (require 'slime-asdf)
     (slime-banner-init)
     (slime-asdf-init)
     (setq slime-complete-symbol*-fancy t)
     (slime-setup '(slime-repl))))


(add-to-list 'load-path "~/elisp/clojure-mode")
(require 'clojure-mode)
;; (require 'clojure-test-mode)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)