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


