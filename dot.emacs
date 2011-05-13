;; -*-emacs-lisp-*-

(require 'cl)

(defun disable (symbols)
  (mapcar (lambda (symbol)
	    (when (fboundp symbol)
	      (funcall symbol 'nil)))
	  symbols))

(let ((to-disable '(tool-bar-mode scroll-bar-mode)))
  (progn 
    (unless (eq window-system 'ns)
      (add-to-list 'to-disable 'menu-bar-mode))
    (disable to-disable)))

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

(add-to-list 'load-path "~/elisp/scala-mode")
(require 'scala-mode-auto)

(add-to-list 'load-path "~/elisp/tuareg-mode")
(setq auto-mode-alist
      (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" "Run the Caml debugger." t)

(load "~/elisp/haskellmode-emacs/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Stolen, shamelessly from Erik

(global-set-key [(control c)(\#)] 'toggle-comment-current-line-or-region)
(global-set-key [(meta z)] 'repeat)
(global-set-key [(meta g)] 'goto-lne)
(global-set-key [(control c)(g)] 'goto-line)
(global-set-key [(control c)(control g)] 'goto-line)

(defun bind-autoindent ()
  (local-set-key "\r" 'indent-newline-and-indent))

(defun indent-if-blank ()
  (interactive "*")
  (progn
    (flet ((error (&REST) nil))
      (indent-according-to-mode))
    (delete-trailing-whitespace-this-line)))

(defun indent-newline-and-indent ()
  (interactive "*")
  (when (not buffer-read-only)
    (indent-if-blank))
  (newline-and-indent))

(defun start-of-current-line-or-region ()
  "Return the point at the start of the current line or region."
  (save-excursion
    (if mark-active
        (progn
          (goto-char (region-beginning))
          (point-at-bol))
      (point-at-bol))))

(defun end-of-current-line-or-region ()
  "Return the point at the end of the current line or region."
  (save-excursion
    (if mark-active
        (progn
          (goto-char (region-end))
          (point-at-eol))
      (point-at-eol))))

(defun delete-trailing-whitespace-this-line ()
  "Delete trailing whitespace on the current line."
  (interactive "*")
  (save-excursion
    (goto-char (point-at-eol))
    (if (re-search-backward "\\S-\\s-+$" (point-at-bol) t)
        (delete-region (+ (match-beginning 0) 1)(point-at-eol)))))

(defun delete-trailing-whitespace-this-region ()
  "Delete trailing whitespace on the current line or region."
  (interactive "*")
  (save-excursion
    (goto-char (end-of-current-line-or-region))
    (let ((sor (start-of-current-line-or-region)))
      (while (and (> (point) sor)
                  (> (point) (point-min)))
        (delete-trailing-whitespace-this-line)
        (if (> (line-beginning-position) (point-min))
            (previous-line 1)
          (goto-char (point-min)))))))

(defun comment-current-line-or-region () (interactive)
  "Comment current line or region."
  (progn
    (comment-region (start-of-current-line-or-region)
                    (end-of-current-line-or-region))
    (end-of-current-line-or-region)
    (indent-region (start-of-current-line-or-region)
                   (end-of-current-line-or-region)
                   nil)))

(defun uncomment-current-line-or-region () (interactive)
  "Uncomment current line or region."
  (progn
    (comment-region (start-of-current-line-or-region)
                    (end-of-current-line-or-region) -2)
    (end-of-current-line-or-region)
    (indent-region (start-of-current-line-or-region)
                   (end-of-current-line-or-region)
                   nil)
    (delete-trailing-whitespace-this-region)))

(defun toggle-comment-current-line-or-region (arg)
  "Toggle comment on current line."
  (interactive "*P")
  (let ((beg (start-of-current-line-or-region))
        (end (end-of-current-line-or-region)))
    ;; if there already is a comment
    (if (save-excursion
          (goto-char beg)
          (Forward-comment 1)
          (<= end (point)))
        ;; kill it
        (uncomment-current-line-or-region)
      ;; otherwise make one
      (comment-current-line-or-region))))

(add-hook 'c-mode-common-hook 'bind-autoindent)
(add-hook 'python-mode-hook 'bind-autoindent)
(add-hook 'tuareg-mode-hook 'bind-autoindent)

(add-to-list 'load-path "~/elisp/color-theme-6.6.0")
(require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-hober)))

;; minor flymake configuration


(require 'flymake)

(add-to-list 
 'flymake-allowed-file-name-masks 
 '("\\.hpp$" flymake-master-make-header-init flymake-master-cleanup))

(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

(let ((local-init-file "~/.emacs.local"))
  (when (file-readable-p local-init-file)
    (load-file local-init-file)))