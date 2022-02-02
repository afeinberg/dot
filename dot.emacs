;; -*-emacs-lisp-*-

(require 'cl)

(setq user-mail-address "alex@strlen.net"
      line-number-mode 1
      column-number-mode 1
      backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      visible-bell t)

(menu-bar-mode 0)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

(global-linum-mode 1)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)

(defun google-c-lineup-expression-plus-2 (langelem)
  "From Google's C++ style guide

Suitable for inclusion in `c-offsets-alist'."
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-backward-syntactic-ws)
    (back-to-indentation)
    ;; We are making a reasonable assumption that if there is a control
    ;; structure to indent past, it has to be at the beginning of the line.
    (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
        (goto-char (match-end 1)))
    (vector (+ 2 (current-column)))))

(defconst af-c-style
  `((c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-offsets-alist .
                     ((arglist-intro google-c-lineup-expression-plus-2)
                      (func-decl-cont . ++)
                      (member-init-intro . ++)
                      (inher-intro . ++)
                      (comment-intro . 0)
                      (arglist-close . c-lineup-arglist)
                      (topmost-intro . 0)
                      (block-open . 0)
                      (inline-open . 0)
                      (substatement-open . 0)
                      (statement-cont
                       .
                       (,(when (fboundp 'c-no-indent-after-java-annotations)
                           'c-no-indent-after-java-annotations)
                        ,(when (fboundp 'c-lineup-assignments)
                           'c-lineup-assignments)
                        ++))
                      (label . /)
                      (case-label . +)
                      (statement-case-open . +)
                      (statement-case-intro . +)       ; case w/o {
                      (access-label . /)
                      (innamespace . 0)
                      (inclass . +))))
  "My personal C/C++ style")

(defun add-to-load-path-if-exists (file)
  "Add a filen to load path if it exists."
  (if (file-exists-p file)
      (add-to-list 'load-path file)))

(defun af-set-c-style ()
  "Set to my personal C/C++ Style"
  (interactive)
  (c-add-style "AF" af-c-style t))

(defconst af-protobuf-style
     '((c-basic-offset . 2)
       (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "af-protobuf-style" af-protobuf-style t)))

(show-paren-mode t)

(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode 't))

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'tramp nil 'noerror)

(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(require 'ido)
(ido-mode)

(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/smex")

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'ahg)

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
(require 'clojure-test-mode)

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
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" "Run the Caml debugger." t)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

(add-to-list 'load-path "~/elisp/haskell-mode/")
;;(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/elisp/haskell-mode/")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

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
          (forward-comment 1)
          (<= end (point)))
        ;; kill it
        (uncomment-current-line-or-region)
      ;; otherwise make one
      (comment-current-line-or-region))))

(add-hook 'c-mode-common-hook 'bind-autoindent)
;; (add-hook 'python-mode-hook 'bind-autoindent)
(add-hook 'tuareg-mode-hook 'bind-autoindent)

(add-to-list 'load-path "~/elisp/go-mode/")
(require 'go-mode-autoloads)

(add-hook 'go-mode-hook 'bind-autoindent)

;; minor flymake configuration

(require 'flymake)

(add-to-list
 'flymake-allowed-file-name-masks
 '("\\.hpp$" flymake-master-make-header-init flymake-master-cleanup))

(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; Ipython. This python-mode takes the Key-map and the menu
(when (executable-find "ipython")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(add-to-list 'load-path "~/elisp/lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-to-list 'load-path
              "~/elisp/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(load "~/elisp/nxhtml/autostart")

(autoload 'pyxl-mode "pyxl-mode" "Major mode for editing pyxl" t)
(setq auto-mode-alist
     (cons '("\\.py\\'" . pyxl-mode) auto-mode-alist))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'protobuf-mode)

(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(require 'xcscope)
(setq cscope-do-not-update-database t)

(add-to-list 'load-path "~/elisp/auto-complete")
(add-to-list 'load-path "~/elisp/popup-el")
(add-to-list 'load-path "~/elisp/s.el")
(add-to-list 'load-path "~/elisp/dash.el")

(add-to-list 'load-path "~/elisp/fsharpbinding/emacs")
(autoload 'fsharp-mode "fsharp-mode"     "Major mode for editing F# code." t)
(add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))

(setq inferior-fsharp-program "fsharpi --readline-")
(setq fsharp-compiler "fsharpc")

(add-to-load-path-if-exists "~/elisp/auto-complete-clang")
(require 'auto-complete-config nil 'noerror)
(add-to-list 'ac-dictionary-directories "~/elisp/auto-complete-clang/ac-dict")

(require 'auto-complete-clang nil 'noerror)
(define-key ac-mode-map  [(control tab)] 'auto-complete)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

(defun af/toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(setq-default
 show-trailing-whitespace 't)

(require 'cmake-mode)

(defun is-display-graphic ()
  (pcase window-system
    (`ns t)
    (`x t)
    (`w32 t)
    (_ nil)))

(when (is-display-graphic)
  (setq-default frame-background-mode 'light)
  (add-to-list 'custom-theme-load-path
               "~/elisp/emacs-color-theme-solarized")
  (load-theme 'solarized t))

(add-to-list 'load-path
             "~/elisp/dash")

(add-to-list 'load-path
             "~/elisp/with-editor")

(add-to-list 'load-path
             "~/elisp/magit/lisp")

(let ((local-init-file "~/.emacs.local"))
  (when (file-readable-p local-init-file)
    (load-file local-init-file)))

;; Last as it causes issues on windows.

(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/elisp/magit/Documentation/"))
