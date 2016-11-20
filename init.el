;;;;  * My Emacs configuration
(setq user-full-name "Fabio Labella")
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   "You can fold this file as an Org buffer"
                                   (setq-local orgstruct-heading-prefix-regexp ";;;;  ")
                                   (turn-on-orgstruct)))
;;;;  * Package sources
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(setq load-prefer-newer t) ;; don't load outdated bytecode

;;;;  * Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(use-package use-package-chords ;; define key-chords in use package declarations
  :ensure key-chord
  :ensure t)

;;;;  * Backups, autosaves, and desktop saves
(let ((backup-dir "backups")
      (save-file-dir "autosaves")
      (desktop-dir "desktop-saves")
      (create-dir-if-nonexistent (lambda (dir-name)
                                   (let ((dir-path (expand-file-name dir-name user-emacs-directory)))
                                     (unless (file-exists-p dir-name)
                                       (make-directory dir-name))))))
  (mapcar create-dir-if-nonexistent `(,backup-dir ,save-file-dir ,desktop-dir))
  (setq make-backup-files t
        version-control t
        delete-old-versions t
        backup-directory-alist `((".*" . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,save-file-dir t)))
  (use-package desktop
  :init
  (setq desktop-path `(,desktop-dir))
  (desktop-save-mode -1)))

;;;;  * Mac specific settings
(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control)
  ;; on a Mac, don't complain about setting variables in .bashrc instead of .bash_profile
  (setq exec-path-from-shell-check-startup-files nil))
(use-package exec-path-from-shell ;; load path from bash
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PS1")
  (exec-path-from-shell-copy-env "CELLAR"))

;;;;  * Misc settings
(setq-default indent-tabs-mode nil) ;; Indent with no tabs
(delete-selection-mode t) ;; Overwrite selected text
(setq require-final-newline t) ;; Newline at the end of files
(fset 'yes-or-no-p 'y-or-n-p) ;; Use y or n instead of yes and no
(put 'narrow-to-region 'disabled nil) ;; Enable narrowing

;;;;  * Appearance
(setq inhibit-startup-message t
      inhibit-splash-screen t
      ring-bell-function 'ignore)
(line-number-mode t) ;; Line numbers in mode line
(column-number-mode t) ;; Column numbers in mode line
(mouse-wheel-mode t) ;; Enable scrolling
(scroll-bar-mode -1)
(tool-bar-mode -1)
(use-package solarized-theme ;; + Blended fringe  - Pervasive shitty pea green
  :ensure t
  :defer t
  :init
  (load-theme 'solarized-dark t t))
(use-package ample-theme ;; + Low-contrast  - Fringe and some of the colours
  :ensure t
  :defer t
  :init
  (load-theme 'ample-flat t t))
(use-package planet-theme ;; + Very nice colours  - Fringe, awful vertical line.
  :ensure t
  :defer t
  :init
  (load-theme 'planet t t))
(use-package subatomic-theme ;; + Nice colours, Blended Fringe and nice vertical line  - Background not nice
  :ensure t
  :defer t
  :init
  (load-theme 'subatomic t t))
(use-package twilight-anti-bright-theme ;; + Nice background and colours  - Fringe, weird comment outlining
  :ensure t
  :defer t
  :init
  (load-theme 'twilight-anti-bright t t))
(defun switch-theme ()
  "Disable any active themes, then load a new one"
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))
(enable-theme 'planet)

;;;;  * General completion interface
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t)
 (add-to-list 'ido-ignore-buffers "*")
 (add-to-list 'ido-ignore-files "\\.DS_Store")
 (ido-mode t))
(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode t))
(use-package flx-ido
  :ensure t
  :config
  (setq ido-use-faces nil) ;; disable ido faces to see flx highlights.
  (flx-ido-mode t))
(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-C-p)
  (ido-vertical-mode t))
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :chords ("fk"  . smex))

;;;;  * Modal editing
(use-package key-chord
  :ensure t
  :bind (("C-x f" . find-file)
         ("C-x C-f" . set-fill-column)
         ("C-x s" . save-buffer)
         ("C-x C-s" . save-some-buffers)
         ("C-x c" . save-buffers-kill-terminal))
  :init
  (key-chord-mode t)
  :config
  (key-chord-define-global "fj" ctl-x-map))
(use-package god-mode
  :ensure t
  :init (require' god-mode-isearch)
  :bind (("<escape>" . god-mode-on)
         :map god-local-mode-map
         ("i" . god-mode-off)
         ("." . repeat)
         :map isearch-mode-map
         ("<escape>" . god-mode-isearch-activate)
         :map god-mode-isearch-map
         ("<escape>" . god-mode-isearch-disable))
  :chords ("jk" . god-mode-on)
  :config
  (defun god-mode-on ()
    (interactive)
    "Activate God mode if the buffer is in insert mode.
     Keep it active if the buffer is in God mode"
    (when (god-local-mode)
      'god-local-mode))
  (defun god-mode-off ()
    (interactive)
    "Turn God mode off silently. Using the minor mode toggle prints a
     message in the minibuffer, which is annoying when you are using
     God-mode there. Also, that message is useless given the nature of
     this mode (constantly on/off, different cursor)"
    (god-local-mode -1)
    (run-hooks 'god-mode-disabled-hook))
  (defun god-mode-custom-update-cursor ()
    "Change cursor shape in god mode"
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'bar
                        'box)))
  (add-hook 'god-mode-enabled-hook 'god-mode-custom-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-custom-update-cursor)
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil))

;;;;  * Quick movement
(use-package avy
  :ensure t
  :chords ("kk" . avy-goto-char-timer)
  :config
  (avy-setup-default)
  (setq avy-timeout-seconds 0.2))

;;;;  * Frame management
(use-package ace-window ;; quick jump to frames and more
  :ensure t
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil))
(winner-mode t) ;; undo-redo frame configuration
(windmove-default-keybindings) ;; use shift-arrow to move between frames

;;;;  * Minibuffer
;; silence messages in minibuffer when editing it
;;;;  * Better undo
(use-package undo-tree
  :ensure t
  :bind (("C-/" . undo-tree-undo)         ;; undo-tree has these bindings in a local
         ("C-?" . undo-tree-redo)         ;; keymap only, causing various issues
         ("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode t))

;;;;  * Autocompletion
(use-package hippie-exp
  :ensure t
  :chords ("jj" . hippie-expand))

;;;;  * Parentheses
(use-package smartparens
  :ensure t
  :config
  (setq sp-highlight-pair-overlay nil) ;; Don't highlight current sexp
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))
;;;;  * File, Buffer, and Project Management
(use-package dired
  :config
  (setq dired-dwim-target t) ;; allows copying between two open dired buffers automatically
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (require 'dired-x)) ;; dired jump to dir of current buffer
(use-package projectile
  :ensure t
  :config
  (projectile-mode t))
(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer))

;;;;  * Version control
(use-package magit ;; Awesome Git porcelain
  :ensure t)

;;;;  * Rest
(use-package restclient
  :ensure t)

;;;;  * Json
(use-package json
  :ensure t)

;;;;  * Yaml
(use-package yaml-mode
  :ensure t)

;;;;  * Markdown
(use-package markdown-mode
  :ensure t)

;;;;  * Latex
(use-package tex
  :ensure auctex ;; (use-package auctex :ensure t) does not work with :defer
  :bind (:map LaTeX-mode-map
              ("M-p" . latex-preview-pane-start-preview)
              ("C-q" . latex-preview-pane-stop-preview))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; Inserting fonts in Auctex is done via C-c C-f which calls an
  ;; interactive function parameterised by a map of characters (all
  ;; prefixed with C-).  This doesn't work with God-mode, which always
  ;; sends non-prefixed characters to interactive functions, even if
  ;; you're in command-mode. To make font insertion work with God-mode
  ;; I therefore extend the map with the equivalent non prefixed keys.
  (require 'latex) ;; to get LaTeX-font-list standard value
  (cl-labels ((strip-prefix (TeX-font-list-entry)
                            (destructuring-bind
                                (keybinding . rest)
                                TeX-font-list-entry
                              (cons (+ ?a (- keybinding 1)) rest)))
              (get-default (custom-var-symbol)
                           (eval (car (get custom-var-symbol 'standard-value))))
              (add-stripped (keybindings)
                            (let ((existing-keybindings (get-default keybindings)))
                              (append
                               existing-keybindings
                               (mapcar (function strip-prefix) existing-keybindings)))))
    (setq LaTeX-font-list (add-stripped 'LaTeX-font-list))
    (setq TeX-font-list (add-stripped 'TeX-font-list))))
(use-package latex-preview-pane
  :ensure t
  :config
  (defun latex-preview-pane-start-preview ()
    (interactive)
    (unless (bound-and-true-p latex-preview-pane-mode)
      (latex-preview-pane-mode)
      (message "Preview on save started. C-q to stop")))
  (defun latex-preview-pane-stop-preview ()
    (interactive)
    (when (bound-and-true-p latex-preview-pane-mode)
    (latex-preview-pane-mode -1))))

;;;;  * Scala
(use-package ensime
  :ensure t
  :pin melpa-stable)

;;;;  * Haskell
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  :bind ("C-`" . haskell-interactive-bring))
