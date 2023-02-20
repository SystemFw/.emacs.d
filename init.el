;;;  * My Emacs configuration

(setq user-full-name "Fabio Labella")

;;;  * Package init

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

(use-package use-package-chords) ; define key-chords in use package declarations

;;;  * Meta

(use-package outshine ; enable folding of this file
  :hook (emacs-lisp-mode . outshine-mode))

; I don't use Customize but some settings are saved by emacs automatically.
; Save them in different file rather than the end of init.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;  * Appearance

(setq inhibit-startup-message t
      inhibit-splash-screen t
      ring-bell-function 'ignore
      cursor-in-non-selected-windows nil)

(line-number-mode t) ; Line numbers in mode line
(column-number-mode t) ; Column numbers in mode line
(mouse-wheel-mode t) ; Enable scrolling
(scroll-bar-mode -1)
(tool-bar-mode -1)


(use-package planet-theme)

(defun switch-theme ()
  "Disable any active themes, then load a new one"
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))

(setq custom-safe-themes t)
(load-theme 'planet)

(use-package indent-guide
  :config (indent-guide-global-mode))

;;;  * Mac specific settings

(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control)) ; Enables Cmd-G to cancel operations

;;;  * Shell

(setq explicit-shell-file-name "/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("-l")) ; it's a login shell so it sources .zprofile
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t)) ; no command echo
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

;;;  * Backups, autosaves, desktop saves

(setq make-backup-files t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      backup-by-copying t
      kept-new-versions 9
      kept-old-versions 6
      backup-directory-alist `(("." .  "~/.emacs.d/backups")))

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves" t)))

(use-package desktop
  :init
  (setq desktop-path `("~/.emacs.d/desktop-saves"))
  (desktop-save-mode t))

;;;  * Completion interface

(use-package ido
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t)
  (add-to-list 'ido-ignore-buffers "*")
  (add-to-list 'ido-ignore-buffers "magit-*")
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (ido-mode t)
)
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode t))

(use-package flx-ido
  :config
  (setq ido-use-faces nil) ; disable ido faces to see flx highlights.
  (flx-ido-mode t))

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-define-keys 'C-n-C-p)
  (ido-vertical-mode t))

(use-package smex
  :bind ("M-x" . smex)
  :chords ("fk"  . smex))

;;;  * Modal control

(use-package key-chord
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
  :init (require' god-mode-isearch)
  :bind (("<escape>" . god-mode-idempotent-enable)
         :map god-local-mode-map
         ("i" . god-local-mode)
         ("." . repeat)
         :map isearch-mode-map
         ("<escape>" . god-mode-isearch-activate)
         :map god-mode-isearch-map
         ("<escape>" . god-mode-isearch-disable))
  :chords ("jk" . god-mode-idempotent-enable)
  :config
  (defun god-mode-idempotent-enable ()
     "Activate God mode if the buffer is in insert mode.
Keep it active if the buffer is in God mode"
    (interactive)
    (when (god-local-mode)
      'god-local-mode))
  (defun god-mode-custom-update-cursor ()
    "Change cursor shape in god mode"
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'bar
                        'box)))
  (add-hook 'god-mode-enabled-hook 'god-mode-custom-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-custom-update-cursor)
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil))

(use-package avy ; quick movement
  :chords ("kk" . avy-goto-char-timer)
  :config
  (avy-setup-default)
  (setq avy-timeout-seconds 0.2))

;;;  * Frame, Window, Minibuffer

(use-package ace-window ; quick jump to frames and more
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil))

(winner-mode t) ; undo-redo frame configuration
(windmove-default-keybindings) ; use shift-arrow to move between frames

(defun minibuffer-disable-messages ()
  "Disable printing messages to the minibuffer. They will still be
displayed in the *Messages* buffer"
  (setq inhibit-message t))

(defun minibuffer-enable-messages ()
  "Enable printing messages to the minibuffer"
  (setq inhibit-message nil))

;; Disable messages in the minibuffer while it's being edited.
;; Reenable them when the minibuffer is closed, either via C-g or evaluation.
;; If you jump back and forth between the minibuffer and another window
;; message will be disabled until you close the minibuffer as above.
;; No messagesa are lost, since they are still displayed in the *Messages* buffer
(add-hook 'minibuffer-setup-hook 'minibuffer-disable-messages)
(add-hook 'minibuffer-exit-hook 'minibuffer-enable-messages)

;;;  * Editing

(use-package hippie-exp ; basic autocompletion
  :chords ("jj" . hippie-expand))

(use-package smartparens
  :config
  (setq sp-highlight-pair-overlay nil) ; Don't highlight current sexp
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package undo-tree
  ;; undo-tree has these bindings in a local
  ;; keymap only, causing various issues
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-visualizer-diff t))

(use-package yasnippet) ; templates

(setq-default indent-tabs-mode nil) ; Indent with no tabs
(delete-selection-mode t) ; Overwrite selected text
(setq require-final-newline t) ; Newline at the end of files
(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes and no
(put 'narrow-to-region 'disabled nil) ; Enable narrowing
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8) ; prefer UTF-8
(setq sentence-end-double-space nil) ; paragraphs end in a single space


;;;  * Files, VC, Buffers, Projects

(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-dwim-target t) ; allows copying between two open dired buffers automatically
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil) ; enable dired navigation in the same buffer
  (require 'dired-x)) ; dired jump to dir of current buffer

(use-package projectile
  :demand t ; without this, shortcuts only work after calling a command manually once
  :bind (:map projectile-mode-map ; :bind-keymap does not work, have to use this form
              ("C-c C-p" . projectile-command-map))
  :config
  (projectile-mode t))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(setq org-agenda-files '("~/Dropbox/todos/todo.org"))

(use-package magit) ; Awesome Git porcelain

(setq ediff-window-setup-function 'ediff-setup-windows-plain ; Diff in the current frame
      ediff-split-window-function (if (> (frame-width) 150)
                                          'split-window-horizontally
                                        'split-window-vertically))

;;;  * Web
(use-package restclient)

(use-package json)

(use-package yaml-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;;  * Latex

(use-package tex
  :straight auctex ; (use-package auctex :ensure t) does not work with :defer
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
                               (mapcar #'strip-prefix existing-keybindings)))))
    (setq LaTeX-font-list (add-stripped 'LaTeX-font-list))
    (setq TeX-font-list (add-stripped 'TeX-font-list))))

(use-package latex-preview-pane
  :config
  (defun latex-preview-pane-start-preview ()
    "Start a live pdf preview-on-save of a TeX document in an adjacent window"
    (interactive)
    (unless (bound-and-true-p latex-preview-pane-mode)
      (latex-preview-pane-mode)
      (message "Preview on save started. C-q to stop")))
  (defun latex-preview-pane-stop-preview ()
    "Stop the preview started by latex-preview-pane-start-preview"
    (interactive)
    (when (bound-and-true-p latex-preview-pane-mode)
    (latex-preview-pane-mode -1))))

;;;  * Scala

(use-package scala-mode
  ;; :hook
  ;; (scala-mode . lsp)
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :bind
  (:prefix-map my-sbt--map
               :prefix "C-c C-b"
               ("c" . sbt-do-compile)
               ("t" . sbt-do-test)
               ("o" . sbt-switch-to-active-sbt-buffer) ; mnemonic: open
               ("l" . run-scala) ; mnemonic: load
               ("g" . sbt-grep))
  :config
  (setq sbt:prefer-nested-projects t))

;;;  * Haskell

(use-package haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . flymake-mode)
  :config
  (setq haskell-process-load-or-reload-prompt t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t))

(use-package dante
  :hook
  (haskell-mode . dante-mode))

;;;  * Bazel

(use-package bazel
  :mode (("BUILD'" . bazel-mode)))

;;;  * Terraform
(use-package terraform-mode)

;;;  * Unison
(use-package unisonlang-mode)

;;;  * Racket
(use-package racket-mode
  :mode "\\.ss\\'") ; Use it on other schemes as well
