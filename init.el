;;;  * My Emacs configuration

(setq user-full-name "Fabio Labella")

;;;  * Package init

(setq load-prefer-newer t) ; don't load outdated bytecode

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; https://github.com/jkitchin/scimax/issues/150
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))


(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package use-package-chords ; define key-chords in use package declarations
  :ensure key-chord
  :ensure t)

;;;  * Meta

(use-package outshine ; enable folding of this file
  :ensure t
  :hook (emacs-lisp-mode . outshine-mode))

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


(use-package planet-theme
  :ensure t
  :defer t)

(defun switch-theme ()
  "Disable any active themes, then load a new one"
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))

(setq custom-safe-themes t)
(load-theme 'planet)

;;;  * Mac specific settings

(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'control)
  ;; on a Mac, don't complain about setting variables in .bashrc instead of .bash_profile
  (setq exec-path-from-shell-check-startup-files nil))

(use-package exec-path-from-shell ; load path from bash
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PS1")
  (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-copy-env "PATH"))

;; Emacs on newer Macs not being able to access Desktop/Downloads/Documents:
;; Assumes installation via brew install emacs --cask
;; then:
;; Settings > Privacy and Security > Full Disk Access and add Emacs
;; then:
;; cd /Applications/Emacs.app/Contents/MacOS
;; mv Emacs Emacs-launcher
;; mv Emacs-arm64-12 Emacs
;; cd /Applications/Emacs.app/Contents/
;; mv _CodeSignature _CodeSignature.old
;; then when accessing one of the folders a pop up appears asking for permissions
;; give it and it works

;;;  * Backups, autosaves, desktop saves

(let* ((full-path (lambda (dir-name)
                    (expand-file-name dir-name user-emacs-directory)))
       (create-dir-if-nonexistent (lambda (dir-name)
                                    (unless (file-exists-p dir-name)
                                      (make-directory dir-name))))
       (backup-dir (funcall full-path "backups"))
       (save-file-dir (funcall full-path "autosaves"))
       (desktop-dir (funcall full-path "desktop-saves")))
  (mapcar create-dir-if-nonexistent `(,backup-dir ,save-file-dir ,desktop-dir))
  (setq make-backup-files t
        version-control t
        delete-old-versions t
        backup-directory-alist `((".*" . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,save-file-dir t)))
  (use-package desktop
    :init
    (setq desktop-path `(,desktop-dir))
    (desktop-save-mode t)))

;;;  * Completion interface

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t)
  (add-to-list 'ido-ignore-buffers "*")
  (add-to-list 'ido-ignore-buffers "intero-script*")
  (add-to-list 'ido-ignore-buffers "magit-*")
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (ido-mode t)
)
(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package flx-ido
  :ensure t
  :config
  (setq ido-use-faces nil) ; disable ido faces to see flx highlights.
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

;;;  * Modal control

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
  :ensure t
  :chords ("kk" . avy-goto-char-timer)
  :config
  (avy-setup-default)
  (setq avy-timeout-seconds 0.2))

;;;  * Frame, Window, Minibuffer

(use-package ace-window ; quick jump to frames and more
  :ensure t
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
  :ensure t
  :chords ("jj" . hippie-expand))

(use-package smartparens
  :ensure t
  :config
  (setq sp-highlight-pair-overlay nil) ; Don't highlight current sexp
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package undo-tree
  :ensure t
  ;; undo-tree has these bindings in a local
  ;; keymap only, causing various issues
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-saves"))))

(use-package yasnippet ; templates
  :ensure t)

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
  :config
  (setq dired-dwim-target t) ; allows copying between two open dired buffers automatically
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil) ; enable dired navigation in the same buffer
  (require 'dired-x)) ; dired jump to dir of current buffer

(use-package projectile
  :ensure t
  :demand t ; without this, shortcuts only work after calling a command manually once
  :bind (:map projectile-mode-map ; :bind-keymap does not work, have to use this form
              ("C-c C-p" . projectile-command-map))
  :config
  (projectile-mode t))

(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer))

(setq org-agenda-files '("~/Dropbox/todos/todo.org"))

(use-package magit ; Awesome Git porcelain
  :ensure t
  :config
  (setq magit-visit-ref-behavior ; To make 'Enter' check out things in the 'y' panel
      '(create-branch
        checkout-any
        checkout-branch)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain ; Diff in the current frame
      ediff-split-window-function (if (> (frame-width) 150)
                                          'split-window-horizontally
                                        'split-window-vertically))

;;;  * Web
(use-package restclient
  :ensure t)

(use-package json
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;;  * Latex

(use-package tex
  :ensure auctex ; (use-package auctex :ensure t) does not work with :defer
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
  :ensure t
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

;;;  * LSP

(use-package posframe ; needs to be installed manually
  :ensure t)

(use-package lsp-mode
  :ensure t
  :ensure flycheck
  :ensure lsp-ui
  :ensure company
  :ensure dap-mode
  :hook
  (lsp-mode . lsp-lens-mode)
  (lsp-mode . flycheck-mode)
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  (lsp-mode . company-mode)
  :config ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil))

;;;  * Scala

(use-package scala-mode
  :ensure t
  :hook
  (scala-mode . lsp)
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :bind
  (:prefix-map my-sbt--map
               :prefix "C-c C-b"
               ("c" . sbt-do-compile)
               ("t" . sbt-do-test)
               ("o" . sbt-switch-to-active-sbt-buffer) ; mnemonic: open
               ("l" . run-scala) ; mnemonic: load
               ("g" . sbt-grep)
               ("f" . lsp-format-buffer))
  :config
  (setq sbt:prefer-nested-projects t))

(use-package lsp-metals
  :ensure t)

;;;  * Haskell

(use-package intero
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

;;;  * Bazel

(use-package bazel
  :ensure t
  :mode (("BUILD'" . bazel-mode)))

;;;  * Terraform
(use-package terraform-mode
  :ensure t)
