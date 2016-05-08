;; ========== Setup ==========

;; Package list 
(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;; Load PATH from bash
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PS1")
(exec-path-from-shell-copy-env "CELLAR")


;; ========== Backup and autosaves ==========

;; Enable backup files.
(setq make-backup-files t)
;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)
;; Delete old backup versions
(setq delete-old-versions t)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
;; Autosave Files in Specific directory
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/autosaves/" t)))
;; Save desktop and reload at startup
(setq desktop-path '("~/.emacs.d/.desktop-saves/"))
(desktop-save-mode 1)


;; ========== General Appearance and Settings==========

;; Inhibit startup message 
(setq inhibit-startup-message t) 
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable alarms
(setq ring-bell-function 'ignore)
;; Colour theme 
(load-theme 'tango-dark t) 
;; Use line numbers globally
(global-linum-mode t)
;; Show line-number in the mode line
(line-number-mode 1)
;; Show column-number in the mode line
(column-number-mode 1)
;;Prompt y or n instead of yes or no always.
(fset 'yes-or-no-p 'y-or-n-p)
;; Use only spaces for indentation
(setq-default indent-tabs-mode nil) 
;; Support Wheel Mouse Scrolling 
(mouse-wheel-mode t)
;; Show matching parentheses
(show-paren-mode 1)
;; Insert matching parentheses
(electric-pair-mode 1)

;; ========== Core usage ==========

;; Set Right Command as Ctrl 
(setq mac-right-command-modifier 'control)
;; Window management
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(windmove-default-keybindings)
;; Enable Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)
(add-to-list 'ido-ignore-buffers "*")
(add-to-list 'ido-ignore-files "\\.DS_Store")
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;; Enable Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Redefine common keybindings to work smoothly with keychords
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x C-f") 'set-fill-column)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "C-x C-s") 'save-some-buffers)
(global-set-key (kbd "C-x c") 'save-buffers-kill-terminal)
;; God mode
(require 'god-mode)
(defun my-god-mode-switch () (interactive)
  "Make the switching only work from insert to god mode"     
       (when (god-local-mode)
         'god-local-mode)
       )
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)
(global-set-key (kbd "<escape>") 'my-god-mode-switch)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
;; Update cursor when in god mode
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
;; Use god mode in search 
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
;; Key chords 
(require 'key-chord)
(key-chord-define-global "fk" 'smex)
(key-chord-define-global "fj" ctl-x-map)
(key-chord-define-global "jk" 'my-god-mode-switch)
(key-chord-define-global "jj" 'dabbrev-expand)
(key-chord-mode +1)



;; ========== Haskell mode ==========
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
;;This is the only useful shortcut not enabled out of the box
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

;; workaround until #754 is fixed
(defun haskell-process-trigger-suggestions-ad (orig-fun &rest args)
  (turn-off-haskell-doc)
  (apply orig-fun args)
  (turn-on-haskell-doc))

(advice-add 'haskell-process-trigger-suggestions
   :around #'haskell-process-trigger-suggestions-ad)


;; ========== Scala mode ==========
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; ========== Dired mode ==========
;; if you open two buffers in the same frame
;; copying/moving from one  will default
;; to the other as the target folder
(setq dired-dwim-target t)

;; ========== Projectile ==========
(projectile-global-mode)



