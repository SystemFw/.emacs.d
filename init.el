;;==== Add Marmalade to package list ====
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;;==== Load PATH from bash====
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PS1")
(exec-path-from-shell-copy-env "CELLAR")
;; ==== Inhibit startup message =====
(setq inhibit-startup-message t) 

;; ==== Disable the toolbar ====
(tool-bar-mode -1)

;; ==== Use line numbers globally ====
(global-linum-mode t)


;; ==== Prompt y or n ==== 
;;Prompt y or n instead of yes or no always.
(fset 'yes-or-no-p 'y-or-n-p)

;; ===== Turn off tab character =====

;;
;; Emacs normally uses both tabs and spaces to indent lines. If you
;; prefer, all indentation can be made from spaces only. To request this,
;; set `indent-tabs-mode' to `nil'. This is a per-buffer variable;
;; altering the variable affects only the current buffer, but it can be
;; disabled for all buffers.

;;
;; Use (setq ...) to set value locally to a buffer
;; Use (setq-default ...) to set value globally 
;;
(setq-default indent-tabs-mode nil) 

;; ========== Support Wheel Mouse Scrolling ==========

(mouse-wheel-mode t)

;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

;; ========== Place Autosave Files in Specific Directory ==========
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/autosaves/" t)))
;; ========== Enable Line and Column Numbering ==========

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;;========== Ido mode and Smex ==========
;; Enable Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)
;; Enable Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;========== Set Right Command as Ctrl ==========
(setq mac-right-command-modifier 'control)

;;========== WindMove ==========
(windmove-default-keybindings)

;;========== Haskell mode ==========
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation) 

;;========== Colour theme ==========
(load-theme 'tango-dark t) 

;;========== Key chords ==========
(require 'key-chord)

(key-chord-define-global "fj" 'smex)

(key-chord-mode +1)
