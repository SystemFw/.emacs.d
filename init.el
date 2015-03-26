;;==== Add Marmalade to package list ====
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;;==== Load PATH from bash====x
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
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

;; ========== Enable Line and Column Numbering ==========

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)


