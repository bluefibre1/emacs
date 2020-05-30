;;; init.el -- My Emacs's Init File
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimizations making Emacs start up faster
(defvar startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-optimizations()
  "Revert startup optimisations."
  (setq gc-cons-threshold startup/gc-cons-threshold
   file-name-handler-alist startup/file-name-handler-alist))
(add-hook 'emacs-startup-hook 'startup/revert-optimizations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(defvar package-list '(package gruvbox-theme diminish evil async flycheck
			       csharp-mode js2-mode json-mode markdown-mode
			       yaml-mode counsel which-key recentf
			       auto-complete rainbow-delimiters
			       counsel-projectile ivy-rich evil-collection
			       ws-butler))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save/Restore Frame Position/Size


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
(require 'gruvbox-theme)
(load-theme 'gruvbox t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode
(defvar evil-want-integration t)
(defvar evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(evil-collection-init)
(evil-set-leader 'normal (kbd "SPC"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor

;; Delete white space at end of edited lines
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; 80 column max
(setq-default whitespace-line-column 80
	      whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'tex-mode-hook 'whitespace-mode)

;; spell check
(require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Remove startup screen
(setq inhibit-startup-message t)

;; Disable menus and scrollbars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Scroll one line at the time
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Disable linewrap (this helps with performance on malformed documents)
(defun disable-line-wrap ()
  "Disable linewrap."
  (setq truncate-lines t))
(add-hook 'prog-mode-hook 'disable-line-wrap)
(add-to-list 'auto-mode-alist '("\\.log\\'" . text-mode))
(add-hook 'text-mode-hook 'disable-line-wrap)

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'tex-mode-hook 'display-line-numbers-mode)

;; Electric Pairs
(defvar electric-pair-pairs '(
			      (?\{ . ?\})
			      (?\( . ?\))
			      (?\[ . ?\])
			      (?\" . ?\")
			      ))
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Compilation follow
(require 'compile)
(setq compilation-scroll-output 'first-error)

;; Cursor
(blink-cursor-mode 0)

;; Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Delete Selection
(delete-selection-mode t)

;; Set _ as part of a word
(modify-syntax-entry ?_ "w")

;; Backups And Auto Saves
;; I don't use either, you might want to turn those from =nil= to =t= if you do.
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Change yes-or-no questions into y-or-n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Recent files
(require 'recentf)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)
(add-to-list 'recentf-exclude "\\.el\\'")

;; Rainbow delimiters
(require 'rainbow-delimiters)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Lets us use asynchronous processes wherever possible
(require 'async)
(dired-async-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))	;

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-current-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bl") 'counsel-buffer-or-recentf)
(evil-define-key 'normal 'global (kbd "<leader>ba") 'kill-other-buffers)
(evil-define-key 'normal 'global (kbd "<leader>bn") 'evil-buffer-new)
(evil-define-key 'normal 'global (kbd "<leader>bo") 'evil-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
(defun window-split-and-follow-horizontally ()
  "Split window horizontally and set as current."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun window-split-and-follow-vertically ()
  "Split window vertically and set as current."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(evil-define-key 'normal 'global (kbd "<leader>wh") 'window-split-and-follow-horizontally)
(evil-define-key 'normal 'global (kbd "<leader>wv") 'window-split-and-follow-vertically)
(evil-define-key 'normal 'global (kbd "<leader>wk") 'delete-window)
(evil-define-key 'normal 'global (kbd "<leader>wa") 'delete-other-windows)
(evil-define-key 'normal 'global (kbd "<leader>wo") 'other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete and Search Helpers
(require 'ivy)
(require 'counsel)
(require 'counsel-projectile)
(require 'ivy-rich)
(counsel-projectile-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(ivy-rich-mode 1)

(evil-define-key 'normal 'global (kbd "<leader>ss") 'counsel-grep-or-swiper)
(evil-define-key 'normal 'global (kbd "<leader>sr") 'ivy-resume)
(evil-define-key 'normal 'global (kbd "<leader>ss") 'counsel-grep-or-swiper)
(evil-define-key 'normal 'global (kbd "<leader>mx") 'counsel-M-x)
(evil-define-key 'normal 'global (kbd "C-p") 'counsel-projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>pf") 'counsel-projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>ps") 'counsel-projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>sg") 'counsel-git-grep)
(evil-define-key 'normal 'global (kbd "<leader>ff") 'counsel-find-file)
(evil-define-key 'normal 'global (kbd "<leader>cc") 'counsel-compile)
(evil-define-key 'normal 'global (kbd "<leader>ce") 'counsel-compilation-errors)

(require 'which-key)
(which-key-mode 1)

(require 'auto-complete)
(global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Configuration
(defun config-load ()
  "Open Emacs config file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(evil-define-key 'normal 'global (kbd "<leader>cl") 'config-load)

(defun config-reload ()
  "Reload Emacs config file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(evil-define-key 'normal 'global (kbd "<leader>cr") 'config-reload)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operating Systems Specific
(if (eq system-type 'darwin)
    (progn
      (set-frame-font "Monaco 15")
      (global-set-key [home] 'move-beginning-of-line)
      (global-set-key [end] 'move-end-of-line)
      (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
      (setq exec-path (append exec-path '("/usr/local/bin")))))

(if (eq system-type 'gnu/linux)
    (progn t))

(if (eq system-type 'windows-nt)
    (progn
      (set-frame-font "Consolas 11")
      (setq default-frame-alist '((font . "Monaco 11")))
      (setenv "PATH"
	      (concat
	       "c:\\Program Files\\Git\\usr\\bin;"
	       (getenv "PATH")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline
;; This must stay at bottom of the config file
(setq line-number-mode t)
(setq column-number-mode t)
(require 'diminish)
(diminish 'visual-line-mode)
(diminish 'subword-mode)
(diminish 'auto-complete-mode)
(diminish 'which-key-mode)
(diminish 'projectile-mode)
(diminish 'flycheck-mode)
(diminish 'undo-tree-mode)
(diminish 'eldoc-mode)
(diminish 'ws-butler-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Anything below is personal preference.
;;; I recommend changing these values with the "customize" menu
;;; You can change the font to suit your liking, it won't break anything.
;;; The one currently set up is called Terminus.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(esup ws-butler which-key rainbow-delimiters markdown-mode json-mode js2-mode ivy-rich gruvbox-theme flycheck evil-collection diminish csharp-mode counsel-projectile auto-complete async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
