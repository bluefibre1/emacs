;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This fixed garbage collection, makes emacs start up faster
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(defvar package-list)
(setq package-list '(package gruvbox-theme fill-column-indicator diminish evil
			     async flycheck csharp-mode js2-mode json-mode
			     markdown-mode yaml-mode counsel which-key recentf
			     auto-complete rainbow-delimiters counsel-projectile))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode
(require 'evil)
(evil-mode 1)
(evil-set-leader 'normal (kbd "SPC"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operating Systems
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
      (set-frame-font "Monaco 11")
      (setq default-frame-alist '((font . "Monaco 11")))
      (setenv "PATH"
	      (concat
	       ;; Change this with your path to MSYS bin directory
	       "c:\\Program Files\\Git\\usr\\bin;"
	       (getenv "PATH")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete
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
  (org-babel-load-file (expand-file-name "~/.emacs.d/init.el")))
(evil-define-key 'normal 'global (kbd "<leader>cr") 'config-reload)

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-current-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
(require 'gruvbox-theme)
(load-theme 'gruvbox-dark-hard t)

;; Save emacs dimensions
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(setq desktop-dirname user-emacs-directory)

;; 80 column ruler
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'tex-mode-hook 'fci-mode)

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

;; Disable linewrap (this helps with performance on malformed documents)
(set-default 'truncate-lines t)

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'tex-mode-hook 'display-line-numbers-mode)

;; Modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; Diminish
(require 'diminish)
(diminish 'visual-line-mode)
(diminish 'subword-mode)

;; Ivy search mode
(require 'counsel)
(require 'counsel-projectile)
(counsel-projectile-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(evil-define-key 'normal 'global (kbd "<leader>ss") 'counsel-grep-or-swiper)
(evil-define-key 'normal 'global (kbd "<leader>sr") 'ivy-resume)
(evil-define-key 'normal 'global (kbd "<leader>ss") 'counsel-grep-or-swiper)
(evil-define-key 'normal 'global (kbd "<leader>mx") 'counsel-M-x)
(evil-define-key 'normal 'global (kbd "C-p") 'counsel-projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>pf") 'counsel-projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>ps") 'counsel-projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>sg") 'counsel-git-grep)
(evil-define-key 'normal 'global (kbd "<leader>ff") 'counsel-find-file)

;; Which key
(require 'which-key)
(which-key-mode 1)

;; Cursor
(blink-cursor-mode 0)

;; General
;; Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Delete Selection
;; By default when you start typing and have a selection, Emacs just moves the
;; cursor at the end of the selection and start adding characters from there.
;; This is fine, but different from most editor on Windows now days. So to be
;; more consistent, we change that back to overwriting the selection.
(delete-selection-mode t)

;; Backups And Auto Saves
;; I don't use either, you might want to turn those from =nil= to =t= if you do.
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Change yes-or-no questions into y-or-n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Async
;; Lets us use asynchronous processes wherever possible, pretty useful.
(require 'async)
(dired-async-mode 1)

;; Scrolling and why does the screen move
;; I don't know to be honest, but this little bit of code makes scrolling with emacs a lot nicer.
(setq scroll-conservatively 100)

;; Following window splits
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

;; Buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (remove-if-not 'buffer-file-name (buffer-list)))))

(require 'recentf)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)
(add-to-list 'recentf-exclude "\\.el\\'")


(evil-define-key 'normal 'global (kbd "<leader>bl") 'counsel-buffer-or-recentf)
(evil-define-key 'normal 'global (kbd "<leader>bo") 'kill-other-buffers)
(evil-define-key 'normal 'global (kbd "<leader>bn") 'evil-buffer-new)
(evil-define-key 'normal 'global (kbd "<leader>ba") 'evil-buffer)

;; Rainbow delimiters
(require 'rainbow-delimiters)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Electric Pairs
;; Typing the first character in a set of 2, completes the second one after
;; your cursor. Opening a bracket? It's closed for you already. Quoting
;; something? It's closed for you already.
(defvar electric-pair-pairs)
(setq electric-pair-pairs '(
			    (?\{ . ?\})
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\" . ?\")
			    ))
(add-hook 'prog-mode-hook 'electric-pair-mode)


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
 '(package-selected-packages (quote (esup which-key ivy evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
