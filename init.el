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
			       ws-butler neotree general hydra ivy-hydra))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
(defun add-hooks (hooks function)
  "Add more than one HOOKS to FUNCTION."
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save/Restore Frame Position/Size
(defvar startup/frame-state-filename "~/.emacs.d/frame-state.el")
(defun frame-state-save()
  "Save the frame information."
  (with-temp-file startup/frame-state-filename
    (require 'cl)
    (insert (format "%s" (list (first (frame-position))
                               (rest (frame-position))
                               (frame-width)
                               (frame-height))))))
(add-hook 'kill-emacs-hook 'frame-state-save)

(if window-system
(when (file-exists-p startup/frame-state-filename)
  (with-temp-buffer
    (insert-file-contents startup/frame-state-filename)
    (setq l (read (buffer-string))
          x (nth 0 l)
          y (nth 1 l)
          w (nth 2 l)
          h (nth 3 l))
    (setq initial-frame-alist
          (list (cons 'top y) (cons 'left x) (cons 'width w) (cons 'height h))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
(require 'gruvbox-theme)
(load-theme 'gruvbox t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode
(setq-default evil-want-integration t)
(setq-default evil-want-keybinding nil)
(setq-default evil-search-module 'evil-search)
(require 'evil)
(evil-mode 1)
(evil-collection-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor

;; Delete white space at end of edited lines
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; 80 column max
(setq-default whitespace-line-column 80
	      whitespace-style '(face lines-tail))
(add-hooks '(prog-mode-hook tex-mode-hook) 'whitespace-mode)

;; spell check
(require 'flycheck)
(add-hooks '(prog-mode-hook tex-mode-hook) 'flycheck-mode)

;; Disable bell"
(setq ring-bell-function 'ignore)

;; Remove startup screen
(setq inhibit-startup-message t)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

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
(add-hooks '(prog-mode-hook text-mode-hook) 'disable-line-wrap)
(add-to-list 'auto-mode-alist '("\\.log\\'" . text-mode))

;; Line numbers
(add-hooks '(prog-mode-hook tex-mode-hook) 'display-line-numbers-mode)

;; Highlight current line
(require 'hl-line)
(add-hooks '(prog-mode-hook tex-mode-hook) 'hl-line-mode)
(set-face-background 'hl-line "#1f1f1f")

;; Electric Pairs
(defvar electric-pair-pairs '(
			      (?\{ . ?\})
			      (?\( . ?\))
			      (?\[ . ?\])
			      (?\" . ?\")
			      ))
(add-hooks '(prog-mode-hook tex-mode-hook) 'electric-pair-mode)

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
;;(add-to-list 'recentf-exclude "\\.el\\'")

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

(require 'neotree)
(setq-default neo-show-hidden-files t)
(neotree-show)
(switch-to-buffer-other-window "*scratch*")
(setq neo-window-fixed-size nil)

(require 'which-key)
(which-key-mode 1)

(require 'auto-complete)
(global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Configuration
(defun load-emacs-config ()
  "Open Emacs config file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operating Systems Specific
(if (eq system-type 'darwin)
    (progn
      (set-frame-font "Monaco 15")
      (setq default-frame-alist '((font . "Monaco 15")))
      (global-set-key [home] 'move-beginning-of-line)
      (global-set-key [end] 'move-end-of-line)
      (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
      (setq exec-path (append exec-path '("/usr/local/bin")))))

(if (eq system-type 'gnu/linux)
    (progn t))

(if (eq system-type 'windows-nt)
    (progn
      (set-frame-font "Consolas 11")
      (setq default-frame-alist '((font . "Consolas 11")))
      (setenv "PATH"
	      (concat
	       "c:\\Program Files\\Git\\usr\\bin;"
	       (getenv "PATH")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline
;; This must stay at bottom of the config file
(defface mode-line-evil-state-face
  '((t :foreground "black" :background "OliveDrab" ))
  "Face for mode line: Evil State"
  :group 'my-modeline-group)

(setq-default mode-line-format
              (list
               ;; show evil state
               '(:eval (propertize
                        (concat " " (concat (upcase (symbol-name evil-state)) " "))
                                   'face 'mode-line-evil-state-face))
               " "

               ;; was this buffer modified since the last save?
               '(:eval (if (buffer-modified-p)
                         (propertize "!"
                                     'face 'font-lock-keyword-face
                                     'help-echo "Buffer has been modified") " "))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b  " 'help-echo (buffer-file-name)))

               ;; column and line
               (propertize "%c" 'face 'font-lock-type-face)
               ":"
               (propertize "%l" 'face 'font-lock-type-face)

               ;; encoding
               "  "
               (propertize (upcase (symbol-name locale-coding-system))
                           'face 'font-lock-preprocessor-face)
               "  "

               ;; the current major mode for the buffer.
               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))

               ;; relative position, size of file
               "  "
               (propertize "%p" 'face 'font-lock-variable-name-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-variable-name-face) ;; size

               ;; time and date
               "  "
               '(:eval (propertize (format-time-string "%F %R")))
               ))


(setq-default line-number-mode 1)
(setq-default column-number-mode 1)

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
(diminish 'whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra
(require 'hydra)
(defhydra hydra-window-zoom ()
  "window zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("w" (evil-window-increase-width 1) "+ width")
  ("W" (evil-window-decrease-width 1) "- width")
  ("h" (evil-window-increase-height 1) "+ height")
  ("H" (evil-window-decrease-height 1) "- height"))

(defhydra hydra-window-jump ()
  "window jump"
  ("<up>" evil-window-up  "up")
  ("k" evil-window-up  "up")
  ("<left>" evil-window-left "left")
  ("h" evil-window-left "left")
  ("<down>" evil-window-down "down")
  ("j" evil-window-down "down")
  ("<right>" evil-window-right "right")
  ("l" evil-window-right "right"))

(require 'ivy-hydra)
(define-key ivy-minibuffer-map (kbd "C-o") 'hydra-ivy/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Key Map
(require 'general)
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 "M-x" '(counsel-M-x :which-key "M-x")
 "C-x C-f" '(counsel-find-file :which-key "find file")
 "C-s" '(counsel-grep-or-swiper :which-key "swiper")
 "C-p" '(counsel-projectile-find-file :which-key "find project file")
 )

(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
  ;; buffer/bookmark
  "b" '(:ignore t :which-key "Buffer/Bookmark")
  "bo" '(evil-buffer :which-key "other")
  "bd" '(kill-current-buffer :which-key "delete")
  "b0" '(kill-current-buffer :which-key "delete")
  "bD" '(kill-other-buffers :which-key "delete others")
  "b1" '(kill-other-buffers :which-key "delete others")
  "bs" '(save-buffer :which-key "save")
  "bS" '(save-some-buffers :which-key "save all")
  "bn" '(evil-buffer-new :which-key "new")
  "bb" '(counsel-bookmark :which-key "set bookmark")
  "bl" '(bookmark-bmenu-list :which-key "browse bookmark")
  "TAB" '(ivy-switch-buffer :which-key "switch buffer")

  ;; compile/code
  "c" '(:ignore t :which-key "Compile/Code")
  "cc" '(counsel-compile :which-key "compile")
  "ce" '(counsel-compilation-errors :which-key "show errors")
  "c/" '(comment-or-uncomment-region-or-line :which-key "comment")

  ;; delete
  "d" '(:ignore t :which-key "Delete")
  "db" '(kill-current-buffer :which-key "delete buffer")
  "dw" '(delete-window :which-key "delete window")
  "ds" '(evil-ex-nohighlight :which-key "delete search")

  ;; evalation
  "e" '(:ignore t :which-key "Eval")
  "ed" '(eval-defun :which-key "function")
  "eb" '(eval-buffer :which-key "buffer")

  ;; files
  "f" '(:ignore t :which-key "Files")
  "ff" '(counsel-find-file :which-key "find file")
  "fc" '(load-emacs-config :which-key "emacs config")

  ;; help
  "h" '(:ignore t :which-key "Help")
  "hf" '(counsel-describe-function :which-key "function")
  "hv" '(counsel-describe-variable :which-key "variable")
  "hs" '(counsel-describe-symbol :which-key "symbol")
  "hb" '(counsel-descbinds :which-key "binding")
  "hm" '(describe-mode :which-key "mode")
  "hk" '(describe-key :which-key "key")

  ;; other
  "SPC" '(counsel-M-x :which-key "M-x")

  ;; project
  "p" '(:ignore t :which-key "Project")
  "pg" '(counsel-projectile-grep :which-key "grep")
  "pf" '(counsel-projectile-find-file :which-key "find file")
  "ps" '(counsel-projectile-switch-project :which-key "switch")

  ;; registers
  "r" '(:ignore t :which-key "Register")
  "rr" '(point-to-register :which-key "set point")
  "rj" '(jump-to-register :which-key "jump")
  "rw" '(window-configuration-to-register :which-key "set window")
  "rf" '(frameset-to-register :which-key "set frame")
  "rl" '(counsel-register :which-key "set frame")

  ;; search
  "/" '(counsel-grep-or-swiper :wich-key "swiper")
  "s" '(:ignore t :which-key "Search")
  "ss" '(cousel-grep-or-swiper :which-key "swiper")
  "sd" '(evil-ex-nohighlight :which-key "delete")
  "s0" '(evil-ex-nohighlight :which-key "delete")
  "sh" '(highlight-regexp :which-key "highlight")
  "su" '(unhighlight-regexp :which-key "unhighlight")

  ;; tree
  "t" '(:ignore t :which-key "Tree")
  "tt" '(neotree-toggle :which-key "show/hide")
  "tf" '(neotree-find : which-key "find")

  ;; window
  "w" '(:ignore t :which-key "Window")
  "wh" '(window-split-and-follow-horizontally :which-key "h-split")
  "wv" '(window-split-and-follow-vertically :which-key "v-split")
  "wd" '(delete-window :which-key "delete")
  "w0" '(delete-window :which-key "delete")
  "wD" '(delete-other-windows :which-key "delete others")
  "w1" '(delete-other-windows :which-key "delete others")
  "wo" '(other-window :which-key "other")
  "wz" '(hydra-window-zoom/body t :which-key "zoom")
  "wj" '(hydra-window-jump/body t :which-key "jump")
  )

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
 '(line-number-mode nil)
 '(package-selected-packages
   '(general neotree esup ws-butler which-key rainbow-delimiters markdown-mode json-mode js2-mode ivy-rich gruvbox-theme flycheck evil-collection diminish csharp-mode counsel-projectile auto-complete async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
