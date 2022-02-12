;;; init.el -- My Emacs's Init File
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimizations making Emacs start up faster
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(defvar package-list '(package naysayer-theme diminish evil async project xref eldoc
			       csharp-mode js2-mode json-mode markdown-mode
			       yaml-mode which-key recentf vertico savehist orderless consult marginalia
			       auto-complete rainbow-delimiters bazel wgrep embark embark-consult
 			       evil-collection ws-butler general hydra eglot))

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
    (let (l x y w h)(setq l (read (buffer-string))
          x (nth 0 l)
          y (nth 1 l)
          w (nth 2 l)
          h (nth 3 l))
    (setq initial-frame-alist
          (list (cons 'top y) (cons 'left x) (cons 'width w) (cons 'height h)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(require 'naysayer-theme)
(load-theme 'naysayer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode
(setq-default evil-want-integration t)
(setq-default evil-want-keybinding nil)
(setq-default evil-search-module 'evil-search) ; todo replace by swiper or equivalent
(require 'evil)
(evil-mode 1)
(evil-collection-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor

;; Delete white space at end of edited lines
(require 'ws-butler)
(add-hooks '(prog-mode-hook) 'ws-butler-mode)

;; 120 column max
(setq-default whitespace-line-column 120
	      whitespace-style '(face lines-tail)
              set-fill-column 120)
(add-hooks '(prog-mode-hook tex-mode-hook text-mode-hook) 'whitespace-mode)


;; spell check
(require 'flymake)
(add-hooks '(prog-mode-hook tex-mode-hook text-mode-hook) 'flymake-mode)

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
(add-hooks '(prog-mode-hook tex-mode-hook text-mode-hook) 'disable-line-wrap)
(add-to-list 'auto-mode-alist '("\\.log\\'" . text-mode))

;; Line numbers
(add-hooks '(prog-mode-hook tex-mode-hook text-mode-hook) 'display-line-numbers-mode)

;; Highlight current line
(require 'hl-line)
(add-hooks '(prog-mode-hook tex-mode-hook text-mode-hook) 'hl-line-mode)
(set-face-background 'hl-line "#041c23")

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
(recentf-mode)
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
(require 'vertico)
(setq vertico-cycle t)
(vertico-mode)

(require 'consult)
(setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

(require 'orderless)
(setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

(require 'savehist)
(savehist-mode)

(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode)

(require 'wgrep)

(require 'embark)
(require 'embark-consult)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "/usr/local/Cellar/llvm/13.0.0_1/bin/clangd"))
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(require 'bazel)

(require 'which-key)
(which-key-mode 1)

(require 'auto-complete)
(global-auto-complete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (shell . t)
   (emacs-lisp . t)
   (python . t)
   ))
(setq org-confirm-babel-evaluate nil)

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
                        (concat " " (concat (upcase (symbol-name evil-state)) " "))))
               " "

               ;; was this buffer modified since the last save?
               '(:eval (if (buffer-modified-p)
                         (propertize "!"
                                     'help-echo "Buffer has been modified") " "))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b  " 'help-echo (buffer-file-name)))

               ;; column and line
               (propertize "%c")
               ":"
               (propertize "%l")

               ;; encoding
               "  "
               (propertize (upcase (symbol-name locale-coding-system)))
               "  "

               ;; the current major mode for the buffer.
               '(:eval (propertize "%m"
                                   'help-echo buffer-file-coding-system))

               ;; relative position, size of file
               "  "
               (propertize "%p" ) ;; % above top
               "/"
               (propertize "%I" ) ;; size

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
(diminish 'flymake-mode)
(diminish 'undo-tree-mode)
(diminish 'eldoc-mode)
(diminish 'ws-butler-mode)
(diminish 'whitespace-mode)
(diminish 'embark-mode)

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

(defhydra hydra-vertico (vertico-map nil)
  "vertico"
  ("C-j" vertico-next)
  ("C-k" vertico-previous)
  ("C-e" wgrep))

(defhydra hydra-embark (minibuffer-local-map nil)
  "vertico"
  ("C-." embark-act)
  ("C-;" embark-dwim)
  ("C-e" wgrep))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Key Map
(require 'general)
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 "C-x C-f" '(find-file :which-key "find file")
 "C-s" '(consult-line :which-key "swiper")
 "C-p" '(project-find-file :which-key "find project file")
 "C-x 2" '(window-split-and-follow-horizontally :which-key "window split horizontal and follow")
 "C-x 3" '(window-split-and-follow-vertically :which-key "window split vertical and follow")
 "M-g" '(eglot-find-declaration :which-key "find declaration")
 "M-G" '(eglot-find-implementation :which-key "find definition")
 "<f8>" '(flymake-goto-next-error :which-key "goto next error")
 "<f7>" '(bazel-build :which-key "build")
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
  "bb" '(bookmark-set :which-key "set bookmark")
  "bl" '(bookmark-bmenu-list :which-key "browse bookmark")
  "TAB" '(consult-buffer :which-key "switch buffer")

  ;; compile/code
  "c" '(:ignore t :which-key "Compile/Code")
  "cc" '(project-compile :which-key "compile")
  "c/" '(comment-or-uncomment-region-or-line :which-key "comment")
  "ce" '(consult-compile-error :which-key "error")
  "c." '(eglot-code-action-quickfix :which-key "quickfix")
  "cr" '(eglot-rename :which-key "rename")
  "cf" '(eglot-format :which-key "format")

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
  "ff" '(find-file :which-key "find file")
  "fr" '(consult-recent-file :which-key "find recent")
  "fc" '(load-emacs-config :which-key "emacs config")

  ;; help
  "h" '(:ignore t :which-key "Help")
  "hf" '(describe-function :which-key "function")
  "hv" '(describe-variable :which-key "variable")
  "hs" '(describe-symbol :which-key "symbol")
  "hb" '(describe-bindings :which-key "binding")
  "hm" '(describe-mode :which-key "mode")
  "hk" '(describe-key :which-key "key")

  ;; other
  "SPC" '(execute-extended-command :which-key "M-x")

  ;; project
  "p" '(:ignore t :which-key "Project")
  "pf" '(project-find-file :which-key "find file")
  "ps" '(project-switch-project :which-key "switch")

  ;; registers
  "r" '(:ignore t :which-key "Register")
  "rr" '(point-to-register :which-key "set point")
  "rj" '(jump-to-register :which-key "jump")
  "rw" '(window-configuration-to-register :which-key "set window")
  "rf" '(frameset-to-register :which-key "set frame")
  "rl" '(consult-register :which-key "set frame")

  ;; search
  "s" '(:ignore t :which-key "Search")
  "ss" '(consult-line :which-key "swiper")
  "sg" '(consult-ripgrep :which-key "grep")
  "sh" '(highlight-regexp :which-key "highlight")
  "su" '(unhighlight-regexp :which-key "unhighlight")
  "sr" '(unhighlight-regexp :which-key "replace")

  ;; vc
  "v" '(:ignore t :which-key "Vcs")
  "vn" '(vc-next-action :which-key "next action")
  "vP" '(vc-pull : which-key "pull")
  "vp" '(vc-push : which-key "push")
  "vl" '(vc-print-log : which-key "file log")
  "vL" '(vc-print-root-log : which-key "root log")
  "vd" '(vc-diff : which-key "file log")
  "vD" '(vc-root-diff : which-key "root log")

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
 '(package-selected-packages
   '(yaml-mode ws-butler which-key vertico rainbow-delimiters orderless naysayer-theme markdown-mode marginalia json-mode js2-mode hydra general flycheck evil-collection eglot diminish csharp-mode consult clang-format bazel auto-complete async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
