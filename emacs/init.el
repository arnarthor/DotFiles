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

(straight-use-package 'use-package)
(setq straight-use-package-by-default 1)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq auto-save-default nil)
(global-linum-mode 1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)
(electric-pair-mode 1)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy Hacking")

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode  1)

(eval-after-load 'smartparens
  '(progn
     (sp-pair "`" nil :actions :rem)))

(add-hook 'after-change-major-mode-hook
  (lambda ()
    (modify-syntax-entry ?_ "w")))

;; PATH
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode))

;; Themes
(use-package darkokai-theme
  :ensure t
  :config (load-theme 'darkokai t))

;; Ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

;; Company
(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "S-TAB") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)

;; Which key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; vterm
(use-package vterm :ensure t)

;; magit
(use-package magit :ensure t)
(use-package evil-magit :ensure t)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))

;; Neo tree
(use-package neotree :ensure t)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; Reason setup
(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(defun reason-cmd-where (cmd)
  (let ((where (shell-cmd cmd)))
    (if (not (string-equal "unknown flag ----where" where))
      where)))

(let* ((refmt-bin (or (reason-cmd-where "refmt ----where")
                      (shell-cmd "which refmt")
                      (shell-cmd "which bsrefmt")))
       (merlin-bin (or (reason-cmd-where "ocamlmerlin ----where")
                       (shell-cmd "which ocamlmerlin")))
       (merlin-base-dir (when merlin-bin
                          (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
  ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
(when merlin-bin
  (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
  (setq merlin-command merlin-bin))

(when refmt-bin
  (setq refmt-command refmt-bin)))

(use-package merlin :ensure t)

(use-package reason-mode
  :ensure t
  :config
  (add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode))))

(use-package utop :ensure t)

(setq utop-command "opam config exec -- rtop -emacs")
(add-hook 'reason-mode-hook #'utop-minor-mode) 
(setq merlin-completion-with-doc t)

;; undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "SPC" '(counsel-M-x :which-key "show all commands")
  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  ; "SPC" '( :which-key "M-x")
  ;; Files
  "fs" '(save-buffer :which-key "save")
  "ff" '(find-file :which-key "find file")
  ;; Buffers
  "bd" '(evil-delete-buffer :which-key "delete buffer")
  ; "fed" '(load-file "~/.emacs.d/init.el" :which-key "reload configuration")
  "qq" '(evil-quit-all :which-key "quit")
  ;; Magit
  "gs" '(magit :which-key "git status")
  ;; Misc
  "cl" '(evil-commentary-line :which-key "comment line")
  "au" '(undo-tree-visualize :which-key "undo tree")
  ;; Projectile
  "p" '(:keymap projectile-command-map :wk "projectile prefix")
))

;; Reason keybindings
(general-define-key
  :states '(normal visual insert emacs)
  :prefix ","
  :non-normal-prefix "M-,"
  "ht" '(merlin-type-enclosing :wk "show type under cursor")
  "gg" '(merlin-locate :wk "go to definition")
  "gi" '(merlin-switch-to-ml :wk "switch to ml")
  "gI" '(merlin-switch-to-mli :wk "switch to mli")
)

