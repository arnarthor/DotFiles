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

;; Set tab as completion trigger
(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [backtab] 'tab-indent-or-complete)

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

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
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
  ;; Projectile
  "ph" '(counsel-projectile-find-file :which-key "find file")
))
