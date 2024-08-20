;; Set up the visible bell
(setq visible-bell 1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)
;; Old configuration
; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
; (package-initialize)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Set up environment variables, needed to get npm (from nvm) working
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package rainbow-delimiters
 :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; *NOTE:* The first time you load your configuration on a new machine, you'll need to
;; run `M-x all-the-icons-install-fonts` so that mode line icons display correctly.
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))
;; TODO: all-the-icons not supported since 4.0.0, use nerd-fonts instead
;; Do not know how to automatically setup but running M-x nerd-icons-install-fonts after the below line works
(use-package nerd-icons)
;; If seeing lag also add this
;; ;; Don’t compact font caches during GC.
;; (setq inhibit-compacting-font-caches t)

;; See if want to install this
;; (use-package all-the-icons-dired
;;   :if (display-graphic-p)
;;   :hook (dired-mode . all-the-icons-dired-mode)

(use-package doom-themes
  :init (load-theme 'doom-city-lights t))

;; Package will be released in Emacs30
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;; Line numbers + Column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Tabbing and window management
(global-set-key (kbd "M-o") 'other-window)
(windmove-default-keybindings)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)
(global-tab-line-mode t)
(winner-mode t)


;; Auto-completion
;; (fido-mode t)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package swiper)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Mode
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :hook (lsp-mode . efs/lsp-mode-setup)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-code-actions t))
  ;:init
  ;(setq lsp-ui-sideline-show-code-actions t))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;; Commenting
;; Note by default Alt+;, emac does commenting, not sure if uncomments
(use-package evil-nerd-commenter
  :bind("M-/" . evilnc-comment-or-uncomment-lines))

;; Typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Company Mode (Better Completions UI for LSP)
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
	      ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package conda
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniforge3"))
  (setq conda-env-home-directory (expand-file-name "~/miniforge3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (rune/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Evil Mode setup
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t) ;; For some reason does not work right now
  (setq evil-want-C-i-jump nil)
  ;; :hook (evil-mode . rune/evil-hook) ;; Does not work as evil needs to be loaded before 'evil-emacs-states-modes is available, need to somehow fix this
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :custom
  (evil-collection-magit-want-horizontal-movement t))
  ;; (evil-collection-magit-use-y-for-yank t)) ;; Not sure what this does

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))


;; Project Management

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work")
    (setq projectile-search-path '("~/work")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Run counsel projectile rg for ripgrep
;; Pop results out with C-c C-o - ivy-occur (ivy-minibuffer-map)
(use-package counsel-projectile
  :config (counsel-projectile-mode))


;; Source control
(use-package magit
  ;; :commands (magit-status magit-get-current-branch) ;; Do not need anymore
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)) ;; Remove default behaviour of showing diff in different buffer

(use-package forge)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" default))
 '(package-selected-packages
   '(counsel-projectile evil-collection general all-the-icons evil-nerd-commenter lsp-ivy lsp-treemacs doom-themes lsp-ui company-box company exec-path-from-shell typescript-mode lsp-mode gnu-elpa-keyring-update use_package which-key doom-modeline rainbow-delimiters))
 '(tab-bar-history-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

