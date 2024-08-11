;; Set up the visible bell
(setq visible-bell 1)


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
  (package-install 'use_package))

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
                term-mode-hook))
    ;shell-mode-hook
    ;            eshell-mode-hook
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Tabbing and window management
(global-set-key (kbd "M-o") 'other-window)
(windmove-default-keybindings)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)
(global-tab-line-mode t)
(winner-mode t)


(fido-mode t)

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
  :hook (lsp-mode . lsp-ui-mode))
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-code-actions t))
  ;:init
  ;(setq lsp-ui-sideline-show-code-actions t))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ui company-box company exec-path-from-shell typescript-mode lsp-mode gnu-elpa-keyring-update use_package which-key doom-modeline rainbow-delimiters))
 '(tab-bar-history-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 
