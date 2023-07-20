;(require 'package)
(set-frame-position (selected-frame) 85 40)
(set-frame-width (selected-frame) 170)
(set-frame-height (selected-frame) 40)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code" :foundry "outline" :slant normal :weight normal :height 102 :width normal)))))


;=======================================================
;Set Channels
;=======================================================
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line

(defvar my/packages '(
		use-package
		corfu
		swiper
		vertico
		orderless
	        flycheck
	        yasnippet
		yasnippet-snippets
		dashboard
		org-bullets
		neotree
		racket-mode
		good-scroll
		auctex
		which-key
		highlight-symbol
		monokai-theme
		rainbow-delimiters
		neotree
		kind-icon
		all-the-icons
		spaceline
		) "Default packages")

 (setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (setq tmp t)
  (dolist (pkg my/packages)
       (when (not (package-installed-p pkg)) (setq tmp nil)))
  tmp)

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))
;========================================================
;basic Settings
;;======================================================
(show-paren-mode t) ;括号匹配高亮

(size-indication-mode t)
(electric-pair-mode t)                       ; 自动补全括号
(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(setq confirm-kill-emacs #'yes-or-no-p)

(set-locale-environment "en_US.utf-8")
(setq system-time-locale "C")
(setq auto-save-default nil
      make-backup-files nil)
(use-package emacs :config (defalias 'yes-or-no-p 'y-or-n-p))

(setq gc-cons-threshold (* 50 1000 1000))
;;==============================================
;;hightlight-symbol
;;=============================================
(use-package highlight-symbol
  :defer 1
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol))


;===============================================
;dashboard
;================================================

(use-package dashboard
  :defer 1
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq-default dashboard-startup-banner 'logo))

;;============================================
;;which-key
;;===============================================
(use-package which-key
  :defer 1
  :init (which-key-mode))


;;===============================================
;;neotree
;;================================================
(use-package neotree
  :defer 1
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-show-hidden-files t)
  (setq neo-window-fixed-size nil)
  (setq-default neo-window-width 37)
  (neotree))


;;=======================================
;;orgSet
;;========================================
;;  (require 'org-bullets)
(use-package org-bullets
  :defer 1
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-todo-keywords
	'((sequence "TODO(t!)" "HACK(h!)" "DONE(d!)" "CANCELED(c!)")))
  (setq org-todo-keyword-faces '(("TODO" . (:foreground "red" :background "clear" :weight bold))
				 ("HACK" . (:foreground "yellow" :background "clear" :weight bold))
				 ("DONE" . (:foreground "green" :background "clear" :weight bold))
				 ("CANCELED" . (:foreground "gray" :background "clear")))))


;=================================================
;corfu
;;================================================
(use-package corfu
  :defer 1
  :init
  (global-corfu-mode)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (setq corfu-auto t
	corfu-quit-no-match 'separator))

(use-package kind-icon ;;kind-icon美化
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;===========================================================
;;orderless
;;===========================================================
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;;=======================================
;;vertico
;;======================================
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))
(use-package savehist
  :init
  (savehist-mode))
(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

;================================================
;racket
;;===============================================
(use-package racket
  :defer 1
  :init
  (setq scheme-program-name "racket"))

;;================================================
;;undo-tree
;;================================================
(use-package undo-tree
  :defer 1
  :init (global-undo-tree-mode)
  :after hydra
  :bind ("C-x C-u" . undo-tree-undo)
  :bind ("C-x C-r" . undo-tree-redo))


;;=================================================
;;yasnippet
;;=================================================
(use-package yas-global-mode
  :defer 1
  :init
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer 1
  :after yasnippet)
;;================================================
;;rainbow-delimiters
;;=============================================
(use-package rainbow-delimiters
  :defer 1
  :hook (prog-mode . rainbow-delimiters-mode))

;;==============================================
;;flycheck
;;==============================================
(use-package global-flycheck-mode
  :defer 1
  :init
  (global-flycheck-mode 1))

;=================================================
;goodScroll
;;=================================================
(use-package good-scroll
  :defer 1
  :init
  (good-scroll-mode t))

;==================================================
;有道翻译
;==================================================
;;youdao
(setq url-automatic-caching t)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
(setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")

;;swiper

(global-set-key (kbd "C-s") 'swiper) 




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "288482f5c627c1fe5a1d26fcc17ec6ca8837f36bf940db809895bf3f8e2e4edd" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" default))
 '(display-time-mode t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(vertico orderless neotree kind-icon corfu hydra yasnippet-snippets highlight-symbol use-package-hydra which-key spaceline atom-one-dark-theme undo-tree rainbow-delimiters yasnippets auctex dashboard swiper youdao-dictionary monokai-theme))
 '(size-indication-mode t)
 '(tool-bar-mode nil))


;;======================================================
;;spaceline
;;=====================================================;
(use-package spaceline
  :defer 1
  :config
  (use-package spaceline-config
    :config
    (spaceline-toggle-minor-modes-on)
    (spaceline-toggle-buffer-encoding-on)
    (spaceline-toggle-buffer-encoding-abbrev-on)
    (setq powerline-default-separator 'rounded)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
    (spaceline-emacs-theme 'date 'time)))


(provide 'init)

;;; init.el ends here
