;;; package --- Summary
;;; Commentary:
(setq gc-cons-threshold (* 50 1024 1024))
(set-frame-position (selected-frame) 85 40)
(set-frame-width (selected-frame) 192)
(set-frame-height (selected-frame) 43)
;;=======================================================
;Set Channels
;;=======================================================
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
		undo-tree
		auctex
		which-key
		monokai-theme
		rainbow-delimiters
		neotree
		kind-icon
		all-the-icons
		spaceline
		cape
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
(add-hook 'prog-mode-hook
	  (lambda()
	    (show-paren-mode t) ;括号匹配高亮
	    (size-indication-mode t)
	    (electric-pair-mode t)                       ; 自动补全括号
	    (global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
	    (delete-selection-mode t)))                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq confirm-kill-emacs #'yes-or-no-p)

(set-locale-environment "en_US.utf-8")
(setq system-time-locale "C")
(setq auto-save-default nil
      make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)


;===============================================
;dashboard
;================================================

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq-default dashboard-startup-banner 'logo)
  (dashboard-open))
;;============================================
;;which-key
;;===============================================
(which-key-mode)


;;===============================================
;;neotree
;;================================================
(use-package neotree
  :defer 1
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-show-hidden-files t)
  (setq-default neo-window-width 37)
  (neotree)) 

;;=======================================
;;orgSet
;;========================================
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("☰" "☷" "☯" "☭" "⋐"))
(setq org-ellipsis " ▼ ")
(setq org-todo-keywords
      '((sequence "TODO(t!)" "HACK(h!)" "DONE(d!)" "CANCELED(c!)")))
(setq org-todo-keyword-faces '(("TODO" . (:foreground "red" :weight bold))
			       ("HACK" . (:foreground "yellow" :weight bold))
			       ("DONE" . (:foreground "green" :weight bold))
			       ("CANCELED" . (:foreground "gray"))))

(defface hi-red-b '((t (:foreground "#e50062"))) t)
(defface hi-blue-b '((t (:foreground "#6200e5"))) t)
(defun org-red-highlight ()
  (interactive)
  (hi-lock-mode t)
  (highlight-regexp "[ \\t]\\(\\*\\(\\S-[^*]+\\S-\\|[^*]\\{1,2\\}\\)\\*\\)[ \\t\\n]*" 'hi-red-b))
(add-hook 'org-mode-hook 'org-red-highlight)
;=================================================
;corfu
;;================================================
(add-hook 'prog-mode-hook 'corfu-mode)
(setq corfu-auto t
      corfu-quit-no-match 'separator)
(use-package kind-icon ;;kind-icon美化
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))
;;===========================================================
;;orderless
;;===========================================================
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;;=======================================
;;vertico
;;======================================
;; Enable vertico
(add-hook
 'minibuffer-mode-hook
 (progn
   (vertico-mode)
   (savehist-mode)
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
   (setq enable-recursive-minibuffers t)))

;================================================
;racket
;;===============================================
(setq scheme-program-name "racket")

;;================================================
;;undo-tree
;;================================================
(setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/.cache/")))
(global-undo-tree-mode)


;;=================================================
;;yasnippet
;;=================================================
(add-hook
 'prog-mode-hook
 (progn
   (setq yas-snippet-dirs
	      '("~/.emacs.d/snippets"))
   (yas-global-mode)))



;;================================================
;;rainbow-delimiters
;;=============================================
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;==============================================
;;flycheck
;;==============================================
(add-hook 'prog-mode-hook 'flycheck-mode)

;=================================================
;goodScroll
;;=================================================
(good-scroll-mode t)

;==================================================
;有道翻译
;==================================================
;;youdao
(setq url-automatic-caching t)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
(setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")

;;swiper
(global-set-key (kbd "C-s") 'swiper)


;;======================================================
;;spaceline
;;=====================================================;
(use-package spaceline
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
   '(company use-package corfu swiper vertico orderless flycheck yasnippet yasnippet-snippets dashboard org-bullets neotree racket-mode good-scroll undo-tree auctex which-key monokai-theme rainbow-delimiters neotree kind-icon all-the-icons spaceline cape))
 '(safe-local-variable-values '((eval org-content 3)))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(set-face-attribute 'default nil :font (font-spec :family "Consolas" :weight 'regular :height 122))
(set-fontset-font t 'han (font-spec :family "黑体" :weight 'regular :height 122))
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
