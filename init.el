;;; package --- Summary
;;; Commentary:
(setq gc-cons-threshold (* 50 1024 1024))
(set-frame-position (selected-frame) 85 40)
(set-frame-width (selected-frame) 192)
(set-frame-height (selected-frame) 43)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'doom-molokai t)

(set-face-attribute 'default nil :font (font-spec :family "Consolas" :weight 'regular :height 122))
(set-fontset-font t 'han (font-spec :family "黑体" :weight 'regular :height 122))
;;============
;;Set Channels
;;============
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
;;		monokai-theme
		rainbow-delimiters
		neotree
		kind-icon
		all-the-icons
		cape
		doom-modeline
		doom-themes
		dirvish
		all-the-icons-dired
		projectile
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


;;============
;;base
;;============
(hl-line-mode t)
(show-paren-mode t)
(global-linum-mode t)
(size-indication-mode t)
(electric-pair-mode t)
(global-auto-revert-mode t)
(delete-selection-mode t)
(setq auto-save-default nil
      make-backup-files nil)


(setq confirm-kill-emacs #'yes-or-no-p)
(set-locale-environment "en_US.utf-8")
(setq system-time-locale "C")
(defalias 'yes-or-no-p 'y-or-n-p)

;;============
;;which-key
;;============
(which-key-mode)

;;============
;;computerLanguage
;;===========
(defun g++-compile-and-run()
  (interactive)
  (eshell-command
   (format "cd %s; g++ %s.cpp -o %s;"
	   (file-name-directory (buffer-file-name))
	   (file-name-base (buffer-file-name))
	   (file-name-base (buffer-file-name))
	   (file-name-base (buffer-file-name)))))
;;============
;;org
;;============
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("☰" "☷" "☯" "☭" "⋐"))
(setq org-ellipsis " ▼ ")
(setq org-todo-keywords
      '((sequence "TODO(t!)" "HACK(h!)" "DONE(d!)" "CANCELED(c!)")))
(setq org-todo-keyword-faces '(("TODO" . (:foreground "red" :weight bold))
			       ("HACK" . (:foreground "yellow" :weight bold))
			       ("DONE" . (:foreground "green" :weight bold))
			       ("CANCELED" . (:foreground "gray"))))

(defface hi-red-b '((t (:foreground "purple"))) t)
(defun org-red-highlight ()
  (hi-lock-mode 1)
  (interactive)
  (highlight-regexp "[ \\t]\\(\\*\\(\\S-[^*]+\\S-\\|[^*]\\{1,2\\}\\)\\*\\)[ \\t\\n]*" 'hi-red-b))
(add-hook 'org-mode-hook 'org-red-highlight)

;;============
;;corfu
;;============
(global-corfu-mode)
(setq corfu-auto t
      corfu-quit-no-match 'separator)
(use-package kind-icon ;;kind-icon美化
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package cape
  :init
  (setq-local completion-at-point-functions
	      (list (cape-super-capf
		     #'cape-dabbrev
		     #'cape-file
		     #'cape-elisp-block))))

;;============
;;orderless
;;============
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;;============
;;vertico
;;============
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

;;============
;;racket
;;============
(setq scheme-program-name "racket")

;;============
;;undo-tree
;;============
(setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/.cache/")))
(global-undo-tree-mode t)


;;============
;;yasnippet
;;============
(setq yas-snippet-dirs
	 '("~/.emacs.d/snippets"))
(yas-global-mode)

;;============
;;rainbow-delimiters
;;============
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;============
;;flycheck
;;============
(add-hook 'prog-mode-hook 'flycheck-mode)

;;============
;;goodScroll
;;============
(good-scroll-mode t)

;;============
;;有道翻译
;;============
(setq url-automatic-caching t)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
(setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")

;;============
;;swiper
;;============
(global-set-key (kbd "C-s") 'swiper)

;;============
;;doom-modeline
;;============
(doom-modeline-mode t)

;;============
;;dashboard
;;============
(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq-default dashboard-startup-banner 'logo)
  (dashboard-open))

;;============
;;dirvish
;;============
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(dirvish-override-dired-mode t)
(dirvish-side)

(provide 'init)
;;; init.el ends here
