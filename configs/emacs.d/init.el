;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary: my own init.el

;; Produce backtraces when errors occur
;;(setq debug-on-error t)

;; Leave this here, or package.el will just add it again.
(package-initialize)

;; Set encoding to utf-8
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Show line numbers by default
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Ignore error function
(defun ignore-error-wrapper (fn)
  (let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Measure startup time
(require 'init-benchmarking)

;; All emacs lisp files are under emacs.d/lisp
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

;; Also add themes path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq vc-follow-symlinks t)
(setq custom-safe-themes t)
(column-number-mode t)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(global-set-key (kbd "C-c <left>")  (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "C-c <right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "C-c <up>")    (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "C-c <down>")  (ignore-error-wrapper 'windmove-down))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; White Space configurations
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; (setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq c-backspace-function 'backward-delete-char)
(setq-default c-basic-offset 4
              c-default-style "allman"
              tab-width 4)

(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face empty tabs spaces lines-tail trailing))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (tab-mark 9 [8594 9] [92 9])
        ))
(global-whitespace-mode t)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Save last opened file line ;)
(save-place-mode 1)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; gnu global
(use-package ggtags :ensure t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; EVIL MODE Setup work in progress
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Undo tree dependency for evil
(use-package undo-tree :ensure t)
(global-undo-tree-mode)

;; goto-chg dependency for evil
(use-package goto-chg :ensure t)
(require 'goto-chg)

(use-package evil :ensure t)
(setq evil-toggle-key "")
(require 'evil)
(setq evil-default-state 'emacs)
(evil-mode 0)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; THEMES
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package zenburn-theme :ensure t :defer t)
(use-package spacemacs-common :ensure spacemacs-theme)
(load-theme 'spacemacs-dark t)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Clang Formatting setup
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package clang-format :ensure t :defer t)
(require 'clang-format)
(global-set-key (kbd "C-c r") 'clang-format-region)
(global-set-key (kbd "C-c b") 'clang-format-buffer)
;; Set style option for clang-format
;;(setq clang-format-style-option "WebKit")
(setq clang-format-style-option "'{BasedOnStyle: WebKit, AlignConsecutiveAssignments: true, Standard: LS_Auto, TabWidth: 4, UseTab: UT_ForIndentation}'")
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Editor Config plugin
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package editorconfig :ensure t :config (editorconfig-mode 1))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Perspective package to differentiate buffers in frames
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package perspective :ensure t :defer t)
(require 'perspective)
(persp-mode)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Company auto completion
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package company :ensure t :defer t)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-TAB") 'company-complete-common)
;; Facing
(deftheme spacemacs-dark-overrides)
(let (
		(class '((class color) (min-colors 257)))
		(terminal-class '((class color) (min-colors 89)))
	)
	(custom-theme-set-faces 'spacemacs-dark-overrides
		;; Company tweaks.
		`(company-tooltip-common
			((t :foreground "magenta"
				:background "black"
				:underline t))
		)
		`(company-template-field
			((t :inherit company-tooltip
				:foreground "magenta")))
		`(company-tooltip-selection
			((t :background "gray40"
				:foreground "magenta")))
		`(company-tooltip-common-selection
			((t :foreground "orange"
				:background "gray40"
				:underline t)))
		`(company-scrollbar-fg
			((t :background "magenta")))
		`(company-tooltip-annotation
			((t :inherit company-tooltip
				:foreground "magenta")))
		;; Popup menu tweaks.
		`(popup-menu-face
			((t :foreground "magenta"
				:background "black")))
		;; Popup menu tweaks.
		`(popup-menu-selection-face
			((t :background "magenta"
				:foreground "black")))
		;; Linum and mode-line improvements
		`(linum
			((,class :foreground "magenta"
					:background "black")))
		;; Custom region colouring.
		`(region
			((,class :foreground "magenta"
					:background "black")
			(,terminal-class :foreground "magenta"
							:background "black")))
	)
)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Projectile for projects
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(setq projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0")
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(provide 'init)
;;; init.el ends here
