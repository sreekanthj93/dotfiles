;;; init.el

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; Leave this here, or package.el will just add it again.
(package-initialize)

;; Set encoding to utf-8
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking) ;; Measure startup time

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

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; White Space configurations
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face empty tabs lines-tail trailin))
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

(provide 'init)
;;; init.el ends here
