;;; init-interface.el --- Provide a interface settings

;;; Commentary:
;;;

;;; Code:

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(menu-bar-mode -1)				; Removes the menu bar
(tool-bar-mode -1)				; Removes the tool bar
(scroll-bar-mode -1)				; Removes the scroll bar
(show-paren-mode 1)				; turn on bracket match highlight
(setq large-file-warning-threshold nil)         ; Removes warning for large files
(setq split-width-threshold nil)
(setq visible-bell nil)
(setq custom-safe-themes t)
(put 'narrow-to-region 'disabled nil)
(desktop-save-mode 1)				; save/restore opened files
(global-hl-line-mode 1)				; turn on highlighting current line
(setq auto-save-default nil)			; stop creating those #auto-save# files

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)


;; ----------------------------------------------------------------------------
;; Fonts
;; ----------------------------------------------------------------------------
(set-frame-font "Fira Mono for Powerline 13")	; Change default font


;; ----------------------------------------------------------------------------
;; Themes
;; ----------------------------------------------------------------------------
(use-package color-theme-sanityinc-solarized :ensure t)
(load-theme 'sanityinc-solarized-dark)


;; ----------------------------------------------------------------------------
;; Match parens
;; ----------------------------------------------------------------------------
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")  ; Change the foreground color
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)    ; Add extra-bold to matched paren
(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

;; rainbow parens, brackets ...
(use-package rainbow-delimiters :ensure t)


;; -----------------------------------------------------------------------------
;; Minubuffer settings
;; -----------------------------------------------------------------------------
(defun rzani/minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun rzani/minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'rzani/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'rzani/minibuffer-exit-hook)

(provide 'init-interface)

;;; init-interface.el ends here
