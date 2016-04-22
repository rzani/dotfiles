;;; init.el --- Provide a init file to emacs

;;; Commentary:
;;;

;;; Code:

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(package-initialize)

; Load .emacs.d/lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; -----------------------------------------------------------------------------
;; Essentials
;; -----------------------------------------------------------------------------
(defconst *is-a-mac* (eq system-type 'darwin))

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

; BACKUP
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)


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

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))


(require 'init-utils)
(require 'init-elpa)                                   ;; require-package

(maybe-require-package 'use-package)
(eval-when-compile
  (require 'use-package))

(require 'init-flycheck)
(require 'init-helm)
(require 'init-multiple-cursors)
(require 'init-move-text)
;; ;(require 'init-whitespace)
;; (require 'init-editorconfig)
(require 'init-linum)
(require 'init-powerline)
;(require 'init-tabbar)
(require 'init-neotree)
(require 'init-smartparens)
(require 'init-fiplr)
(require 'init-multiterm)
(require 'init-ctags)
(require 'init-php)
(require 'init-colors)
(require 'init-evil)
(require 'init-markdown)
(require 'init-web)
(require 'init-windows)

(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . swiper))

;; ;; -----------------------------------------------------------------------------
;; ;; HELM
;; ;; -----------------------------------------------------------------------------

(global-set-key (kbd "M-b") 'helm-buffers-list)
(defalias 'list-buffers 'helm-buffers-list)	; make helm-buffers-list default


;; ;(ido-mode 1)
;; ;(setq ido-separator "\n")			; make ido display choices vertically
;; ;(setq ido-enable-flex-matching t)		; display any item that contains the chars you typed


;; -----------------------------------------------------------------------------
;; User Interface
;; -----------------------------------------------------------------------------

(set-frame-font "Fira Mono for Powerline 13")	; Change default font
(use-package sublime-themes :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)

(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")  ; Change the foreground color
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)    ; Add extra-bold to matched paren

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

;; -----------------------------------------------------------------------------
;; macos stuff
;; -----------------------------------------------------------------------------

(when *is-a-mac*
  (set-default-font "Fira Mono for Powerline 14")	; Change default font
  (global-unset-key (kbd "<home>"))                     ; Unset home key binding
  (global-set-key (kbd "<home>") 'beginning-of-line)    ; Set home key to beginning of line
  (global-unset-key (kbd "<end>"))                      ; Unset end key binding
  (global-set-key (kbd "<end>") 'end-of-line)           ; Set end key to end of line
  (setq ns-use-srgb-colorspace nil))

(provide 'init)
;;; init ends here
