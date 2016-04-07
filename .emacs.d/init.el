;;; init.el --- Provide a init file to emacs

;;; Commentary:
;;;
;;; Content table
;;;     1 - Packages
;;;     2 - Custom key bindings
;;;     3 - User Interface
;;;     4 - MacOS stuff

;;; Code:

;; -----------------------------------------------------------------------------
;; Packages
;; -----------------------------------------------------------------------------

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(defconst *is-a-mac* (eq system-type 'darwin))

(setq package-enable-at-startup nil)

(add-to-list 'load-path
    (expand-file-name "lisp" user-emacs-directory))    ; Load .emacs.d/lisp directory

(require 'init-elpa)                                   ;; require-package

(require 'init-utils)

(require 'init-helm)

(require 'init-flycheck)

(require 'init-multiple-cursors)

(require 'init-move-text)

;(require 'init-whitespace)

(require 'init-editorconfig)

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

(require 'init-windows)

;; -----------------------------------------------------------------------------
;; Custom key bindings
;; -----------------------------------------------------------------------------

;; inserting lines
(define-key evil-motion-state-map (kbd "C-o") 'insert-line-after)
(define-key evil-motion-state-map (kbd "C-S-o") 'insert-line-before)

(global-set-key (kbd "M-b") 'helm-buffers-list)

;; -----------------------------------------------------------------------------
;; User Interface
;; -----------------------------------------------------------------------------

(require-package 'atom-one-dark-theme)          ; require theme
(load-theme 'atom-one-dark t)			; load theme

(set-default-font "Fira Mono for Powerline 13")	; Change default font

;; Paren match
(require 'paren)
(show-paren-mode 1)				; turn on bracket match highlight
(set-face-background 'show-paren-match          ; Change the background color
		     (face-background 'default))
(set-face-foreground 'show-paren-match "#def")  ; Change the foreground color
(set-face-attribute 'show-paren-match
		    nil :weight 'extra-bold)    ; Add extra-bold to matched paren

(setq make-backup-files nil)			; stop creating those backup~ files
(setq auto-save-default nil)			; stop creating those #auto-save# files
(desktop-save-mode 1)				; save/restore opened files
(global-hl-line-mode 1)				; turn on highlighting current line
(defalias 'list-buffers 'helm-buffers-list)	; make helm-buffers-list default

;(ido-mode 1)
;(setq ido-separator "\n")			; make ido display choices vertically
;(setq ido-enable-flex-matching t)		; display any item that contains the chars you typed

(menu-bar-mode -1)				; Removes the menu bar
(tool-bar-mode -1)				; Removes the tool bar
(scroll-bar-mode -1)				; Removes the scroll bar

;; Set custom settings to ~/.emacs.d/custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; -----------------------------------------------------------------------------
;; MacOS stuff
;; -----------------------------------------------------------------------------

(when *is-a-mac*
  (set-default-font "Fira Mono for Powerline 14")	; Change default font
  (global-unset-key (kbd "<home>"))                     ; Unset home key binding
  (global-set-key (kbd "<home>") 'beginning-of-line)    ; Set home key to beginning of line
  (global-unset-key (kbd "<end>"))                      ; Unset end key binding
  (global-set-key (kbd "<end>") 'end-of-line)           ; Set end key to end of line
)

(provide 'init)
;;; init ends here
