;;;
;;; Thanks @purcell and @aaronbieber whithout you guys, this awesome
;;; changes wouldn't possible.
;;;
;;;

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;; create constant to check OS
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
	  (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
;; Calls (package-initialize)
(require 'init-elpa)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'emmet-mode)

(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-grep)

(require 'init-recentf)
(require 'init-windows)
(require 'init-flycheck)

;; @todo - make it work
;; (require 'init-gtags)

(require 'init-sessions)
(require 'init-font)
(require 'init-spaceline)
(require 'init-mmm)
(require 'init-helm)
(require 'init-linum)
(require 'init-editing-utils)
(require 'init-neotree)

;; @todo - add some features
(require 'init-dired)

(require 'init-projectile)
(require 'init-evil)
(require 'init-multiterm)

(require 'init-markdown)
(require 'init-php)
(require 'init-html)

(require 'init-editorconfig)

(when *spell-check-support-enabled*
  (require 'init-spelling))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)

;;------------------------------------------------------------
;; Finish benchmarking
;;------------------------------------------------------------
(add-hook 'after-init-hook
	  (lambda ()
	    (message "init completed in %.2fms"
		     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)
