;;; init-helm.el --- Helm config

;;; Commentary:

;;; Code:
(require-package 'helm)
(require 'helm-config)
(helm-mode 1)

;; Fuzzy matching
(setq helm-buffers-fuzzy-matching t)                    ; regex match at buffers
(setq helm-M-x-fuzzy-match t)                           ; regex match at
(setq helm-buffer-max-length 40)

;; Key bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(add-hook 'term-mode-hook
	  (lambda()(yas-minor-mode -1)))
(add-hook 'shell-mode
	  (lambda()(yas-minor-mode -1)))

(provide 'init-helm)
;;; init-helm ends here 
