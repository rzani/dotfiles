(require-package 'helm)
(helm-mode 1)

(setq helm-buffers-fuzzy-matching t
      helm-M-x-fuzzy-match t
      helm-autoresize-mode t
      helm-buffer-max-length 40)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)

;; Term tweaks
(add-hook 'term-mode-hook
	  (lambda()(yas-minor-mode -1)))
(add-hook 'shell-mode
	  (lambda()(yas-minor-mode -1)))

;; Make easy to call helm-buffers-list
(global-set-key (kbd "M-b") 'helm-buffers-list)
(defalias 'list-buffers 'helm-buffers-list)	; make helm-buffers-list default

(provide 'init-helm)
