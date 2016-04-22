;;; init-helm.el --- Helm config

;;; Commentary:

;;; Code:
(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)                           ; regex match at
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  ;; (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

;; Term tweaks
(add-hook 'term-mode-hook
	  (lambda()(yas-minor-mode -1)))
(add-hook 'shell-mode
	  (lambda()(yas-minor-mode -1)))

(provide 'init-helm)

;;; init-helm ends here
