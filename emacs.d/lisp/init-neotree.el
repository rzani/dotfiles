(require-package 'neotree)

(global-set-key [f8] 'neotree-toggle)

(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "m") 'neotree-create-node)
	    (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
	    (define-key evil-normal-state-local-map (kbd "r") 'neotree-rename-node)
	    (define-key evil-normal-state-local-map (kbd "c") 'neotree-change-root)
	    (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
	    (define-key evil-normal-state-local-map (kbd "n") 'neotree-hidden-file-toggle)
	    (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(provide 'init-neotree)
