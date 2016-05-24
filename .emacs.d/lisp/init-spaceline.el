;;; init-spaceline.el --- Provide a init file to emacs

;;; Commentary:
;;;

;;; Code:

(custom-set-faces '(mode-line ((t (:background "#073642" :foreground "#93a1a1" :weight bold)))))

(custom-set-faces '(powerline-active1 ((t (:background "#fdf6e3" :foreground "#002b36" :weight bold)))))

(custom-set-faces '(powerline-active2 ((t (:background "#fdf6e3" :foreground "#002b36" :weight bold)))))

(set-face-attribute 'mode-line nil :box nil)

;; "arrow" "arrow-fade" "slant" "chamfer" "wave" "brace" "roundstub" "zigzag" "butt" "rounded" "contour" "curve"
(setq powerline-default-separator 'contour)

(setq spaceline-workspace-numbers-unicode 't)
(setq spaceline-window-numbers-unicode 't)

(use-package persp-mode :ensure t)

(use-package eyebrowse :ensure t)

(use-package window-numbering :ensure t)

(use-package auto-compile :ensure t)

(use-package anzu :ensure t)

(use-package org-pomodoro :ensure t)

(defun spaceline-rzani-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.
ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'spaceline--theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :separator "|"
           :face highlight-face)
         '(buffer-modified buffer-size buffer-id remote-host)
         additional-segments))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-rzani-theme)
  (spaceline-helm-mode)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))


(setq display-time-24hr-format t)
(display-time-mode t)

(provide 'init-spaceline)
;;; init-spaceline.el ends here
