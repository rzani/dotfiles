;; "arrow" "arrow-fade" "slant" "chamfer" "wave" "brace" "roundstub" "zigzag" "butt" "rounded" "contour" "curve"
(setq powerline-default-separator 'wave)
(setq spaceline-workspace-numbers-unicode 't)
(setq spaceline-window-numbers-unicode 't)
(setq spaceline-minor-modes-separator '" ")
(setq spaceline-separator-dir-left '(left . left))
(setq spaceline-separator-dir-right '(right . right))

(custom-set-faces '(mode-line ((t (:box nil)))))
(custom-set-faces '(powerline-active1 ((t (:foreground "#93a1a1" :background "#073642")))))
(custom-set-faces '(powerline-inactive1 ((t (:background "#93a1a1")))))
(custom-set-faces '(powerline-inactive2 ((t (:background "#93a1a1")))))

(require-package 'eyebrowse)
(require-package 'window-numbering)
(require-package 'auto-compile)
(require-package 'org-pomodoro)

(defun rzani/spaceline-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.
ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'spaceline--theme
         '((workspace-number
            window-number)
           :fallback evil-state
           :separator "|"
           :face highlight-face)
         '(buffer-modified buffer-size buffer-id remote-host)
         additional-segments))

(require-package 'spaceline)

(require 'spaceline-config)
(rzani/spaceline-theme)
(spaceline-helm-mode)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

(setq display-time-24hr-format t)
(display-time-mode t)

(set-face-attribute 'mode-line-inactive nil :box nil)

(provide 'init-spaceline)
