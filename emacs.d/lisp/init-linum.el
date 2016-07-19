(defcustom linum-relative-current-symbol ""
  "The symbol you want to show on the current line, by default it is 0. You can use any string like \"->\". If this variable is empty string,linum-releative will show the real line number at current line."
  :type 'string
  :group 'linum-relative)

(require-package 'linum-relative)

(linum-relative-mode t)

(custom-set-faces
 '(linum-relative-current-face ((t (:foreground "#a89984" :weight bold)))))

(global-set-key (kbd "<f7>") 'linum-mode)

(provide 'init-linum)
