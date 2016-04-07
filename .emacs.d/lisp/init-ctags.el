;;; init-ctags.el -- Configuration for ctags
;;; Commentary:
;;; Code:

(require-package 'ctags)

(setq path-to-ctags "/home/rzani/ctags/tags") ;; <- Your ctags path here

(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDiRectory: ")
    (shell-command
     (format "ctags-exuberant -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
    )

(provide 'init-ctags)
;;; init-ctags.el ends here
