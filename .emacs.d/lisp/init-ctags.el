;;; init-ctags.el -- Configuration for ctags
;;; Commentary:
;;; Code:

(require-package 'ctags)

(setq path-to-ctags "~/ctags/tags") ;; <- Your ctags path here
(setq ctags-command "ctags-exuberant")

(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDiRectory: ")
    (when *is-a-mac*
      (setq ctags-command "ctags"))
    (shell-command
     (format "%s -f %s -e -R %s" ctags-command path-to-ctags (directory-file-name dir-name)))
    )

(provide 'init-ctags)
;;; init-ctags.el ends here
