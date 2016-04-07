;;; package --- sumary
;;; Code:
;;; Commentary:

(require-package 'fiplr)
(global-set-key (kbd "M-p") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
			    (files ("*.jpg" "*.png" "*.zip" "*~"))))

(provide 'init-fiplr)
;;; init-fiplr ends here
