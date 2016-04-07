;;; package --- sumary
;;; Code:
;;; Commentary:

(require-package 'multi-term)
(require 'multi-term)

(setq multi-term-program "/bin/zsh")

;; term
(defface term-color-black
  '((t (:foreground "#3f3f3f" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-red
  '((t (:foreground "#cc9393" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-green
  '((t (:foreground "#7f9f7f" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-yellow
  '((t (:foreground "#f0dfaf" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-blue
  '((t (:foreground "#6d85ba" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-magenta
  '((t (:foreground "#dc8cc3" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-cyan
  '((t (:foreground "#93e0e3" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-white
  '((t (:foreground "#dcdccc" :background "#272822")))
  "Unhelpful docstring.")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

;; ansi-term colors
(setq ansi-term-color-vector
  [term term-color-black term-color-red term-color-green term-color-yellow
    term-color-blue term-color-magenta term-color-cyan term-color-white])

(add-hook 'term-mode-hook
	  (lambda()(yas-minor-mode -1)))

(provide 'init-multiterm)

;;; init-multiterm ends here
