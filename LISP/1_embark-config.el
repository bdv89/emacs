;;; embark-config.el --- Embark integration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures and integrates Embark (and Embark-consult) into Emacs.
;; Place this file in your Emacs load-path (for example ~/.emacs.d/lisp/),
;; then add (require 'embark-config) in your init.el to load it.

;;; Code:

;; Dynamically add this file's directory to `load-path`
(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (when dir
    (add-to-list 'load-path dir)))

(require 'use-package)

(use-package embark
  :ensure t
  ;; Keybindings centralisés dans keybindings.el
  :init
  ;; Use Embark's prefix help for more informative binding hints
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after embark consult
  :hook
  ;; Enable live previews in the Embark collect buffer using Consult
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'embark-config)
;;; embark-config.el ends here
