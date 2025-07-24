;; ~/.emacs.d/init.el
(load (expand-file-name "~/00_PKM/init.el"))

(when (file-exists-p "~/00_PKM/init.el")
  (load (expand-file-name "~/00_PKM/init.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-mode nil)
 '(package-selected-packages nil)
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-guides-even-face ((t (:background "#262626"))))
 '(highlight-indent-guides-odd-face ((t (:background "#2e2e2e"))))
 '(org-modern-keyword ((t (:inherit (org-meta-line org-modern-label)))))
 '(org-modern-priority ((t (:inherit (org-priority org-modern-label)))))
 '(org-modern-tag ((t (:inherit (secondary-selection org-modern-label)))))
 '(org-modern-todo ((t (:inherit (org-todo org-modern-label))))))
