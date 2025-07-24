(defun my/clear-messages-buffer ()
  "Clear the *Messages* buffer."
  (interactive)
  (with-current-buffer "*Messages*"
    (let ((inhibit-read-only t))
      (erase-buffer))))

;; Keybinding pour nettoyer les messages
(global-set-key (kbd "C-c k m") (quote my/clear-messages-buffer))
