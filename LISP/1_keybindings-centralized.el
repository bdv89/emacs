;;; 1_keybindings-centralized.el --- Centralisation DRY des keybindings

;; Auteur: Configuration Emacs PKM
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Ce fichier centralise les keybindings par groupes logiques pour éviter
;; les répétitions (DRY) et les conflits entre modules.

;;; Code:

;; --------------------------------------------------
;; Fonction utilitaire pour définir des groupes de keybindings
;; --------------------------------------------------

(defun my/setup-keybinding-group (prefix bindings description)
  "Configure un groupe de keybindings avec PREFIX.
BINDINGS est une liste de (suffix function description).
DESCRIPTION explique le groupe."
  (message "Setting up %s keybindings..." description)
  (dolist (binding bindings)
    (let ((key (concat prefix " " (car binding)))
          (func (cadr binding))
          (desc (caddr binding)))
      (when (fboundp func)
        (global-set-key (kbd key) func)
        (message "  ✓ %s → %s (%s)" key func desc))
      (unless (fboundp func)
        (message "  ✗ %s → %s (function not found)" key func)))))

;; --------------------------------------------------
;; Résolution des conflits C-c f (File operations)
;; --------------------------------------------------

(defun my/resolve-file-keybindings-conflicts ()
  "Résout les conflits dans le groupe C-c f en réorganisant logiquement."
  (interactive)
  
  ;; Déplacer frequency tracking vers C-c F (majuscule)
  (when (fboundp 'my/update-frequency-md)
    (global-unset-key (kbd "C-c f u"))
    (global-unset-key (kbd "C-c f s"))
    (global-unset-key (kbd "C-c f r"))
    (global-unset-key (kbd "C-c f b"))
    
    (global-set-key (kbd "C-c F u") #'my/update-frequency-md)
    (global-set-key (kbd "C-c F s") #'my/frequency-stats)
    (global-set-key (kbd "C-c F r") #'my/frequency-reset)
    (global-set-key (kbd "C-c F b") #'my/frequency-backup))
  
  ;; Déplacer folding state vers C-c v (View state)
  (when (fboundp 'my-org-save-fold-state)
    (global-unset-key (kbd "C-c f s"))
    (global-unset-key (kbd "C-c f r"))
    
    (global-set-key (kbd "C-c v s") #'my-org-save-fold-state)
    (global-set-key (kbd "C-c v r") #'my-org-restore-fold-state))
  
  ;; Conserver C-c f pour file operations (déjà bien organisé dans dirvish)
  (message "✓ Conflits C-c f résolus : F=frequency, v=view-state, f=files"))

;; --------------------------------------------------
;; Auto-résolution au chargement
;; --------------------------------------------------

;; Résoudre automatiquement les conflits quand ce fichier est chargé
(add-hook 'after-init-hook #'my/resolve-file-keybindings-conflicts)

(provide '1_keybindings-centralized)
;;; 1_keybindings-centralized.el ends here