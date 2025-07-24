;;; org-config.el --- Configuration modulaire Org Mode -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Documentation de l'architecture modulaire
;; --------------------------------------------------
;; Cette configuration Org-mode est séparée en 4 modules :
;;
;; 1. org-base.el           - Configuration de base d'Org-mode
;; 2. org-roam-config.el    - Tout Org-roam + état de pliage persistant
;; 3. org-latex.el          - LaTeX et exports PDF
;; 4. org-ui-enhancements.el - Apparence et workflow (statuts, checkboxes)
;;
;; Chaque module est autonome et peut être désactivé individuellement.

;; --------------------------------------------------
;; Load modules in order
;; --------------------------------------------------

;; 1. Base configuration (required first)
(require 'org-base)

;; 2. UI enhancements (can be loaded independently)
(require 'org-ui-enhancements)

;; 3. Org-roam and persistence (depends on org-base)
(require 'org-roam-config)

;; 4. LaTeX and export (can be loaded independently) 
(require 'org-latex)

;; --------------------------------------------------
;; Global keybindings summary
;; --------------------------------------------------
;; Cette section documente tous les raccourcis définis dans les modules

;; From org-base.el:
;; "C-c a"    ; Agenda                     ; org-agenda
;; "C-c c"    ; Capture                    ; org-capture
;; "<f1>"     ; Ouvrir inbox               ; find-file inbox.org

;; From org-roam-config.el:
;; "C-c n f"  ; Trouver note roam          ; org-roam-node-find
;; "C-c n i"  ; Insérer lien roam          ; org-roam-node-insert
;; "C-c n g"  ; Ouvrir graphe roam         ; org-roam-ui-open
;; "C-c r r"  ; Extraire vers nouvelle note ; my/org-roam-extract-to-note
;; "C-c f s"  ; Sauver état pliage         ; my-org-save-fold-state
;; "C-c f r"  ; Restaurer état pliage      ; my-org-restore-fold-state

;; From org-latex.el:
;; "C-c e p"  ; Export PDF                 ; my/org-export-pdf
;; "C-c e o"  ; Export PDF et ouvrir       ; my/org-export-pdf-and-open
;; "C-c e P"  ; Export PDF Pandoc          ; my/org-export-pandoc-pdf (si pandoc installé)
;; "C-c t s"  ; Test LaTeX simple          ; my/test-latex-simple
;; "C-c l c"  ; Nettoyer config LaTeX      ; my/clean-latex-config
;; "C-c l s"  ; Setup LaTeX final          ; my/setup-latex-final
;; "C-c l x"  ; Nettoyer cache LaTeX       ; my/clean-latex-cache
;; "C-c l f"  ; Setup LaTeX final          ; my/setup-latex-final
;; "C-c l v"  ; Vérifier export PDF        ; my/verify-export-pdf-clean
;; "C-c l t"  ; Test preview et export     ; my/test-both-latex-modes

;; From org-ui-enhancements.el:
;; "C-l"      ; Cycle checkbox             ; cycle-checkbox-symbol

;; --------------------------------------------------
;; Configuration summary
;; --------------------------------------------------
(defun my/org-config-info ()
  "Display information about the current Org configuration."
  (interactive)
  (with-current-buffer (get-buffer-create "*Org Config Info*")
    (erase-buffer)
    (insert "=== CONFIGURATION ORG MODE MODULAIRE ===\n\n")
    
    (insert "MODULES CHARGÉS :\n")
    (insert (format "✓ org-base.el           - Répertoire : %s\n" my-notes-dir))
    (insert (format "✓ org-roam-config.el    - Répertoire roam : %s\n" my-roam-dir))
    (insert "✓ org-latex.el          - Preview et export PDF\n")
    (insert "✓ org-ui-enhancements.el - Apparence et workflow\n\n")
    
    (insert "FONCTIONNALITÉS DISPONIBLES :\n")
    (insert "• Configuration Org de base avec liens cliquables\n")
    (insert "• Org-roam avec extraction de notes\n")
    (insert "• État de pliage persistant entre sessions\n")
    (insert "• Export LaTeX/PDF avec preview équations\n")
    (insert "• Système de statuts visuels (ok/ko/bof)\n")
    (insert "• Checkboxes personnalisées cycliques\n")
    (insert "• Apparence moderne avec org-superstar et org-modern\n\n")
    
    (insert "Pour voir tous les raccourcis : consulter les commentaires dans org-config.el\n")
    
    (display-buffer (current-buffer))))

;; --------------------------------------------------
;; Module management functions
;; --------------------------------------------------
(defun my/reload-org-modules ()
  "Reload all org modules."
  (interactive)
  (mapc #'load-library '("org-base" "org-ui-enhancements" "org-roam-config" "org-latex"))
  (message "✓ Modules Org rechargés"))

(defun my/disable-org-roam ()
  "Disable org-roam functionality."
  (interactive)
  (global-unset-key (kbd "C-c n f"))
  (global-unset-key (kbd "C-c n i"))
  (global-unset-key (kbd "C-c n g"))
  (global-unset-key (kbd "C-c r r"))
  (message "✓ Org-roam désactivé"))

(defun my/disable-org-latex ()
  "Disable LaTeX functionality."
  (interactive)
  (global-unset-key (kbd "C-c e p"))
  (global-unset-key (kbd "C-c e o"))
  (global-unset-key (kbd "C-c l c"))
  (global-unset-key (kbd "C-c l s"))
  (global-unset-key (kbd "C-c l x"))
  (message "✓ LaTeX désactivé"))

;; --------------------------------------------------
;; Management keybindings
;; --------------------------------------------------
(global-set-key (kbd "C-c o i") #'my/org-config-info)
(global-set-key (kbd "C-c o r") #'my/reload-org-modules)

;; --------------------------------------------------
;; Success message
;; --------------------------------------------------
(message "✓ Configuration Org modulaire chargée - C-c o i pour infos")

(provide 'org-config)
;;; org-config.el ends here