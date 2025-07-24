;;; 1_avy.el --- Configuration avy pour navigation rapide -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Configuration avy SIMPLIFIÉE - Raccourcis individuels
;; --------------------------------------------------

(use-package avy
  :after org
  ;; Keybindings centralisés dans keybindings.el
  
  :config
  ;; Configuration générale - timeout augmenté
  (setq avy-timeout-seconds 1.5             ; Délai augmenté pour laisser le temps
        avy-all-windows t                   ; Considérer toutes les fenêtres
        avy-case-fold-search nil            ; Recherche sensible à la casse
        avy-background t                    ; Assombrir le fond
        avy-style 'at-full)                 ; Style d'affichage des caractères
  
  ;; Caractères utilisés pour la sélection (optimisés pour AZERTY)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
  
  ;; Actions personnalisées simplifiées
  (setq avy-dispatch-alist
        '((?x . avy-action-kill-move)        ; Tuer et déplacer
          (?X . avy-action-kill-stay)        ; Tuer et rester
          (?c . avy-action-copy)             ; Copier
          (?m . avy-action-mark)             ; Marquer
          (?i . avy-action-ispell)           ; Vérifier orthographe
          (?z . avy-action-zap-to-char)))    ; Zap jusqu'au caractère
  
  ;; Message de confirmation
  (message "✓ Avy configuré - navigation universelle avec M-j"))

;; --------------------------------------------------
;; Fonctions PKM simplifiées
;; --------------------------------------------------

(defun my/avy-goto-org-heading-in-buffer ()
  "Utilise avy pour naviguer vers un heading org dans le buffer courant."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (avy-jump "^\\*+ " :pred (lambda () (derived-mode-p 'org-mode)))))

(defun my/avy-goto-org-link ()
  "Utilise avy pour naviguer vers un lien org dans le buffer courant."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (avy-jump "\\[\\[" :pred (lambda () (derived-mode-p 'org-mode)))))

(defun my/avy-goto-line-simple ()
  "Version simplifiée d'avy-goto-line."
  (interactive)
  (avy-goto-line))

(defun my/avy-goto-word-simple ()
  "Version simplifiée d'avy-goto-word."
  (interactive)
  (avy-goto-word-0 nil))

;; --------------------------------------------------
;; Raccourcis individuels SANS préfixe
;; --------------------------------------------------

;; Keybindings centralisés dans keybindings.el

;; --------------------------------------------------
;; Fonctions de test et debug
;; --------------------------------------------------

(defun my/avy-test-navigation ()
  "Teste les différentes fonctions de navigation avy."
  (interactive)
  (message "Avy configuré - Raccourcis disponibles:")
  (message "  🎯 M-j : Navigation universelle (PRINCIPAL)")
  (message "  🎯 C-; : Navigation caractère simple")
  (message "  🎯 C-c n j : Navigation headings org")
  (message "  🎯 C-c r : Refiling rapide")
  (message "  🔍 C-c A l : Navigation lignes")
  (message "  🔍 C-c A w : Navigation vers début de mot")
  (message "  🔍 C-c A h : Navigation headings (buffer)")
  (message "  🔍 C-c A k : Navigation liens org")
  (message "  ⚡ Actions dispatch: x (kill), c (copy), m (mark), z (zap)"))

(defun my/avy-config-info ()
  "Affiche la configuration avy courante."
  (interactive)
  (message "Configuration avy:")
  (message "  Timeout: %s | All-windows: %s | Case-fold: %s | Background: %s"
           avy-timeout-seconds avy-all-windows avy-case-fold-search avy-background)
  (message "  Style: %s | Keys: %d" avy-style (length avy-keys))
  (message "  Dispatch actions: %d configurées" (length avy-dispatch-alist)))

;; Keybindings centralisés dans keybindings.el

;; --------------------------------------------------
;; Hooks et intégration finale
;; --------------------------------------------------

;; Hook pour org-mode - keybindings centralisés dans keybindings.el

;; --------------------------------------------------
;; Message de confirmation et aide
;; --------------------------------------------------
(message "✓ Avy configuré - SIMPLIFIÉE")
(message "  🎯 M-j : Navigation universelle (PRINCIPAL)")
(message "  🎯 C-; : Caractère | C-c n j : Headings org | C-c r : Refiling")
(message "  🔍 C-c A [l/w/h/k] : Navigation spécialisée (lignes/mots/headings/liens)")
(message "  📋 C-c A i : Config info | C-c A T : Test navigation")
(message "  ⚡ Timeout 1.5s | Actions dispatch disponibles")

(provide '1_avy)
;;; 1_avy.el ends here