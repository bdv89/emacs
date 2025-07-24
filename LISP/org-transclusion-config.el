;;; org-transclusion-config.el --- Configuration org-transclusion pour PKM -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; org-transclusion : Inclusion dynamique de contenu
;; --------------------------------------------------
(use-package org-transclusion
  :after org
  :config
  ;; Configuration de base
  (setq org-transclusion-activate-persistent-message t)  ; Messages persistants
  
  ;; Filtres personnalisés pour votre PKM
  (defun my/transclusion-pkm-setup ()
    "Configuration spécifique org-transclusion pour workflow PKM."
    ;; Assurer compatibilité avec org-roam
    (when (featurep 'org-roam)
      (setq org-transclusion-include-first-section t))  ; Inclut première section
    
    ;; Messages informatifs
    (message "✓ org-transclusion configuré pour PKM"))
  
  (add-hook 'org-transclusion-mode-hook #'my/transclusion-pkm-setup))

;; --------------------------------------------------
;; Fonctions utilitaires PKM
;; --------------------------------------------------
(defun my/transclude-roam-note ()
  "Transclude une note org-roam avec completion (utilise les ID)."
  (interactive)
  (if (featurep 'org-roam)
      (let* ((node (org-roam-node-read))
             (id (org-roam-node-id node))
             (title (org-roam-node-title node)))
        (insert (format "#+transclude: [[id:%s]] :only-contents\n" id)))
    (message "org-roam non disponible")))

(defun my/transclude-code-snippet ()
  "Transclude un snippet de code avec sélection de lignes."
  (interactive)
  (let* ((file (read-file-name "Fichier source: "))
         (from (read-number "Ligne de début: " 1))
         (to (read-number "Ligne de fin: " 10))
         (lang (read-string "Langage (optionnel): " "")))
    (insert (format "#+transclude: [[file:%s::%d:%d]]%s\n"
                   (file-relative-name file org-directory)
                   from to
                   (if (string-empty-p lang) 
                       "" 
                       (format " :src %s" lang))))))

(defun my/transclusion-toggle-mode ()
  "Toggle org-transclusion-mode avec messages informatifs."
  (interactive)
  (if (bound-and-true-p org-transclusion-mode)
      (progn
        (org-transclusion-mode -1)
        (message "org-transclusion désactivé"))
    (progn
      (org-transclusion-mode 1)
      (message "org-transclusion activé - contenu dynamique"))))

(defun my/transclusion-refresh-all ()
  "Refresh toutes les transclusions du buffer."
  (interactive)
  (when (bound-and-true-p org-transclusion-mode)
    (org-transclusion-refresh)
    (message "Toutes les transclusions refreshées")))

(defun my/transclusion-info ()
  "Affiche l'état des transclusions dans le buffer."
  (interactive)
  (if (bound-and-true-p org-transclusion-mode)
      (message "org-transclusion: ON dans ce buffer")
    (message "org-transclusion: OFF")))

(defun my/transclusion-hide-all ()
  "Masque toutes les transclusions (garde les liens visibles)."
  (interactive)
  (when (bound-and-true-p org-transclusion-mode)
    (org-transclusion-mode -1)
    (message "Transclusions masquées - liens visibles")))

(defun my/transclusion-show-all ()
  "Affiche toutes les transclusions."
  (interactive)
  (org-transclusion-mode 1)
  (message "Transclusions affichées"))

(defun my/insert-transclusion-template ()
  "Insert un template de transclusion."
  (interactive)
  (let ((template-type 
         (completing-read "Type de transclusion: "
                         '("Note org-roam" "Section fichier" "Code snippet" "Personnalisé"))))
    (cond
     ((string= template-type "Note org-roam")
      (my/transclude-roam-note))
     ((string= template-type "Section fichier")
      (insert "#+transclude: [[file:]] :only-contents\n"))
     ((string= template-type "Code snippet")
      (my/transclude-code-snippet))
     ((string= template-type "Personnalisé")
      (insert "#+transclude: ")))))

(defun my/test-transclusion ()
  "Test des fonctionnalités org-transclusion."
  (interactive)
  (with-current-buffer (get-buffer-create "*Test org-transclusion*")
    (erase-buffer)
    (org-mode)
    (insert "#+TITLE: Test org-transclusion\n\n")
    (insert "* Exemples de transclusion\n\n")
    (insert "** Note org-roam (remplacez par une vraie note)\n")
    (insert "#+transclude: [[file:roam/exemple.org::*Section]] :only-contents\n\n")
    (insert "** Code snippet\n")
    (insert "#+transclude: [[file:../code/exemple.py::1:10]] :src python\n\n")
    (insert "** Section complète\n")
    (insert "#+transclude: [[file:IDEA.org::*Mes idées]] :level 2\n\n")
    (insert "* Instructions\n")
    (insert "1. Activez org-transclusion-mode: C-c t m\n")
    (insert "2. Ajoutez transclusion: C-c t a\n")
    (insert "3. Refresh: C-c t r\n")
    (insert "4. Info: C-c t i\n\n")
    (insert "État: " (if (bound-and-true-p org-transclusion-mode) "ACTIVÉ" "DÉSACTIVÉ"))
    
    (display-buffer (current-buffer))
    (goto-char (point-min))))

;; --------------------------------------------------
;; Keybindings - Préfixe C-c t pour Transclusion
;; --------------------------------------------------
(global-set-key (kbd "C-c t m") #'my/transclusion-toggle-mode)     ; Mode on/off
(global-set-key (kbd "C-c t a") #'my/insert-transclusion-template) ; Add transclusion
(global-set-key (kbd "C-c t r") #'my/transclusion-refresh-all)     ; Refresh all
(global-set-key (kbd "C-c t i") #'my/transclusion-info)            ; Info
(global-set-key (kbd "C-c t n") #'my/transclude-roam-note)         ; Note roam
(global-set-key (kbd "C-c t c") #'my/transclude-code-snippet)      ; Code snippet
(global-set-key (kbd "C-c t t") #'my/test-transclusion)            ; Test
(global-set-key (kbd "C-c t h") #'my/transclusion-hide-all)        ; Hide all
(global-set-key (kbd "C-c t s") #'my/transclusion-show-all)        ; Show all

;; Raccourcis natifs org-transclusion (facultatifs)
(with-eval-after-load 'org-transclusion
  (define-key org-transclusion-map (kbd "C-c C-t") #'org-transclusion-add)
  (define-key org-transclusion-map (kbd "C-c C-r") #'org-transclusion-refresh))

(provide 'org-transclusion-config)
;;; org-transclusion-config.el ends here