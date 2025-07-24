;;; 1_search-cache.el --- Cache system for Everything and org-roam searches -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Variables de cache
;; --------------------------------------------------
(defvar my/everything-cache nil
  "Cache mémoire pour les résultats Everything.
Format: ((query . (timestamp . results)) ...)")

(defvar my/everything-cache-timeout 300
  "Durée de vie du cache Everything en secondes (5 minutes).")

(defvar my/everything-cache-max-entries 20
  "Nombre maximum d'entrées dans le cache Everything.")

(defvar my/org-roam-recent-nodes nil
  "Cache des nœuds org-roam récemment utilisés.")

(defvar my/org-roam-recent-max 50
  "Nombre maximum de nœuds récents à garder en cache.")

;; --------------------------------------------------
;; Cache Everything
;; --------------------------------------------------
(defun my/everything-search-cached (query)
  "Recherche Everything avec cache mémoire.
Retourne les résultats mis en cache si disponibles et valides,
sinon effectue une nouvelle recherche."
  (let ((cached-result (assoc query my/everything-cache)))
    (if (and cached-result 
             (< (time-to-seconds (time-since (cadr cached-result)))
                my/everything-cache-timeout))
        ;; Retourner résultat du cache
        (cddr cached-result)
      ;; Nouvelle recherche
      (let ((results (consult-everything query)))
        (my/everything-update-cache query results)
        results))))

(defun my/everything-update-cache (query results)
  "Met à jour le cache Everything avec une nouvelle entrée."
  (let ((cache-entry (cons query (cons (current-time) results))))
    ;; Supprimer l'ancienne entrée si elle existe
    (setq my/everything-cache 
          (assoc-delete-all query my/everything-cache))
    ;; Ajouter la nouvelle entrée au début
    (push cache-entry my/everything-cache)
    ;; Limiter la taille du cache
    (when (> (length my/everything-cache) my/everything-cache-max-entries)
      (setq my/everything-cache 
            (butlast my/everything-cache 
                     (- (length my/everything-cache) my/everything-cache-max-entries))))))

(defun my/everything-clear-cache ()
  "Vide le cache Everything."
  (interactive)
  (setq my/everything-cache nil)
  (message "Cache Everything vidé"))

;; --------------------------------------------------
;; Cache org-roam nœuds récents
;; --------------------------------------------------
(defun my/org-roam-track-recent-node (node)
  "Ajoute un nœud à la liste des récents."
  (when node
    (let ((node-info (list (org-roam-node-title node)
                          (org-roam-node-file node)
                          (current-time))))
      ;; Supprimer l'ancienne entrée si elle existe
      (setq my/org-roam-recent-nodes
            (seq-remove (lambda (item) 
                         (string= (car item) (org-roam-node-title node)))
                       my/org-roam-recent-nodes))
      ;; Ajouter au début
      (push node-info my/org-roam-recent-nodes)
      ;; Limiter la taille
      (when (> (length my/org-roam-recent-nodes) my/org-roam-recent-max)
        (setq my/org-roam-recent-nodes
              (butlast my/org-roam-recent-nodes
                       (- (length my/org-roam-recent-nodes) my/org-roam-recent-max)))))))

(defun my/org-roam-get-recent-nodes ()
  "Retourne la liste des nœuds récents (titres uniquement)."
  (mapcar #'car my/org-roam-recent-nodes))

(defun my/org-roam-node-find-with-recents ()
  "org-roam-node-find avec priorité aux nœuds récents."
  (interactive)
  (let* ((recent-titles (my/org-roam-get-recent-nodes))
         (all-nodes (org-roam-node-read--completions))
         ;; Mettre les récents en premier, puis les autres
         (ordered-completions 
          (append recent-titles
                  (seq-difference all-nodes recent-titles))))
    (let ((selected (completing-read "Nœud org-roam: " ordered-completions)))
      (if-let ((recent-entry (seq-find (lambda (item) (string= (car item) selected))
                                       my/org-roam-recent-nodes)))
          ;; Nœud récent - utiliser le fichier du cache
          (find-file (cadr recent-entry))
        ;; Nouveau nœud - utiliser org-roam standard
        (let ((node (org-roam-node-from-title-or-alias selected)))
          (find-file (org-roam-node-file node))
          (my/org-roam-track-recent-node node))))))

;; --------------------------------------------------
;; Nettoyage automatique
;; --------------------------------------------------
(defun my/cleanup-old-cache-entries ()
  "Nettoie les entrées expirées du cache."
  (let ((current-time (current-time))
        (cleaned-count 0))
    ;; Nettoyer cache Everything
    (setq my/everything-cache
          (seq-filter (lambda (entry)
                       (let ((entry-time (cadr entry)))
                         (if (< (time-to-seconds (time-subtract current-time entry-time))
                                my/everything-cache-timeout)
                             t
                           (setq cleaned-count (1+ cleaned-count))
                           nil)))
                     my/everything-cache))
    ;; Nettoyer nœuds récents (garder ceux visités dans les 7 derniers jours)
    (setq my/org-roam-recent-nodes
          (seq-filter (lambda (entry)
                       (let ((entry-time (caddr entry)))
                         (< (time-to-seconds (time-subtract current-time entry-time))
                            (* 7 24 3600))))  ; 7 jours
                     my/org-roam-recent-nodes))
    (when (> cleaned-count 0)
      (message "Cache nettoyé: %d entrées supprimées" cleaned-count))))

;; --------------------------------------------------
;; Configuration et hooks
;; --------------------------------------------------
(defun my/setup-search-cache ()
  "Configure le système de cache pour les recherches."
  ;; Démarrer le nettoyage automatique une fois par jour (86400 secondes)
  ;; DÉSACTIVÉ temporairement pour éviter erreurs timer
  ;; (run-with-timer 86400 86400 #'my/cleanup-old-cache-entries)
  ;; Hook pour tracker les nœuds org-roam visités
  (advice-add 'org-roam-node-visit :after 
              (lambda (node &rest _)
                (my/org-roam-track-recent-node node)))
  (message "Système de cache initialisé"))

;; --------------------------------------------------
;; Fonctions d'information
;; --------------------------------------------------
(defun my/cache-info ()
  "Affiche des informations sur l'état du cache."
  (interactive)
  (message "Cache Everything: %d entrées | Nœuds récents: %d | Nettoyage actif: %s"
           (length my/everything-cache)
           (length my/org-roam-recent-nodes)
           (if (timerp (get 'my/cleanup-old-cache-entries 'timer)) "Oui" "Non")))

;; Raccourcis clavier
(global-set-key (kbd "C-c s e") #'my/everything-search-cached)
(global-set-key (kbd "C-c n f") #'my/org-roam-node-find-with-recents)  ; Remplace le binding par défaut
(global-set-key (kbd "C-c s i") #'my/cache-info)
(global-set-key (kbd "C-c s c") #'my/everything-clear-cache)

;; Initialisation
(add-hook 'emacs-startup-hook #'my/setup-search-cache)

(provide '1_search-cache)
;;; 1_search-cache.el ends here