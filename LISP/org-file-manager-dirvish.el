;;; org-file-manager-dirvish.el --- Gestionnaire de fichiers Dirvish pour PKM -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Packages pour icônes et couleurs
;; --------------------------------------------------
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :after diredff
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; Force les couleurs même en mode terminal
  (setq all-the-icons-dired-monochrome nil))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil))

;; --------------------------------------------------
;; Configuration Dirvish
;; --------------------------------------------------
(use-package dirvish
  :ensure t
  :init 
  (dirvish-override-dired-mode)
  
  :custom
  ;; Accès rapide aux dossiers importants
  (dirvish-quick-access-entries 
   '(("h" "~/" "Home")
     ("p" "~/00_PKM/" "PKM")
     ("d" "~/Documents/" "Documents") 
     ("t" "~/Downloads/" "Downloads")
     ("o" "C:/Users/BrandonDeVoeght/Documents/00_Portals" "Portals")))
  
  ;; Attributs simples sans conflit d'icônes
  (dirvish-attributes '(file-size subtree-state vc-state))
  
  ;; Configuration panneau unique strict
  (dirvish-default-layout '(1 0 0))
  
  :config
  ;; Désactiver complètement le système d'icônes de Dirvish
  (setq dirvish-emerge-groups '()
        dirvish-use-header-line t
        dirvish-header-line-height 25
        dirvish-preview-dispatchers '()
        dirvish-cache-dir nil)
  
  ;; Mode line simple
  (setq dirvish-mode-line-format
        '(:left (sort file-time " " file-size)
          :right (omit yank index)))

  ;; --------------------------------------------------
  ;; Fonctions de filtrage par mot-clé
  ;; --------------------------------------------------
  
  (defun my/dirvish-filter-by-keyword ()
    "Filtre les fichiers par mot-clé dans le nom ou l'extension.
Exemples:
- .torrent : affiche seulement les fichiers .torrent
- pomme : affiche tous les éléments contenant 'pomme' dans leur nom
- Vide : supprime le filtre et affiche tout"
    (interactive)
    (let* ((keyword (read-string "Filtrer par mot-clé (vide pour tout afficher): "))
           (case-fold-search t))  ; Recherche insensible à la casse
      (if (string-empty-p keyword)
          ;; Réinitialiser le filtre
          (progn
            (my/dirvish-clear-filter-overlays)
            (when (boundp 'my-dirvish-filter-active)
              (setq my-dirvish-filter-active nil))
            (message "✓ Filtre supprimé - tous les fichiers affichés"))
        ;; Appliquer le filtre
        (progn
          ;; Nettoyer les anciens filtres
          (my/dirvish-clear-filter-overlays)
          
          ;; Marquer qu'un filtre est actif
          (setq-local my-dirvish-filter-active keyword)
          
          ;; Appliquer le nouveau filtre
          (let ((visible-count (my/dirvish-apply-filter keyword)))
            (message "✓ Filtre appliqué: '%s' (%d éléments visibles)" 
                     keyword visible-count))))))

  (defun my/dirvish-apply-filter (keyword)
    "Applique un filtre en cachant les lignes qui ne matchent pas le keyword.
Retourne le nombre d'éléments visibles."
    (let ((buffer-read-only nil)
          (inhibit-read-only t)
          (keyword-regex (regexp-quote keyword))
          (visible-count 0)
          (total-count 0))
      
      ;; Parcourir toutes les lignes et appliquer le filtre
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (dired-get-filename nil t)
            (setq total-count (1+ total-count))
            (let ((filename (file-name-nondirectory (dired-get-filename nil t))))
              (if (string-match-p keyword-regex filename)
                  ;; Fichier matche - le garder visible
                  (setq visible-count (1+ visible-count))
                ;; Fichier ne matche pas - le cacher
                (let ((line-start (line-beginning-position))
                      (line-end (1+ (line-end-position))))
                  (let ((overlay (make-overlay line-start line-end)))
                    (overlay-put overlay 'invisible 'dired-filter)
                    (overlay-put overlay 'my-dirvish-filter t))))))
          (forward-line 1)))
      
      ;; Activer l'invisibilité
      (add-to-invisibility-spec 'dired-filter)
      
      visible-count))

  (defun my/dirvish-clear-filter-overlays ()
    "Supprime tous les overlays de filtre."
    (remove-overlays (point-min) (point-max) 'my-dirvish-filter t)
    (remove-from-invisibility-spec 'dired-filter))

  ;; --------------------------------------------------
  ;; Fonctions personnalisées
  ;; --------------------------------------------------
  
  (defun my/dirvish-copy-path ()
    "Copie le chemin du fichier/dossier dans le kill-ring."
    (interactive)
    (when-let ((file (dired-get-filename nil t)))
      (kill-new file)
      (message "Chemin copié: %s" file)))
  
  (defun my/dirvish-search-in-directory ()
    "Recherche dans le répertoire courant et ses sous-dossiers avec autocomplétion."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (if (executable-find "rg")
          (consult-ripgrep default-directory)
        (if (executable-find "grep")
            (consult-grep default-directory)
          (message "Ni ripgrep ni grep trouvés")))))
  
  (defun my/dirvish-everything-search ()
    "Recherche Everything dans le dossier courant, incluant ses racines."
    (interactive)
    (let ((current-dir (dired-current-directory))
          (everything-cmd (executable-find "es")))
      (if everything-cmd
          (let ((search-path (if (string-match "^C:/" current-dir)
                                (concat (substring current-dir 0 3) "*")  ; Recherche depuis la racine C:
                              current-dir)))
            (call-interactively #'my/counsel-everything-general))
        (message "Everything (es.exe) non trouvé - utilisation de la recherche standard")
        (my/dirvish-search-in-directory))))
  
  (defun my/dirvish-everything-search-current-dir ()
    "Recherche Everything UNIQUEMENT dans le dossier courant avec affichage live."
    (interactive)
    (let ((current-dir (dired-current-directory))
          (everything-cmd (executable-find "es")))
      (if everything-cmd
          (let ((windows-path (replace-regexp-in-string "/" "\\\\" current-dir)))
            (ivy-read (format "Everything dans %s: " (file-name-nondirectory (directory-file-name current-dir)))
                      (lambda (query)
                        (when (> (length query) 2)
                          (let* ((cmd (format "es -path \"%s\" -n 50 %s" windows-path query))
                                 (result (shell-command-to-string cmd))
                                 (results (when (> (length result) 0)
                                           (split-string result "\n" t))))
                            (or results '()))))
                      :dynamic-collection t
                      :action (lambda (item)
                                (when item
                                  (if (file-directory-p item)
                                      (dired item)
                                    (find-file item))))))
        (message "Everything (es.exe) non trouvé - utilisation de la recherche standard")
        (my/dirvish-search-in-directory))))
  
  (defun my/dirvish-parent-directory ()
    "Navigation vers le dossier parent."
    (interactive)
    (dired-up-directory))
  
  (defun my/dirvish-open-folder ()
    "Ouvre le dossier/fichier sous le curseur."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (if file
          (if (file-directory-p file)
              (dired-find-file)
            (dired-find-file-other-window))
        (message "Aucun fichier sélectionné"))))
  
  (defun my/dirvish-goto-directory ()
    "Navigue vers un dossier avec autocomplétion."
    (interactive)
    (let ((dir (read-directory-name "Aller à: " (dired-current-directory))))
      (dired dir)))
  
  (defun my/dirvish-new-file ()
    "Crée un nouveau fichier en copiant depuis le dossier template_fichiers."
    (interactive)
    (let* ((template-dir (expand-file-name "template_fichiers" "~/00_PKM/"))
           (templates (when (file-directory-p template-dir)
                       (directory-files template-dir nil "^[^.].*")))  ; Exclut les fichiers cachés
           (choice (if templates
                      (completing-read "Choisir un template: " templates nil t)
                    (progn
                      (message "Dossier template_fichiers introuvable ou vide")
                      (my/create-basic-file))))
           (template-file (when choice (expand-file-name choice template-dir)))
           (filename (when choice (read-string "Nom du nouveau fichier: " choice)))
           (filepath (when filename (expand-file-name filename (dired-current-directory)))))
      
      (when (and template-file filepath)
        (if (file-exists-p filepath)
            (message "Le fichier existe déjà: %s" filepath)
          (progn
            (copy-file template-file filepath)
            (dired-revert)
            (dired-goto-file filepath)
            (message "✓ Fichier créé depuis template: %s" (file-name-nondirectory template-file))
            
            ;; Ouvrir selon le type
            (if (member (file-name-extension filepath) '("xlsx" "pptx" "docx" "pdf"))
                (my/dirvish-open-external)  ; Ouvre avec app externe
              (find-file filepath)))))))   ; Ouvre dans Emacs
  
  (defun my/create-basic-file ()
    "Crée un fichier basique si aucun template n'est disponible."
    (let* ((basic-types '("document.txt" "notes.org" "script.py" "page.html" "data.json"))
           (choice (completing-read "Type de fichier basique: " basic-types nil t))
           (filename (read-string "Nom du fichier: " choice))
           (filepath (expand-file-name filename (dired-current-directory))))
      
      (unless (file-exists-p filepath)
        (cond
         ((string-suffix-p ".org" filename)
          (my/create-org-template filepath))
         ((string-suffix-p ".html" filename)
          (my/create-html-template filepath))
         ((string-suffix-p ".py" filename)
          (my/create-python-template filepath))
         ((string-suffix-p ".json" filename)
          (my/create-json-template filepath))
         (t (write-region "" nil filepath)))
        
        (dired-revert)
        (dired-goto-file filepath)
        (find-file filepath))
      
      nil))  ; Retourne nil pour arrêter le processus principal
  
  (defun my/setup-template-directory ()
    "Configure le dossier template_fichiers avec des exemples."
    (interactive)
    (let ((template-dir (expand-file-name "template_fichiers" "~/00_PKM/")))
      (unless (file-directory-p template-dir)
        (make-directory template-dir t)
        (message "✓ Dossier template_fichiers créé: %s" template-dir))
      
      ;; Créer quelques templates de base s'ils n'existent pas
      (let ((templates '(("document.org" . my/create-org-template)
                        ("page.html" . my/create-html-template)
                        ("script.py" . my/create-python-template)
                        ("data.json" . my/create-json-template)
                        ("donnees.csv" . my/create-csv-template))))
        
        (dolist (template templates)
          (let ((template-path (expand-file-name (car template) template-dir)))
            (unless (file-exists-p template-path)
              (funcall (cdr template) template-path)
              (message "✓ Template créé: %s" (car template))))))
      
      (message "")
      (message "=== CONFIGURATION TEMPLATE_FICHIERS ===")
      (message "Dossier: %s" template-dir)
      (message "")
      (message "Pour ajouter des templates Office:")
      (message "1. Créez vos fichiers Excel/Word/PowerPoint modèles")
      (message "2. Placez-les dans le dossier template_fichiers")
      (message "3. Ils apparaîtront automatiquement dans la liste")
      (message "")
      (message "Templates actuels:")
      (let ((current-templates (directory-files template-dir nil "^[^.].*")))
        (if current-templates
            (dolist (template current-templates)
              (message "  • %s" template))
          (message "  (aucun template trouvé)")))))
  
  (defun my/create-org-template (filepath)
    "Crée un fichier org avec template de base."
    (let ((content (format "#+TITLE: %s\n#+DATE: %s\n#+AUTHOR: %s\n\n* Introduction\n\n"
                          (file-name-sans-extension (file-name-nondirectory filepath))
                          (format-time-string "%Y-%m-%d")
                          (or user-full-name user-login-name "Auteur"))))
      (write-region content nil filepath)))
  
  (defun my/create-html-template (filepath)
    "Crée un fichier HTML avec template de base."
    (let ((title (file-name-sans-extension (file-name-nondirectory filepath)))
          (content "<!DOCTYPE html>\n<html lang=\"fr\">\n<head>\n    <meta charset=\"UTF-8\">\n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n    <title>%s</title>\n</head>\n<body>\n    <h1>%s</h1>\n    \n</body>\n</html>"))
      (write-region (format content title title) nil filepath)))
  
  (defun my/create-python-template (filepath)
    "Crée un fichier Python avec template de base."
    (let ((content "#!/usr/bin/env python3\n# -*- coding: utf-8 -*-\n\"\"\"\nScript: %s\nAuteur: %s\nDate: %s\n\"\"\"\n\ndef main():\n    \"\"\"Fonction principale.\"\"\"\n    pass\n\nif __name__ == \"__main__\":\n    main()\n"))
      (write-region (format content 
                            (file-name-nondirectory filepath)
                            (or user-full-name "Auteur")
                            (format-time-string "%Y-%m-%d")) 
                    nil filepath)))
  
  (defun my/create-elisp-template (filepath)
    "Crée un fichier Elisp avec template de base."
    (let ((package-name (file-name-sans-extension (file-name-nondirectory filepath)))
          (content ";;; %s --- Description -*- lexical-binding: t; -*-\n\n;; Author: %s\n;; Version: 1.0\n;; Package-Requires: ((emacs \"24.1\"))\n\n;;; Commentary:\n;; \n\n;;; Code:\n\n\n\n(provide '%s)\n;;; %s ends here\n"))
      (write-region (format content 
                            (file-name-nondirectory filepath)
                            (or user-full-name "Auteur")
                            package-name
                            (file-name-nondirectory filepath)) 
                    nil filepath)))
  
  (defun my/create-json-template (filepath)
    "Crée un fichier JSON avec template de base."
    (let ((content "{\n    \"name\": \"%s\",\n    \"version\": \"1.0.0\",\n    \"description\": \"\",\n    \"data\": {\n        \n    }\n}"))
      (write-region (format content (file-name-sans-extension (file-name-nondirectory filepath))) 
                    nil filepath)))
  
  (defun my/create-csv-template (filepath)
    "Crée un fichier CSV avec en-têtes de base."
    (let ((content "nom,valeur,date,commentaire\n"))
      (write-region content nil filepath)))
  
  (defun my/dirvish-new-directory ()
    "Crée un nouveau dossier."
    (interactive)
    (let ((dirname (read-string "Nom du dossier: ")))
      (when (not (string-empty-p dirname))
        (let ((dirpath (expand-file-name dirname (dired-current-directory))))
          (make-directory dirpath)
          (dired-revert)
          (dired-goto-file dirpath)))))
  
  (defun my/dirvish-open-external ()
    "Ouvre le fichier avec l'application externe par défaut."
    (interactive)
    (when-let ((file (dired-get-filename nil t)))
      (cond
       ((eq system-type 'windows-nt) 
        (w32-shell-execute "open" file))
       ((eq system-type 'darwin)
        (call-process "open" nil nil nil file))
       (t 
        (call-process "xdg-open" nil nil nil file)))))
  
  (defun my/dirvish-add-bookmark ()
    "Ajoute le dossier courant aux favoris."
    (interactive)
    (let ((dir (dired-current-directory))
          (name (read-string "Nom du favori: " (file-name-nondirectory (directory-file-name dir)))))
      (bookmark-set name)
      (message "Favori ajouté: %s -> %s" name dir)))
  
  (defun my/dirvish-delete-no-confirm ()
    "Supprime les fichiers/dossiers marqués sans demander confirmation."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (when files
        (dolist (file files)
          (if (file-directory-p file)
              (delete-directory file t)
            (delete-file file)))
        (dired-unmark-all-marks)
        (revert-buffer)
        (message "Supprimé: %d élément(s)" (length files)))))
  
  (defun my/dirvish-quit ()
    "Quitte Dirvish en une seule fois et nettoie les filtres."
    (interactive)
    (my/dirvish-clear-filter-overlays)  ; Nettoyer les filtres
    (if (eq major-mode 'dirvish-mode)
        (dirvish-quit)
      (quit-window)))

  ;; --------------------------------------------------
  ;; Raccourcis clavier dans Dirvish - centralisés dans keybindings.el
  ;; --------------------------------------------------
  )

;; --------------------------------------------------
;; Raccourcis globaux pour accès rapide
;; --------------------------------------------------
(defun my/open-file-manager (&optional dir)
  "Ouvre Dirvish dans le répertoire DIR ou le répertoire courant, remplace le buffer actuel."
  (interactive)
  (let ((target-dir (or dir 
                       (if (derived-mode-p 'dired-mode)
                           (dired-current-directory)
                         default-directory))))
    (find-file target-dir)))

(defun my/open-home-directory ()
  "Ouvre le gestionnaire dans le dossier home, remplace le buffer actuel."
  (interactive)
  (find-file "~/"))

(defun my/open-pkm-directory ()
  "Ouvre le gestionnaire dans le dossier PKM, remplace le buffer actuel."
  (interactive)
  (find-file "~/00_PKM/"))

(defun my/open-documents-directory ()
  "Ouvre le gestionnaire dans Documents, remplace le buffer actuel."
  (interactive)
  (find-file "~/Documents/"))

(defun my/open-downloads-directory ()
  "Ouvre le gestionnaire dans Downloads, remplace le buffer actuel."
  (interactive)
  (find-file "~/Downloads/"))

;; --------------------------------------------------
;; Raccourcis globaux
;; --------------------------------------------------
(global-set-key (kbd "C-c f f") #'my/open-file-manager)
(global-set-key (kbd "C-c f h") #'my/open-home-directory)
(global-set-key (kbd "C-c f p") #'my/open-pkm-directory)
(global-set-key (kbd "C-c f d") #'my/open-documents-directory)
(global-set-key (kbd "C-c f t") #'my/open-downloads-directory)
(global-set-key (kbd "C-c f s") #'my/setup-template-directory)  ; Setup templates

;; --------------------------------------------------
;; Configuration supplémentaire pour paste/cut
;; --------------------------------------------------
(with-eval-after-load 'dired
  ;; Variables pour le clipboard
  (defvar dired-clipboard nil "Liste des fichiers dans le clipboard.")
  (defvar dired-clipboard-action nil "Action: 'cut ou 'copy.")
  
  ;; Ajouter fonction paste si elle n'existe pas
  (unless (fboundp 'dired-do-paste)
    (defun dired-do-paste ()
      "Colle les fichiers marqués pour cut/copy."
      (interactive)
      (if (and (boundp 'dired-clipboard) dired-clipboard)
          (let ((target-dir (dired-current-directory)))
            (dolist (file dired-clipboard)
              (let ((target-file (expand-file-name (file-name-nondirectory file) target-dir)))
                (if (eq dired-clipboard-action 'cut)
                    (rename-file file target-file)
                  (copy-file file target-file))))
            (setq dired-clipboard nil
                  dired-clipboard-action nil)
            (revert-buffer))
        (message "Rien à coller"))))
  
  ;; Améliorer les fonctions cut/copy
  (defun my/dired-do-cut ()
    "Marque les fichiers sélectionnés pour cut."
    (interactive)
    (setq dired-clipboard (dired-get-marked-files)
          dired-clipboard-action 'cut)
    (message "Fichiers marqués pour cut: %d" (length dired-clipboard)))
  
  (defun my/dired-do-copy-for-paste ()
    "Marque les fichiers sélectionnés pour copy."
    (interactive)
    (setq dired-clipboard (dired-get-marked-files)
          dired-clipboard-action 'copy)
    (message "Fichiers marqués pour copy: %d" (length dired-clipboard)))
  
  ;; Remplacer les raccourcis par défaut dans dirvish
  (with-eval-after-load 'dirvish
    (define-key dirvish-mode-map (kbd "x") #'my/dired-do-cut)
    (define-key dirvish-mode-map (kbd "c") #'my/dired-do-copy-for-paste)))

;; --------------------------------------------------
;; Force l'activation des icônes colorées après chargement
;; --------------------------------------------------
(with-eval-after-load 'dirvish
  (with-eval-after-load 'all-the-icons-dired
    ;; Hook pour s'assurer que les icônes sont activées
    (add-hook 'dirvish-mode-hook #'all-the-icons-dired-mode)
    
    ;; Force le refresh des icônes
    (defun my/refresh-icons ()
      "Force le refresh des icônes colorées."
      (when (derived-mode-p 'dired-mode)
        (all-the-icons-dired-mode -1)
        (all-the-icons-dired-mode 1)))
    
    ;; Refresh automatique après navigation
    (add-hook 'dired-after-readin-hook #'my/refresh-icons)))

;; --------------------------------------------------
;; Message de confirmation
;; --------------------------------------------------
(message "✓ Dirvish configuré pour PKM avec icônes colorées et filtrage")
(message "  📁 Raccourcis centralisés dans keybindings.el")
(message "  🔍 Dans Dirvish : raccourcis centralisés dans keybindings.el")
(message "  🔽 Filtrage : raccourcis centralisés dans keybindings.el")

(provide 'org-file-manager-dirvish)
;;; org-file-manager-dirvish.el ends here