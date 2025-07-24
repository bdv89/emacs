;;; search.el --- Unified search configuration -*- lexical-binding: t; -*-

(setq consult-ripgrep-args
      "rg --null --line-buffered --color=never --max-columns=1000 \
--path-separator / --no-heading --with-filename --line-number --smart-case -e ARG")

(setq consult-find-args "find . -not ( -path */\\.* -prune )")

(defun my/rg-search-results (query pattern pkm-dir)
  "Execute ripgrep search and return formatted results."
  (let ((default-directory pkm-dir))
    (with-temp-buffer
      (when (zerop (call-process "rg" nil t nil
                                "--color=never"
                                "--line-number"
                                "--with-filename"
                                "--type" "org"
                                pattern "."))
        (split-string (buffer-string) "\n" t)))))

(defun my/search-notes-ivy ()
  "Search PKM notes using ivy-read with live ripgrep results."
  (interactive)
  (let ((pkm-dir (expand-file-name "~/00_PKM/")))
    (if (file-directory-p pkm-dir)
        (ivy-read "Rechercher dans les notes: "
                  (lambda (query)
                    (when (>= (length query) 3)
                      (my/rg-search-results query query pkm-dir)))
                  :dynamic-collection t
                  :action (lambda (result)
                            (when result
                              (let* ((parts (split-string result ":"))
                                     (file (car parts))
                                     (line (string-to-number (cadr parts))))
                                (find-file (expand-file-name file pkm-dir))
                                (goto-line line)
                                (recenter)))))
      (message "Répertoire PKM introuvable"))))

(defun my/search-org-titles-ivy ()
  "Search Org titles using ivy-read with live ripgrep results."
  (interactive)
  (let ((pkm-dir (expand-file-name "~/00_PKM/")))
    (if (file-directory-p pkm-dir)
        (ivy-read "Rechercher dans titres Org: "
                  (lambda (query)
                    (when (>= (length query) 3)
                      (my/rg-search-results query (concat "^\\*+.*" query) pkm-dir)))
                  :dynamic-collection t
                  :action (lambda (result)
                            (when result
                              (let* ((parts (split-string result ":"))
                                     (file (car parts))
                                     (line (string-to-number (cadr parts))))
                                (find-file (expand-file-name file pkm-dir))
                                (goto-line line)
                                (recenter)))))
      (message "Répertoire PKM introuvable"))))

(defun my/get-all-org-files ()
  "Get all .org files from notes directory (includes roam subdirectory).
Excludes files starting with a dot (hidden files)."
  (let ((files '()))
    (when (and (boundp 'my-notes-dir) (file-directory-p my-notes-dir))
      (setq files (directory-files-recursively my-notes-dir "\\.org$")))
    ;; Filter out files starting with a dot and convert to absolute paths
    (delete-dups 
     (mapcar #'file-truename 
             (seq-filter (lambda (file)
                          (not (string-match-p "^\\." (file-name-nondirectory file))))
                        files)))))

(defun my/consult-org-files-by-name ()
  "Search .org files by filename with live filtering."
  (interactive)
  (let ((org-files (my/get-all-org-files)))
    (if org-files
        (let* ((file-names (mapcar (lambda (file)
                                    (cons (file-name-nondirectory file) file))
                                  org-files))
               (choice (completing-read "Org file: " file-names nil t)))
          (find-file (cdr (assoc choice file-names))))
      (message "No .org files found in notes directories"))))

(when (eq system-type 'windows-nt)
  (defvar my/everything-path
    (cond
     ((executable-find "es.exe") "es.exe")
     ((file-exists-p "C:/Program Files/Everything/es.exe") 
      "C:/Program Files/Everything/es.exe")
     ((file-exists-p "C:/Program Files (x86)/Everything/es.exe") 
      "C:/Program Files (x86)/Everything/es.exe")
     (t nil))
    "Path to Everything executable.")

  (defun my/everything-search (query &optional max-results file-type)
    "Simple Everything search with file type filtering."
    (when my/everything-path
      (let* ((limit (or max-results 20))
             (type-flag (cond
                        ((eq file-type 'files) "/a-d")
                        ((eq file-type 'folders) "/ad")
                        (t "")))
             (cmd (format "%s -n %d %s %s" my/everything-path limit type-flag query))
             (result (shell-command-to-string cmd)))
        (when (> (length result) 0)
          (split-string result "\n" t)))))

  ;; FONCTION CORRIGÉE : Lance les raccourcis .lnk du menu Démarrer
  (defun my/launch-shortcuts-fixed ()
    "Lance les raccourcis .lnk du menu Démarrer via Everything."
    (interactive)
    (if (not (and (boundp 'my/everything-path) my/everything-path))
        (message "❌ Everything non disponible")
      
      (ivy-read "Raccourcis (.lnk) : "
                (lambda (query)
                  (when (> (length query) 2)
                    (let* ((search-query (format "%s ext:lnk" query))
                           (results (my/everything-search search-query 50 'files))
                           (filtered-results (seq-filter 
                                            (lambda (path)
                                              (or (string-match-p "Start Menu" path)
                                                  (string-match-p "Mes programmes" path)
                                                  (string-match-p "Desktop" path)))
                                            results)))
                      
                      ;; Structure simple : "NOM|CHEMIN"
                      (when filtered-results
                        (mapcar (lambda (path)
                                  (let ((name (file-name-sans-extension 
                                              (file-name-nondirectory path))))
                                    (format "%s|%s" name path)))
                                filtered-results)))))
                
                :dynamic-collection t
                
                :action (lambda (selection)
                          ;; Parse "NOM|CHEMIN"
                          (if (and (stringp selection) (string-match "\\(.+\\)|\\(.+\\)" selection))
                              (let ((name (match-string 1 selection))
                                    (path (match-string 2 selection)))
                                (if (and path (file-exists-p path))
                                    (condition-case err
                                        (progn
                                          (w32-shell-execute "open" path)
                                          (message "✅ Raccourci lancé : %s" name))
                                      (error 
                                       (message "❌ Erreur de lancement : %s" (error-message-string err))))
                                  (message "❌ Fichier introuvable : %s" path)))
                            (message "❌ Format invalide : %s" selection))))))

  (defun my/counsel-everything-general ()
    "Search everything (files and folders) using Everything."
    (interactive)
    (if (not my/everything-path)
        (message "Everything not available")
      (ivy-read "Everything: "
                (lambda (query)
                  (when (> (length query) 2)
                    (or (my/everything-search query 50) '())))
                :dynamic-collection t
                :action (lambda (item)
                          (if (file-directory-p item)
                              (dired item)
                            (find-file item))))))

 (defun my/counsel-everything-files ()
  "Rechercher des fichiers avec Everything.
Les fichiers .org et .el s'ouvrent dans Emacs, les autres avec l'application par défaut."
  (interactive)
  (if (not my/everything-path)
      (message "Everything non disponible")
    (let ((candidates-hash (make-hash-table :test 'equal)))
      (ivy-read "Everything fichiers : "
                (lambda (query)
                  (when (> (length query) 2)
                    (let ((results (my/everything-search query 50 'files)))
                      ;; Stocker les chemins dans la hash table
                      (clrhash candidates-hash)
                      (when results
                        (dolist (path results)
                          (let ((name (file-name-nondirectory path)))
                            ;; Gérer les doublons en ajoutant le dossier parent
                            (if (gethash name candidates-hash)
                                (let ((unique-name (format "%s (%s)" name 
                                                         (file-name-nondirectory 
                                                          (directory-file-name 
                                                           (file-name-directory path))))))
                                  (puthash unique-name path candidates-hash))
                              (puthash name path candidates-hash)))))
                      ;; Retourner seulement les noms, ou liste vide si hash table vide
                      (or (hash-table-keys candidates-hash) '()))))
                :dynamic-collection t
                :action (lambda (selected-name)
                          (let ((file-path (gethash selected-name candidates-hash)))
                            (if (and file-path (file-exists-p file-path))
                                (let ((extension (file-name-extension file-path)))
                                  ;; Décider comment ouvrir selon l'extension
                                  (if (member extension '("org" "el" "txt" "md" "py" "js" "html" "css" "json" "xml"))
                                      ;; Ouvrir dans Emacs
                                      (progn
                                        (find-file file-path)
                                        (message "✓ Fichier ouvert dans Emacs: %s" selected-name))
                                    ;; Ouvrir avec l'application externe
                                    (progn
                                      (w32-shell-execute "open" file-path)
                                      (message "✓ Fichier ouvert: %s" selected-name))))
                              (message "❌ Fichier introuvable: %s" file-path))))))))



 
  (defun my/counsel-everything-folders ()
    "Search folders using Everything."
    (interactive)
    (if (not my/everything-path)
        (message "Everything not available")
      (ivy-read "Everything folders: "
                (lambda (query)
                  (when (> (length query) 2)
                    (or (my/everything-search query 50 'folders) '())))
                :dynamic-collection t
                :action (lambda (item)
                          (dired item)))))

  ;; Fonction de test pour debug
  (defun my/test-direct-launch ()
    "Test direct du lancement d'un .lnk"
    (interactive)
    (let ((test-file "C:/ProgramData/Microsoft/Windows/Start Menu/Programs/Obsidian.lnk"))
      (if (file-exists-p test-file)
          (condition-case err
              (progn
                (w32-shell-execute "open" test-file)
                (message "✅ Test direct réussi!"))
            (error (message "❌ Test direct échoué: %s" (error-message-string err))))
        (message "❌ Fichier test introuvable : %s" test-file))))

  ;; Raccourcis Everything (Windows uniquement)
  (when my/everything-path
    (global-set-key (kbd "C-c e e") #'my/counsel-everything-general)
    (global-set-key (kbd "C-c e f") #'my/counsel-everything-files)
    (global-set-key (kbd "C-c e d") #'my/counsel-everything-folders)
    (global-set-key (kbd "C-c l l") #'my/launch-shortcuts-fixed)
    (global-set-key (kbd "C-c d l") #'my/test-direct-launch)))

;; Configuration pour consult-line avec preview automatique
(setq consult-line-start-from-top t)     ; Commence du début du buffer

;; Force le preview automatique pour consult-line
(with-eval-after-load 'consult
  (setq consult-preview-key 'any)
  (consult-customize
   consult-line :preview-key 'any))

;; Raccourcis gérés dans 1_packages.el - pas de doublons ici

(provide 'search)
;;; search.el ends here