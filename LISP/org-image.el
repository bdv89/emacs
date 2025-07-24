;;; org-image.el --- Gestion des images dans org-mode avec ImageMagick -*- lexical-binding: t; -*-

(require 'org)
(require 'org-download)

;; --------------------------------------------------
;; Configuration du répertoire d'images
;; --------------------------------------------------
(defvar my-org-image-dir (expand-file-name "images" "~/00_PKM/")
  "Répertoire pour stocker les images org-mode.")

;; Configuration org-download
(setq org-download-image-dir my-org-image-dir)

;; Assurer que le répertoire existe
(unless (file-directory-p my-org-image-dir)
  (make-directory my-org-image-dir t)
  (message "✓ Répertoire d'images créé : %s" my-org-image-dir))

;; --------------------------------------------------
;; Fonctions utilitaires
;; --------------------------------------------------
(defun my/org-image-generate-name ()
  "Génère un nom unique basé sur la date et l'heure."
  (concat (format-time-string "%Y%m%d_%H%M%S") ".png"))

(defun my/org-image-get-full-path (filename)
  "Retourne le chemin complet pour FILENAME dans le répertoire d'images."
  (expand-file-name filename my-org-image-dir))

;; --------------------------------------------------
;; Fonctions principales
;; --------------------------------------------------
(defun my/org-image-paste ()
  "Capture une image du presse-papier via ImageMagick et l'insère dans le buffer Org."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Cette fonction nécessite un buffer en mode Org"))
  
  (let* ((filename (my/org-image-generate-name))
         (fullpath (my/org-image-get-full-path filename))
         (command (format "magick clipboard: \"%s\"" fullpath)))
    
    (condition-case err
        (progn
          (message "⏳ Capture de l'image du presse-papier...")
          (shell-command command)
          
          (if (file-exists-p fullpath)
              (progn
                ;; Insérer le lien avec attributs
                (insert (format "#+ATTR_ORG: :width 300\n[[file:%s]]\n"
                                (file-relative-name fullpath)))
                ;; Rafraîchir l'affichage
                (org-display-inline-images)
                (message "✅ Image insérée : %s" (file-name-nondirectory fullpath)))
            (message "❌ Aucune image trouvée dans le presse-papier ou erreur avec ImageMagick.")))
      (error (message "❌ Erreur dans org-image-paste: %s" (error-message-string err))))))

(defun my/org-image-refresh ()
  "Réactualise l'affichage des images dans le buffer Org courant."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-redisplay-inline-images)
    (message "🔄 Images rafraîchies.")))

(defun my/org-image-edit-last ()
  "Ouvre la dernière image insérée avec MSPaint pour édition."
  (interactive)
  (let ((last-image (car (last (directory-files my-org-image-dir t "\\.png$")))))
    (if (and last-image (file-exists-p last-image))
        (progn
          (start-process "mspaint" nil "mspaint" last-image)
          (message "🎨 Ouverture de %s dans MSPaint" (file-name-nondirectory last-image)))
      (message "❌ Aucune image trouvée dans %s" my-org-image-dir))))

(defun my/org-image-show-directory ()
  "Ouvre le répertoire d'images dans l'explorateur Windows."
  (interactive)
  (when (eq system-type 'windows-nt)
    (w32-shell-execute "open" my-org-image-dir))
  (message "📁 Répertoire d'images : %s" my-org-image-dir))

(defun my/org-image-list-images ()
  "Affiche la liste des images dans le répertoire."
  (interactive)
  (let ((images (directory-files my-org-image-dir nil "\\.\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\)$")))
    (if images
        (with-current-buffer (get-buffer-create "*Images PKM*")
          (erase-buffer)
          (insert (format "=== IMAGES DANS %s ===\n\n" my-org-image-dir))
          (dolist (img images)
            (let ((fullpath (my/org-image-get-full-path img)))
              (insert (format "• %s (%s)\n" 
                              img 
                              (file-size-human-readable (nth 7 (file-attributes fullpath)))))))
          (insert (format "\nTotal : %d images\n" (length images)))
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (message "📂 Aucune image trouvée dans %s" my-org-image-dir))))

(defun my/org-image-toggle-display ()
  "Bascule l'affichage des images inline dans le buffer Org courant."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Cette fonction nécessite un buffer en mode Org"))
  
  (if (get 'org-inline-image-display 'state)
      ;; Les images sont affichées, les masquer
      (progn
        (org-remove-inline-images)
        (put 'org-inline-image-display 'state nil)
        (message "👁️‍🗨️ Images masquées"))
    ;; Les images sont masquées, les afficher
    (progn
      (org-display-inline-images)
      (put 'org-inline-image-display 'state t)
      (message "🖼️ Images affichées"))))

(defun my/org-image-hide-all ()
  "Masque toutes les images inline dans le buffer Org courant."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Cette fonction nécessite un buffer en mode Org"))
  
  (org-remove-inline-images)
  (put 'org-inline-image-display 'state nil)
  (message "👁️‍🗨️ Toutes les images masquées"))

(defun my/org-image-show-all ()
  "Affiche toutes les images inline dans le buffer Org courant."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Cette fonction nécessite un buffer en mode Org"))
  
  (org-display-inline-images)
  (put 'org-inline-image-display 'state t)
  (message "🖼️ Toutes les images affichées"))

;; --------------------------------------------------
;; Configuration org-download avancée
;; --------------------------------------------------
;; Personnaliser le nom des fichiers org-download
(setq org-download-image-org-width 400
      org-download-heading-lvl nil
      org-download-timestamp "%Y%m%d_%H%M%S_")

;; Hook pour mettre à jour l'affichage après insertion d'image
(add-hook 'org-download-after-insert-functions
          (lambda () (org-display-inline-images)))

;; --------------------------------------------------
;; Raccourcis clavier
;; --------------------------------------------------
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c i i") #'my/org-image-paste)
  (define-key org-mode-map (kbd "C-c i a") #'my/org-image-refresh)
  (define-key org-mode-map (kbd "C-c i e") #'my/org-image-edit-last)
  (define-key org-mode-map (kbd "C-c i d") #'my/org-image-show-directory)
  (define-key org-mode-map (kbd "C-c i l") #'my/org-image-list-images)
  (define-key org-mode-map (kbd "C-c i t") #'my/org-image-toggle-display)
  (define-key org-mode-map (kbd "C-c i h") #'my/org-image-hide-all)
  (define-key org-mode-map (kbd "C-c i s") #'my/org-image-show-all))

;; Raccourcis globaux (disponibles dans tous les modes)
(global-set-key (kbd "C-c i i") #'my/org-image-paste)
(global-set-key (kbd "C-c i a") #'my/org-image-refresh)
(global-set-key (kbd "C-c i t") #'my/org-image-toggle-display)

;; --------------------------------------------------
;; Messages de confirmation
;; --------------------------------------------------
(message "✓ org-image.el chargé - Répertoire : %s" my-org-image-dir)

(provide 'org-image)
;;; org-image.el ends here