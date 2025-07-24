;;; kill.el --- Utilities to clear caches and kill buffers  -*- lexical-binding: t; -*-
;;
;; Filename: kill.el
;; Description: Fournit deux fonctions : effacer les caches Emacs et fermer tous les buffers non-actifs.
;; Author: ChatGPT
;; Version: 1.0
;; Keywords: convenience, buffers, cache
;; URL: 
;; License: Public Domain

;;; Commentary:
;; Ce fichier définit :
;; - `kill/clear-all-caches` : nettoie plusieurs caches courants d'Emacs.
;; - `kill/kill-other-buffers` : ferme tous les buffers sauf celui en cours.
;; Ces fonctions sont liées aux raccourcis :
;; C-c k a -> kill/clear-all-caches
;; C-c k z -> kill/kill-other-buffers

;;; Code:

;; Construire dynamiquement le chemin du répertoire du script
(defconst kill--directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Répertoire où se trouve kill.el.")

(defun kill/clear-all-caches ()
  "Nettoie plusieurs caches courants d'Emacs :
- TRAMP
- recentf
- cache d'images
- cache de complétion (Emacs 29+)."
  (interactive)
  (when (fboundp 'tramp-cleanup-all-connections)
    (tramp-cleanup-all-connections))
  (when (and (boundp 'recentf-list) (fboundp 'recentf-cleanup))
    (recentf-cleanup))
  (when (fboundp 'clear-image-cache-image-data)
    (clear-image-cache-image-data))
  (when (fboundp 'completion--remove-all-cache)
    (completion--remove-all-cache))
  (message "Caches TRAMP, recentf, images et complétion vidés."))

(defun kill/kill-other-buffers ()
  "Ferme tous les buffers sauf celui en cours."  
  (interactive)
  (let ((current (current-buffer)))
    (dolist (buf (buffer-list))
      (unless (eq buf current)
        (kill-buffer buf))))
  (message "Tous les autres buffers ont été fermés."))

;; Raccourcis clavier
(global-set-key (kbd "C-c k a") #'kill/clear-all-caches)
(global-set-key (kbd "C-c k z") #'kill/kill-other-buffers)

(provide 'kill)
;;; kill.el ends here
