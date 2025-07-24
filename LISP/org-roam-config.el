;;; org-roam-config.el --- Configuration complète Org-roam -*- lexical-binding: t; -*-

;; Require org-base for directory variables
(require 'org-base)

;; --------------------------------------------------
;; Org-roam setup
;; --------------------------------------------------
(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory my-roam-dir)
  :config 
  ;; Délai pour éviter les erreurs SQLite au démarrage
  (run-with-timer 2 nil 
    (lambda ()
      (condition-case err
        (org-roam-db-autosync-enable)
        (error
         (message "Erreur org-roam db: %s" err)
         (run-with-timer 5 nil #'org-roam-db-autosync-enable))))))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; Force ID links for roam
(setq org-roam-prefer-id-links t)

;; --------------------------------------------------
;; Note extraction function
;; --------------------------------------------------
(defun my/org-roam-extract-to-note ()
  "Extract selected text into a new Org-roam note with backlink."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  
  (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (title (string-trim selected-text))
         ;; Récupère l'ID de la note org-roam actuelle
         (source-id (save-excursion
                     (goto-char (point-min))
                     ;; Cherche l'ID dans le drawer :PROPERTIES:
                     (when (re-search-forward "^:ID: +\\([^ \n]+\\)" nil t)
                       (match-string 1))))
         ;; Si pas d'ID trouvé, le crée
         (source-id (or source-id
                       (save-excursion
                         (goto-char (point-min))
                         (org-id-get-create))))
         (source-title (or (save-excursion
                            (goto-char (point-min))
                            (when (re-search-forward "^#\\+title: +\\(.+\\)" nil t)
                              (match-string 1)))
                          (file-name-base (buffer-file-name))))
         (slug (org-roam-node-slug (org-roam-node-create :title title)))
         (filename (format "%s.org" slug))
         (filepath (expand-file-name filename org-roam-directory)))
    
    ;; Vérifie si le fichier existe déjà
    (when (file-exists-p filepath)
      (user-error "File already exists: %s" filename))
    
    ;; Crée le fichier de la nouvelle note
    (with-temp-file filepath
      (insert (format "#+title: %s\n" title))
      (insert (format-time-string "Created: %y%m%d\n"))
      (insert (format "Source : [[id:%s][%s]]\n\n" source-id source-title))
      (insert "* Content\n")
      (insert "\n"))
    
    ;; Ajoute l'ID à la nouvelle note et récupère cet ID
    (let ((new-note-id))
      (with-current-buffer (find-file-noselect filepath)
        (setq new-note-id (org-id-get-create))
        (save-buffer))
      
      ;; Supprime le texte et insère le lien basé sur l'ID
      (delete-region (region-beginning) (region-end))
      (insert (format "[[id:%s][%s]]" new-note-id title)))
    
    ;; Ouvre la nouvelle note
    (find-file filepath)
    (goto-char (point-max))))

;; --------------------------------------------------
;; Persistent org folding state
;; --------------------------------------------------
(defvar my-org-fold-data-dir
  (expand-file-name "org-fold" user-emacs-directory)
  "Directory to store org folding state files.")

(defun my-org-fold-data-file (file)
  "Get the fold data file path for FILE."
  (when file
    (let ((hash (secure-hash 'md5 (expand-file-name file))))
      (expand-file-name (concat hash ".fold") my-org-fold-data-dir))))

(defun my-org-save-fold-state ()
  "Save current org buffer's folding state to file."
  (when (and (derived-mode-p 'org-mode)
             buffer-file-name
             (not (buffer-modified-p))) ; Only save if buffer is saved
    (let ((fold-file (my-org-fold-data-file buffer-file-name))
          (fold-data '()))
      
      ;; Ensure fold data directory exists
      (unless (file-directory-p my-org-fold-data-dir)
        (make-directory my-org-fold-data-dir t))
      
      ;; Collect folding information
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ " nil t)
          (let* ((pos (line-beginning-position))
                 (heading (buffer-substring-no-properties 
                          (line-beginning-position) (line-end-position)))
                 (folded (save-excursion
                          (goto-char pos)
                          (end-of-line)
                          (outline-invisible-p))))
            (when folded
              (push (list pos heading) fold-data)))))
      
      ;; Save to file or remove if no folds
      (if fold-data
          (with-temp-file fold-file
            (prin1 fold-data (current-buffer)))
        (when (file-exists-p fold-file)
          (delete-file fold-file))))))

(defun my-org-restore-fold-state ()
  "Restore org buffer's folding state from file."
  (when (and (derived-mode-p 'org-mode)
             buffer-file-name)
    (let ((fold-file (my-org-fold-data-file buffer-file-name)))
      (when (file-exists-p fold-file)
        (condition-case nil
            (let ((fold-data (with-temp-buffer
                              (insert-file-contents fold-file)
                              (read (current-buffer)))))
              ;; Show everything first
              (outline-show-all)
              
              ;; Then fold the saved headings
              (save-excursion
                (dolist (fold-info fold-data)
                  (let ((pos (car fold-info))
                        (heading (cadr fold-info)))
                    (goto-char pos)
                    (when (and (outline-on-heading-p)
                               (string= heading 
                                       (buffer-substring-no-properties 
                                        (line-beginning-position) (line-end-position))))
                      (outline-hide-subtree))))))
          (error nil))))))

(defun my-org-cleanup-fold-data ()
  "Clean up fold data files older than 30 days."
  (when (file-directory-p my-org-fold-data-dir)
    (dolist (fold-file (directory-files my-org-fold-data-dir t "\\.fold$"))
      (when (> (float-time (time-subtract (current-time)
                                          (nth 5 (file-attributes fold-file))))
               (* 30 24 60 60)) ; 30 days in seconds
        (delete-file fold-file)))))

;; --------------------------------------------------
;; Setup hooks for folding
;; --------------------------------------------------
(add-hook 'org-mode-hook #'my-org-restore-fold-state)
(add-hook 'after-save-hook #'my-org-save-fold-state)
(add-hook 'kill-buffer-hook #'my-org-save-fold-state)

;; Clean up old fold files weekly
(run-with-timer 0 (* 7 24 60 60) #'my-org-cleanup-fold-data)

;; --------------------------------------------------
;; Key bindings
;; --------------------------------------------------
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n g") #'org-roam-ui-open)
(global-set-key (kbd "C-c r r") #'my/org-roam-extract-to-note)

;; Debugging keybindings for folding
(global-set-key (kbd "C-c f s") #'my-org-save-fold-state)
(global-set-key (kbd "C-c f r") #'my-org-restore-fold-state)

(provide 'org-roam-config)
;;; org-roam-config.el ends here