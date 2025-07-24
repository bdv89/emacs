;;; org-base.el --- Configuration de base Org Mode -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Directory Setup
;; --------------------------------------------------
(defvar my-notes-dir
  (cond
   ((file-directory-p "~/00_PKM/") "~/00_PKM/")
   ((file-directory-p (expand-file-name "notes" (getenv "HOME")))
    (expand-file-name "notes" (getenv "HOME")))
   (t (make-directory (expand-file-name "notes" (getenv "HOME")) t)
      (expand-file-name "notes" (getenv "HOME"))))
  "Répertoire principal pour vos fichiers Org.")

(defvar my-roam-dir
  (let ((roam (expand-file-name "roam" my-notes-dir)))
    (unless (file-directory-p roam)
      (make-directory roam t))
    roam)
  "Répertoire pour Org-roam.")

;; --------------------------------------------------
;; Custom link opening behavior for mouse clicks
;; --------------------------------------------------
(defun my/advice-org-open-at-point (orig-fun &optional arg)
  "Advice for org-open-at-point to handle mouse clicks differently.
- Left click (mouse-1): open in current window
- Right click (mouse-3): open in other window (split if needed)"
  (let* ((mouse-event (and (boundp 'last-input-event) 
                          (listp last-input-event)
                          last-input-event))
         (event-type (when mouse-event (car mouse-event)))
         (basic-type (when event-type (event-basic-type event-type)))
         (is-right-click (eq basic-type 'mouse-3)))
    
    (if is-right-click
        ;; Clic droit (mouse-3): ouvre dans une autre fenêtre
        (let ((org-link-frame-setup 
               (cons '(file . find-file-other-window) 
                     (assq-delete-all 'file org-link-frame-setup))))
          (funcall orig-fun arg))
      ;; Clic gauche (mouse-1) ou autre: ouvre dans la fenêtre actuelle
      (let ((org-link-frame-setup 
             (cons '(file . find-file) 
                   (assq-delete-all 'file org-link-frame-setup))))
        (funcall orig-fun arg)))))

;; --------------------------------------------------
;; Core Org Mode Configuration
;; --------------------------------------------------
(use-package org
  :init
  (setq org-directory            my-notes-dir
        org-default-notes-file   (expand-file-name "inbox.org" org-directory)
        org-startup-indented     nil  ; Désactivé pour compatibilité org-modern
        org-hide-leading-stars   t
        org-return-follows-link  t
        org-fold-core-style      'overlays
        org-hide-emphasis-markers t
        ;; Configuration des subscripts/superscripts
        org-use-sub-superscripts '{} ; Utilise {} pour délimiter (plus flexible)
        org-export-with-sub-superscripts '{}
        org-pretty-entities t         ; Active l'affichage des entités prettifiées
        org-pretty-entities-include-sub-superscripts t ; Inclut sub/superscripts
        ;; Configure link frame setup for default behavior
        org-link-frame-setup     '((vm . vm-visit-folder)
                                  (vm-imap . vm-visit-imap-folder)
                                  (gnus . gnus)
                                  (file . find-file)  ; Default: same window
                                  (wl . wl)))
  :config
  ;; Add advice to org-open-at-point to support mouse click detection
  (advice-add 'org-open-at-point :around #'my/advice-org-open-at-point)

  ;; Agenda files
  (when (file-directory-p org-directory)
    (setq org-agenda-files
          (directory-files-recursively org-directory "\\.org$")))

  ;; Capture templates

  (setq org-capture-templates
        `(("t" "Tâche" entry
           (file+headline ,org-default-notes-file "Tâches à faire")
           "* TODO %?\n  %U")
          ("n" "Note volante" entry
           (file+headline ,(expand-file-name "NOTE_VOLANTES.org" my-notes-dir) "Notes volantes")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n")
          ("i" "Idée" entry
           (file+headline ,(expand-file-name "IDEA.org" my-notes-dir) "Idées")
           "* %^{Titre de l'idée}\n:PROPERTIES:\n:CREATED: %<%Y-%m-%d>\n:URL: %^{url}p\n:ISBN: %^{isbn}p\n:DOI: %^{doi}p\n:CITATIONKEY: %^{citationkey}p\n:TAGS: %^{tags}p\n:END:\n\n* Explications de l'idée\n%?")))




  ;; Basic hooks (sans org-indent)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  
  ;; Désactiver explicitement org-indent-mode
  (add-hook 'org-mode-hook (lambda () (org-indent-mode -1)))

  ;; Key bindings
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

;; --------------------------------------------------
;; Configuration ISO 8601 (Standard International)
;; --------------------------------------------------
;; Format ISO 8601 pour les timestamps
(setq org-time-stamp-custom-formats 
      '("<%Y%m%d %a>" . "<%Y%m%d %a %H:%M>"))

;; Activer l'affichage personnalisé
(setq org-display-custom-times t)

;; Configuration agenda avec format 24h
(setq org-agenda-time-leading-zero t
      org-agenda-timegrid-use-ampm nil)

;; Configuration calendrier ISO (semaine commence lundi)
(setq calendar-week-start-day 1
      org-agenda-start-on-weekday 1)

;; Format d'export pour timestamps
(setq org-export-date-timestamp-format "%Y%m%d")

;; --------------------------------------------------
;; Babel configuration
;; --------------------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; --------------------------------------------------
;; Configuration de navigation du calendrier
;; --------------------------------------------------
(with-eval-after-load 'calendar
  ;; Configuration des raccourcis de navigation (évite conflits avec window navigation)
  (define-key calendar-mode-map (kbd "C-<left>") 'calendar-backward-week)
  (define-key calendar-mode-map (kbd "C-<right>") 'calendar-forward-week)
  (define-key calendar-mode-map (kbd "C-<up>") 'calendar-backward-month) 
  (define-key calendar-mode-map (kbd "C-<down>") 'calendar-forward-month)
  (define-key calendar-mode-map (kbd "M-<left>") 'calendar-backward-month)
  (define-key calendar-mode-map (kbd "M-<right>") 'calendar-forward-month)
  (define-key calendar-mode-map (kbd "M-<up>") 'calendar-backward-year)
  (define-key calendar-mode-map (kbd "M-<down>") 'calendar-forward-year))

;; --------------------------------------------------
;; Création automatique d'un ID (roam) à chaque capture
;; --------------------------------------------------
(add-hook 'org-capture-prepare-finalize-hook
          (lambda ()
            (when (and (derived-mode-p 'org-mode)
                       (not (org-entry-get nil "ID")))
              (org-id-get-create))))

;; --------------------------------------------------
;; Org Super Agenda Configuration
;; --------------------------------------------------
(defun my/org-super-agenda-groups ()
  "Generate org-super-agenda groups with dynamic dates."
  (let ((week-ahead (format-time-string "%Y-%m-%d" (time-add (current-time) (* 7 24 3600)))))
    `(;; Time-based groups
      (:name "Overdue"
       :deadline past
       :scheduled past
       :face (:foreground "red"))
      (:name "Today"
       :time-grid t
       :scheduled today
       :deadline today)
      (:name "This Week"
       :scheduled (before ,week-ahead)
       :deadline (before ,week-ahead))
      
      ;; Content-based groups
      (:name "Ideas & Notes"
       :file-path "IDEA\\|NOTE_VOLANTES"
       :face (:foreground "cyan"))
      (:name "Roam Tasks"
       :file-path "roam/"
       :face (:foreground "yellow"))
      (:name "Inbox"
       :file-path "inbox"
       :face (:foreground "orange"))
      
      ;; Priority groups
      (:name "Important"
       :priority "A"
       :face (:foreground "red"))
      (:name "Medium Priority"
       :priority "B"
       :face (:foreground "yellow"))
      
      ;; Catch-all
      (:name "Other Tasks"
       :anything t))))

(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode 1)
  (setq org-super-agenda-groups (my/org-super-agenda-groups)))

;; --------------------------------------------------
;; Basic key bindings
;; --------------------------------------------------
(global-set-key (kbd "<f1>") (lambda () (interactive) (find-file org-default-notes-file)))

(provide 'org-base)
;;; org-base.el ends here