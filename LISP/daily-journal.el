;;; daily-journal.el --- Système de journaling quotidien -*- lexical-binding: t; -*-

;; --------------------------------------------------
(require 'cl-lib)

;; Configuration
;; --------------------------------------------------

;; Système de synchronisation optimisé
;; --------------------------------------------------
(defvar my-planning-sync-lock nil
  "Mutex pour éviter les races conditions lors de la synchronisation.")

(defvar my-planning-sync-timer nil
  "Timer pour debounce des synchronisations.")

(defvar my-planning-files-cache (make-hash-table :test 'equal)
  "Cache des timestamps des fichiers pour détecter les changements.")

(defvar my-planning-last-sync-time 0
  "Timestamp de la dernière synchronisation complète.")
(defvar daily-journal-directory nil
  "Répertoire pour les notes journalières dans org-roam.")

(defun daily-journal-get-directory ()
  "Retourne le répertoire journal, en le créant si nécessaire."
  (unless daily-journal-directory
    (setq daily-journal-directory
          (expand-file-name "journal" 
                            (or (and (boundp 'my-roam-dir) my-roam-dir)
                                (expand-file-name "roam" "~/00_PKM/")))))
  daily-journal-directory)

(defvar daily-journal-template
  "#+title: Journal %s
#+filetags: :journaling:daily:
#+date: %s

* 🌅 Gratitude du matin
- Je suis reconnaissant(e) pour :
  - 
  - 
  - 

* 💫 Intention de la journée
- Mon focus principal aujourd'hui :
- Comment je veux me sentir :

* ⏰ Planning Temporel
** Tâches programmées
# Format: - ☐ Nom tâche - Durée - Heure début → Fin calculée
# Exemple: - ☐ Emails matinaux - 30min - 9h00 → 9h30

** Temps morts identifiés
# Les créneaux libres seront détectés automatiquement

** Bilan temps réel
# Mis à jour automatiquement pendant la journée

* 📝 Notes de la journée


* 🌙 Réflexion du soir
- Moment de joie aujourd'hui :
- Apprentissage ou découverte :
- Geste de bienveillance (donné ou reçu) :

* 🎯 Demain
- Une chose que je veux améliorer :
- Une chose que je veux célébrer :
"
  "Template pour les notes journalières.")

;; --------------------------------------------------
;; Fonctions principales
;; --------------------------------------------------
(defun daily-journal-ensure-directory ()
  "Crée le répertoire journal s'il n'existe pas."
  (let ((dir (daily-journal-get-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (message "✓ Répertoire journal créé : %s" dir))))

(defun daily-journal-get-filename (&optional date)
  "Retourne le nom de fichier pour DATE (aujourd'hui par défaut)."
  (let ((date-str (or date (format-time-string "%Y-%m-%d"))))
    (expand-file-name (concat date-str ".org") (daily-journal-get-directory))))

(defun daily-journal-create-entry (&optional date)
  "Crée une entrée journalière pour DATE (aujourd'hui par défaut)."
  (let* ((date-str (or date (format-time-string "%Y-%m-%d")))
         (filepath (daily-journal-get-filename date-str))
         (display-date (format-time-string "%A %d %B %Y")))
    
    (daily-journal-ensure-directory)
    
    (unless (file-exists-p filepath)
      (with-temp-file filepath
        (insert (format daily-journal-template date-str display-date)))
      (message "✓ Note journalière créée : %s" date-str))
    
    filepath))

;;;###autoload
(defun daily-journal-open-today ()
  "Ouvre ou crée la note journalière d'aujourd'hui."
  (interactive)
  (let ((filepath (daily-journal-create-entry)))
    (find-file filepath)
    ;; Positionner le curseur à la fin de la première section gratitude
    (goto-char (point-min))
    (when (search-forward "- Je suis reconnaissant(e) pour :" nil t)
      (end-of-line)
      (newline)
      (insert "  - "))
    (message "📝 Journal ouvert - %s" (format-time-string "%A %d %B %Y"))))

;; --------------------------------------------------
;; Planning Temporel - Fonctions de parsing
;; --------------------------------------------------

(defvar daily-journal-task-regex
  "^- \\([☐✓]\\) \\(.*?\\) - \\([0-9]+\\(?:h\\|min\\|h[0-9]+\\)\\) - \\([0-9]+h[0-9]+\\)\\(?: → \\([0-9]+h[0-9]+\\)\\)?"
  "Regex pour parser les tâches : - ☐ Nom - Durée - Début → Fin")

(defun daily-journal-parse-duration (duration-str)
  "Convertit une durée (ex: '2h', '30min', '1h30') en minutes."
  (cond
   ((string-match "^\\([0-9]+\\)h\\([0-9]+\\)$" duration-str)
    (+ (* (string-to-number (match-string 1 duration-str)) 60)
       (string-to-number (match-string 2 duration-str))))
   ((string-match "^\\([0-9]+\\)h$" duration-str)
    (* (string-to-number (match-string 1 duration-str)) 60))
   ((string-match "^\\([0-9]+\\)min$" duration-str)
    (string-to-number (match-string 1 duration-str)))
   (t 0)))

(defun daily-journal-parse-time (time-str)
  "Convertit une heure (ex: '9h30', '14h00') en minutes depuis minuit."
  (when (string-match "^\\([0-9]+\\)h\\([0-9]+\\)$" time-str)
    (+ (* (string-to-number (match-string 1 time-str)) 60)
       (string-to-number (match-string 2 time-str)))))

(defun daily-journal-minutes-to-time (minutes)
  "Convertit minutes depuis minuit en format 'Xh00'."
  (format "%dh%02d" (/ minutes 60) (% minutes 60)))

(defun daily-journal-calculate-end-time (start-time duration)
  "Calcule l'heure de fin à partir du début et de la durée."
  (let ((start-min (daily-journal-parse-time start-time))
        (duration-min (daily-journal-parse-duration duration)))
    (when (and start-min duration-min)
      (daily-journal-minutes-to-time (+ start-min duration-min)))))

(defun daily-journal-parse-tasks-in-buffer ()
  "Parse toutes les tâches du planning dans le buffer actuel."
  (save-excursion
    (goto-char (point-min))
    (let ((tasks '()))
      (while (re-search-forward daily-journal-task-regex nil t)
        (let* ((checkbox (match-string 1))
               (name (match-string 2))
               (duration (match-string 3))
               (start (match-string 4))
               (end (or (match-string 5)
                       (daily-journal-calculate-end-time start duration)))
               (completed (string= checkbox "✓")))
          (when end
            (push (list :name name
                       :duration duration
                       :start start
                       :end end
                       :completed completed
                       :start-min (daily-journal-parse-time start)
                       :end-min (daily-journal-parse-time end))
                  tasks))))
      (reverse tasks))))

;;;###autoload
(defun daily-journal-update-end-times ()
  "Met à jour automatiquement les heures de fin dans le planning."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward daily-journal-task-regex nil t)
      (let* ((duration (match-string 3))
             (start (match-string 4))
             (current-end (match-string 5))
             (calculated-end (daily-journal-calculate-end-time start duration)))
        (when (and calculated-end (not current-end))
          (end-of-line)
          (insert (format " → %s" calculated-end)))))))

;; --------------------------------------------------
;; Buffer Chrono - Affichage temps réel
;; --------------------------------------------------

(defvar daily-journal-chrono-buffer "*Focus*"
  "Nom du buffer d'affichage du chrono.")

(defvar daily-journal-chrono-timer nil
  "Timer pour la mise à jour du chrono.")

(defvar daily-journal-current-tasks nil
  "Liste des tâches du planning courant.")

(defun daily-journal-get-current-time-minutes ()
  "Retourne l'heure actuelle en minutes depuis minuit."
  (let ((time (decode-time)))
    (+ (* (nth 2 time) 60) (nth 1 time))))

(defun daily-journal-find-current-task (tasks current-time-min)
  "Trouve la tâche en cours selon l'heure actuelle."
  (cl-find-if (lambda (task)
                (and (<= (plist-get task :start-min) current-time-min)
                     (> (plist-get task :end-min) current-time-min)
                     (not (plist-get task :completed))))
              tasks))

(defun daily-journal-detect-idle-times (tasks)
  "Détecte les temps morts entre les tâches."
  (let ((idle-times '())
        (sorted-tasks (sort (copy-sequence tasks)
                           (lambda (a b) (< (plist-get a :start-min)
                                           (plist-get b :start-min))))))
    (when sorted-tasks
      (let ((prev-end 0))
        (dolist (task sorted-tasks)
          (let ((start (plist-get task :start-min)))
            (when (> start prev-end)
              (push (list :start-min prev-end
                         :end-min start
                         :duration (- start prev-end)
                         :start (daily-journal-minutes-to-time prev-end)
                         :end (daily-journal-minutes-to-time start))
                    idle-times))
            (setq prev-end (plist-get task :end-min))))))
    (reverse idle-times)))

(defun daily-journal-load-current-planning ()
  "Charge le planning depuis le journal du jour."
  (let ((journal-file (daily-journal-get-filename)))
    (when (file-exists-p journal-file)
      (with-temp-buffer
        (insert-file-contents journal-file)
        (setq daily-journal-current-tasks (daily-journal-parse-tasks-in-buffer))))))

(defun daily-journal-update-chrono-buffer ()
  "Met à jour le buffer chrono avec les informations actuelles."
  (when (get-buffer daily-journal-chrono-buffer)
    (with-current-buffer daily-journal-chrono-buffer
      (let* ((inhibit-read-only t)
             (current-time-min (daily-journal-get-current-time-minutes))
             (current-task (daily-journal-find-current-task daily-journal-current-tasks current-time-min))
             (idle-times (daily-journal-detect-idle-times daily-journal-current-tasks)))
        (erase-buffer)
        
        ;; Tâche actuelle
        (insert "🎯 TÂCHE ACTUELLE\n")
        (if current-task
            (let* ((remaining (- (plist-get current-task :end-min) current-time-min))
                   (remaining-str (if (> remaining 0)
                                    (format "%dh%02dmin" (/ remaining 60) (% remaining 60))
                                    "DÉPASSÉ")))
              (insert (format "► %s\n" (plist-get current-task :name)))
              (insert (format "⏰ %s → %s (%s)\n\n" 
                            (plist-get current-task :start)
                            (plist-get current-task :end)
                            remaining-str)))
          (insert "Aucune tâche en cours\n\n"))
        
        ;; Liste des tâches
        (insert "📋 PLANNING JOURNÉE\n")
        (if daily-journal-current-tasks
            (dolist (task daily-journal-current-tasks)
              (let* ((task-is-current (daily-journal-find-current-task (list task) current-time-min))
                     (status (cond ((plist-get task :completed) "✅")
                                  (task-is-current "🔄")
                                  ((< current-time-min (plist-get task :start-min)) "⏳")
                                  (t "⏸️")))
                     (name (plist-get task :name))
                     (time-range (format "%s-%s" (plist-get task :start) (plist-get task :end))))
                (insert (format "%s %s %s\n" status name time-range))))
          (insert "Aucune tâche trouvée\n"))
        
        ;; Temps morts
        (when idle-times
          (insert "\n⚡ TEMPS MORTS DÉTECTÉS\n")
          (dolist (idle idle-times)
            (when (> (plist-get idle :duration) 10) ; Afficher si > 10min
              (insert (format "• %s-%s (%dmin)\n"
                            (plist-get idle :start)
                            (plist-get idle :end)
                            (plist-get idle :duration))))))))))

;;;###autoload
(defun daily-journal-start-chrono ()
  "Lance le chrono pour le planning du jour."
  (interactive)
  (daily-journal-load-current-planning)
  (unless (get-buffer daily-journal-chrono-buffer)
    (with-current-buffer (get-buffer-create daily-journal-chrono-buffer)
      (setq buffer-read-only t)))
  (daily-journal-update-chrono-buffer)
  (display-buffer daily-journal-chrono-buffer)
  (when daily-journal-chrono-timer
    (cancel-timer daily-journal-chrono-timer))
  (setq daily-journal-chrono-timer
        (run-with-timer 0 60 (lambda () 
                                (daily-journal-load-current-planning)
                                (daily-journal-update-chrono-buffer))))
  (message "📊 Chrono planning démarré - Mise à jour toutes les minutes"))

;;;###autoload
(defun daily-journal-stop-chrono ()
  "Arrête le chrono."
  (interactive)
  (when daily-journal-chrono-timer
    (cancel-timer daily-journal-chrono-timer)
    (setq daily-journal-chrono-timer nil))
  (when (get-buffer daily-journal-chrono-buffer)
    (kill-buffer daily-journal-chrono-buffer))
  (message "📊 Chrono arrêté"))

;; --------------------------------------------------
;; Création rapide de tâches avec complétion
;; --------------------------------------------------

(defvar daily-journal-duration-options '("15min" "30min" "45min" "1h" "1h30" "2h")
  "Options de durée pour la complétion.")

(defun daily-journal-get-agenda-dates ()
  "Retourne une liste des dates basée sur org-agenda (aujourd'hui + 7 jours)."
  (let ((dates '()))
    (dotimes (i 8)
      (let* ((date (time-add (current-time) (* i 86400)))
             (date-str (format-time-string "%Y-%m-%d" date))
             (day-name (format-time-string "%A" date))
             (display (if (= i 0) 
                        (format "%s (aujourd'hui)" date-str)
                        (format "%s (%s)" date-str day-name))))
        (push (cons display date-str) dates)))
    (reverse dates)))

(defun daily-journal-extract-task-name-from-line ()
  "Extrait le nom d'une tâche depuis la ligne courante si elle contient - ☐."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[[:space:]]*- ☐ \\(.*?\\)\\(?:[[:space:]]*-.*\\)?$")
      (match-string-no-properties 1))))

(defun daily-journal-insert-task-with-completion ()
  "Insère une tâche avec complétion step-by-step ou transforme une tâche existante."
  (interactive)
  (let* ((existing-task (daily-journal-extract-task-name-from-line))
         (task-name (if existing-task
                        existing-task
                        (read-string "Nom de la tâche: ")))
         (duration (completing-read "Durée: " daily-journal-duration-options))
         (date-options (daily-journal-get-agenda-dates))
         (date-choice (completing-read "Date: " date-options))
         (date-value (cdr (assoc date-choice date-options))))
    
    (when (and task-name duration date-value)
      (if existing-task
          ;; Transformer la ligne existante
          (progn
            (beginning-of-line)
            (kill-line)
            (insert (format "- ☐ %s - %s - %s" task-name duration date-value))
            (message "✓ Tâche transformée: %s" task-name))
        ;; Insérer nouvelle tâche
        (progn
          (insert (format "- ☐ %s - %s - %s" task-name duration date-value))
          (message "✓ Tâche ajoutée: %s" task-name))))))

;; --------------------------------------------------
;; Planning Global - Collecteur et Buffer éditeur
;; --------------------------------------------------

;; YYYY-MM-DD
(defconst daily-journal--date-re
  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regexp pour une date au format ISO (YYYY-MM-DD).")

(defvar daily-journal-global-planning-buffer "*Planning Global*"
  "Nom du buffer pour le planning global.")

(defvar daily-journal-global-sort-mode 'date
  "Mode de tri pour le planning global: 'date, 'status, 'duration.")

(defvar daily-journal-global-tasks nil
  "Liste des tâches collectées avec métadonnées.")

(defun daily-journal-get-search-directory ()
  "Retourne le répertoire de base pour la recherche des tâches."
  (or (and (boundp 'my-notes-dir) my-notes-dir)
      "~/00_PKM/"))

(defun daily-journal-scan-all-tasks ()
  "Scanne tous les fichiers .org pour collecter les tâches planifiées."
  (let* ((search-dir (expand-file-name (daily-journal-get-search-directory)))
         (tasks '())
         (file-count 0)
         (task-count 0))
    
    (message "🔍 Recherche dans: %s" search-dir)
    
    ;; Approche native Emacs : parcourir tous les fichiers .org
    (dolist (file (directory-files-recursively search-dir "\\.org$"))
      (setq file-count (1+ file-count))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((line-num 0))
            (while (not (eobp))
              (setq line-num (1+ line-num))
              (let ((line-content (buffer-substring-no-properties 
                                  (line-beginning-position) 
                                  (line-end-position))))
                
                ;; Chercher les tâches avec format complet
                (when (string-match
                       (concat "- ☐ \\(.*?\\) - \\([0-9]+\\(?:h\\|min\\|h[0-9]+\\)\\) - \\("
                               daily-journal--date-re "\\)")
                       line-content)
                  (let* ((task-name (match-string 1 line-content))
                         (duration (string-trim (match-string 2 line-content)))
                         (date (match-string 3 line-content))
                         (relative-file (file-relative-name file search-dir)))
                    
                    (setq task-count (1+ task-count))
                    (message "✅ Tâche trouvée: [%s:%d] %s - %s - %s" 
                            relative-file line-num task-name duration date)
                    
                    (push (list :file file
                               :relative-file relative-file
                               :line-num line-num
                               :name task-name
                               :duration duration
                               :date date
                               :content line-content)
                          tasks))))
              (forward-line 1))))))
    
    (message "🔍 Fichiers scannés: %d, Tâches trouvées: %d" file-count task-count)
    (setq daily-journal-global-tasks (reverse tasks))
    (message "✓ %d tâches collectées" (length tasks))
    tasks))

(defun daily-journal-format-task-for-display (task)
  "Formate une tâche pour l'affichage dans le buffer."
  (format "[%s:%d] ☐ %s - %s - %s"
          (plist-get task :relative-file)
          (plist-get task :line-num)
          (plist-get task :name)
          (plist-get task :duration)
          (plist-get task :date)))

(defun daily-journal-update-global-planning-buffer ()
  "Met à jour le contenu du buffer planning global."
  (when (get-buffer daily-journal-global-planning-buffer)
    (with-current-buffer daily-journal-global-planning-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Header
        (insert "📋 TOUTES LES TÂCHES PLANIFIÉES\n")
        (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
        
        ;; Grouper par date
        (let ((tasks-by-date (make-hash-table :test 'equal)))
          (dolist (task daily-journal-global-tasks)
            (let ((date (plist-get task :date)))
              (push task (gethash date tasks-by-date))))
          
          ;; Afficher par date croissante
          (dolist (date (sort (hash-table-keys tasks-by-date) #'string<))
            (let ((tasks (reverse (gethash date tasks-by-date))))
              (insert (format "📅 %s\n" date))
              (dolist (task tasks)
                (insert (format "  %s\n" (daily-journal-format-task-for-display task))))
              (insert "\n"))))
        
        ;; Instructions
        (insert "💡 Instructions:\n")
        (insert "  • Éditez directement les tâches (nom, durée, date)\n")
        (insert "  • C-c C-s : Sauvegarder les modifications\n")
        (insert "  • C-c C-r : Actualiser depuis les fichiers\n")))))

(defun daily-journal-parse-edited-task-line (line)
  "Parse une ligne éditée du buffer pour extraire les informations - version robuste."
  (condition-case err
      (when (and line (stringp line))
        ;; Support des différents symboles de tâche : ☐ ☑ ❍ - capture le symbole
        (when (string-match
               (concat "\\[\\(.*?\\):\\([0-9]+\\)\\] \\([☐☑❍]\\) \\(.*?\\) - \\([^ -]+\\) - \\("
                       daily-journal--date-re "\\)")
               line)
          (let ((relative-file (match-string 1 line))
                (line-num-str (match-string 2 line))
                (checkbox-symbol (match-string 3 line))
                (name (match-string 4 line))
                (duration (match-string 5 line))
                (date (match-string 6 line)))
            ;; Validation des données parsées
            (when (and relative-file line-num-str checkbox-symbol name duration date
                      (> (string-to-number line-num-str) 0)
                      (not (string-empty-p (string-trim name)))
                      (not (string-empty-p (string-trim duration))))
              (list :relative-file relative-file
                    :line-num (string-to-number line-num-str)
                    :checkbox-symbol checkbox-symbol
                    :name (string-trim name)
                    :duration (string-trim duration)
                    :date (string-trim date))))))
    (error 
     (message "❌ Erreur parsing ligne: %s - %s" line (error-message-string err))
     nil)))

(defun daily-journal-update-task-in-file (file-path line-num new-content)
  "Met à jour une ligne spécifique dans un fichier avec robustesse."
  (condition-case err
      (when (and file-path line-num new-content (file-exists-p file-path))
        ;; Backup du fichier avant modification
        (let ((backup-file (concat file-path ".backup-" (format-time-string "%Y%m%d%H%M%S"))))
          (copy-file file-path backup-file t)
          (with-temp-buffer
            (insert-file-contents file-path)
            (goto-char (point-min))
            (when (>= line-num 1)
              (forward-line (1- line-num))
              (beginning-of-line)
              (when (not (eobp))
                ;; Vérifier que la ligne contient bien une tâche avant modification
                (let ((current-line (buffer-substring-no-properties 
                                   (line-beginning-position) 
                                   (line-end-position))))
                  (if (string-match "^- [☐☑❍]" current-line)
                      (progn
                        (kill-line)
                        (insert new-content)
                        (write-region (point-min) (point-max) file-path)
                        ;; Supprimer le backup si tout va bien
                        (when (file-exists-p backup-file)
                          (delete-file backup-file))
                        (message "🔄 Ligne %d mise à jour: %s" line-num new-content)
                        t)
                    (progn
                      (message "⚠️ Ligne %d ne contient pas de tâche valide: %s" line-num current-line)
                      (when (file-exists-p backup-file)
                        (delete-file backup-file))
                      nil))))))))
    (error 
     (message "❌ Erreur synchronisation %s:%d - %s" file-path line-num (error-message-string err))
     nil)))

(defun daily-journal-sync-task-change (original-task edited-line)
  "Synchronise les modifications d'une tâche vers le fichier source - version robuste."
  (condition-case err
      (let* ((parsed (daily-journal-parse-edited-task-line edited-line))
             (file-path (plist-get original-task :file))
             (line-num (plist-get original-task :line-num)))
        
        (if (and parsed file-path line-num
                 (file-exists-p file-path)
                 (> line-num 0))
            (let ((new-content (format "- %s %s - %s - %s"
                                      (plist-get parsed :checkbox-symbol)
                                      (plist-get parsed :name)
                                      (plist-get parsed :duration)
                                      (plist-get parsed :date))))
              (if (daily-journal-update-task-in-file file-path line-num new-content)
                  (progn
                    (message "✓ Tâche synchronisée dans %s:%d" 
                             (plist-get original-task :relative-file) line-num)
                    t)
                (progn
                  (message "❌ Échec sync %s:%d" 
                           (plist-get original-task :relative-file) line-num)
                  nil)))
          (progn
            (message "❌ Données invalides pour sync: parsed=%s file=%s line=%s" 
                     (if parsed "OK" "NIL") 
                     (if file-path "OK" "NIL") 
                     line-num)
            nil)))
    (error 
     (message "❌ Erreur sync tâche: %s" (error-message-string err))
     nil)))

(define-derived-mode daily-journal-global-planning-mode fundamental-mode "Global Planning"
  "Mode pour éditer le planning global des tâches."
  (setq buffer-read-only nil)
  (local-set-key (kbd "C-c C-s") 'daily-journal-save-global-planning)
  (local-set-key (kbd "C-c C-r") 'daily-journal-refresh-global-planning)
  (local-set-key (kbd "C-c C-t") 'daily-journal-cycle-sort-mode))

(defun daily-journal-save-global-planning ()
  "Sauvegarde les modifications du planning global. Séquence: sync puis C-x s t."
  (interactive)
  ;; Déclencher une sync après sauvegarde avec fonction lambda correcte
  (let ((sync-fn (lambda () 
                   (remove-hook 'post-command-hook sync-fn)
                   (my-planning-sync-request))))
    (add-hook 'post-command-hook sync-fn nil t))
  (let ((changes 0)
        (errors 0)
        (modified-files '()))
    (condition-case err
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              ;; Détecter les lignes éditables avec tous les symboles: ☐ ☑ ❍
              (when (string-match "^  \\[[^]]+:\\([0-9]+\\)\\] [☐☑❍]" line)
                (message "🔍 Ligne détectée pour sync: %s" line)
                (condition-case sync-err
                    ;; Trouver la tâche originale correspondante
                    (let ((original-task (cl-find-if 
                                         (lambda (task) 
                                           (and task
                                                (plist-get task :relative-file)
                                                (plist-get task :line-num)
                                                (string-match (format "\\[%s:%d\\]" 
                                                                      (regexp-quote (plist-get task :relative-file))
                                                                      (plist-get task :line-num)) 
                                                              line)))
                                         daily-journal-global-tasks)))
                      (if original-task
                          (if (daily-journal-sync-task-change original-task line)
                              (progn
                                (setq changes (1+ changes))
                                ;; Ajouter le fichier à la liste des fichiers modifiés
                                (let ((file-path (plist-get original-task :file)))
                                  (unless (member file-path modified-files)
                                    (push file-path modified-files))))
                            (setq errors (1+ errors)))
                        (message "⚠️ Tâche originale non trouvée pour: %s" line)))
                  (error 
                   (setq errors (1+ errors))
                   (message "❌ Erreur sync ligne %d: %s" (line-number-at-pos) (error-message-string sync-err))))))
            (forward-line 1)))
      (error 
       (message "❌ Erreur globale synchronisation: %s" (error-message-string err))))
    ;; Recharger les buffers des fichiers modifiés
    (when (> changes 0)
      (daily-journal-reload-modified-buffers modified-files))
    
    ;; 3. Sauvegarder tous les buffers après sync (C-x s t)
    (save-some-buffers t)
    
    (if (> errors 0)
        (message "🔄 %d tâches synchronisées, %d erreurs, buffers sauvegardés" changes errors)
      (message "🔄 %d tâches synchronisées, buffers sauvegardés" changes))))

(defun daily-journal-reload-modified-buffers (modified-files)
  "Recharge intelligemment les buffers des fichiers modifiés."
  (when modified-files
    (let ((reloaded-count 0))
      (dolist (file-path modified-files)
        (let ((buffer (find-buffer-visiting file-path)))
          (when buffer
            (condition-case err
                (with-current-buffer buffer
                  (cond
                   ;; Si le buffer n'a pas de modifications non sauvées
                   ((not (buffer-modified-p))
                    (revert-buffer t t t)
                    (setq reloaded-count (1+ reloaded-count))
                    (message "🔄 Buffer rechargé: %s" (buffer-name)))
                   ;; Si le buffer a des modifications, demander confirmation
                   ((y-or-n-p (format "Le buffer %s a des modifications. Recharger quand même ? " (buffer-name)))
                    (revert-buffer t t t)
                    (setq reloaded-count (1+ reloaded-count))
                    (message "🔄 Buffer rechargé (modifications perdues): %s" (buffer-name)))
                   ;; Sinon, ignorer
                   (t
                    (message "⚠️ Buffer non rechargé (modifications présentes): %s" (buffer-name)))))
              (error
               (message "❌ Erreur rechargement %s: %s" (buffer-name) (error-message-string err)))))))
      (when (> reloaded-count 0)
        (message "✅ %d buffer%s rechargé%s" reloaded-count 
                 (if (> reloaded-count 1) "s" "")
                 (if (> reloaded-count 1) "s" ""))))))

;; Système de synchronisation unifié
;; --------------------------------------------------
(defun my-planning-sync-request (&optional force)
  "Demande une synchronisation avec debounce. Si FORCE, sync immédiate."
  (cond
   ;; Sync forcée immédiate
   (force
    (my-planning-sync-execute))
   
   ;; Debounce: annuler timer précédent et en créer un nouveau
   (t
    (when my-planning-sync-timer
      (cancel-timer my-planning-sync-timer))
    (setq my-planning-sync-timer
          (run-with-timer 2.0 nil #'my-planning-sync-execute)))))

(defun my-planning-sync-execute ()
  "Exécute la synchronisation avec protection mutex."
  (when my-planning-sync-timer
    (cancel-timer my-planning-sync-timer)
    (setq my-planning-sync-timer nil))
  
  ;; Protection mutex
  (when my-planning-sync-lock
    (message "⏳ Synchronisation déjà en cours...")
    (return))
  
  (setq my-planning-sync-lock t)
  (unwind-protect
      (progn
        (message "🔄 Synchronisation planning...")
        
        ;; Vérifier si des fichiers ont changé
        (when (my-planning-files-changed-p)
          (daily-journal-scan-all-tasks)
          (my-planning-update-files-cache))
        
        ;; Mettre à jour le buffer si ouvert
        (when (get-buffer daily-journal-global-planning-buffer)
          (daily-journal-update-global-planning-buffer))
        
        (setq my-planning-last-sync-time (float-time))
        (message "✅ Synchronisation terminée"))
    
    ;; Libérer le mutex dans tous les cas
    (setq my-planning-sync-lock nil)))

(defun my-planning-files-changed-p ()
  "Vérifie si des fichiers .org ont été modifiés depuis le dernier scan."
  (let* ((search-dir (expand-file-name (daily-journal-get-search-directory)))
         (files (directory-files-recursively search-dir "\\.org$"))
         (changed nil))
    
    (dolist (file files)
      (when (file-readable-p file)
        (let* ((file-time (float-time (nth 5 (file-attributes file))))
               (cached-time (gethash file my-planning-files-cache 0)))
          (when (> file-time cached-time)
            (setq changed t)
            (puthash file file-time my-planning-files-cache)))))
    
    changed))

(defun my-planning-update-files-cache ()
  "Met à jour le cache des timestamps des fichiers."
  (let* ((search-dir (expand-file-name (daily-journal-get-search-directory)))
         (files (directory-files-recursively search-dir "\\.org$")))
    
    (dolist (file files)
      (when (file-readable-p file)
        (let ((file-time (float-time (nth 5 (file-attributes file)))))
          (puthash file file-time my-planning-files-cache))))))

(defun daily-journal-refresh-global-planning ()
  "Actualise le planning global depuis les fichiers. Séquence: C-x s t puis refresh."
  (interactive)
  ;; 1. Sauvegarder tous les buffers d'abord (C-x s t)
  (save-some-buffers t)
  ;; 2. Puis actualiser le planning (ancien C-c C-r)
  (my-planning-sync-request t))

;;;###autoload
(defun daily-journal-open-global-planning ()
  "Ouvre le buffer de planning global avec toutes les tâches."
  (interactive)
  (daily-journal-scan-all-tasks)
  (let ((buffer (get-buffer-create daily-journal-global-planning-buffer)))
    (with-current-buffer buffer
      (daily-journal-global-planning-mode)
      (daily-journal-update-global-planning-buffer))
    (switch-to-buffer buffer)
    (message "📋 Planning global ouvert - %d tâches trouvées" (length daily-journal-global-tasks))))

;; --------------------------------------------------
;; Hooks pour synchronisation automatique
;; --------------------------------------------------
(defun my-planning-org-after-save-hook ()
  "Hook après sauvegarde des fichiers .org pour sync automatique."
  (when (and (eq major-mode 'org-mode)
             (get-buffer daily-journal-global-planning-buffer))
    (my-planning-sync-request)))

;; Ajouter le hook pour tous les fichiers .org
(add-hook 'after-save-hook #'my-planning-org-after-save-hook)

;; Hook de nettoyage
(add-hook 'kill-emacs-hook 
          (lambda ()
            (when my-planning-sync-timer
              (cancel-timer my-planning-sync-timer))))

;; --------------------------------------------------
;; Configuration et raccourcis
;; --------------------------------------------------
(global-set-key (kbd "<f3>") #'daily-journal-open-today)
(global-set-key (kbd "C-c j u") #'daily-journal-update-end-times)
(global-set-key (kbd "C-c j c") #'daily-journal-start-chrono)
(global-set-key (kbd "C-c j x") #'daily-journal-stop-chrono)
(global-set-key (kbd "C-c j g") #'daily-journal-open-global-planning)
(global-set-key (kbd "C-u") #'daily-journal-insert-task-with-completion)

(message "✅ Version complète fonctionnelle - Buffer chrono avec détection des tâches")
(provide 'daily-journal)
;;; daily-journal.el ends here