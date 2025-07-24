;;; org-ui-enhancements.el --- Apparence et workflow pour Org Mode avec Smart Bullet Lists -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Org Appearance avec org-superstar uniquement
;; --------------------------------------------------
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Configuration des bullets
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿"))
  (setq org-superstar-remove-leading-stars t)
  (setq org-hide-leading-stars nil)
  (setq org-superstar-leading-bullet ?\s)
  
  ;; Désactiver la stylisation des listes (gérée par smart-bullet)
  (setq org-superstar-prettify-item-bullets nil)
  
  ;; Assurer que org-indent n'interfère pas
  (setq org-indent-mode-turns-on-hiding-stars nil))

;; --------------------------------------------------
;; org-appear : masquage intelligent des marqueurs d'emphase
;; --------------------------------------------------
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  ;; Configuration org-appear
  (setq org-appear-autoemphasis t      ; Auto-toggle emphasis markers
        org-appear-autolinks t         ; Auto-toggle links
        org-appear-autosubmarkers t)   ; Auto-toggle sub/superscripts
  
  ;; Intégration avec la fonction existante toggle-org-wysiwym
  (defun my/org-appear-setup ()
    "Setup org-appear avec intégration toggle global."
    (when (bound-and-true-p org-appear-mode)
      ;; org-appear fonctionne mieux avec les marqueurs cachés par défaut
      (setq-local org-hide-emphasis-markers t)
      (font-lock-flush)
      ;; Activé silencieusement
      nil)))
  
  (add-hook 'org-appear-mode-hook #'my/org-appear-setup)
  
  ;; Fonction pour basculer entre org-appear et affichage global
  (defun my/toggle-emphasis-display-mode ()
    "Bascule entre org-appear (intelligent) et affichage global des marqueurs."
    (interactive)
    (if (bound-and-true-p org-appear-mode)
        (progn
          (org-appear-mode -1)
          (setq-local org-hide-emphasis-markers nil)
          (font-lock-flush)
          ;; Désactivé silencieusement
          nil)
      (progn
        (org-appear-mode 1)
        (setq-local org-hide-emphasis-markers t)
        (font-lock-flush)
        ;; Activé silencieusement
        nil)))
  
  ;; Fonction pour debug org-appear
  (defun my/org-appear-info ()
    "Affiche l'état d'org-appear."
    (interactive)
    (message "org-appear: %s | hide-emphasis-markers: %s | curseur sur emphase: %s"
             (if (bound-and-true-p org-appear-mode) "ON" "OFF")
             (if org-hide-emphasis-markers "ON" "OFF")
             (if (org-in-regexp org-emph-re 2) "OUI" "NON")))
  
  ;; Fonction de test pour subscripts/superscripts
  (defun my/test-sub-superscripts ()
    "Test des subscripts/superscripts avec org-appear."
    (interactive)
    (with-current-buffer (get-buffer-create "*Test Sub/Superscripts*")
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Test Subscripts/Superscripts\n\n")
      (insert "* Test des indices et exposants\n\n")
      (insert "** Chimie\n")
      (insert "- Eau : H_{2}O (avec accolades)\n")
      (insert "- Eau : H_2O (sans accolades)\n") 
      (insert "- Dioxyde de carbone : CO_{2}\n")
      (insert "- Ion calcium : Ca^{2+}\n\n")
      (insert "** Mathématiques\n")
      (insert "- Einstein : E = mc^{2}\n")
      (insert "- Einstein : E = mc^2\n")
      (insert "- Puissances : x^{n+1}\n")
      (insert "- Variables : x_{1}, x_{2}\n\n")
      (insert "** Instructions\n")
      (insert "1. Les formules avec {} devraient s'afficher automatiquement\n")
      (insert "2. Bougez votre curseur IN/OUT des formules\n")
      (insert "3. org-appear devrait toggler l'affichage des marqueurs\n\n")
      (insert "État des variables :\n")
      (insert (format "- org-use-sub-superscripts: %s\n" org-use-sub-superscripts))
      (insert (format "- org-pretty-entities: %s\n" org-pretty-entities))
      (insert (format "- org-appear-autosubmarkers: %s\n" 
                      (if (boundp 'org-appear-autosubmarkers) 
                          org-appear-autosubmarkers 
                          "NON DÉFINI")))
      
      (display-buffer (current-buffer))
      (goto-char (point-min))
      (search-forward "H_{2}O" nil t)
      (backward-char 2)
      (message "Test subscripts/superscripts créé. Variables configurées.")
      
      ;; Forcer le refresh de font-lock
      (font-lock-flush)))

;; Fin du bloc use-package org-appear

;; --------------------------------------------------
;; Configuration org-modern avec éléments personnalisés
;; --------------------------------------------------
(use-package org-modern
  :after org
  :custom
  ;; Configuration des tables
  (org-modern-table t)                    ; Activer la beautification des tables
  (org-modern-table-vertical 2)           ; Largeur des lignes verticales en pixels
  (org-modern-table-horizontal 0.1)       ; Beautifier les lignes horizontales
  
  ;; Configuration des timestamps
  (org-modern-timestamp t)                ; Activer la beautification des timestamps
  
  ;; Configuration des blocs
  (org-modern-block-name t)               ; Beautifier les noms de blocs
  (org-modern-block-fringe 2)             ; Bordure des blocs dans la frange
  
  ;; Configuration des tags
  (org-modern-tag t)                      ; Activer la beautification des tags
  (org-modern-label-border 0.1)           ; Largeur des bordures des étiquettes
  
  ;; Configuration des métadonnées
  (org-modern-keyword t)                  ; Beautifier les mots-clés comme #+title
  (org-modern-todo t)                     ; Beautifier les mots-clés TODO
  (org-modern-priority t)                 ; Beautifier les priorités
  
  ;; Configuration des étoiles (compatible avec org-superstar)
  (org-modern-star nil)                   ; Désactiver - géré par org-superstar
  (org-modern-replace-stars nil)          ; Désactiver - géré par org-superstar
  
  ;; Configuration des barres de progression
  (org-modern-progress 4)                 ; Largeur des barres de progression
  
  ;; Faces personnalisées pour les tags
  (org-modern-tag-faces
   '(("urgent" :background "#ff4444" :foreground "white" :weight bold)
     ("important" :background "#ff8800" :foreground "white")
     ("note" :background "#4488ff" :foreground "white")
     ("done" :background "#44aa44" :foreground "white")
     ("project" :background "#8844ff" :foreground "white")
     ("work" :background "#666666" :foreground "white")
     ("personal" :background "#aa44aa" :foreground "white")))
  
  ;; Faces personnalisées pour les TODO
  (org-modern-todo-faces
   '(("TODO" :background "#ff6666" :foreground "white" :weight bold)
     ("NEXT" :background "#ffaa44" :foreground "white" :weight bold)
     ("WAIT" :background "#ffcc44" :foreground "black" :weight bold)
     ("DONE" :background "#44aa44" :foreground "white" :weight bold)
     ("CANCELLED" :background "#888888" :foreground "white")))
  
  ;; Faces personnalisées pour les priorités
  (org-modern-priority-faces
   '(("A" :background "#ff0000" :foreground "white" :weight bold)
     ("B" :background "#ff8800" :foreground "white")
     ("C" :background "#ffcc00" :foreground "black")))
  
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  
  :config
  ;; Paramètres recommandés pour org-modern
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…")
  
  ;; Désactiver org-indent globalement (incompatible avec org-modern)
  (setq org-startup-indented nil)
  (remove-hook 'org-mode-hook #'org-indent-mode)
  
  ;; Adaptation pour la font Windows existante (Cutive Mono)
  ;; Org-modern fonctionne mieux avec des fonts harmonisées
  (when (eq system-type 'windows-nt)
    ;; Configurer variable-pitch pour une meilleure intégration org-modern
    (set-face-attribute 'variable-pitch nil :family "Cutive Mono" :height 108)
    (set-face-attribute 'fixed-pitch nil :family "Cutive Mono" :height 108)
    
    ;; Ajuster line-spacing pour une meilleure apparence avec org-modern
    (setq-default line-spacing 0.2)
    
    ;; Configuration spécifique pour org-modern sur Windows
    (custom-set-faces
     '(org-modern-tag ((t (:inherit (secondary-selection org-modern-label)))))
     '(org-modern-todo ((t (:inherit (org-todo org-modern-label)))))
     '(org-modern-priority ((t (:inherit (org-priority org-modern-label)))))
     '(org-modern-keyword ((t (:inherit (org-meta-line org-modern-label))))))
    
    (message "✓ org-modern adapté pour Cutive Mono sur Windows"))
  
  ;; Message de confirmation
  (message "✓ org-modern configuré avec tables, timestamps, blocs, tags et metadata"))

;; --------------------------------------------------
;; Visual enhancements for headings
;; --------------------------------------------------
(defun my/setup-org-heading-faces ()
  "Configure visual enhancements for org headings."
  (dolist (spec '((org-level-1 . 1.3) (org-level-2 . 1.2) 
                  (org-level-3 . 1.1) (org-level-4 . 1.0) (org-level-5 . 1.0)))
    (set-face-attribute (car spec) nil :weight 'bold :height (cdr spec))))

;; Apply heading faces
(with-eval-after-load 'org
  (my/setup-org-heading-faces))

;; --------------------------------------------------
;; Status indicators with colored Unicode circles
;; --------------------------------------------------
(defvar my-status-overlay-list nil
  "List of overlays for status indicators.")

(defun my-clear-status-overlays ()
  "Clear all existing status overlays."
  (mapc #'delete-overlay my-status-overlay-list)
  (setq my-status-overlay-list nil))

(defun my-org-status-pretty ()
  "Create colored status indicators using Unicode circles."
  (my-clear-status-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\b\\(ok\\|ko\\|bof\\)\\b" nil t)
      (let* ((start (match-beginning 1))
             (end (match-end 1))
             (word (match-string 1))
             (overlay (make-overlay start end))
             (symbol "●")  ; BULLET U+2022
             (color (cond ((string= word "ok") "green3")
                         ((string= word "ko") "red3")
                         ((string= word "bof") "orange3"))))
        (overlay-put overlay 'display 
                     (propertize symbol 
                                'face `(:foreground ,color 
                                       :weight bold)))
        (overlay-put overlay 'priority 100)
        (push overlay my-status-overlay-list)))))

(defun my-refresh-status-overlays ()
  "Refresh status overlays after changes."
  (when (derived-mode-p 'org-mode)
    (my-org-status-pretty)))

;; --------------------------------------------------
;; SMART BULLET LISTS - LOGIQUE COMPLÈTE INTÉGRÉE
;; --------------------------------------------------

;; Variables de configuration
(defvar smart-bullet-patterns
  '(("^\\([ \t]*\\)\\([-+]\\) \\(\\[ \\]\\|\\[x\\]\\|\\[o\\]\\|\\[X\\]\\)?\\s-*$" . bullet-checkbox)
    ("^\\([ \t]*\\)\\([-+]\\)\\s-+\\S-.*$" . bullet-item)
    ("^\\([ \t]*\\)\\([0-9]+[.)]\\)\\s-+\\S-.*$" . numbered-item))
  "Patterns pour détecter les différents types de listes.")

(defvar smart-bullet-indent-width 2
  "Nombre d'espaces pour l'indentation des listes.")

;; Faces pour checkboxes
(defface checkbox-unchecked-face
  '((t :foreground "gray50"))
  "Face for unchecked checkbox symbol ☐")

(defface checkbox-checked-face
  '((t :foreground "green3" :weight bold))
  "Face for checked checkbox symbol ☑")

(defface checkbox-partial-face
  '((t :foreground "orange3"))
  "Face for partial checkbox symbol ❍")

;; Fonctions de détection
(defun smart-bullet-current-line-info ()
  "Analyse la ligne courante et retourne les informations de liste.
Retourne (TYPE INDENT BULLET CHECKBOX CONTENT) ou nil."
  (save-excursion
    (beginning-of-line)
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (cond
       ;; Ligne vide avec puce vide (fin de liste potentielle)
       ((string-match "^\\([ \t]*\\)\\([-+]\\)\\s-*$" line)
        (list 'bullet-empty 
              (length (match-string 1 line))
              (match-string 2 line)
              nil
              ""))
       
       ;; Puce avec checkbox
       ((string-match "^\\([ \t]*\\)\\([-+]\\)\\s-+\\(\\[ \\]\\|\\[x\\]\\|\\[o\\]\\|\\[X\\]\\)\\s-*\\(.*\\)$" line)
        (list 'bullet-checkbox
              (length (match-string 1 line))
              (match-string 2 line)
              (match-string 3 line)
              (match-string 4 line)))
       
       ;; Puce simple avec contenu
       ((string-match "^\\([ \t]*\\)\\([-+]\\)\\s-+\\(.*\\)$" line)
        (list 'bullet-item
              (length (match-string 1 line))
              (match-string 2 line)
              nil
              (match-string 3 line)))
       
       ;; Liste numérotée
       ((string-match "^\\([ \t]*\\)\\([0-9]+[.)]\\)\\s-+\\(.*\\)$" line)
        (list 'numbered-item
              (length (match-string 1 line))
              (match-string 2 line)
              nil
              (match-string 3 line)))
       
       ;; Pas une liste
       (t nil)))))

(defun smart-bullet-find-previous-list-item ()
  "Trouve l'item de liste sur la ligne précédente directe."
  (save-excursion
    (forward-line -1)
    (unless (bobp)
      (smart-bullet-current-line-info))))

(defun smart-bullet-should-handle-return ()
  "Vérifie si smart-bullet doit gérer RET."
  (or (smart-bullet-current-line-info)
      (smart-bullet-find-previous-list-item)))

;; Fonction principale : gestion de Entrée
(defun smart-bullet-handle-return ()
  "Gère l'appui sur Entrée dans les listes à puces."
  (interactive)
  (let ((current-info (smart-bullet-current-line-info)))
    (cond
     ;; Cas 1: Ligne avec puce vide -> sortir de la liste
     ((and current-info (eq (car current-info) 'bullet-empty))
      (beginning-of-line)
      (delete-region (line-beginning-position) (line-end-position))
      (newline))
     
     ;; Cas 2: Dans une liste -> créer nouvelle puce
     (current-info
      (let* ((type (nth 0 current-info))
             (indent (nth 1 current-info))
             (bullet (nth 2 current-info))
             (checkbox (nth 3 current-info)))
        (end-of-line)
        (newline)
        (insert (make-string indent ?\s))
        (cond
         ((eq type 'bullet-checkbox)
          (insert (format "%s [ ] " bullet)))
         ((eq type 'bullet-item)
          (insert (format "%s " bullet)))
         ((eq type 'numbered-item)
          (let* ((num-str (nth 2 current-info))
                 (num (string-to-number num-str))
                 (suffix (if (string-match "[.)]$" num-str)
                            (match-string 0 num-str) ".")))
            (insert (format "%d%s " (1+ num) suffix)))))))
     
     ;; Cas 3: Pas dans une liste -> vérifier ligne précédente
     (t
      (let ((prev-info (smart-bullet-find-previous-list-item)))
        (if prev-info
            (let* ((type (nth 0 prev-info))
                   (indent (nth 1 prev-info))
                   (bullet (nth 2 prev-info)))
              (newline)
              (insert (make-string indent ?\s))
              (cond
               ((memq type '(bullet-checkbox bullet-item))
                (insert (format "%s " bullet)))
               ((eq type 'numbered-item)
                (insert "- "))))
          (newline)))))))

;; Fonctions d'indentation
(defun smart-bullet-indent ()
  "Indente l'item de liste courant."
  (interactive)
  (let ((info (smart-bullet-current-line-info)))
    (if info
        (let* ((current-indent (nth 1 info))
               (new-indent (+ current-indent smart-bullet-indent-width)))
          (beginning-of-line)
          (insert (make-string smart-bullet-indent-width ?\s)))
      (indent-for-tab-command))))

(defun smart-bullet-unindent ()
  "Désindente l'item de liste courant."
  (interactive)
  (let ((info (smart-bullet-current-line-info)))
    (when info
      (let* ((current-indent (nth 1 info))
             (spaces-to-remove (min smart-bullet-indent-width current-indent)))
        (when (> spaces-to-remove 0)
          (beginning-of-line)
          (delete-char spaces-to-remove))))))

;; Fonction de création de puce
(defun smart-bullet-create-bullet ()
  "Crée une nouvelle puce au début de la ligne courante."
  (interactive)
  (beginning-of-line)
  (insert "- ")
  (when (eolp)
    (insert " ")))

;; Cycle des checkboxes - VERSION AMÉLIORÉE
(defun cycle-checkbox-symbol ()
  "Cycle checkbox symbol: - ☐ → - ☑ → - ❍ → - (nothing) → - ☐
  Works with indented list items, file references, and preserves indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ;; File reference patterns - [file:line] ☐/☑/❍ text
     ((looking-at "^\\([ \t]*\\)\\(\\[.*\\][ \t]*\\)☐ ") (replace-match "\\1\\2☑ "))
     ((looking-at "^\\([ \t]*\\)\\(\\[.*\\][ \t]*\\)☑ ") (replace-match "\\1\\2❍ "))
     ((looking-at "^\\([ \t]*\\)\\(\\[.*\\][ \t]*\\)❍ ") (replace-match "\\1\\2"))
     ;; Indented or non-indented patterns - preserve indentation
     ((looking-at "^\\([ \t]*\\)- ☑ ") (replace-match "\\1- ❍ "))
     ((looking-at "^\\([ \t]*\\)- ❍ ") (replace-match "\\1- "))
     ((looking-at "^\\([ \t]*\\)- ☐ ") (replace-match "\\1- ☑ "))
     ((looking-at "^\\([ \t]*\\)☑ ") (replace-match "\\1- ☑ "))
     ((looking-at "^\\([ \t]*\\)❍ ") (replace-match "\\1- ❍ "))
     ((looking-at "^\\([ \t]*\\)☐ ") (replace-match "\\1- ☐ "))
     ((looking-at "^\\([ \t]*\\)- ") (replace-match "\\1- ☐ "))
     ;; File reference without checkbox - add checkbox
     ((looking-at "^\\([ \t]*\\)\\(\\[.*\\][ \t]*\\)\\([^☐☑❍]\\)")
      (replace-match "\\1\\2☐ \\3"))
     ;; No match - insert at beginning after any indentation
     (t (if (looking-at "^\\([ \t]*\\)")
            (progn
              (goto-char (match-end 1))
              (insert "- ☐ "))
          (insert "- ☐ "))))))

(defun smart-bullet-create-checkbox ()
  "Convertit la ligne courante en checkbox ou en crée une nouvelle."
  (interactive)
  (let ((info (smart-bullet-current-line-info)))
    (cond
     ;; Déjà une checkbox
     ((and info (eq (car info) 'bullet-checkbox))
      (message "Déjà une checkbox"))
     
     ;; Puce simple -> convertir en checkbox
     ((and info (eq (car info) 'bullet-item))
      (save-excursion
        (beginning-of-line)
        (when (re-search-forward "\\([-+]\\)\\s-+" (line-end-position) t)
          (replace-match "\\1 [ ] "))))
     
     ;; Ligne normale -> créer puce avec checkbox
     (t
      (beginning-of-line)
      (insert "- [ ] ")))))

;; Fonction debug
(defun smart-bullet-list-info ()
  "Affiche des informations sur la liste courante (debug)."
  (interactive)
  (let ((info (smart-bullet-current-line-info)))
    (if info
        (message "Type: %s | Indent: %d | Bullet: %s | Checkbox: %s | Content: %s"
                 (nth 0 info) (nth 1 info) (nth 2 info) 
                 (or (nth 3 info) "none") (nth 4 info))
      (message "Pas dans une liste"))))

;; Mode mineur pour les listes intelligentes
(defvar smart-bullet-mode-map
  (let ((map (make-sparse-keymap)))
;    (define-key map (kbd "TAB") #'smart-bullet-indent)
;    (define-key map (kbd "C-TAB") #'smart-bullet-unindent)
;    (define-key map (kbd "S-TAB") #'smart-bullet-unindent)
;    (define-key map (kbd "C-c l b") #'smart-bullet-create-bullet)
;    (define-key map (kbd "C-c l RET") #'smart-bullet-handle-return)
    map)
  "Keymap pour smart-bullet-mode.")

;;;###autoload
(define-minor-mode smart-bullet-mode
  "Mode mineur pour la gestion intelligente des listes à puces."
  :lighter " •"
  :keymap smart-bullet-mode-map
  :group 'smart-bullet
  (if smart-bullet-mode
      ;; Message silencieux - activé uniquement une fois
      nil
    ;; Message silencieux - désactivé
    nil))

;; --------------------------------------------------
;; Système de callouts personnalisés
;; --------------------------------------------------
;; Faces pour les callouts
(defface callout-note-face
  '((t :background "#e3f2fd" :foreground "#1976d2" 
       :extend t))
  "Face pour les callouts de type note (bleu).")

(defface callout-warning-face
  '((t :background "#fff3e0" :foreground "#f57c00" 
       :extend t))
  "Face pour les callouts d'avertissement (orange).")

(defface callout-tip-face
  '((t :background "#e8f5e8" :foreground "#388e3c" 
       :extend t))
  "Face pour les callouts de conseil (vert).")

(defface callout-important-face
  '((t :background "#fce4ec" :foreground "#c2185b" 
       :extend t))
  "Face pour les callouts importants (rose).")

(defface callout-danger-face
  '((t :background "#ffebee" :foreground "#d32f2f" 
       :extend t))
  "Face pour les callouts de danger (rouge).")

(defface callout-diary-face
  '((t :background "#080808" :foreground "#ffffff" 
       :extend t))
  "Face pour les callouts de journal personnel (noir avec texte blanc).")

;; Variables pour les overlays de callouts (locales au buffer)
(defvar-local callout-overlay-list nil
  "Liste des overlays pour les callouts (locale au buffer).")

(defvar-local callout-folded-positions nil
  "Liste des positions des callouts repliés pour maintenir l'état (locale au buffer).")

(defun callout-clear-overlays ()
  "Supprime tous les overlays de callouts."
  ;; Sauvegarder les positions des callouts repliés (uniquement les overlays de fold)
  (setq callout-folded-positions nil)
  (dolist (overlay callout-overlay-list)
    (when (and (overlay-get overlay 'callout-fold)
               (overlay-get overlay 'invisible))
      (push (overlay-start overlay) callout-folded-positions)))
  ;; Supprimer tous les overlays (background et fold)
  (mapc #'delete-overlay callout-overlay-list)
  (setq callout-overlay-list nil))

(defun callout-is-position-folded (pos)
  "Vérifie si la position POS était précédemment repliée."
  (cl-some (lambda (folded-pos)
             (<= (abs (- pos folded-pos)) 10))  ; Tolérance de 10 caractères
           callout-folded-positions))

(defun callout-find-block-end (start-pos)
  "Trouve la fin d'un bloc callout à partir de START-POS."
  (save-excursion
    (goto-char start-pos)
    (forward-line 1)
    (let ((end-pos (point)))
      (while (and (not (eobp))
                  (not (looking-at "^\\s-*$"))  ; ligne vide
                  (not (looking-at "^\\*\\*\\[\\(NOTE\\|WARNING\\|TIP\\|IMPORTANT\\|DANGER\\|DIARY\\)\\]\\*\\*"))  ; nouveau callout
                  (not (looking-at "^\\*+ ")))  ; nouveau heading
        (forward-line 1)
        (setq end-pos (point)))
      end-pos)))

(defun callout-create-overlay (type start end)
  "Crée un callout de TYPE entre START et END avec overlay à basse priorité."
  (let* ((overlay (make-overlay start end))
         (face (cond ((string= type "NOTE") 'callout-note-face)
                    ((string= type "WARNING") 'callout-warning-face)
                    ((string= type "TIP") 'callout-tip-face)
                    ((string= type "IMPORTANT") 'callout-important-face)
                    ((string= type "DANGER") 'callout-danger-face)
                    ((string= type "DIARY") 'callout-diary-face)
                    (t 'callout-note-face)))
         (was-folded (callout-is-position-folded start)))
    
    ;; Utiliser overlay avec priorité très basse pour ne pas masquer la sélection
    (overlay-put overlay 'face face)
    (overlay-put overlay 'callout-type type)
    (overlay-put overlay 'priority -10)  ; Priorité très basse pour ne pas masquer la sélection
    
    ;; Gérer le repliage avec un overlay séparé si nécessaire
    (when was-folded
      (let ((fold-overlay (make-overlay start end)))
        (overlay-put fold-overlay 'invisible 'callout)
        (overlay-put fold-overlay 'display 
                   (concat (buffer-substring start 
                                           (save-excursion 
                                             (goto-char start)
                                             (line-end-position))) 
                          " [...]"))
        (overlay-put fold-overlay 'callout-fold t)
        (overlay-put fold-overlay 'priority 100)  ; Priorité haute pour le fold
        (push fold-overlay callout-overlay-list)))
    
    (push overlay callout-overlay-list)
    overlay))

(defun setup-callout-overlays ()
  "Configure les overlays pour les callouts multi-lignes."
  (callout-clear-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*\\*\\[\\(NOTE\\|WARNING\\|TIP\\|IMPORTANT\\|DANGER\\|DIARY\\)\\]\\*\\*\\)\\(.*\\)$" nil t)
      (let* ((start (match-beginning 0))
             (type (match-string 2))
             (end (callout-find-block-end start)))
        (callout-create-overlay type start end)))))

(defun callout-refresh-overlays ()
  "Refresh les overlays avec gestion des délais pour éviter les appels multiples."
  (when (derived-mode-p 'org-mode)
    (setup-callout-overlays)))

(defun callout-restore-on-buffer-switch ()
  "Restaure les callouts quand on revient à un buffer."
  (when (and (derived-mode-p 'org-mode)
             (not callout-overlay-list))  ; Seulement si pas d'overlays
    (setup-callout-overlays)))

(defun setup-callout-font-lock ()
  "Configuration des callouts personnalisés avec support multi-lignes."
  ;; Setup initial des overlays
  (setup-callout-overlays)
  ;; Refresh automatique après modifications avec délai plus long (local au buffer)
  (add-hook 'after-change-functions 
            (lambda (&rest _) 
              (run-with-idle-timer 0.5 nil #'callout-refresh-overlays)) 
            nil t)
)

;; Configuration des checkboxes
(defun setup-checkbox-font-lock ()
  "Add font-lock keywords for checkboxes."
  (font-lock-add-keywords
   nil
   '(("- \\(☐\\) " 1 'checkbox-unchecked-face t)
     ("- \\(☑\\) " 1 'checkbox-checked-face t)
     ("- \\(❍\\) " 1 'checkbox-partial-face t))))

;; Configuration d'intégration avec org-mode
(defun smart-bullet-setup-org-integration ()
  "Configure l'intégration avec org-mode."
  (when (derived-mode-p 'org-mode)
    ;; Configuration spéciale pour org-mode : RET conditionnel
    (local-set-key (kbd "RET") 
                   (lambda ()
                     (interactive)
                     (if (smart-bullet-should-handle-return)
                         (smart-bullet-handle-return)
                       (org-return))))))

;; --------------------------------------------------
;; UI and workflow hooks setup - UNIFIÉ
;; --------------------------------------------------
(defun my/setup-ui-workflow-hooks ()
  "Setup hooks for UI and workflow features."
  (add-hook 'org-mode-hook #'setup-checkbox-font-lock)
  (add-hook 'org-mode-hook #'setup-callout-font-lock)
  ;; Hook global pour restaurer les callouts lors des changements de buffer
  (add-hook 'window-state-change-hook 
            (lambda ()
              (when (derived-mode-p 'org-mode)
                (callout-restore-on-buffer-switch))))  ; AJOUT: callouts
  (add-hook 'org-mode-hook #'my-org-status-pretty 90)
  (add-hook 'org-mode-hook 
            (lambda () 
              (my-org-status-pretty)
              (add-hook 'after-change-functions 
                        (lambda (&rest _) 
                          (run-with-idle-timer 0.1 nil #'my-refresh-status-overlays)) 
                        nil t)))
  
  ;; AJOUTER: Activation smart-bullet dans les modes appropriés
  (add-hook 'org-mode-hook 
            (lambda ()
              (smart-bullet-mode 1)
              (smart-bullet-setup-org-integration)))
  (add-hook 'markdown-mode-hook 
            (lambda () (smart-bullet-mode 1)))
  (add-hook 'text-mode-hook 
            (lambda () (smart-bullet-mode 1))))

;; Setup hooks when loading
(with-eval-after-load 'org
  (my/setup-ui-workflow-hooks))

;; --------------------------------------------------
;; Fonctions utilitaires
;; --------------------------------------------------
(defun my/debug-org-status ()
  "Affiche le statut d'org-superstar pour debug."
  (interactive)
  (message "Org-superstar: %s | Smart-bullet: %s | Org-indent: %s | Major-mode: %s"
           (if (bound-and-true-p org-superstar-mode) "ON" "OFF")
           (if (bound-and-true-p smart-bullet-mode) "ON" "OFF")
           (if (bound-and-true-p org-indent-mode) "ON" "OFF")
           major-mode))

(defun my/force-org-superstar-restart ()
  "Force le redémarrage d'org-superstar dans le buffer actuel."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-superstar-mode -1)
    (org-superstar-mode 1)
    (font-lock-flush)
    (message "✓ Org-superstar redémarré")))

(defun my/clean-org-modes ()
  "Nettoie tous les modes org conflictuels et active seulement org-superstar + smart-bullet."
  (interactive)
  (when (derived-mode-p 'org-mode)
    ;; Désactiver les modes conflictuels
    (when (bound-and-true-p org-indent-mode)
      (org-indent-mode -1))
    (when (bound-and-true-p org-modern-mode)
      (org-modern-mode -1))
    
    ;; Réactiver les bons modes
    (unless (bound-and-true-p org-superstar-mode)
      (org-superstar-mode 1))
    (unless (bound-and-true-p smart-bullet-mode)
      (smart-bullet-mode 1))
    
    (font-lock-flush)
    (message "✓ Modes org nettoyés - org-superstar + smart-bullet actifs")))

;; --------------------------------------------------
;; Key bindings - COMPLET
;; --------------------------------------------------
;; Raccourci principal unifié
(global-set-key (kbd "C-l") #'cycle-checkbox-symbol)

;; Raccourcis utilitaires
(global-set-key (kbd "C-c u d") #'my/debug-org-status)
(global-set-key (kbd "C-c u r") #'my/force-org-superstar-restart)
(global-set-key (kbd "C-c u c") #'my/clean-org-modes)

;; Raccourcis org-appear (extension de votre système de toggle)
(global-set-key (kbd "C-c w") #'my/toggle-emphasis-display-mode)  ; Remplace l'ancien toggle
(global-set-key (kbd "C-c W") #'my/org-appear-info)  ; Debug org-appear
(global-set-key (kbd "C-c u s") #'my/test-sub-superscripts)  ; Test subscripts/superscripts

;; Raccourcis smart-bullet
(global-set-key (kbd "C-c l i") #'smart-bullet-list-info)
(global-set-key (kbd "C-c l c") #'smart-bullet-create-checkbox)
(global-set-key (kbd "C-c l b") #'smart-bullet-create-bullet)

;; --------------------------------------------------
;; Fonctions utilitaires pour callouts
;; --------------------------------------------------
(defun insert-callout (type title)
  "Insert un callout avec le TYPE et TITLE spécifiés."
  (interactive
   (list
    (completing-read "Type de callout: " 
                     '("NOTE" "WARNING" "TIP" "IMPORTANT" "DANGER" "DIARY"))
    (read-string "Titre du callout: ")))
  (insert (format "**[%s]** %s\n" type title))
  (insert "Contenu du callout ici...\n")
  (insert "\n")  ; Ligne vide pour délimiter la fin
  (forward-line -2)  ; Retour au contenu pour édition
  (end-of-line))

(defun insert-diary-callout ()
  "Insert un callout DIARY avec timestamp automatique."
  (interactive)
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format "**[DIARY]** %s\n" timestamp))
    (insert "Entrée de journal personnel...\n")
    (insert "\n")  ; Ligne vide pour délimiter la fin
    (forward-line -2)  ; Retour au contenu pour édition
    (end-of-line)))

(defun callout-toggle-fold ()
  "Replie/déplie le callout courant."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\*\\*\\[\\(NOTE\\|WARNING\\|TIP\\|IMPORTANT\\|DANGER\\|DIARY\\)\\]\\*\\*")
      (let* ((start (line-beginning-position))
             (end (callout-find-block-end start))
             (fold-overlay (cl-find-if 
                           (lambda (ov) 
                             (and (>= (overlay-start ov) start)
                                  (<= (overlay-end ov) end)
                                  (overlay-get ov 'callout-fold)))
                           callout-overlay-list)))
        (if fold-overlay
            (if (overlay-get fold-overlay 'invisible)
                ;; Déplier
                (progn
                  (delete-overlay fold-overlay)
                  (setq callout-overlay-list (delq fold-overlay callout-overlay-list))
                  (message "Callout déplié"))
              ;; Shouldn't happen, but handle gracefully
              (message "Callout déjà déplié"))
          ;; Replier - créer un overlay de fold
          (let ((new-overlay (make-overlay start end)))
            (overlay-put new-overlay 'invisible 'callout)
            (overlay-put new-overlay 'display 
                       (concat (buffer-substring start (line-end-position)) " [...]"))
            (overlay-put new-overlay 'callout-fold t)
            (overlay-put new-overlay 'priority 100)  ; Priorité haute pour le fold
            (push new-overlay callout-overlay-list)
            (message "Callout replié")))))))

(defun callout-fold-all ()
  "Replie tous les callouts du buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\*\\[\\(NOTE\\|WARNING\\|TIP\\|IMPORTANT\\|DANGER\\|DIARY\\)\\]\\*\\*" nil t)
      (let* ((start (line-beginning-position))
             (end (callout-find-block-end start))
             (existing-fold (cl-find-if 
                            (lambda (ov) 
                              (and (>= (overlay-start ov) start)
                                   (<= (overlay-end ov) end)
                                   (overlay-get ov 'callout-fold)))
                            callout-overlay-list)))
        (unless existing-fold
          (let ((fold-overlay (make-overlay start end)))
            (overlay-put fold-overlay 'invisible 'callout)
            (overlay-put fold-overlay 'display 
                       (concat (buffer-substring start (line-end-position)) " [...]"))
            (overlay-put fold-overlay 'callout-fold t)
            (overlay-put fold-overlay 'priority 100)  ; Priorité haute pour le fold
            (push fold-overlay callout-overlay-list))))))
  (message "Tous les callouts repliés"))

(defun callout-unfold-all ()
  "Déplie tous les callouts du buffer."
  (interactive)
  (dolist (overlay callout-overlay-list)
    (when (overlay-get overlay 'callout-fold)
      (delete-overlay overlay)))
  (setq callout-overlay-list 
        (cl-remove-if (lambda (ov) (overlay-get ov 'callout-fold)) callout-overlay-list))
  (message "Tous les callouts dépliés"))

;; Raccourcis pour callouts (utilise C-c o pour callOut)
(global-set-key (kbd "C-c o n") (lambda () (interactive) (insert-callout "NOTE" "")))
(global-set-key (kbd "C-c o w") (lambda () (interactive) (insert-callout "WARNING" "")))
(global-set-key (kbd "C-c o t") (lambda () (interactive) (insert-callout "TIP" "")))
(global-set-key (kbd "C-c o i") (lambda () (interactive) (insert-callout "IMPORTANT" "")))
(global-set-key (kbd "C-c o g") (lambda () (interactive) (insert-callout "DANGER" "")))  ; g pour danGer
(global-set-key (kbd "C-c o d") #'insert-diary-callout)  ; d pour diary avec timestamp
(global-set-key (kbd "C-c o o") #'insert-callout)

;; Raccourcis pour le repliage
(global-set-key (kbd "C-c o f") #'callout-toggle-fold)
(global-set-key (kbd "C-c o F") #'callout-fold-all)
(global-set-key (kbd "C-c o u") #'callout-unfold-all)

;; Fonction pour débugger l'état des callouts
(defun callout-debug-state ()
  "Affiche l'état des callouts pour debug."
  (interactive)
  (let ((fold-overlays (cl-count-if (lambda (ov) (overlay-get ov 'callout-fold)) callout-overlay-list))
        (text-props-count 0))
    ;; Compter les callouts par regex (plus simple et sûr)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\*\\[\\(NOTE\\|WARNING\\|TIP\\|IMPORTANT\\|DANGER\\|DIARY\\)\\]\\*\\*" nil t)
        (setq text-props-count (1+ text-props-count))))
    (message "Buffer: %s | Fold overlays: %d | Text properties: %d | Positions repliées: %s" 
             (buffer-name)
             fold-overlays
             text-props-count
             callout-folded-positions)))

;; Fonction pour forcer la restauration des callouts
(defun callout-force-restore ()
  "Force la restauration des callouts dans le buffer courant."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (callout-clear-overlays)
    (setup-callout-overlays)
    (message "Callouts restaurés dans %s" (buffer-name))))

(global-set-key (kbd "C-c o D") #'callout-debug-state)
(global-set-key (kbd "C-c o R") #'callout-force-restore)

;; Fonction pour tester la visibilité de la sélection dans les callouts
(defun callout-test-selection ()
  "Teste la visibilité de la sélection de texte dans les callouts."
  (interactive)
  (message "Sélectionnez du texte dans un callout pour tester la visibilité"))

;; --------------------------------------------------
;; Message de confirmation
;; --------------------------------------------------
(message "✓ org-ui-enhancements chargé (org-superstar + smart-bullet + callouts)")
(message "  ⭐ Org-superstar pour les titres | Smart-bullet pour les listes | Callouts personnalisés")
(message "  📝 C-l : Cycle checkbox (☐→☑→❍→-) | TAB/S-TAB : Indent/Unindent dans listes")
(message "  🔧 C-c l RET : Smart return | C-c l b : Nouvelle puce | C-c l i : Debug liste")
(message "  📢 C-c o [n/w/t/i/g] : Callouts | C-c o d : DIARY avec timestamp | C-c o f : Toggle fold | C-c o F/u : Fold/Unfold all")
(message "  🛠️ C-c u d : Debug modes | C-c u c : Clean modes")

(provide 'org-ui-enhancements)
;;; org-ui-enhancements.el ends here