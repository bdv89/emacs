;;; text-replacement.el --- Module de remplacement de texte avec Cape -*- lexical-binding: t; -*-

;; Author: Votre nom
;; Version: 1.2
;; Package-Requires: ((emacs "28.1") (cape "0.1"))
;; Keywords: completion, text, replacement

;;; Commentary:
;; Ce module fournit des fonctions de remplacement de texte intelligentes
;; qui s'intègrent avec Cape et le système de complétion.
;; 
;; Fonctionnalités :
;; - Remplacement de séquences par des symboles Unicode
;; - Expansion d'abréviations
;; - Mode automatique ou manuel
;; - Interface de gestion des remplacements
;;
;; Usage :
;; (require 'text-replacement)
;; (text-replacement-setup)

;;; Code:

(require 'cape)

;; --------------------------------------------------
;; Variables de configuration
;; --------------------------------------------------
(defgroup text-replacement nil
  "Configuration du système de remplacement de texte."
  :group 'completion
  :prefix "text-replacement-")

(defcustom text-replacement-data-file
  (expand-file-name "text-replacements.el" user-emacs-directory)
  "Fichier pour sauvegarder les remplacements personnalisés."
  :type 'file
  :group 'text-replacement)

(defcustom text-replacement-auto-save t
  "Si non-nil, sauvegarde automatiquement les changements."
  :type 'boolean
  :group 'text-replacement)

(defvar text-replacement-default-mappings
  '(;; Symboles mathématiques et flèches
    ("->" . "→")    ("<-" . "←")    ("<=>" . "⇔")
    ("=>" . "⇒")     ("<==" . "⇐")    ("!=" . "≠")
    (">=" . "≥")     ("<=" . "≤")     ("+-" . "±")
    ("infinis" . "∞")    ("..." . "…") 
    
    ;; Lettres grecques
    ("alpha" . "α")   ("beta" . "β")    ("gamma" . "γ")
    ("delta" . "δ")   ("epsilon" . "ε")  ("lambda" . "λ")
    ("_mu_" . "μ")      ("_pi_" . "π")      ("sigma" . "σ")
    ("theta" . "θ")   ("omega" . "ω")   ("_phi_" . "φ")
    
    ;; Opérateurs mathématiques
    ("_sum_" . "∑")     ("_prod_" . "∏")    ("_int_" . "∫")
    ("sqrt" . "√")    ("cbrt" . "∛")    ("times" . "×")
    ("_div_" . "÷")     ("_pm_" . "±")      ("_mp_" . "∓")
    
    ;; Fractions
    ("1/2" . "½")     ("1/4" . "¼")     ("3/4" . "¾")
    ("1/3" . "⅓")     ("2/3" . "⅔")     ("1/8" . "⅛")
    
    ;; Symboles légaux/commerciaux
    ("(c)" . "©")     ("(r)" . "®")     ("(tm)" . "™")
    ("(p)" . "℗")     ("sect" . "§")    ("para" . "¶")
    
    ;; Abréviations techniques courantes
    ("h1" . "*")     ("h2" . "**")     ("h3" . "***")
    ("h4" . "****")     ("h5" . "*****")     ("h6" . "******")
    ("h6" . "*******")     ("h7" . "********")     ("h8" . "*********")



    ;; Caractères spéciaux français
    ("_oe_" . "œ")      ("_OE_" . "Œ")      ("_ae_" . "æ")
    ("_AE_" . "Æ")      ("_c,_" . "ç")      ("_C,_" . "Ç"))
    

    
    ;; Abréviations générales

  "Mappings de remplacement par défaut.")

(defvar text-replacement-mappings nil
  "Mappings de remplacement actuels (par défaut + personnalisés).")

(defvar text-replacement-auto-mode t
  "Si non-nil, les remplacements sont effectués automatiquement.")

;; --------------------------------------------------
;; Fonctions de gestion des données
;; --------------------------------------------------
(defun text-replacement-load-mappings ()
  "Charge les mappings depuis le fichier et combine avec les défauts."
  (setq text-replacement-mappings (copy-alist text-replacement-default-mappings))
  
  (when (file-exists-p text-replacement-data-file)
    (condition-case err
        (let ((custom-mappings (with-temp-buffer
                                (insert-file-contents text-replacement-data-file)
                                (read (current-buffer)))))
          (when (listp custom-mappings)
            ;; Fusionner les mappings (les custom prennent priorité)
            (dolist (mapping custom-mappings)
              (setf (alist-get (car mapping) text-replacement-mappings 
                              nil nil #'string=) (cdr mapping)))))
      (error (message "Erreur lors du chargement des remplacements: %s" 
                     (error-message-string err))))))

(defun text-replacement-save-mappings ()
  "Sauvegarde les mappings personnalisés."
  (when text-replacement-auto-save
    (condition-case err
        (let ((custom-mappings 
               ;; Extraire seulement les mappings non-défaut
               (seq-filter (lambda (mapping)
                            (not (member mapping text-replacement-default-mappings)))
                          text-replacement-mappings)))
          (with-temp-file text-replacement-data-file
            (prin1 custom-mappings (current-buffer))))
      (error (message "Erreur lors de la sauvegarde des remplacements: %s" 
                     (error-message-string err))))))

;; --------------------------------------------------
;; Fonctions Cape pour les remplacements
;; --------------------------------------------------
(defun text-replacement-cape-exact ()
  "Cape function pour les remplacements exacts de mots complets."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              (start (car bounds))
              (end (cdr bounds))
              (text (buffer-substring-no-properties start end))
              (replacement (alist-get text text-replacement-mappings nil nil #'string=)))
    (list start end 
          (list replacement)
          :annotation-function (lambda (_) " [replace]")
          :exclusive t)))

(defun text-replacement-cape-symbols ()
  "Cape function pour les remplacements basés sur des séquences de symboles."
  (save-excursion
    ;; Chercher le début d'une séquence de symboles
    (let ((end (point))
          (start (progn
                   ;; Se déplacer vers l'arrière tant qu'on trouve des caractères de symboles
                   (while (and (> (point) (point-min))
                              (memq (char-before) '(?- ?= ?< ?> ?+ ?* ?/ ?% ?& ?| ?!)))
                     (backward-char))
                   (point))))
      (when (and (< start end)
                 ;; S'assurer qu'on n'est pas dans un mot normal
                 (not (and (> start (point-min))
                          (or (alphanumericp (char-before start))
                              (eq (char-before start) ?_)))))
        (let* ((text (buffer-substring-no-properties start end))
               (replacement (alist-get text text-replacement-mappings nil nil #'string=)))
          (when replacement
            (list start end 
                  (list replacement)
                  :annotation-function (lambda (_) " [symbol]")
                  :exclusive t)))))))

(defun text-replacement-cape-partial ()
  "Cape function pour les remplacements basés sur préfixes."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              (start (car bounds))
              (end (cdr bounds))
              (prefix (buffer-substring-no-properties start end)))
    (when (>= (length prefix) 2)
      (let ((candidates '()))
        (dolist (mapping text-replacement-mappings)
          (when (string-prefix-p prefix (car mapping) t)
            (push (cdr mapping) candidates)))
        (when candidates
          (list start end (delete-dups candidates)
                :annotation-function (lambda (_) " [expand]")
                :exclusive 'no))))))

(defun text-replacement-cape-partial-symbols ()
  "Cape function pour les remplacements partiels de séquences de symboles."
  (save-excursion
    ;; Chercher le début d'une séquence de symboles
    (let ((end (point))
          (start (progn
                   ;; Se déplacer vers l'arrière tant qu'on trouve des caractères de symboles
                   (while (and (> (point) (point-min))
                              (memq (char-before) '(?- ?= ?< ?> ?+ ?* ?/ ?% ?& ?| ?!)))
                     (backward-char))
                   (point))))
      (when (and (< start end) 
                 (>= (- end start) 1)
                 ;; S'assurer qu'on n'est pas dans un mot normal
                 (not (and (> start (point-min))
                          (or (alphanumericp (char-before start))
                              (eq (char-before start) ?_)))))
        (let* ((prefix (buffer-substring-no-properties start end))
               (candidates '()))
          (dolist (mapping text-replacement-mappings)
            (when (string-prefix-p prefix (car mapping) t)
              (push (cdr mapping) candidates)))
          (when candidates
            (list start end (delete-dups candidates)
                  :annotation-function (lambda (_) " [symbol expand]")
                  :exclusive 'no)))))))

;; --------------------------------------------------
;; Mode automatique
;; --------------------------------------------------
(defun text-replacement-auto-replace-at-point ()
  "Effectue un remplacement automatique si une séquence est détectée."
  (when (and text-replacement-auto-mode
             (not (minibufferp)))
    
    (let ((replacement-done nil))
      
      ;; Essayer d'abord un remplacement de mot/symbole complet
      (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                  (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
                  (replacement (alist-get text text-replacement-mappings nil nil #'string=)))
        (delete-region (car bounds) (cdr bounds))
        (insert replacement)
        (setq replacement-done t))
      
      ;; Si aucun remplacement de mot, essayer une séquence de symboles
      (unless replacement-done
        (save-excursion
          (let ((end (point))
                (start (progn
                         ;; Se déplacer vers l'arrière pour trouver le début des symboles
                         (while (and (> (point) (point-min))
                                    (memq (char-before) '(?- ?= ?< ?> ?+ ?* ?/ ?% ?& ?| ?!)))
                           (backward-char))
                         (point))))
            
            (when (< start end)
              (let* ((text (buffer-substring-no-properties start end))
                     (replacement (alist-get text text-replacement-mappings nil nil #'string=)))
                (when replacement
                  (delete-region start end)
                  (insert replacement))))))))))

;; --------------------------------------------------
;; Interface utilisateur
;; --------------------------------------------------
(defun text-replacement-add (sequence replacement)
  "Ajoute un nouveau remplacement."
  (interactive "sSequence à remplacer: \nsRemplacement: ")
  (when (string-empty-p sequence)
    (user-error "La séquence ne peut pas être vide"))
  (when (string-empty-p replacement)
    (user-error "Le remplacement ne peut pas être vide"))
  
  (setf (alist-get sequence text-replacement-mappings nil nil #'string=) replacement)
  (text-replacement-save-mappings)
  (message "✓ Remplacement ajouté: %s → %s" sequence replacement))

(defun text-replacement-remove (sequence)
  "Supprime un remplacement."
  (interactive 
   (list (completing-read "Sequence à supprimer: " 
                          (mapcar #'car text-replacement-mappings)
                          nil t)))
  (setq text-replacement-mappings
        (assoc-delete-all sequence text-replacement-mappings #'string=))
  (text-replacement-save-mappings)
  (message "✓ Remplacement supprimé: %s" sequence))

(defun text-replacement-list ()
  "Affiche tous les remplacements disponibles."
  (interactive)
  (with-current-buffer (get-buffer-create "*Text Replacements*")
    (erase-buffer)
    (insert "=== REMPLACEMENTS DE TEXTE DISPONIBLES ===\n\n")
    
    ;; Séparer défaut et personnalisés
    (let ((default-mappings '())
          (custom-mappings '()))
      (dolist (mapping text-replacement-mappings)
        (if (member mapping text-replacement-default-mappings)
            (push mapping default-mappings)
          (push mapping custom-mappings)))
      
      ;; Afficher les mappings par défaut
      (when default-mappings
        (insert "=== MAPPINGS PAR DÉFAUT ===\n")
        (insert (format "%-20s → %s\n" "SEQUENCE" "REMPLACEMENT"))
        (insert (make-string 50 ?-) "\n")
        (dolist (mapping (sort default-mappings 
                              (lambda (a b) (string< (car a) (car b)))))
          (insert (format "%-20s → %s\n" (car mapping) (cdr mapping))))
        (insert "\n"))
      
      ;; Afficher les mappings personnalisés
      (when custom-mappings
        (insert "=== MAPPINGS PERSONNALISÉS ===\n")
        (insert (format "%-20s → %s\n" "SEQUENCE" "REMPLACEMENT"))
        (insert (make-string 50 ?-) "\n")
        (dolist (mapping (sort custom-mappings 
                              (lambda (a b) (string< (car a) (car b)))))
          (insert (format "%-20s → %s\n" (car mapping) (cdr mapping))))
        (insert "\n")))
    
    (insert (format "Total: %d remplacements (%d défaut + %d personnalisés)\n" 
                    (length text-replacement-mappings)
                    (length text-replacement-default-mappings)
                    (- (length text-replacement-mappings) 
                       (length text-replacement-default-mappings))))
    
    (insert "\nRaccourcis:\n")
    (insert "  C-c r a  : Ajouter un remplacement\n")
    (insert "  C-c r d  : Supprimer un remplacement\n")
    (insert "  C-c r t  : Toggle mode automatique\n")
    (insert "  C-c r r  : Reset aux valeurs par défaut\n")
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun text-replacement-toggle-auto ()
  "Bascule le mode de remplacement automatique."
  (interactive)
  (setq text-replacement-auto-mode (not text-replacement-auto-mode))
  (if text-replacement-auto-mode
      (progn
        (add-hook 'post-self-insert-hook #'text-replacement-auto-replace-at-point)
        (message "✓ Mode remplacement automatique activé"))
    (progn
      (remove-hook 'post-self-insert-hook #'text-replacement-auto-replace-at-point)
      (message "✗ Mode remplacement automatique désactivé"))))

(defun text-replacement-reset ()
  "Remet les mappings aux valeurs par défaut."
  (interactive)
  (when (yes-or-no-p "Remettre tous les remplacements aux valeurs par défaut ? ")
    (setq text-replacement-mappings (copy-alist text-replacement-default-mappings))
    (text-replacement-save-mappings)
    (message "✓ Remplacements remis aux valeurs par défaut")))

;; --------------------------------------------------
;; Fonction de debug
;; --------------------------------------------------
(defun text-replacement-debug-at-point ()
  "Affiche des informations de debug sur ce qui est détecté au point."
  (interactive)
  (let ((word-bounds (bounds-of-thing-at-point 'symbol))
        (symbol-bounds (save-excursion
                         (let ((end (point))
                               (start (progn
                                        (while (and (> (point) (point-min))
                                                   (memq (char-before) '(?- ?= ?< ?> ?+ ?* ?/ ?% ?& ?| ?!)))
                                          (backward-char))
                                        (point))))
                           (if (< start end) (cons start end) nil)))))
    
    (message "Debug - Point: %d | Word: %s | Symbols: %s | Char before: %s"
             (point)
             (if word-bounds 
                 (buffer-substring-no-properties (car word-bounds) (cdr word-bounds))
               "none")
             (if symbol-bounds
                 (buffer-substring-no-properties (car symbol-bounds) (cdr symbol-bounds))
               "none")
             (if (> (point) (point-min))
                 (char-to-string (char-before))
               "none"))))

;; --------------------------------------------------
;; Setup et intégration
;; --------------------------------------------------
(defun text-replacement-setup ()
  "Configure le système de remplacement de texte."
  (interactive)
  
  ;; Charger les mappings
  (text-replacement-load-mappings)
  
  ;; Ajouter aux fonctions de complétion - ORDRE IMPORTANT
  (add-to-list 'completion-at-point-functions #'text-replacement-cape-exact)
  (add-to-list 'completion-at-point-functions #'text-replacement-cape-symbols)
  (add-to-list 'completion-at-point-functions #'text-replacement-cape-partial)
  (add-to-list 'completion-at-point-functions #'text-replacement-cape-partial-symbols)
  
  ;; Activer le mode automatique par défaut
  (when text-replacement-auto-mode
    (add-hook 'post-self-insert-hook #'text-replacement-auto-replace-at-point))
  
  ;; Définir les raccourcis
  (global-set-key (kbd "C-c r a") #'text-replacement-add)
  (global-set-key (kbd "C-c r d") #'text-replacement-remove)
  (global-set-key (kbd "C-c r l") #'text-replacement-list)
  (global-set-key (kbd "C-c r t") #'text-replacement-toggle-auto)

  
  (message "✓ Text replacement system configured (auto-mode: %s)" 
           (if text-replacement-auto-mode "ON" "OFF"))
  (message "  📝 Add: C-c r a | 📋 List: C-c r l | ⚡ Toggle auto: C-c r t | 🐛 Debug: C-c r ?"))

;; --------------------------------------------------
;; Intégration avec completion.el existant
;; --------------------------------------------------
(defun text-replacement-integrate-with-completion ()
  "Intègre avec le système de complétion existant."
  (when (fboundp 'my/dictionary-completion-at-point)
    ;; Créer une fonction combinée qui donne priorité aux remplacements
    (defun my/enhanced-completion-at-point ()
      "Complétion combinée : remplacements + dictionnaires."
      (or (text-replacement-cape-symbols)
          (text-replacement-cape-exact)
          (text-replacement-cape-partial-symbols)
          (text-replacement-cape-partial)
          (my/dictionary-completion-at-point)))
    
    ;; Remplacer dans la liste des fonctions de complétion
    (setq completion-at-point-functions
          (cons #'my/enhanced-completion-at-point
                (delq #'my/dictionary-completion-at-point 
                      completion-at-point-functions)))))

;; --------------------------------------------------
;; Auto-setup si cape est disponible
;; --------------------------------------------------
(with-eval-after-load 'cape
  (text-replacement-setup)
  (when (fboundp 'my/dictionary-completion-at-point)
    (text-replacement-integrate-with-completion)))

(provide 'text-replacement)
;;; text-replacement.el ends here