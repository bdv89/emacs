;;; completion.el --- Advanced completion configuration - ORG MODE ONLY -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Dictionnaire et sources de complétion
;; --------------------------------------------------
(defvar my/dictionary-dir
  (expand-file-name "dictionaries" (concat (getenv "HOME") "/00_PKM/"))
  "Répertoire pour stocker les dictionnaires de complétion.")

(defun my/ensure-dictionary-dir ()
  "Crée le répertoire des dictionnaires s'il n'existe pas."
  (unless (file-directory-p my/dictionary-dir)
    (make-directory my/dictionary-dir t)))

(defun my/load-dictionary (filename)
  "Charge un dictionnaire depuis un fichier.
Retourne une liste de mots ou nil si le fichier n'existe pas."
  (my/ensure-dictionary-dir)
  (let ((filepath (expand-file-name filename my/dictionary-dir)))
    (when (file-readable-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (split-string (buffer-string) "[\n\r]+" t)))))

(defvar my/programming-dict nil "Dictionnaire de mots pour la programmation.")
(defvar my/french-dict nil      "Dictionnaire français.")
(defvar my/english-dict nil     "Dictionnaire anglais.")

(defun my/reload-dictionaries ()
  "Recharge tous les dictionnaires depuis les fichiers.
Fichiers attendus : programming-dict.txt, french-dict.txt, english-dict.txt."
  (interactive)
  (setq my/programming-dict (my/load-dictionary "programming-dict.txt")
        my/french-dict      (my/load-dictionary "french-dict.txt")
        my/english-dict     (my/load-dictionary "english-dict.txt"))
  (message "Dictionnaires rechargés : %d prog, %d fr, %d en"
           (length my/programming-dict) (length my/french-dict) (length my/english-dict)))

;; --------------------------------------------------
;; Fonction de complétion personnalisée intelligente
;; --------------------------------------------------
(defun my/dictionary-completion-at-point ()
  "Propose la complétion en utilisant mes dictionnaires personnels avec scoring intelligent."
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start  (car bounds))
         (end    (cdr bounds))
         (prefix (when bounds
                   (buffer-substring-no-properties start end))))
    (when (and prefix (>= (length prefix) 1))
      (let ((candidates
             (delete-dups
              (seq-concatenate 'list
                               (seq-filter (lambda (w)
                                             (string-prefix-p prefix w t)) my/programming-dict)
                               (seq-filter (lambda (w)
                                             (string-prefix-p prefix w t)) my/french-dict)
                               (seq-filter (lambda (w)
                                             (string-prefix-p prefix w t)) my/english-dict)))))
        (when candidates
          (list start end candidates
                :annotation-function (lambda (cand) " [dict]")
                :company-docsig #'identity))))))

;; --------------------------------------------------
;; Prescient : Intelligence et apprentissage
;; --------------------------------------------------
(use-package prescient
  :ensure t
  :init
  ;; Configuration avant le chargement du package
  (setq prescient-save-file (expand-file-name "prescient-save.el" user-emacs-directory))
  (setq prescient-history-length 1000)
  (setq prescient-frequency-decay 0.997)
  (setq prescient-frequency-threshold 0.05)
  :config
  (prescient-persist-mode 1))

(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :config
  (corfu-prescient-mode 1)
  ;; Configuration spécifique pour optimiser l'apprentissage
  (setq corfu-prescient-enable-filtering t)
  (setq corfu-prescient-enable-sorting t))

;; --------------------------------------------------
;; Orderless avec optimisations Prescient
;; --------------------------------------------------
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(prescient orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (setq orderless-matching-styles
        '(orderless-prefixes
          orderless-initialism
          orderless-regexp
          orderless-flex)))

;; --------------------------------------------------
;; Corfu - CONFIGURATION RESTRICTIVE (ORG SEULEMENT)
;; --------------------------------------------------
(use-package corfu
  :ensure t
  :custom
  ;; DÉSACTIVER l'auto-complétion globale
  (corfu-auto nil)  ; ← CHANGEMENT PRINCIPAL : nil au lieu de t
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt)
  (corfu-preview-current 'insert)
  (corfu-count 15)
  ;; Keybindings centralisés dans keybindings.el
  :init
  ;; NE PAS activer le mode global
  ;; (global-corfu-mode)  ; ← LIGNE DÉSACTIVÉE
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; --------------------------------------------------
;; Cape avec priorité intelligente
;; --------------------------------------------------
(use-package cape
  :ensure t
  :config
  ;; Configuration pour améliorer la pertinence
  (setq cape-dabbrev-min-length 2)
  (setq cape-dabbrev-check-other-buffers 'some))

;; --------------------------------------------------
;; Marginalia avec plus d'informations
;; --------------------------------------------------
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  ;; Afficher plus d'infos pour comprendre la pertinence
  (setq marginalia-align 'right))

;; --------------------------------------------------
;; NOUVELLE fonction pour activer uniquement dans Org
;; --------------------------------------------------
(defun my/enable-corfu-org-only ()
  "Active Corfu avec auto-complétion uniquement dans org-mode."
  (when (derived-mode-p 'org-mode)
    (corfu-mode 1)
    ;; Activer l'auto-complétion SEULEMENT pour org-mode
    (setq-local corfu-auto t)
    (setq-local corfu-auto-prefix 1)  ; Réactif pour Org
    (setq-local corfu-auto-delay 0.1)
    ;; Configuration des fonctions de complétion pour Org
    (setq-local completion-at-point-functions
                (list #'my/dictionary-completion-at-point
                      #'cape-dabbrev
                      #'cape-file))
    
    
    ;; NOUVEAU: Advice pour Corfu pour préserver le dimming
    (when (bound-and-true-p auto-dim-other-buffers-mode)
      (defun my/corfu-advice-preserve-dimming (orig-fun &rest args)
        "Advice pour préserver le dimming durant les opérations Corfu."
        (let ((result (apply orig-fun args)))
          (run-with-idle-timer 0.1 nil 
                               (lambda ()
                                 (when (and (buffer-live-p (current-buffer))
                                            (fboundp 'auto-dim-other-buffers--refresh-buffer))
                                   (auto-dim-other-buffers--refresh-buffer (current-buffer)))))
          result))
      
      ;; Appliquer l'advice aux fonctions clés de Corfu
      (advice-add 'corfu--exhibit :around #'my/corfu-advice-preserve-dimming)
      (advice-add 'corfu--teardown :around #'my/corfu-advice-preserve-dimming))))

;; --------------------------------------------------
;; FONCTION pour activer manuellement dans d'autres modes si besoin
;; --------------------------------------------------
(defun my/enable-manual-completion ()
  "Active la complétion manuelle (sans auto) dans le buffer actuel."
  (interactive)
  (corfu-mode 1)
  (setq-local corfu-auto nil)  ; Pas d'auto-complétion
  ;; Configurer les fonctions de complétion selon le mode
  (cond
   ((derived-mode-p 'prog-mode)
    (setq-local completion-at-point-functions
                (list #'my/dictionary-completion-at-point
                      #'cape-keyword
                      #'cape-dabbrev
                      #'cape-file)))
   (t
    (setq-local completion-at-point-functions
                (list #'my/dictionary-completion-at-point
                      #'cape-dabbrev))))
  (message "✓ Complétion manuelle activée (utilisez C-SPC)"))

;; --------------------------------------------------
;; DÉSACTIVER complètement dans le minibuffer
;; --------------------------------------------------
(defun my/disable-completion-in-minibuffer ()
  "Désactive complètement la complétion dans le minibuffer."
  (when (minibufferp)
    (corfu-mode -1)
    (setq-local completion-at-point-functions nil)))

;; --------------------------------------------------
;; Configuration spécifique par mode - VERSION RESTRICTIVE
;; --------------------------------------------------
(defun my/setup-completion-for-mode ()
  "Configure la complétion selon le mode actuel - VERSION RESTRICTIVE."
  (cond
   ;; Org Mode : auto-complétion complète
   ((derived-mode-p 'org-mode)
    (my/enable-corfu-org-only))
   
   ;; Autres modes : Configurer les fonctions mais PAS d'activation auto
   ((derived-mode-p 'prog-mode)
    ;; Préparer les fonctions mais ne pas activer corfu automatiquement
    (setq-local completion-at-point-functions
                (list #'my/dictionary-completion-at-point
                      #'cape-keyword
                      #'cape-dabbrev
                      #'cape-file)))
   
   ;; Mode texte : même chose, pas d'auto-complétion
   ((derived-mode-p 'text-mode)
    (unless (derived-mode-p 'org-mode)  ; Éviter conflit avec org
      (setq-local completion-at-point-functions
                  (list #'my/dictionary-completion-at-point
                        #'cape-dabbrev))))))

;; --------------------------------------------------
;; Fonctions utilitaires pour l'analyse de l'intelligence
;; --------------------------------------------------
(defun my/prescient-stats ()
  "Affiche les statistiques d'utilisation de Prescient."
  (interactive)
  (if (not (featurep 'prescient))
      (message "❌ Prescient n'est pas encore chargé")
    (with-current-buffer (get-buffer-create "*Prescient Stats*")
      (erase-buffer)
      (insert "=== STATISTIQUES PRESCIENT ===\n\n")
      (insert (format "Nombre d'items appris : %d\n" 
                       (if (boundp 'prescient-frequency-table)
                           (hash-table-count prescient-frequency-table) 0)))
      (insert (format "Seuil de fréquence : %.3f\n" 
                       (if (boundp 'prescient-frequency-threshold)
                           prescient-frequency-threshold 0.05)))
      (insert (format "Déclin de fréquence : %.3f\n" 
                       (if (boundp 'prescient-frequency-decay)
                           prescient-frequency-decay 0.997)))
      (insert "\n=== TOP 20 MOTS LES PLUS UTILISÉS ===\n")
      
      (when (and (boundp 'prescient-frequency-table)
                 (hash-table-p prescient-frequency-table))
        (let ((sorted-items '()))
          (maphash (lambda (key value)
                     (push (cons key value) sorted-items))
                   prescient-frequency-table)
          (setq sorted-items (seq-take (sort sorted-items 
                                            (lambda (a b) (> (cdr a) (cdr b))))
                                      20))
          (dolist (item sorted-items)
            (insert (format "• %-20s : %.3f\n" (car item) (cdr item))))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun my/reset-prescient-data ()
  "Remet à zéro les données d'apprentissage de Prescient."
  (interactive)
  (if (not (featurep 'prescient))
      (message "❌ Prescient n'est pas encore chargé")
    (when (yes-or-no-p "Remettre à zéro toutes les données d'apprentissage Prescient ? ")
      (when (boundp 'prescient-frequency-table)
        (clrhash prescient-frequency-table))
      (when (boundp 'prescient-history-table)
        (clrhash prescient-history-table))
      (when (fboundp 'prescient-save)
        (prescient-save))
      (message "✓ Données Prescient remises à zéro"))))

(defun my/boost-word-frequency (word &optional boost-factor)
  "Augmente artificiellement la fréquence d'un mot dans Prescient."
  (interactive "sBooster le mot : \nnFacteur de boost (défaut 10.0) : ")
  (if (not (featurep 'prescient))
      (message "❌ Prescient n'est pas encore chargé")
    (let ((factor (or boost-factor 10.0)))
      (when (fboundp 'prescient-remember)
        (prescient-remember word))
      (when (boundp 'prescient-frequency-table)
        (puthash word (+ (gethash word prescient-frequency-table 0.0) factor)
                 prescient-frequency-table))
      (when (fboundp 'prescient-save)
        (prescient-save))
      (message "✓ Mot '%s' boosté avec facteur %.1f" word factor))))

;; --------------------------------------------------
;; FONCTION de vérification du statut
;; --------------------------------------------------
(defun my/check-completion-status ()
  "Vérifie le statut de la complétion dans le buffer actuel."
  (interactive)
  (message "Mode: %s | Corfu: %s | Auto: %s | CAPF: %d fonctions | Minibuffer: %s" 
           major-mode
           (if (bound-and-true-p corfu-mode) "ON" "OFF")
           (if (bound-and-true-p corfu-auto) "ON" "OFF")
           (length completion-at-point-functions)
           (if (minibufferp) "YES" "NO")))

;; --------------------------------------------------
;; Hooks et activation - VERSION RESTRICTIVE
;; --------------------------------------------------

;; SUPPRIMER tous les anciens hooks automatiques s'ils existent
(remove-hook 'prog-mode-hook #'my/setup-completion-for-mode)
(remove-hook 'text-mode-hook #'my/setup-completion-for-mode)
(remove-hook 'prog-mode-hook #'corfu-mode)
(remove-hook 'text-mode-hook #'corfu-mode)

;; Hooks restrictifs : seulement Org + désactivation minibuffer
(add-hook 'org-mode-hook #'my/setup-completion-for-mode)
(add-hook 'minibuffer-setup-hook #'my/disable-completion-in-minibuffer)

;; --------------------------------------------------
;; Désactiver text-replacement auto-mode si activé
;; --------------------------------------------------
;(with-eval-after-load 'text-replacement
;  (when (boundp 'text-replacement-auto-mode)
;    (setq text-replacement-auto-mode nil)
;    (remove-hook 'post-self-insert-hook #'text-replacement-auto-replace-at-point)
;    (message "✓ Text-replacement auto-mode désactivé")))

;; --------------------------------------------------
;; Intégration avec Eglot/LSP - VERSION MANUELLE
;; --------------------------------------------------
(with-eval-after-load 'eglot
  ;; Ne pas activer automatiquement, juste préparer les fonctions
  (setq completion-category-defaults nil)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Préparer mais ne pas activer automatiquement
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 #'eglot-completion-at-point
                                 #'my/dictionary-completion-at-point
                                 #'cape-dabbrev)))
              ;; Activation manuelle requise avec C-c c m
              )))

;; --------------------------------------------------
;; Raccourcis clavier pour contrôle manuel
;; --------------------------------------------------
(global-set-key (kbd "M-/")       #'completion-at-point)  ; Complétion manuelle universelle
(global-set-key (kbd "C-M-/")     #'cape-dabbrev)
(global-set-key (kbd "C-c d r")   #'my/reload-dictionaries)
(global-set-key (kbd "C-c d s")   #'my/prescient-stats)
(global-set-key (kbd "C-c d c")   #'my/reset-prescient-data)
(global-set-key (kbd "C-c d b")   #'my/boost-word-frequency)

;; Nouveaux raccourcis pour contrôle de la complétion (éviter conflit avec org-capture)
(global-set-key (kbd "C-c m m")   #'my/enable-manual-completion)
(global-set-key (kbd "C-c m s")   #'my/check-completion-status)
(global-set-key (kbd "C-c m o")   (lambda () 
                                    (interactive) 
                                    (if (derived-mode-p 'org-mode)
                                        (my/enable-corfu-org-only)
                                      (message "Pas en mode Org"))))

;; --------------------------------------------------
;; Initialisation et messages
;; --------------------------------------------------
(my/reload-dictionaries)

;; Chargement des données Prescient sauvegardées (avec protection)
(with-eval-after-load 'prescient
  (when (and (boundp 'prescient-save-file) 
             (file-exists-p prescient-save-file))
    ;; Prescient charge automatiquement ses données avec prescient-persist-mode
    ;; Pas besoin d'appel manuel
    (message "✓ Prescient data loaded from %s" prescient-save-file)))

;; --------------------------------------------------
;; GLOBAL: Protection du dimming contre l'autocomplétion
;; --------------------------------------------------
(defun my/setup-dimming-protection ()
  "Configure la protection globale du dimming contre l'autocomplétion."
  (when (bound-and-true-p auto-dim-other-buffers-mode)
    
    ;; Variable pour détecter l'activité de complétion
    (defvar-local my/corfu-active nil)
    
    ;; Advice global pour préserver le dimming
    (defun my/completion-dimming-advice (orig-fun &rest args)
      "Advice global pour préserver le dimming durant la complétion."
      (setq my/corfu-active t)
      (let ((result (apply orig-fun args)))
        (run-with-idle-timer 0.1 nil 
                             (lambda ()
                               (when (and (buffer-live-p (current-buffer))
                                          (fboundp 'auto-dim-other-buffers--refresh-buffer))
                                 (auto-dim-other-buffers--refresh-buffer (current-buffer))
                                 (setq my/corfu-active nil))))
        result))
    
    ;; Hook plus ciblé pour surveiller pendant la navigation Corfu
    (defun my/dimming-protection-during-completion ()
      "Protection ciblée du dimming pendant la navigation Corfu."
      (when (and (bound-and-true-p corfu--candidates)  ; Menu Corfu ouvert
                 (bound-and-true-p auto-dim-other-buffers-mode)
                 (or (eq this-command 'corfu-next)     ; Navigation dans le menu
                     (eq this-command 'corfu-previous)
                     (eq this-command 'corfu-complete)
                     (eq this-command 'corfu-insert)))
        (run-with-idle-timer 0.05 nil 
                             (lambda ()
                               (when (and (buffer-live-p (current-buffer))
                                          (fboundp 'auto-dim-other-buffers--refresh-buffer))
                                 (auto-dim-other-buffers--refresh-buffer (current-buffer)))))))
    
    ;; Hook pour surveiller les commandes de navigation
    (add-hook 'post-command-hook #'my/dimming-protection-during-completion)
    
    ;; Appliquer l'advice aux fonctions critiques de Corfu
    (when (fboundp 'corfu--show)
      (advice-add 'corfu--show :around #'my/completion-dimming-advice))
    (when (fboundp 'corfu--hide)
      (advice-add 'corfu--hide :around #'my/completion-dimming-advice))
    (when (fboundp 'corfu--exhibit)
      (advice-add 'corfu--exhibit :around #'my/completion-dimming-advice))
    
    ;; NOUVEAU: Protection pour la navigation dans le menu
    (when (fboundp 'corfu-next)
      (advice-add 'corfu-next :around #'my/completion-dimming-advice))
    (when (fboundp 'corfu-previous)
      (advice-add 'corfu-previous :around #'my/completion-dimming-advice))
    (when (fboundp 'corfu--update)
      (advice-add 'corfu--update :around #'my/completion-dimming-advice))
    
    (message "✓ Protection dimming vs autocomplétion activée")))

;; Fonction de debug pour diagnostiquer les problèmes de dimming
(defun my/debug-dimming-status ()
  "Debug du statut du dimming et de Corfu."
  (interactive)
  (message "=== DEBUG DIMMING ===")
  (message "Auto-dim mode: %s" (if (bound-and-true-p auto-dim-other-buffers-mode) "ON" "OFF"))
  (message "Corfu mode: %s" (if (bound-and-true-p corfu-mode) "ON" "OFF"))
  (message "Corfu candidates: %s" (if (bound-and-true-p corfu--candidates) "ACTIF" "INACTIF"))
  (message "This command: %s" this-command)
  (message "Buffer: %s" (buffer-name))
  (when (fboundp 'auto-dim-other-buffers--buffer-dim-state)
    (message "Dim state: %s" (auto-dim-other-buffers--buffer-dim-state (current-buffer)))))

(global-set-key (kbd "C-c d D") #'my/debug-dimming-status)

;; Activer la protection après le chargement de auto-dim-other-buffers
(with-eval-after-load 'auto-dim-other-buffers
  (my/setup-dimming-protection))

;; Hook pour activer la protection si auto-dim-other-buffers est déjà chargé
(when (featurep 'auto-dim-other-buffers)
  (my/setup-dimming-protection))

;; --------------------------------------------------
;; Messages de confirmation
;; --------------------------------------------------
(message "✓ Configuration de complétion RESTRICTIVE chargée")
(message "  📝 AUTOCOMPLÉTION : Uniquement dans Org Mode")
(message "  🔧 Contrôles : C-c m s (statut) | C-c m m (manuel) | C-SPC (complétion)")
(message "  📊 Stats : C-c d s | 🔄 Reload dicos : C-c d r | 🚀 Boost mot : C-c d b")
(message "  🔅 Protection dimming : Configurée")

(provide 'completion)
;;; completion.el ends here