;;; keybindings.el --- Centralisation de tous les keybindings - Configuration Emacs PKM

;; Auteur: Configuration Emacs PKM
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Ce fichier centralise TOUS les keybindings de la configuration Emacs PKM
;; pour éliminer les répétitions (principe DRY) et éviter les conflits.
;; 
;; Tous les keybindings sont organisés par domaines fonctionnels avec
;; des préfixes logiques pour une maintenance simplifiée.

;;; Code:

;; --------------------------------------------------
;; KEYBINDINGS GLOBAUX DE BASE
;; --------------------------------------------------

;; Touches principales
(global-set-key (kbd "C-c a") 'org-agenda)                    ; Org agenda
(global-set-key (kbd "C-c c") 'org-capture)                  ; Org capture  
(global-set-key (kbd "<f1>") (lambda () (interactive) (find-file org-default-notes-file))) ; Inbox.org
(global-set-key (kbd "<f3>") #'daily-journal-open-today)     ; Journal aujourd'hui

;; Navigation universelle
(global-set-key (kbd "M-j") 'avy-goto-char-timer)           ; Navigation universelle principale
(global-set-key (kbd "C-;") 'avy-goto-char)                 ; Navigation caractère simple
(global-set-key (kbd "C-z") 'undo)                          ; Undo (remplace suspend-frame)
(global-set-key (kbd "C-l") #'cycle-checkbox-symbol)        ; Cycle checkbox symbols

;; --------------------------------------------------
;; C-c A : AVY NAVIGATION SPÉCIALISÉE
;; --------------------------------------------------

(global-set-key (kbd "C-c A l") #'my/avy-goto-line-simple)              ; Navigation lignes
(global-set-key (kbd "C-c A w") #'my/avy-goto-word-simple)              ; Navigation mots
(global-set-key (kbd "C-c A h") #'my/avy-goto-org-heading-in-buffer)    ; Headings buffer
(global-set-key (kbd "C-c A k") #'my/avy-goto-org-link)                 ; Liens org
(global-set-key (kbd "C-c A i") #'my/avy-config-info)                   ; Config info
(global-set-key (kbd "C-c A T") #'my/avy-test-navigation)               ; Test navigation

;; --------------------------------------------------
;; C-c d : DATA/DICTIONARIES MANAGEMENT
;; --------------------------------------------------

(global-set-key (kbd "C-c d r") #'my/reload-dictionaries)     ; Reload dictionaries
(global-set-key (kbd "C-c d s") #'my/prescient-stats)         ; Prescient stats
(global-set-key (kbd "C-c d c") #'my/reset-prescient-data)    ; Reset prescient
(global-set-key (kbd "C-c d b") #'my/boost-word-frequency)    ; Boost word frequency
(global-set-key (kbd "C-c d l") #'my/test-direct-launch)      ; Test direct launch
(global-set-key (kbd "C-c d D") #'my/debug-dimming-status)    ; Debug dimming

;; --------------------------------------------------
;; C-c e : EXPORT/EVERYTHING SEARCH
;; --------------------------------------------------

;; LaTeX/PDF Export
(global-set-key (kbd "C-c e p") #'my/org-export-pdf)               ; Export PDF
(global-set-key (kbd "C-c e o") #'my/org-export-pdf-and-open)      ; Export PDF et ouvrir

;; Everything Search (Windows)
(global-set-key (kbd "C-c e e") #'my/counsel-everything-general)   ; Everything general
(global-set-key (kbd "C-c e f") #'my/counsel-everything-files)     ; Everything files
(global-set-key (kbd "C-c e d") #'my/counsel-everything-folders)   ; Everything folders

;; --------------------------------------------------
;; C-c f : FILE OPERATIONS (Dirvish/File Manager)
;; --------------------------------------------------

(global-set-key (kbd "C-c f f") #'my/open-file-manager)        ; Open file manager
(global-set-key (kbd "C-c f h") #'my/open-home-directory)      ; Home directory
(global-set-key (kbd "C-c f p") #'my/open-pkm-directory)       ; PKM directory
(global-set-key (kbd "C-c f d") #'my/open-documents-directory) ; Documents directory
(global-set-key (kbd "C-c f t") #'my/open-downloads-directory) ; Downloads directory
(global-set-key (kbd "C-c f x") #'my/dirvish-sidebar-toggle)   ; Sidebar toggle
(global-set-key (kbd "C-c f b") #'my/dirvish-sidebar-focus)    ; Focus sidebar

;; --------------------------------------------------
;; C-c F : FREQUENCY TRACKING (Majuscule pour éviter conflit)
;; --------------------------------------------------

(global-set-key (kbd "C-c F u") #'my/update-frequency-md)      ; Update frequency
(global-set-key (kbd "C-c F s") #'my/frequency-stats)          ; Frequency stats
(global-set-key (kbd "C-c F r") #'my/frequency-reset)          ; Frequency reset
(global-set-key (kbd "C-c F b") #'my/frequency-backup)         ; Frequency backup

;; --------------------------------------------------
;; C-c i : IMAGE MANAGEMENT
;; --------------------------------------------------

(global-set-key (kbd "C-c i i") #'my/org-image-paste)          ; Paste image
(global-set-key (kbd "C-c i a") #'my/org-image-refresh)        ; Refresh image
(global-set-key (kbd "C-c i e") #'my/org-image-edit-last)      ; Edit last image
(global-set-key (kbd "C-c i d") #'my/org-image-show-directory) ; Show directory
(global-set-key (kbd "C-c i l") #'my/org-image-list-images)    ; List images
(global-set-key (kbd "C-c i t") #'my/org-image-toggle-display) ; Toggle display
(global-set-key (kbd "C-c i h") #'my/org-image-hide-all)       ; Hide all images
(global-set-key (kbd "C-c i s") #'my/org-image-show-all)       ; Show all images

;; --------------------------------------------------
;; C-c j : DAILY JOURNAL
;; --------------------------------------------------

(global-set-key (kbd "C-c j j") #'daily-journal-open-today)    ; Journal today
(global-set-key (kbd "C-c j d") #'daily-journal-open-date)     ; Journal date
(global-set-key (kbd "C-c j l") #'daily-journal-list-entries)  ; List entries
(global-set-key (kbd "C-c j s") #'daily-journal-search)        ; Search journal
(global-set-key (kbd "C-c j y") #'daily-journal-yesterday)     ; Yesterday
(global-set-key (kbd "C-c j w") #'daily-journal-week-view)     ; Week view
(global-set-key (kbd "C-c j i") #'daily-journal-stats)         ; Journal stats

;; --------------------------------------------------
;; C-c k : KILL/CACHE MANAGEMENT
;; --------------------------------------------------

(global-set-key (kbd "C-c k a") #'kill/clear-all-caches)       ; Clear all caches
(global-set-key (kbd "C-c k z") #'kill/kill-other-buffers)     ; Kill other buffers

;; --------------------------------------------------
;; C-c l : LATEX/LIST OPERATIONS
;; --------------------------------------------------

;; LaTeX operations
(global-set-key (kbd "C-c l c") #'my/clean-latex-config)       ; Clean LaTeX config
(global-set-key (kbd "C-c l s") #'my/setup-latex-final)        ; Setup LaTeX final
(global-set-key (kbd "C-c l x") #'my/clean-latex-cache)        ; Clean LaTeX cache
(global-set-key (kbd "C-c l f") #'my/setup-latex-final)        ; Setup LaTeX final (duplicate)
(global-set-key (kbd "C-c l v") #'my/verify-export-pdf-clean)  ; Verify export PDF clean
(global-set-key (kbd "C-c l t") #'my/test-both-latex-modes)    ; Test LaTeX modes
(global-set-key (kbd "C-c l g") #'my/toggle-fragtog)           ; Toggle fragtog
(global-set-key (kbd "C-c l G") #'my/test-fragtog)             ; Test fragtog
(global-set-key (kbd "C-c l l") #'my/launch-shortcuts-fixed)   ; Launch shortcuts

;; Smart bullets
(global-set-key (kbd "C-c l i") #'smart-bullet-list-info)      ; List info
(global-set-key (kbd "C-c l b") #'smart-bullet-create-bullet)  ; Create bullet

;; --------------------------------------------------
;; C-c m : MATH/MANUAL COMPLETION
;; --------------------------------------------------

;; Math calculator
(global-set-key (kbd "C-c m =") #'my/calc-evaluate-and-append-result) ; Calculer et ajouter
(global-set-key (kbd "C-c m r") #'my/calc-replace-with-result)         ; Remplacer par résultat

;; Manual completion
(global-set-key (kbd "C-c m m") #'my/enable-manual-completion)  ; Enable manual completion
(global-set-key (kbd "C-c m s") #'my/check-completion-status)   ; Check completion status

;; --------------------------------------------------
;; C-c n : NOTES/NAVIGATION (Org-roam)
;; --------------------------------------------------

(global-set-key (kbd "C-c n f") #'org-roam-node-find)          ; Find node
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)        ; Insert node
(global-set-key (kbd "C-c n g") #'org-roam-ui-open)            ; Open UI graphique
(global-set-key (kbd "C-c n j") 'avy-org-goto-heading-timer)   ; Navigation headings org

;; --------------------------------------------------
;; C-c o : ORG OPERATIONS/CALLOUTS
;; --------------------------------------------------

;; Configuration Org
(global-set-key (kbd "C-c o i") #'my/org-config-info)          ; Config info
(global-set-key (kbd "C-c o r") #'my/reload-org-modules)       ; Reload modules

;; Callouts insertion
(global-set-key (kbd "C-c o n") (lambda () (interactive) (insert-callout "NOTE" "")))      ; Note callout
(global-set-key (kbd "C-c o w") (lambda () (interactive) (insert-callout "WARNING" "")))   ; Warning callout
(global-set-key (kbd "C-c o t") (lambda () (interactive) (insert-callout "TIP" "")))       ; Tip callout
(global-set-key (kbd "C-c o I") (lambda () (interactive) (insert-callout "IMPORTANT" ""))) ; Important callout
(global-set-key (kbd "C-c o g") (lambda () (interactive) (insert-callout "DANGER" "")))    ; Danger callout
(global-set-key (kbd "C-c o d") #'insert-diary-callout)        ; Diary callout
(global-set-key (kbd "C-c o o") #'insert-callout)              ; Generic callout

;; Callouts folding
(global-set-key (kbd "C-c o f") #'callout-toggle-fold)         ; Toggle fold
(global-set-key (kbd "C-c o F") #'callout-fold-all)            ; Fold all
(global-set-key (kbd "C-c o u") #'callout-unfold-all)          ; Unfold all
(global-set-key (kbd "C-c o D") #'callout-debug-state)         ; Debug state
(global-set-key (kbd "C-c o R") #'callout-force-restore)       ; Force restore

;; --------------------------------------------------
;; C-c q : QUIT/WORKSPACE
;; --------------------------------------------------

(global-set-key (kbd "C-c q r") #'workspace/restart-with-state) ; Restart with state

;; --------------------------------------------------
;; C-c r : TEXT REPLACEMENT/REFILING
;; --------------------------------------------------

;; Text replacement
(global-set-key (kbd "C-c r a") #'text-replacement-add)        ; Add replacement
(global-set-key (kbd "C-c r d") #'text-replacement-remove)     ; Remove replacement
(global-set-key (kbd "C-c r l") #'text-replacement-list)       ; List replacements
(global-set-key (kbd "C-c r t") #'text-replacement-toggle-auto) ; Toggle auto

;; Org refiling
(global-set-key (kbd "C-c r r") #'my/org-roam-extract-to-note) ; Extract to roam note

;; --------------------------------------------------
;; C-c s : SEARCH OPERATIONS
;; --------------------------------------------------

(global-set-key (kbd "C-c s b") 'consult-buffer)               ; Consult buffer
(global-set-key (kbd "C-c s l") 'consult-line)                 ; Consult line  
(global-set-key (kbd "C-c s k") 'consult-ripgrep)              ; Consult ripgrep
(global-set-key (kbd "C-c s e") #'my/everything-search-cached) ; Everything cached
(global-set-key (kbd "C-c s i") #'my/cache-info)               ; Cache info
(global-set-key (kbd "C-c s c") #'my/everything-clear-cache)   ; Clear cache
(global-set-key (kbd "C-c s n") #'my/search-notes-ivy)        ; Search notes with ivy
(global-set-key (kbd "C-c s t") #'my/search-org-titles-ivy)   ; Search org titles with ivy
(global-set-key (kbd "C-c s f") #'my/consult-org-files-by-name) ; Search org files by name
(global-set-key (kbd "C-c s o") #'my/get-all-org-files)       ; Get all org files

;; --------------------------------------------------
;; C-c t : TRANSCLUSION/TEST
;; --------------------------------------------------

;; Transclusion operations
(global-set-key (kbd "C-c t m") #'my/transclusion-toggle-mode)      ; Toggle mode
(global-set-key (kbd "C-c t a") #'my/insert-transclusion-template)  ; Add template
(global-set-key (kbd "C-c t r") #'my/transclusion-refresh-all)      ; Refresh all
(global-set-key (kbd "C-c t i") #'my/transclusion-info)             ; Info
(global-set-key (kbd "C-c t n") #'my/transclude-roam-note)          ; Note roam
(global-set-key (kbd "C-c t c") #'my/transclude-code-snippet)       ; Code snippet
(global-set-key (kbd "C-c t t") #'my/test-transclusion)             ; Test transclusion
(global-set-key (kbd "C-c t h") #'my/transclusion-hide-all)         ; Hide all

;; Test functions
(global-set-key (kbd "C-c t s") #'my/test-latex-simple)             ; Test LaTeX simple

;; --------------------------------------------------
;; C-c T : TRANSCLUSION SHOW (Majuscule pour éviter conflit)
;; --------------------------------------------------

(global-set-key (kbd "C-c T s") #'my/transclusion-show-all)         ; Show all transclusions

;; --------------------------------------------------
;; C-c u : UI/UTILITY OPERATIONS
;; --------------------------------------------------

(global-set-key (kbd "C-c u d") #'my/debug-org-status)              ; Debug org status
(global-set-key (kbd "C-c u r") #'my/force-org-superstar-restart)   ; Force restart
(global-set-key (kbd "C-c u c") #'my/clean-org-modes)               ; Clean org modes
(global-set-key (kbd "C-c u s") #'my/test-sub-superscripts)         ; Test subscripts
(global-set-key (kbd "C-c u x") #'smart-bullet-create-checkbox)     ; Create checkbox

;; --------------------------------------------------
;; C-c v : VIEW STATE OPERATIONS (Folding)
;; --------------------------------------------------

(global-set-key (kbd "C-c v s") #'my-org-save-fold-state)      ; Save fold state
(global-set-key (kbd "C-c v r") #'my-org-restore-fold-state)   ; Restore fold state

;; --------------------------------------------------
;; C-c w : WINDOW/DISPLAY OPERATIONS
;; --------------------------------------------------

(global-set-key (kbd "C-c w") #'my/toggle-emphasis-display-mode) ; Toggle emphasis display
(global-set-key (kbd "C-c W") #'my/org-appear-info)              ; Debug org-appear

;; --------------------------------------------------
;; KEYBINDINGS SPÉCIALISÉS PAR MODE
;; --------------------------------------------------

;; Calendar Mode Navigation
(with-eval-after-load 'calendar
  (define-key calendar-mode-map (kbd "C-<left>") 'calendar-backward-week)
  (define-key calendar-mode-map (kbd "C-<right>") 'calendar-forward-week)
  (define-key calendar-mode-map (kbd "C-<up>") 'calendar-backward-month)
  (define-key calendar-mode-map (kbd "C-<down>") 'calendar-forward-month)
  (define-key calendar-mode-map (kbd "M-<left>") 'calendar-backward-month)
  (define-key calendar-mode-map (kbd "M-<right>") 'calendar-forward-month)
  (define-key calendar-mode-map (kbd "M-<up>") 'calendar-backward-year)
  (define-key calendar-mode-map (kbd "M-<down>") 'calendar-forward-year))

;; Org Mode Spécialisé
(with-eval-after-load 'org
  ;; Avy refiling - désactiver
  ;;(define-key org-mode-map (kbd "C-c r") 'avy-org-refile-as-child)
  
  ;; Export LaTeX/PDF (mode spécifique)
  (define-key org-mode-map (kbd "C-c e p") #'my/org-export-pdf)
  (define-key org-mode-map (kbd "C-c e o") #'my/org-export-pdf-and-open)
  (define-key org-mode-map (kbd "C-c e P") #'my/org-export-pandoc-pdf)
  
  ;; Navigation avy dans org
  (define-key org-mode-map (kbd "M-j") 'avy-goto-char-timer))

;; Dirvish Mode
(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "C-.") #'embark-act)
  (define-key dirvish-mode-map (kbd "C-,") #'embark-dwim)
  (define-key dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "h") #'dirvish-history-jump)
  (define-key dirvish-mode-map (kbd "f") #'my/dirvish-filter-by-keyword)
  (define-key dirvish-mode-map (kbd "?") #'dirvish-dispatch)
  (define-key dirvish-mode-map (kbd "q") #'dirvish-quit)
  (define-key dirvish-mode-map (kbd "S") #'my/dirvish-everything-search-current-dir)
  (define-key dirvish-mode-map (kbd "x") #'my/dired-do-cut)
  (define-key dirvish-mode-map (kbd "c") #'my/dired-do-copy-for-paste)
  (define-key dirvish-mode-map (kbd "p") #'dired-do-paste)
  ;; Navigation avec les flèches
  (define-key dirvish-mode-map (kbd "<right>") #'my/dirvish-open-folder)
  (define-key dirvish-mode-map (kbd "<left>") #'my/dirvish-parent-directory)
  ;; Everything search
  (define-key dirvish-mode-map (kbd "e") #'my/dirvish-everything-search))

;; Dired Mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "S") #'my/dirvish-everything-search-current-dir)
  (define-key dired-mode-map (kbd "p") #'dired-do-paste)
  ;; Navigation avec les flèches (aussi pour dired classique)
  (define-key dired-mode-map (kbd "<right>") #'my/dirvish-open-folder)
  (define-key dired-mode-map (kbd "<left>") #'my/dirvish-parent-directory)
  ;; Everything search
  (define-key dired-mode-map (kbd "e") #'my/dirvish-everything-search))

;; Corfu Completion
(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map [tab] #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map [backtab] #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-insert)
  (define-key corfu-map (kbd "C-SPC") #'corfu-complete)
  (define-key corfu-map (kbd "C-g") #'corfu-quit)
  (define-key corfu-map (kbd "M-d") #'corfu-info-documentation)
  (define-key corfu-map (kbd "M-l") #'corfu-info-location))

;; Org Transclusion Native
(with-eval-after-load 'org-transclusion
  (define-key org-transclusion-map (kbd "C-c C-t") #'org-transclusion-add)
  (define-key org-transclusion-map (kbd "C-c C-r") #'org-transclusion-refresh))

;; --------------------------------------------------
;; KEYBINDINGS UNIVERSELS SUPPLÉMENTAIRES
;; --------------------------------------------------

;; Completion universelle
(global-set-key (kbd "M-/") #'completion-at-point)         ; Complétion manuelle universelle
(global-set-key (kbd "C-M-/") #'cape-dabbrev)              ; Cape dabbrev

;; Swiper (depuis 1_packages.el)
(global-set-key (kbd "C-s") #'swiper)                      ; Recherche dans buffer avec swiper

;; Winum - Navigation fenêtres par numéro (depuis 1_packages.el)
(global-set-key (kbd "M-1") #'winum-select-window-1)       ; Fenêtre 1
(global-set-key (kbd "M-2") #'winum-select-window-2)       ; Fenêtre 2
(global-set-key (kbd "M-3") #'winum-select-window-3)       ; Fenêtre 3
(global-set-key (kbd "M-4") #'winum-select-window-4)       ; Fenêtre 4
(global-set-key (kbd "M-5") #'winum-select-window-5)       ; Fenêtre 5
(global-set-key (kbd "M-6") #'winum-select-window-6)       ; Fenêtre 6
(global-set-key (kbd "M-7") #'winum-select-window-7)       ; Fenêtre 7
(global-set-key (kbd "M-8") #'winum-select-window-8)       ; Fenêtre 8
(global-set-key (kbd "M-9") #'winum-select-window-9)       ; Fenêtre 9

;; Smartparens - Navigation S-expressions (depuis 1_packages.el)
(global-set-key (kbd "C-M-f") #'sp-forward-sexp)           ; S-expression suivante
(global-set-key (kbd "C-M-b") #'sp-backward-sexp)          ; S-expression précédente

;; --------------------------------------------------
;; FONCTION DE NETTOYAGE DES KEYBINDINGS EXISTANTS
;; --------------------------------------------------

(defun my/cleanup-existing-keybindings ()
  "Supprime tous les keybindings définis dans les autres fichiers pour éviter les doublons."
  (interactive)
  (message "🧹 Nettoyage des keybindings existants..."))

;; --------------------------------------------------
;; MESSAGE DE CONFIRMATION
;; --------------------------------------------------

(message "✓ Keybindings centralisés chargés - 150+ raccourcis organisés par domaines fonctionnels")

(provide 'keybindings)
;;; keybindings.el ends here