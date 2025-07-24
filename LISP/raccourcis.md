# Emacs Configuration - Raccourcis Clavier Complets

Ce document présente l'ensemble des raccourcis clavier définis dans cette configuration Emacs modulaire orientée PKM (Personal Knowledge Management).

## Table des Matières

1. [Raccourcis Globaux](#raccourcis-globaux)
2. [Raccourcis Spécifiques aux Modes](#raccourcis-spécifiques-aux-modes)
3. [Touches de Fonction](#touches-de-fonction)
4. [Statistiques](#statistiques)

---

## Raccourcis Globaux

### Navigation et Fenêtres de Base
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-z` | `undo` | Annulation (remplace suspend par défaut) |
| `C-s` | `swiper` | Recherche dans le buffer |

### Système de Complétion et Dictionnaire
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `M-/` | `completion-at-point` | Complétion manuelle |
| `C-M-/` | `cape-dabbrev` | Complétion dabbrev |
| `C-c d r` | `my/reload-dictionaries` | Recharger les dictionnaires |
| `C-c d s` | `my/prescient-stats` | Statistiques prescient |
| `C-c d c` | `my/reset-prescient-data` | Réinitialiser données prescient |
| `C-c d b` | `my/boost-word-frequency` | Augmenter fréquence mot |
| `C-c m m` | `my/enable-manual-completion` | Activer complétion manuelle |
| `C-c m s` | `my/check-completion-status` | Vérifier statut complétion |
| `C-c m o` | Enable org-only completion | Complétion Org uniquement |

### Recherche et Navigation Buffer
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c s b` | `consult-buffer` | Consultation des buffers |
| `C-c s l` | `consult-line` | Consultation des lignes |
| `C-c s k` | `consult-ripgrep` | Recherche ripgrep |
| `C-c s h` | `my/consult-org-headings` | Recherche en-têtes org |
| `C-c s f` | `my/consult-org-files-by-name` | Recherche fichiers org |
| `C-c s n` | `my/search-notes-ivy` | Recherche notes avec ivy |
| `C-c s t` | `my/search-org-titles-ivy` | Recherche titres org avec ivy |
| `C-c s o` | `my/get-all-org-files` | Obtenir tous les fichiers org |
| `C-c s e` | `my/everything-search-cached` | Recherche Everything avec cache |
| `C-c s i` | `my/cache-info` | Information cache |
| `C-c s c` | `my/everything-clear-cache` | Vider cache Everything |

### Navigation Avy
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `M-j` | `avy-goto-char-timer` | Navigation universelle avec timer |
| `C-;` | `avy-goto-char` | Saut de caractère simple |
| `C-c n j` | `avy-org-goto-heading-timer` | Navigation vers en-têtes org |
| `C-c A l` | `my/avy-goto-line-simple` | Navigation vers lignes |
| `C-c A w` | `my/avy-goto-word-simple` | Navigation vers début de mot |
| `C-c A h` | `my/avy-goto-org-heading-in-buffer` | Navigation vers en-têtes org (buffer) |
| `C-c A k` | `my/avy-goto-org-link` | Navigation vers liens org |
| `C-c A i` | `my/avy-config-info` | Informations configuration avy |
| `C-c A T` | `my/avy-test-navigation` | Test navigation avy |

### Recherche Everything (Windows uniquement)
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c e e` | `my/counsel-everything-general` | Recherche générale Everything |
| `C-c e f` | `my/counsel-everything-files` | Recherche fichiers Everything |
| `C-c e d` | `my/counsel-everything-folders` | Recherche dossiers Everything |
| `C-c l l` | `my/launch-shortcuts-fixed` | Lancer raccourcis .lnk |
| `C-c d l` | `my/test-direct-launch` | Test lanceur (debug) |

### Actions Embark
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-.` | `embark-act` | Choisir une action |
| `C-,` | `embark-dwim` | Faire ce que je veux |
| `C-h B` | `embark-bindings` | Afficher keymap |

### Calculatrice Mathématique
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c m =` | `my/calc-evaluate-and-append-result` | Évaluer et ajouter résultat |
| `C-c m e` | `my/calc-evaluate-expression` | Évaluer expression |
| `C-c m r` | `my/calc-replace-with-result` | Remplacer par résultat |
| `C-c m t` | `my/test-math-extraction` | Test extraction math (debug) |

### Remplacement de Texte
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c r a` | `text-replacement-add` | Ajouter remplacement |
| `C-c r d` | `text-replacement-remove` | Supprimer remplacement |
| `C-c r l` | `text-replacement-list` | Lister remplacements |
| `C-c r t` | `text-replacement-toggle-auto` | Basculer auto-remplacement |

### Org Mode Base
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c a` | `org-agenda` | Agenda Org |
| `C-c c` | `org-capture` | Capture Org |
| `<f1>` | Open inbox.org | Ouvrir inbox |

### Org-roam et Gestion des Notes
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c n f` | `org-roam-node-find` | Trouver nœud roam |
| `C-c n i` | `org-roam-node-insert` | Insérer nœud roam |
| `C-c n g` | `org-roam-ui-open` | Ouvrir interface roam |
| `C-c r r` | `my/org-roam-extract-to-note` | Extraire vers note roam |
| `C-c r` | `avy-org-refile-as-child` | Refiling rapide avec avy (org-mode) |
| `C-c f s` | `my-org-save-fold-state` | Sauvegarder état pliage |
| `C-c f r` | `my-org-restore-fold-state` | Restaurer état pliage |

### LaTeX et Export PDF
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c e p` | `my/org-export-pdf` | Export PDF |
| `C-c e o` | `my/org-export-pdf-and-open` | Export PDF et ouvrir |
| `C-c e P` | `my/org-export-pandoc-pdf` | Export PDF via Pandoc |
| `C-c t s` | `my/test-latex-simple` | Test LaTeX simple |
| `C-c l c` | `my/clean-latex-config` | Nettoyer config LaTeX |
| `C-c l s` | `my/setup-latex-final` | Configuration LaTeX finale |
| `C-c l x` | `my/clean-latex-cache` | Nettoyer cache LaTeX |
| `C-c l f` | `my/setup-latex-final` | Configuration LaTeX finale |
| `C-c l v` | `my/verify-export-pdf-clean` | Vérifier export PDF propre |
| `C-c l t` | `my/test-both-latex-modes` | Test des deux modes LaTeX |

### Améliorations UI et Débogage
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-l` | `cycle-checkbox-symbol` | Cycle symboles checkbox |
| `C-c u d` | `my/debug-org-status` | Debug statut org |
| `C-c u r` | `my/force-org-superstar-restart` | Redémarrer org-superstar |
| `C-c u c` | `my/clean-org-modes` | Nettoyer modes org |
| `C-c l i` | `smart-bullet-list-info` | Info liste à puces |
| `C-c u x` | `smart-bullet-create-checkbox` | Créer checkbox |
| `C-c l b` | `smart-bullet-create-bullet` | Créer puce |

### Gestion des Images (Org Mode)
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c i i` | `my/org-image-paste` | Coller image |
| `C-c i a` | `my/org-image-refresh` | Actualiser image |
| `C-c i e` | `my/org-image-edit-last` | Éditer dernière image |
| `C-c i d` | `my/org-image-show-directory` | Afficher dossier images |
| `C-c i l` | `my/org-image-list-images` | Lister images |
| `C-c i t` | `my/org-image-toggle-display` | Basculer affichage image |
| `C-c i h` | `my/org-image-hide-all` | Masquer toutes images |
| `C-c i s` | `my/org-image-show-all` | Afficher toutes images |

### Callouts (Insertion de Blocs Spéciaux)
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c o n` | `insert-callout "NOTE"` | Insérer callout note |
| `C-c o w` | `insert-callout "WARNING"` | Insérer callout warning |
| `C-c o t` | `insert-callout "TIP"` | Insérer callout tip |
| `C-c o I` | `insert-callout "IMPORTANT"` | Insérer callout important |
| `C-c o g` | `insert-callout "DANGER"` | Insérer callout danger |
| `C-c o d` | `insert-diary-callout` | Insérer callout diary avec timestamp |
| `C-c o o` | `insert-callout` | Callout interactif |
| `C-c o f` | `callout-toggle-fold` | Basculer pliage callout |
| `C-c o F` | `callout-fold-all` | Plier tous les callouts |
| `C-c o u` | `callout-unfold-all` | Déplier tous les callouts |

### Transclusion (Inclusion de Contenu)
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c t i` | `my/transclusion-info` | Info transclusion |
| `C-c t n` | `my/transclude-roam-note` | Transclure note roam |
| `C-c t c` | `my/transclude-code-snippet` | Transclure snippet code |
| `C-c t t` | `my/test-transclusion` | Test transclusion |
| `C-c t h` | `my/transclusion-hide-all` | Masquer toutes transclusions |
| `C-c T s` | `my/transclusion-show-all` | Afficher toutes transclusions |

### Gestionnaire de Fichiers (Dirvish)
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c f f` | `my/open-file-manager` | Ouvrir gestionnaire |
| `C-c f h` | `my/open-home-directory` | Ouvrir dossier utilisateur |
| `C-c f p` | `my/open-pkm-directory` | Ouvrir dossier PKM |
| `C-c f d` | `my/open-documents-directory` | Ouvrir documents |
| `C-c f t` | `my/open-downloads-directory` | Ouvrir téléchargements |
| `C-c f s` | `my/setup-template-directory` | Configurer dossier modèles |

### Système de Journal Quotidien
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `<f3>` | `daily-journal-open-today` | Ouvrir journal aujourd'hui |
| `C-c j j` | `daily-journal-open-today` | Ouvrir journal aujourd'hui |
| `C-c j d` | `daily-journal-open-date` | Ouvrir journal date |
| `C-c j l` | `daily-journal-list-entries` | Lister entrées journal |
| `C-c j s` | `daily-journal-search` | Rechercher journal |
| `C-c j y` | `daily-journal-yesterday` | Journal hier |
| `C-c j w` | `daily-journal-week-view` | Vue semaine journal |
| `C-c j i` | `daily-journal-stats` | Statistiques journal |

### Utilitaires Système
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c k a` | `kill/clear-all-caches` | Nettoyer tous les caches |
| `C-c k z` | `kill/kill-other-buffers` | Fermer autres buffers |
| `C-c q r` | `workspace/restart-with-state` | Redémarrer avec état |

### Gestion de Configuration
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-c o i` | `my/org-config-info` | Info configuration org |
| `C-c o r` | `my/reload-org-modules` | Recharger modules org |
| `M-x reload-my-init` | `reload-my-init` | Recharger configuration complète |

---

## Raccourcis Spécifiques aux Modes

### Marginalia (Minibuffer)
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `M-A` | `marginalia-cycle` | Cycle annotations marginalia |

### Corfu (Complétion)
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `TAB` | `corfu-next` | Suivant |
| `S-TAB` | `corfu-previous` | Précédent |
| `RET` | `corfu-insert` | Insérer |
| `C-SPC` | `corfu-complete` | Complétion manuelle |
| `C-g` | `corfu-quit` | Quitter |
| `M-d` | `corfu-info-documentation` | Documentation |
| `M-l` | `corfu-info-location` | Localisation |

### Dirvish Mode
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `TAB` | `dirvish-subtree-toggle` | Basculer sous-arbre |
| `q` | `my/dirvish-quit` | Quitter |
| `?` | `dirvish-dispatch` | Dispatch |
| `r` | `revert-buffer` | Actualiser |
| `g` | `my/dirvish-goto-directory` | Aller au dossier |
| `s` | `my/dirvish-search-in-directory` | Rechercher dans dossier |
| `/` | `dired-isearch-filenames` | Recherche noms fichiers |
| `S n` | `dired-sort-toggle-or-edit` | Basculer tri |
| `S d` | Sort by date | Trier par date |
| `S t` | Sort by size | Trier par taille |
| `S e` | Sort by extension | Trier par extension |
| `S f` | `my/dirvish-filter-by-keyword` | Filtrer par mot-clé |
| `RET` | `dired-find-file` | Ouvrir fichier |
| `o` | `my/dirvish-open-external` | Ouvrir externe |
| `c` | `my/dired-do-copy-for-paste` | Copier pour coller |
| `x` | `my/dired-do-cut` | Couper |
| `v` | `dired-do-paste` | Coller |
| `D` | `my/dirvish-delete-no-confirm` | Supprimer sans confirmation |
| `R` | `dired-do-rename` | Renommer |
| `N` | `my/dirvish-new-file` | Nouveau fichier |
| `M` | `my/dirvish-new-directory` | Nouveau dossier |
| `w` | `my/dirvish-copy-path` | Copier chemin |
| `b` | `my/dirvish-add-bookmark` | Ajouter marque-page |
| `SPC` | `dired-mark` | Marquer |
| `U` | `dired-unmark-all-marks` | Démarquer tout |
| `* *` | `dired-mark-executables` | Marquer exécutables |
| `<left>` | `dired-up-directory` | Dossier parent |
| `<right>` | `dired-find-file` | Ouvrir fichier |
| `u` | `dired-up-directory` | Dossier parent |

### Calendar Mode
| Raccourci | Fonction | Description |
|-----------|----------|-------------|
| `C-<left>` | `calendar-backward-week` | Semaine précédente |
| `C-<right>` | `calendar-forward-week` | Semaine suivante |
| `C-<up>` | `calendar-backward-month` | Mois précédent |
| `C-<down>` | `calendar-forward-month` | Mois suivant |
| `M-<left>` | `calendar-backward-month` | Mois précédent |
| `M-<right>` | `calendar-forward-month` | Mois suivant |
| `M-<up>` | `calendar-backward-year` | Année précédente |
| `M-<down>` | `calendar-forward-year` | Année suivante |

---

## Touches de Fonction

| Touche | Fonction | Description |
|--------|----------|-------------|
| `<f1>` | Open inbox.org | Ouvrir inbox |
| `<f3>` | `daily-journal-open-today` | Ouvrir journal aujourd'hui |

---

## Statistiques

Cette configuration Emacs définit approximativement **150+ raccourcis clavier** répartis sur :

- **Raccourcis globaux** : ~120 raccourcis
- **Raccourcis spécifiques aux modes** : ~30 raccourcis  
- **Touches de fonction** : 2 raccourcis
- **Séquences de touches spéciales** : Diverses séquences multi-touches avec préfixe C-c

### Organisation des Préfixes

La configuration est bien organisée avec des motifs de préfixes cohérents :

- **`C-c [lettre]`** : Domaines fonctionnels (org, gestion fichiers, etc.)
- **`C-c [lettre] [lettre]`** : Sous-fonctions dans les domaines
- **Touches de fonction** : Accès rapide aux outils quotidiens
- **Touches spéciales** : Comme `C-l` pour les fonctions fréquemment utilisées

### Domaines Principaux

1. **Org Mode** : Gestion complète des notes et PKM
2. **Navigation Avy** : Navigation rapide et précise dans les buffers
3. **Recherche** : Everything (Windows) + Consult + Grep
4. **Complétion** : Système avancé avec dictionnaires
5. **Gestion Fichiers** : Dirvish avec raccourcis étendus
6. **LaTeX/PDF** : Export et prévisualisation
7. **Journal** : Système de journal quotidien
8. **Images** : Gestion complète des images dans Org
9. **Callouts** : Insertion de blocs spéciaux (notes, warnings, tips)
10. **Transclusion** : Inclusion dynamique de contenu
11. **Utilitaires** : Calculatrice, remplacement texte, système

Cette configuration est optimisée pour un flux de travail PKM (Personal Knowledge Management) efficace avec des raccourcis logiques et mémorisables.