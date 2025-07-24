# Statistiques d'Utilisation des Raccourcis - Emacs PKM

Ce fichier contient les statistiques d'utilisation des raccourcis clavier, automatiquement mises à jour par Emacs.

## Mise à jour automatique

**Dernière mise à jour** : 2025-07-16 22:18:44

**Commande de mise à jour** : `C-c f u` (my/update-frequency-stats)

**Mise à jour automatique** : Toutes les 24h ou à chaque redémarrage d'Emacs

---

## Raccourcis par Fréquence d'Utilisation

### TOP 10 - Raccourcis les plus utilisés

```
```

### Raccourcis par Domaine Fonctionnel

#### Org Mode Base (C-c prefix)
```
```

#### Org-roam (C-c n prefix)
```
```

#### Navigation Avy
```
```

#### Recherche et Complétion
```
```

#### Utilitaires Système
```
```

#### Touches de Fonction
```
```

---

## Analyse des Tendances

### Raccourcis en Croissance (Dernière Semaine)
```
```

### Raccourcis en Déclin (Dernière Semaine)
```
```

### Raccourcis Jamais Utilisés (Candidats à Suppression)
```
```

---

## Recommandations d'Optimisation

### 1. Raccourcis à Protéger Absolument
Basé sur les statistiques actuelles, ces raccourcis sont **critiques** et ne doivent jamais être modifiés :

```
```

### 2. Raccourcis Candidates à Réassignation
Raccourcis peu utilisés qui pourraient être réassignés à de nouvelles fonctions :

```
```

### 3. Suggestions d'Amélioration
- **Raccourcis similaires** : Regrouper les fonctions similaires sous des préfixes cohérents
- **Raccourcis plus courts** : Attribuer des raccourcis plus courts aux fonctions les plus utilisées
- **Conflits potentiels** : Vérifier les conflits avec les raccourcis système

---

## Données Techniques

### Source des Statistiques
- **Système prescient** : `prescient-save.el` (fréquence des commandes)
- **Org-roam cache** : `my/org-roam-recent-nodes` (nœuds récents)
- **Daily journal** : Statistiques de journaling
- **Tracking personnalisé** : `my/keybinding-stats` (si implémenté)

### Commandes de Gestion
- `C-c f u` : Mettre à jour les statistiques dans ce fichier
- `C-c f r` : Réinitialiser les compteurs
- `C-c f s` : Afficher les statistiques en temps réel
- `C-c f b` : Sauvegarder les statistiques actuelles

### Fichiers de Données
- `prescient-save.el` : Données prescient
- `keybinding-stats.el` : Statistiques personnalisées (si créé)
- `frequency-backup.el` : Sauvegardes des statistiques

---

## Implementation Status

### ✅ Terminé
- Structure de base du fichier frequence.md
- Intégration avec système prescient existant
- Raccourcis de base identifiés

### 🔄 En Cours
- Fonction automatique de mise à jour
- Tracking des raccourcis individuels
- Système de sauvegarde

### 📋 À Faire
- Implémentation du tracking automatique
- Cron job pour mise à jour périodique
- Interface de visualisation des statistiques
- Système d'alerte pour conflits de raccourcis

---

**Note** : Ce fichier est automatiquement généré et mis à jour par Emacs. Ne pas modifier manuellement les sections marquées `[DONNÉES AUTOMATIQUES]`.