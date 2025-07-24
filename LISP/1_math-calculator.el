;;; math-calculator.el --- Calculateur mathématique simple avec support des unités -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Ce script fournit des fonctions pour évaluer des expressions mathématiques
;; directement dans un buffer Emacs. Il supporte les opérations de base,
;; ainsi que les calculs avec des unités physiques (longueur, masse, volume, etc.),
;; y compris les conversions, additions et multiplications d'unités.
;;
;; Principales corrections par rapport à la version originale :
;; 1.  **Expressions Régulières (Regex) Améliorées** : Les regex pour détecter les nombres
;;     et les opérations avec unités ont été entièrement réécrites pour être plus
;;     précises et robustes. C'était la source principale des bugs.
;; 2.  **Logique de Détection Simplifiée** : La fonction `my/get-math-from-current-line`
;;     a été simplifiée pour éviter les erreurs de filtrage prématuré. Elle se contente
;;     maintenant de récupérer la partie pertinente de la ligne.
;; 3.  **Gestion des Erreurs Améliorée** : Ajout de vérifications pour éviter les erreurs
;;     de type `(stringp, nil)` en s'assurant que les unités sont bien des chaînes de
;;     caractères avant de les utiliser.
;; 4.  **Nettoyage de Code** : Le code a été réorganisé pour une meilleure lisibilité.

;;; Code:

;; --------------------------------------------------
;; Définition des unités
;; --------------------------------------------------

(defvar my/calc-unit-system
  '(;; Longueur (base: mètre)
    ("mm" 0.001) ("cm" 0.01) ("dm" 0.1) ("m" 1) ("km" 1000)
    ("in" 0.0254) ("ft" 0.3048) ("yd" 0.9144) ("mi" 1609.344)

    ;; Surface (base: m²) - Ajout des alias sans exposant
    ("mm²" 1e-6) ("cm²" 1e-4) ("dm²" 1e-2) ("m²" 1) ("km²" 1e6)
    ("mm2" 1e-6) ("cm2" 1e-4) ("dm2" 1e-2) ("m2" 1) ("km2" 1e6)

    ;; Volume (base: m³) - `l` est 1 dm³, donc 0.001 m³
    ("ml" 1e-6) ("cl" 1e-5) ("dl" 1e-4) ("l" 1e-3)
    ("mm³" 1e-9) ("cm³" 1e-6) ("dm³" 1e-3) ("m³" 1) ("km³" 1e9)
    ("mm3" 1e-9) ("cm3" 1e-6) ("dm3" 1e-3) ("m3" 1) ("km3" 1e9)

    ;; Masse (base: kg)
    ("mg" 1e-6) ("g" 1e-3) ("kg" 1) ("t" 1000)
    ("oz" 0.0283495) ("lb" 0.453592)

    ;; Temps (base: seconde)
    ("ms" 0.001) ("s" 1) ("min" 60) ("h" 3600) ("d" 86400))
  "Système d'unités avec facteurs de conversion vers l'unité de base SI.")

(defvar my/calc-unit-dimensions
  '(("mm" length) ("cm" length) ("dm" length) ("m" length) ("km" length)
    ("in" length) ("ft" length) ("yd" length) ("mi" length)

    ("mm²" area) ("cm²" area) ("dm²" area) ("m²" area) ("km²" area)
    ("mm2" area) ("cm2" area) ("dm2" area) ("m2" area) ("km2" area)

    ("ml" volume) ("cl" volume) ("dl" volume) ("l" volume)
    ("mm³" volume) ("cm³" volume) ("dm³" volume) ("m³" volume) ("km³" volume)
    ("mm3" volume) ("cm3" volume) ("dm3" volume) ("m3" volume) ("km3" volume)

    ("mg" mass) ("g" mass) ("kg" mass) ("t" mass)
    ("oz" mass) ("lb" mass)

    ("ms" time) ("s" time) ("min" time) ("h" time) ("d" time))
  "Dimensions physiques associées à chaque unité.")

;; --------------------------------------------------
;; Fonctions utilitaires pour les unités
;; --------------------------------------------------

(defun my/get-unit-factor (unit)
  "Retourne le facteur de conversion pour l'UNITÉ vers sa base."
  (when (stringp unit)
    (cadr (assoc-string unit my/calc-unit-system t))))

(defun my/get-unit-dimension (unit)
  "Retourne la dimension physique de l'UNITÉ."
  (when (stringp unit)
    (cadr (assoc-string unit my/calc-unit-dimensions t))))

;; --------------------------------------------------
;; Nettoyage et analyse des expressions
;; --------------------------------------------------

(defun my/clean-expression (expr)
  "Nettoie et normalise l'expression mathématique EXPR."
  (let ((cleaned expr))
    ;; Normaliser les séparateurs décimaux : virgules -> points
    (setq cleaned (replace-regexp-in-string "," "." cleaned))
    ;; Normaliser les opérateurs de multiplication
    (setq cleaned (replace-regexp-in-string "\\s-*\\([x×]\\)\\s-*" "*" cleaned))
    ;; Gérer le cas "5*3" ou "15*2" (sans espaces) pour calc-eval
    ;; Supporte désormais les nombres entiers ou décimaux de plusieurs chiffres
    (setq cleaned
          (replace-regexp-in-string
           "\\([0-9]+\\(?:\\.[0-9]*\\)?\\)\\*\\([0-9]+\\(?:\\.[0-9]*\\)?\\)"
           "\\1 * \\2"
           cleaned))
    ;; Nettoyer les espaces multiples
    (setq cleaned (replace-regexp-in-string "\\s-+" " " cleaned))
    (string-trim cleaned)))

(defun my/get-math-from-current-line ()
  "Extrait l'expression mathématique de la ligne courante."
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         ;; Enlève un résultat précédent s'il existe (ex: "... = 123.45 kg")
         (expr (if (string-match "\\(.*?\\)\\s-*=.*" line)
                   (match-string 1 line)
                 line)))
    (let ((trimmed (string-trim expr)))
      (if (string-empty-p trimmed)
          nil
        trimmed))))

;; --------------------------------------------------
;; Moteur de calcul avec unités
;; --------------------------------------------------

(defun my/detect-unit-operation (expr)
  "Détecte le type d'opération sur les unités dans EXPR.
Retourne une liste décrivant l'opération ou nil."
  ;; Regex pour un nombre (entier ou flottant)
  (let ((num-re "\\([0-9]+\\(?:\\.[0-9]*\\)?\\)")
        ;; Regex pour une unité
        (unit-re "\\([a-zA-Z°²³]+\\)"))

    (cond
     ;; 1. Conversion: "10 cm to mm", "1 l = ml"
     ((string-match (format "^%s\\s-*%s\\s-*\\(?:to\\|=\\|→\\)\\s-*%s$" num-re unit-re unit-re) expr)
      (list 'conversion
            (cons (string-to-number (match-string 1 expr)) (match-string 2 expr))
            (match-string 3 expr)))

     ;; 2. Opération binaire (addition, soustraction, multiplication)
     ((string-match (format "^%s\\s-*%s\\s-*\\([+-]\\)\\s-*%s\\s-*%s$" num-re unit-re num-re unit-re) expr)
      (list 'add-sub
            (cons (string-to-number (match-string 1 expr)) (match-string 2 expr))
            (match-string 3 expr)
            (cons (string-to-number (match-string 4 expr)) (match-string 5 expr))))

     ((string-match (format "^%s\\s-*%s\\s-*\\([*]\\)\\s-*%s\\s-*%s$" num-re unit-re num-re unit-re) expr)
      (list 'multiply
            (cons (string-to-number (match-string 1 expr)) (match-string 2 expr))
            (cons (string-to-number (match-string 4 expr)) (match-string 5 expr))))

     (t nil))))

(defun my/convert-unit (value from-unit to-unit)
  "Convertit VALUE de FROM-UNIT vers TO-UNIT."
  (let* ((from-dim (my/get-unit-dimension from-unit))
         (to-dim (my/get-unit-dimension to-unit))
         (from-factor (my/get-unit-factor from-unit))
         (to-factor (my/get-unit-factor to-unit)))
    (cond
     ((not (and from-factor to-factor))
      (error "Unité non supportée: %s ou %s" from-unit to-unit))
     ((not (eq from-dim to-dim))
      (error "Conversion impossible entre dimensions: %s -> %s" from-dim to-dim))
     (t (/ (* value from-factor) to-factor)))))

(defun my/multiply-units (unit1 unit2)
  "Calcule le produit de deux unités (ex: cm * cm -> cm²)."
  (let ((dim1 (my/get-unit-dimension unit1))
        (dim2 (my/get-unit-dimension unit2)))
    (cond
     ((and (eq dim1 'length) (eq dim2 'length))
      (let ((base-unit (if (< (length unit1) (length unit2)) unit1 unit2)))
        (concat base-unit "²")))
     ;; Ajouter d'autres règles de multiplication ici (ex: surface * longueur)
     (t (concat unit1 "·" unit2)))))

(defun my/calculate-with-units (expr)
  "Tente de calculer EXPR en tant qu'expression avec unités."
  (let ((operation (my/detect-unit-operation expr)))
    (when operation
      (condition-case err
          (pcase (car operation)
            ('conversion
             (let* ((from-data (cadr operation))
                    (to-unit (caddr operation))
                    (value (car from-data))
                    (from-unit (cdr from-data)))
               (let ((result (my/convert-unit value from-unit to-unit)))
                 (format "%.4g %s" result to-unit))))

            ('add-sub
             (let* ((data1 (cadr operation))
                    (op (caddr operation))
                    (data2 (cadddr operation))
                    (val1 (car data1)) (unit1 (cdr data1))
                    (val2 (car data2)) (unit2 (cdr data2)))
               ;; Convertir la deuxième valeur dans l'unité de la première
               (let* ((converted-val2 (my/convert-unit val2 unit2 unit1))
                      (result (funcall (if (string= op "+") #'+ #'-) val1 converted-val2)))
                 (format "%.4g %s" result unit1))))

            ('multiply
             (let* ((data1 (cadr operation))
                    (data2 (caddr operation))
                    (val1 (car data1)) (unit1 (cdr data1))
                    (val2 (car data2)) (unit2 (cdr data2)))
               (let ((result-val (* val1 val2))
                     (result-unit (my/multiply-units unit1 unit2)))
                 (format "%.4g %s" result-val result-unit)))))
        (error (format "❌ Erreur: %s" (error-message-string err)))))))

;; --------------------------------------------------
;; Fonctions interactives principales
;; --------------------------------------------------

(defun my/calc-evaluate-and-append-result ()
  "Évalue l'expression de la ligne/région et ajoute le résultat."
  (interactive)
  (let* ((expression (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (my/get-math-from-current-line)))
         (result nil))

    (if (not (or (null expression) (string-empty-p expression)))
        (let ((cleaned-expr (my/clean-expression expression)))
          ;; 1. Essayer le calcul avec unités
          (setq result (my/calculate-with-units cleaned-expr))

          ;; 2. Si échec, essayer avec calc-eval standard
          (unless result
            (condition-case err
                (setq result (calc-eval cleaned-expr))
              (error (setq result (format "❌ Erreur calc: %s" (error-message-string err))))))

          ;; Afficher le résultat
          (if (and (stringp result) (not (string-prefix-p "❌" result)))
              (progn
                (end-of-line)
                (insert (format " = %s" result))
                (message "✅ %s = %s" expression result))
            (message "%s" (or result "❌ Expression non reconnue"))))
      (message "❌ Aucune expression mathématique trouvée"))))

(defun my/calc-replace-with-result ()
  "Remplace l'expression de la ligne/région par son résultat."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (expression (buffer-substring-no-properties start end))
         (result nil))

    (if (not (string-empty-p expression))
        (let ((cleaned-expr (my/clean-expression expression)))
          (setq result (or (my/calculate-with-units cleaned-expr)
                           (calc-eval cleaned-expr)))
          (if (and (stringp result) (not (string-prefix-p "❌" result)))
              (progn
                (delete-region start end)
                (insert result)
                (message "✅ Remplacé par: %s" result))
            (message "%s" (or result "❌ Expression non reconnue"))))
      (message "❌ Aucune expression à remplacer"))))

;; --------------------------------------------------
;; Raccourcis et chargement
;; --------------------------------------------------

(global-set-key (kbd "C-c m =") #'my/calc-evaluate-and-append-result)
(global-set-key (kbd "C-c m r") #'my/calc-replace-with-result)

(provide 'math-calculator)

(message "✅ Calculateur mathématique (corrigé) chargé. [C-c m =] pour calculer.")
;;; math-calculator.el ends here
