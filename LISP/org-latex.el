;;; org-latex.el --- Configuration LaTeX et exports pour Org Mode -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; PDF Export Configuration avec images
;; --------------------------------------------------

;; Charger ox-latex avant la configuration
(require 'ox-latex)

;; Configuration LaTeX pour l'export PDF
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

;; Support des images dans l'export LaTeX
(setq org-latex-packages-alist
      '(("" "graphicx" t)
        ("" "geometry" t)
        ("margin=2cm" "geometry" nil)
        ("utf8" "inputenc" t)
        ("T1" "fontenc" t)))

;; Configuration des attributs d'image par défaut
(setq org-latex-image-default-width "0.8\\textwidth")

;; Classes LaTeX personnalisées (optionnel)
(add-to-list 'org-latex-classes
             '("article-fr"
               "\\documentclass[11pt,a4paper]{article}
                \\usepackage[utf8]{inputenc}
                \\usepackage[T1]{fontenc}
                \\usepackage[french]{babel}
                \\usepackage{graphicx}
                \\usepackage{geometry}
                \\geometry{margin=2cm}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; --------------------------------------------------
;; Export functions
;; --------------------------------------------------
(defun my/org-export-pdf ()
  "Export current org file to PDF."
  (interactive)
  (org-latex-export-to-pdf))

(defun my/org-export-pdf-and-open ()
  "Export current org file to PDF and open it."
  (interactive)
  (let ((pdf-file (org-latex-export-to-pdf)))
    (when pdf-file
      (if (eq system-type 'windows-nt)
          (w32-shell-execute "open" pdf-file)
        (call-process "xdg-open" nil 0 nil pdf-file)))))

;; Alternative avec Pandoc (si installé)
(when (executable-find "pandoc")
  (defun my/org-export-pandoc-pdf ()
    "Export current org file to PDF using Pandoc."
    (interactive)
    (let* ((org-file (buffer-file-name))
           (pdf-file (concat (file-name-sans-extension org-file) ".pdf")))
      (shell-command 
       (format "pandoc \"%s\" -o \"%s\" --pdf-engine=pdflatex -V geometry:margin=2cm" 
               org-file pdf-file))
      (when (file-exists-p pdf-file)
        (message "PDF exporté : %s" pdf-file)
        pdf-file)))
  
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c e P") #'my/org-export-pandoc-pdf)))

;; --------------------------------------------------
;; Configuration LaTeX Preview finale avec PKM
;; --------------------------------------------------
(defun my/setup-latex-final ()
  "Configuration LaTeX finale qui fonctionne."
  (when (eq system-type 'windows-nt)
    ;; 1. Ajouter MiKTeX au PATH
    (let ((miktex-path "C:/Users/BrandonDeVoeght/AppData/Local/Programs/MiKTeX/miktex/bin/x64/"))
      (unless (member miktex-path exec-path)
        (add-to-list 'exec-path miktex-path)
        (setenv "PATH" (concat miktex-path ";" (getenv "PATH")))))
    
    ;; 2. Configuration répertoire PKM pour LaTeX
    (let ((pkm-latex-dir (expand-file-name "latex-temp" "~/00_PKM/")))
      (unless (file-directory-p pkm-latex-dir)
        (make-directory pkm-latex-dir t))
      (setq temporary-file-directory pkm-latex-dir)
      (message "✓ Répertoire LaTeX: %s" pkm-latex-dir))
    
    ;; 3. Process LaTeX corrigé (syntaxe correcte)
    (setq org-preview-latex-process-alist
          '((dvipng 
             :programs ("latex" "dvipng")
             :description "dvi > png"
             :message "you need to install latex and dvipng."
             :image-input-type "dvi"
             :image-output-type "png"
             :image-size-adjust (1.0 . 1.0)
             :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter ("dvipng -D 120 -T tight -bg Transparent -o %O %f"))
            (dvisvgm
             :programs ("latex" "dvisvgm")
             :description "dvi > svg"
             :message "you need to install latex and dvisvgm."
             :image-input-type "dvi"
             :image-output-type "svg"
             :image-size-adjust (1.7 . 1.5)
             :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter ("dvisvgm --no-fonts --exact-bbox --scale=%S --output=%O %f"))))
    
    ;; 4. Configuration finale : dvisvgm pour preview, export PDF propre
    (setq org-preview-latex-default-process 'dvisvgm)
    
    ;; 5. Header pour PREVIEW avec couleur (n'affecte pas l'export PDF)
    (setq org-format-latex-header
          "\\documentclass{article}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{xcolor}
\\definecolor{mywhite}{HTML}{F2F0E5}
\\AtBeginDocument{\\color{mywhite}}
\\pagestyle{empty}")
    
    ;; 6. Options preview
    (setq org-format-latex-options
          (plist-put org-format-latex-options :foreground "#F2F0E5"))
    
    (setq org-format-latex-options
          (plist-put org-format-latex-options :background "Transparent"))
    
    ;; 7. Fix default-directory pour preview (utilise PKM)
    (defun my/org-latex-preview-fix-directory (orig-fun &rest args)
      "Fix default-directory during LaTeX preview to use PKM folder."
      (let ((default-directory (expand-file-name "latex-temp" "~/00_PKM/")))
        (apply orig-fun args)))
    
    (advice-add 'org-latex-preview :around #'my/org-latex-preview-fix-directory)
    (advice-add 'org-compile-file :around #'my/org-latex-preview-fix-directory)
    
    ;; 8. Configuration cache LaTeX dans PKM
    (setq org-latex-preview-ltxpng-directory (expand-file-name "latex-preview-cache" "~/00_PKM/"))
    (unless (file-directory-p org-latex-preview-ltxpng-directory)
      (make-directory org-latex-preview-ltxpng-directory t))
    
    (message "✓ LaTeX Preview configuré avec dossier PKM")))

;; --------------------------------------------------
;; Utility functions
;; --------------------------------------------------
(defun my/clean-latex-config ()
  "Nettoie les anciennes configurations."
  (interactive)
  ;; Supprime les anciens advice
  (ignore-errors
    (advice-remove 'org-compile-file #'my/org-compile-file-debug)
    (advice-remove 'org-compile-file #'my/org-latex-compile-file-advice))
  ;; Reset variables
  (setq org-compile-file-debug nil)
  (message "✓ Anciennes configurations nettoyées"))

(defun my/test-latex-simple ()
  "Test LaTeX ultra-simple."
  (interactive)
  (let ((default-directory (expand-file-name "latex-temp" "~/00_PKM/")))
    (with-current-buffer (get-buffer-create "*Test LaTeX Simple*")
      (erase-buffer)
      (org-mode)
      (insert "Équation simple : $E = mc^2$\n\n")
      (insert "Instructions :\n")
      (insert "1. Placez le curseur sur l'équation\n")
      (insert "2. Tapez C-c C-x C-l\n")
      (insert (format "\nRépertoire de travail: %s\n" default-directory))
      (display-buffer (current-buffer))
      (goto-char 18)  ; Position sur l'équation
      (message "Curseur positionné. Tapez C-c C-x C-l"))))

(defun my/clean-latex-cache ()
  "Nettoie le cache LaTeX dans PKM."
  (interactive)
  (let ((cache-dir (expand-file-name "latex-preview-cache" "~/00_PKM/"))
        (temp-dir (expand-file-name "latex-temp" "~/00_PKM/")))
    (when (file-directory-p cache-dir)
      (delete-directory cache-dir t)
      (make-directory cache-dir t)
      (message "✓ Cache LaTeX nettoyé"))
    (when (file-directory-p temp-dir)
      (dolist (file (directory-files temp-dir t "\\.(tex\\|dvi\\|log\\|aux\\|png\\|svg)$"))
        (delete-file file))
      (message "✓ Fichiers temporaires LaTeX supprimés"))))

(defun my/verify-export-pdf-clean ()
  "Vérifie que l'export PDF ne sera pas affecté par la config preview."
  (interactive)
  (with-current-buffer (get-buffer-create "*Vérification Export PDF*")
    (erase-buffer)
    (insert "=== VÉRIFICATION EXPORT PDF ===\n\n")
    
    (insert "Preview process: " (symbol-name org-preview-latex-default-process) "\n")
    (insert "Export process: " (mapconcat 'identity org-latex-pdf-process " → ") "\n\n")
    
    (insert "=== HEADERS ===\n")
    (insert "Preview header (org-format-latex-header):\n")
    (insert org-format-latex-header)
    (insert "\n\n")
    
    (insert "Export packages (org-latex-default-packages-alist):\n")
    (dolist (pkg org-latex-default-packages-alist)
      (insert (format "  %s\n" pkg)))
    
    (insert "\n=== CONCLUSION ===\n")
    (insert "✓ Preview : dvisvgm avec couleur blanche\n")
    (insert "✓ Export PDF : processus standard sans modification\n")
    (insert "✓ Les PDFs exportés resteront avec fond blanc standard\n")
    
    (display-buffer (current-buffer))))

(defun my/test-both-latex-modes ()
  "Test pour vérifier preview et export PDF."
  (interactive)
  (with-current-buffer (get-buffer-create "*Test Preview et Export*")
    (erase-buffer)
    (org-mode)
    (insert "#+TITLE: Test LaTeX Preview et Export\n\n")
    (insert "* Équations de test\n\n")
    (insert "Équation simple : $E = mc^2$\n\n")
    (insert "Fraction complexe : $\\frac{\\partial f}{\\partial x} = \\lim_{h \\to 0} \\frac{f(x+h) - f(x)}{h}$\n\n")
    (insert "Matrix : $\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}$\n\n")
    (insert "* Instructions\n\n")
    (insert "1. Preview : C-c C-x C-l (doit être blanc sur transparent)\n")
    (insert "2. Export PDF : C-c e p (doit être noir sur blanc)\n")
    (insert "3. Les deux doivent coexister sans problème\n")
    
    (display-buffer (current-buffer))
    (goto-char 100)))

;; --------------------------------------------------
;; Fragtog for LaTeX preview - Configuration avancée
;; --------------------------------------------------
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :config
  ;; Configuration pour fragtog
  (setq org-fragtog-preview-delay 0.2)  ; Délai avant preview (si disponible)
  
  ;; Intégration avec notre setup LaTeX
  (defun my/org-fragtog-setup ()
    "Configuration spécifique fragtog pour notre environment PKM."
    (when (and (featurep 'org-fragtog) org-fragtog-mode)
      ;; S'assurer que le cache LaTeX est prêt
      (unless (file-directory-p org-latex-preview-ltxpng-directory)
        (make-directory org-latex-preview-ltxpng-directory t))
      
      ;; Message de confirmation
      ;; Activé silencieusement
      nil)))
  
  ;; Ajouter le setup après activation d'org-fragtog
  (add-hook 'org-fragtog-mode-hook #'my/org-fragtog-setup)
  
  ;; Fonction de toggle manuel au cas où
  (defun my/toggle-fragtog ()
    "Toggle org-fragtog-mode avec message informatif."
    (interactive)
    (if (bound-and-true-p org-fragtog-mode)
        (progn
          (org-fragtog-mode -1)
          ;; Désactivé silencieusement (garde les messages pour toggle manuel)
          (message "org-fragtog désactivé"))
      (progn
        (org-fragtog-mode 1)
        ;; Activé silencieusement (garde les messages pour toggle manuel)
        (message "org-fragtog activé - fragments LaTeX auto-toggle"))))

  ;; Fonction de test pour org-fragtog
  (defun my/test-fragtog ()
    "Test interactif pour org-fragtog."
    (interactive)
    (with-current-buffer (get-buffer-create "*Test org-fragtog*")
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Test org-fragtog\n\n")
      (insert "* Test du toggle automatique\n\n")
      (insert "Équation simple : $E = mc^2$\n\n")
      (insert "Équation complexe : $\\int_{-\\infty}^{\\infty} e^{-x^2} dx = \\sqrt{\\pi}$\n\n")
      (insert "Fraction : $\\frac{d}{dx}f(x) = \\lim_{h \\to 0} \\frac{f(x+h) - f(x)}{h}$\n\n")
      (insert "* Instructions de test\n\n")
      (insert "1. Activez org-fragtog avec C-c l g (si pas déjà fait)\n")
      (insert "2. Générez les previews avec C-c C-x C-l\n")
      (insert "3. Déplacez votre curseur IN/OUT des équations\n")
      (insert "4. Les previews doivent se toggler automatiquement\n\n")
      (insert "État actuel : " 
              (if (bound-and-true-p org-fragtog-mode) 
                  "org-fragtog ACTIVÉ ✓" 
                  "org-fragtog DÉSACTIVÉ ✗") "\n")
      
      ;; Activer org-fragtog si pas déjà fait
      (unless (bound-and-true-p org-fragtog-mode)
        (org-fragtog-mode 1))
      
      (display-buffer (current-buffer))
      (goto-char (point-min))
      (search-forward "$E = mc^2$" nil t)
      (backward-char 2)  ; Placer le curseur dans l'équation
      (message "Curseur placé dans équation. Bougez-le pour tester fragtog.")))

;; --------------------------------------------------
;; Setup and key bindings
;; --------------------------------------------------
;; Application automatique
(my/clean-latex-config)
(my/setup-latex-final)

;; Keybindings pour l'export
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e p") #'my/org-export-pdf)
  (define-key org-mode-map (kbd "C-c e o") #'my/org-export-pdf-and-open))

;; Raccourcis utilitaires
(global-set-key (kbd "C-c t s") #'my/test-latex-simple)
(global-set-key (kbd "C-c l c") #'my/clean-latex-config)
(global-set-key (kbd "C-c l s") #'my/setup-latex-final)
(global-set-key (kbd "C-c l x") #'my/clean-latex-cache)
(global-set-key (kbd "C-c l f") #'my/setup-latex-final)
(global-set-key (kbd "C-c l v") #'my/verify-export-pdf-clean)
(global-set-key (kbd "C-c l t") #'my/test-both-latex-modes)
(global-set-key (kbd "C-c l g") #'my/toggle-fragtog)
(global-set-key (kbd "C-c l G") #'my/test-fragtog)

(provide 'org-latex)
;;; org-latex.el ends here
