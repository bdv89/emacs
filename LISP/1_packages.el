;;; packages.el --- Centralized package management -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Package Management Setup
;; --------------------------------------------------
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; --------------------------------------------------
;; Performance et organisation (à charger EN PREMIER)
;; --------------------------------------------------
;; Optimisation garbage collection
(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

;; Organisation des fichiers de configuration
(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; --------------------------------------------------
;; PKM Database packages
;; --------------------------------------------------
(use-package org-ql
  :ensure t)

(use-package org-super-agenda
  :ensure t
  :config (org-super-agenda-mode 1))

;; Package pour org-modern (mise en forme moderne)
(use-package org-modern
  :ensure t
  :after org)

;; Package pour avy (navigation rapide par saut de caractères)
(use-package avy
  :ensure t)

;; Package pour winum (numérotation des fenêtres)
(use-package winum
  :ensure t
  :config
  (winum-mode 1)
  (setq winum-scope 'frame-local)           ; Numérotation par frame
  (setq winum-auto-assign-0-to-minibuffer t) ; Minibuffer = 0
  ;; Keybindings centralisés dans keybindings.el
  )

;; Package pour ultra-scroll (défilement fluide haute performance)
(use-package ultra-scroll
  :ensure t
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; Package pour expand-region (sélection intelligente par unités sémantiques)
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; Package pour smartparens (gestion intelligente des paires)
(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (text-mode . smartparens-mode))
  :config
  (require 'smartparens-config)  ; Configurations par défaut
  
  ;; Org-mode - symboles sélectionnés
  (sp-local-pair 'org-mode "*" "*")          ; *gras*
  (sp-local-pair 'org-mode "_" "_")          ; _souligné_
  (sp-local-pair 'org-mode "~" "~")          ; ~verbatim~
  
  ;; Liens et références
  (sp-local-pair 'org-mode "[[" "]]")        ; [[liens]]
  (sp-local-pair 'org-mode "<<" ">>")        ; <<targets>>
  
  ;; Blocs de code
  (sp-local-pair 'org-mode "#+BEGIN_SRC" "#+END_SRC")
  (sp-local-pair 'org-mode "#+BEGIN_EXAMPLE" "#+END_EXAMPLE")
  (sp-local-pair 'org-mode "#+BEGIN_QUOTE" "#+END_QUOTE")
  
  ;; Math mode
  (sp-local-pair 'org-mode "$" "$")          ; $math$
  
  ;; Désactiver les paires problématiques
  (sp-local-pair 'org-mode "'" nil :actions nil)  ; Éviter auto-quote
  
  ;; Keybindings centralisés dans keybindings.el
  )


;; --------------------------------------------------
;; Ivy/Counsel packages pour Everything shortcuts
;; --------------------------------------------------
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t))

(use-package counsel
  :ensure t
  :after ivy
  :diminish counsel-mode
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  ;; Keybindings centralisés dans keybindings.el
  )

;; --------------------------------------------------
;; Core completion and search packages
;; --------------------------------------------------
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-resize t
        vertico-count 15))

(use-package marginalia
  :pin gnu
  :init (marginalia-mode)
  :config
  (setq marginalia-annotators 
        '(marginalia-annotators-heavy
          marginalia-annotators-light
          nil))
  ;; Keybindings centralisés dans keybindings.el
  )

(use-package consult
  ;; Keybindings centralisés dans keybindings.el
  :config
  ;; Configuration pour recherche live à partir de 3 caractères
  (setq consult-async-min-input 3        ; Live search dès 3 caractères
        consult-async-refresh-delay 0.15  ; Actualisation 150ms
        consult-async-input-throttle 0.2  ; Throttle saisie
        consult-async-input-debounce 0.1) ; Anti-rebond
  
  ;; Fix PATH pour WSL/Windows
  (when (eq system-type 'gnu/linux)
    (setenv "PATH" (concat "/usr/bin:" (getenv "PATH")))))

;; --------------------------------------------------
;; Autocompletion packages
;; --------------------------------------------------

(use-package corfu)
(use-package cape)

;; --------------------------------------------------
;; Image Handling Packages
;; --------------------------------------------------
(use-package org
  :ensure t)

(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-image-dir "~/00_PKM/images"))


;; --------------------------------------------------
;; UI Enhancement packages
;; --------------------------------------------------
(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-display-style 'unicode))

(provide 'packages)

;; --------------------------------------------------
;; Dirvish
;; --------------------------------------------------

(use-package dirvish
  :ensure t
  :init (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort file-time " " file-size) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-size file-time collapse subtree-state vc-state))
  
  ;; Keybindings centralisés dans keybindings.el
  )

;;; packages.el ends here