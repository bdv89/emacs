;;; ui.el --- UI and visual configuration -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Basic UI Settings
;; --------------------------------------------------
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)  ; Désactive le bell
(global-display-line-numbers-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(winner-mode 1)
;; (windmove-default-keybindings)  ; Retiré - winum suffit pour navigation
(defalias 'list-buffers 'ibuffer)

;; Supprimer warning text-scale-increase
(setq ad-redefinition-action 'accept)

;; --------------------------------------------------
;; Windows-specific font configuration
;; --------------------------------------------------
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family "Cutive Mono" :height 108)
  
  ;; Configure emoji fonts for Windows
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend)
  
  ;; Specific Unicode blocks for emoji
  (dolist (range '((#x1f600 . #x1f64f) (#x1f300 . #x1f5ff) 
                   (#x1f680 . #x1f6ff) (#x1f7e0 . #x1f7ef)))
    (set-fontset-font t range "Segoe UI Emoji")))

;; --------------------------------------------------
;; Text editing behavior
;; --------------------------------------------------
(when (display-graphic-p)
  (global-unset-key (kbd "C-z")))
(global-set-key (kbd "C-z") 'undo)

(add-hook 'text-mode-hook #'visual-line-mode)
(setq-default word-wrap t)

;; --------------------------------------------------
;; UTF-8 Configuration
;; --------------------------------------------------
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; --------------------------------------------------
;; Window and frame behavior
;; --------------------------------------------------
;; Note: Configuration des chemins gérée par no-littering dans 1_packages.el
;; Les backups et auto-saves sont automatiquement organisés dans ~/.emacs.d/var/

;; --------------------------------------------------
;; Auto-save configuration - KISS/UNIX approach
;; --------------------------------------------------
;; Timer pour sauvegarde automatique toutes les 10 secondes avec refresh planning
(defun my/auto-save-and-refresh-planning ()
  "Sauvegarde automatique simplifiée - seulement save-some-buffers."
  ;; Sauvegarder tous les buffers (C-x s t)
  (save-some-buffers t)
  
  ;; Note: Sync planning supprimée du timer automatique selon demande utilisateur
  ;; La sync se fait maintenant uniquement via keybindings manuels C-c C-s/C-c C-r
  )

;; Timer moins fréquent (30s au lieu de 10s) car système plus intelligent
(run-with-timer 30 30 'my/auto-save-and-refresh-planning)

(provide 'ui)
;;; ui.el ends here