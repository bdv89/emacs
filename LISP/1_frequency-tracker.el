;;; 1_frequency-tracker.el --- Keybinding Frequency Tracking System -*- lexical-binding: t; -*-

;; Author: Auto-generated for PKM Configuration
;; Keywords: keybindings, statistics, tracking
;; Version: 1.0

;;; Commentary:

;; This module provides automatic tracking of keybinding usage frequency
;; and generates statistics that are automatically written to frequence.md.
;; 
;; The system extends the existing prescient functionality to track
;; individual keybinding usage patterns and protect frequently used shortcuts.

;;; Code:

;; (require 'prescient) ; Temporairement commenté - package non installé

;;; Variables

(defvar my/keybinding-stats (make-hash-table :test 'equal)
  "Hash table for tracking keybinding usage frequency.")

(defvar my/keybinding-stats-file 
  (expand-file-name "keybinding-stats.el" user-emacs-directory)
  "File to persist keybinding statistics.")

(defvar my/frequency-md-file 
  (expand-file-name "LISP/frequence.md" user-emacs-directory)
  "Path to frequence.md file for automatic updates.")

(defvar my/protected-keybindings 
  '(("C-c a" . "org-agenda")
    ("C-c c" . "org-capture")
    ("C-c n f" . "org-roam-node-find")
    ("M-j" . "avy-goto-char-timer")
    ("<f1>" . "open-inbox")
    ("<f3>" . "daily-journal-today")
    ("C-c s b" . "consult-buffer")
    ("C-c s l" . "consult-line")
    ("C-c n i" . "org-roam-node-insert")
    ("C-c q r" . "workspace/restart-with-state"))
  "List of protected keybindings that should not be modified.")

;;; Core Tracking Functions

(defun my/track-keybinding (key-sequence command)
  "Track usage of KEY-SEQUENCE for COMMAND."
  (when (and key-sequence command)
    (let* ((key-str (if (stringp key-sequence)
                        key-sequence
                      (key-description key-sequence)))
           (cmd-str (if (stringp command)
                        command
                      (symbol-name command)))
           (entry-key (format "%s (%s)" key-str cmd-str))
           (current-count (gethash entry-key my/keybinding-stats 0)))
      (puthash entry-key (1+ current-count) my/keybinding-stats)
      (my/save-keybinding-stats))))

(defun my/save-keybinding-stats ()
  "Save keybinding statistics to file."
  (with-temp-file my/keybinding-stats-file
    (insert ";; Keybinding usage statistics\n")
    (insert (format ";; Last updated: %s\n\n" (current-time-string)))
    (insert "(setq my/keybinding-stats-data\n")
    (insert "      '(")
    (maphash (lambda (key count)
               (insert (format "\n        (%S . %d)" key count)))
             my/keybinding-stats)
    (insert "))\n")))

(defun my/load-keybinding-stats ()
  "Load keybinding statistics from file."
  (when (file-exists-p my/keybinding-stats-file)
    (load-file my/keybinding-stats-file)
    (when (boundp 'my/keybinding-stats-data)
      (clrhash my/keybinding-stats)
      (dolist (item my/keybinding-stats-data)
        (puthash (car item) (cdr item) my/keybinding-stats)))))

;;; Statistics Display Functions

(defun my/keybinding-stats-display ()
  "Display keybinding usage statistics in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Keybinding Stats*")
    (erase-buffer)
    (insert "=== STATISTIQUES D'UTILISATION DES RACCOURCIS ===\n\n")
    
    ;; Sort by frequency
    (let ((stats-list '()))
      (maphash (lambda (key count) 
                 (push (cons key count) stats-list)) 
               my/keybinding-stats)
      (setq stats-list (sort stats-list (lambda (a b) (> (cdr a) (cdr b)))))
      
      ;; Display top 20
      (insert "TOP 20 RACCOURCIS LES PLUS UTILISÉS:\n")
      (insert "=" (make-string 50 ?=) "\n\n")
      (let ((rank 1))
        (dolist (item (seq-take stats-list 20))
          (insert (format "%2d. %-40s - %d fois\n" 
                          rank (car item) (cdr item)))
          (setq rank (1+ rank))))
      
      ;; Display protected keybindings status
      (insert "\n\nSTATUT DES RACCOURCIS PROTÉGÉS:\n")
      (insert "=" (make-string 50 ?=) "\n\n")
      (dolist (protected my/protected-keybindings)
        (let* ((key (car protected))
               (cmd (cdr protected))
               (search-key (format "%s (%s)" key cmd))
               (count (gethash search-key my/keybinding-stats 0)))
          (insert (format "%-20s : %d fois\n" key count))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; Automatic Update Functions

(defun my/update-frequency-md ()
  "Update frequence.md with current statistics."
  (interactive)
  (when (file-exists-p my/frequency-md-file)
    (let ((stats-list '())
          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
      
      ;; Collect and sort statistics
      (maphash (lambda (key count) 
                 (push (cons key count) stats-list)) 
               my/keybinding-stats)
      (setq stats-list (sort stats-list (lambda (a b) (> (cdr a) (cdr b)))))
      
      ;; Read current frequence.md
      (with-temp-file my/frequency-md-file
        (insert-file-contents my/frequency-md-file)
        (goto-char (point-min))
        
        ;; Update timestamp
        (when (re-search-forward "\\*\\*Dernière mise à jour\\*\\* : \\[.*\\]" nil t)
          (replace-match (format "**Dernière mise à jour** : %s" timestamp)))
        
        ;; Update TOP 10 section
        (when (re-search-forward "```\n\\[DONNÉES AUTOMATIQUES.*?\n\\(.*?\n\\)*?```" nil t)
          (let ((top-10 (seq-take stats-list 10))
                (replacement "```\n"))
            (let ((rank 1))
              (dolist (item top-10)
                (let* ((full-key (car item))
                       (count (cdr item))
                       ;; Extract key and command from "key (command)" format
                       (parts (split-string full-key " ("))
                       (key (car parts))
                       (cmd (if (cdr parts) 
                               (string-remove-suffix ")" (cadr parts))
                             "unknown")))
                  (setq replacement 
                        (concat replacement
                                (format "%d. %-8s (%-25s) - %d utilisations\n" 
                                        rank key cmd count)))
                  (setq rank (1+ rank)))))
            (setq replacement (concat replacement "```"))
            (replace-match replacement)))))))

;;; Integration with Existing Systems

(defun my/track-command-execution ()
  "Track every command execution with its key sequence."
  (let ((keys (this-command-keys))
        (command this-command))
    (when (and keys command 
               (> (length keys) 0)
               (not (memq command '(self-insert-command
                                   digit-argument
                                   negative-argument
                                   universal-argument
                                   keyboard-quit))))
      (my/track-keybinding keys command))))

(defun my/setup-frequency-tracking ()
  "Set up keybinding frequency tracking."
  (interactive)
  
  ;; Load existing statistics
  (my/load-keybinding-stats)
  
  ;; Add global hook to track all commands
  (add-hook 'post-command-hook #'my/track-command-execution)
  
  ;; Set up auto-save timer (save every 30 seconds for live updates)
  (run-at-time 30 30 #'my/save-keybinding-stats)
  
  ;; Set up hourly update of frequence.md (plus live)
  (run-at-time "00:01" (* 60 60) #'my/update-frequency-md))

;;; Interactive Commands

(defun my/frequency-stats ()
  "Show frequency statistics."
  (interactive)
  (my/keybinding-stats-display))

(defun my/frequency-reset ()
  "Reset frequency statistics."
  (interactive)
  (when (y-or-n-p "Reset all keybinding statistics? ")
    (clrhash my/keybinding-stats)
    (my/save-keybinding-stats)
    (message "Keybinding statistics reset.")))

(defun my/frequency-backup ()
  "Backup current frequency statistics."
  (interactive)
  (let ((backup-file (format "%s.backup-%s" 
                             my/keybinding-stats-file
                             (format-time-string "%Y%m%d-%H%M%S"))))
    (copy-file my/keybinding-stats-file backup-file)
    (message "Statistics backed up to %s" backup-file)))

;;; Key Bindings
;; MOVED TO keybindings.el - All keybindings centralized
;; C-c F u/s/r/b → my/update-frequency-md, my/frequency-stats, etc.

;;; Shutdown Hook for Auto-Save

(defun my/frequency-save-on-exit ()
  "Save frequency statistics on Emacs exit."
  (my/save-keybinding-stats)
  (my/update-frequency-md)
  (message "Frequency statistics saved on exit"))

;;; Activation

;; Initialize frequency tracking
(my/setup-frequency-tracking)

;; Add exit hook for automatic saving
(add-hook 'kill-emacs-hook #'my/frequency-save-on-exit)

(provide '1_frequency-tracker)

;;; 1_frequency-tracker.el ends here
