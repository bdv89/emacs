;;; workspace.el --- Simple restart with state preservation -*- lexical-binding: t; -*-
;;
;; Filename: workspace.el
;; Description: Simple Emacs restart with automatic state preservation
;; Author: Claude
;; Version: 2.0
;; Keywords: convenience, restart, session
;; License: Public Domain

;;; Commentary:
;; Simple approach to restarting Emacs while preserving state:
;; - C-c q r -> Save current state and restart Emacs cleanly
;; - Uses desktop.el for robust session management
;; - Automatic state restoration on startup

;;; Code:

(require 'desktop)

;; Configure desktop to save session automatically - MINIMAL approach
(setq desktop-dirname (expand-file-name "desktop-sessions" user-emacs-directory)
      desktop-base-file-name "emacs-session"
      desktop-base-lock-name "emacs-session.lock"
      desktop-path (list desktop-dirname)
      desktop-save t
      desktop-auto-save-timeout 30
      desktop-load-locked-desktop t
      ;; COMPLETELY DISABLE lazy loading
      desktop-restore-eager t
      desktop-lazy-verbose nil
      desktop-lazy-idle-delay 999999)

;; Ensure desktop directory exists
(unless (file-exists-p desktop-dirname)
  (make-directory desktop-dirname t))

;; Exclude most buffer types to avoid clutter, but allow special buffers
(setq desktop-modes-not-to-save '(dired-mode info-mode
                                  help-mode completion-list-mode
                                  magit-status-mode magit-log-mode))

;; Only save/restore essential file buffers and org-agenda views
(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|^/tmp/\\|^/var/tmp/\\)")

;; Allow special buffers like *Planning Global* to be saved
(setq desktop-buffers-not-to-save "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\)$")

;; Variable to track special buffers state
(defvar my/special-buffers-state nil
  "List of special buffers that were open before restart.")

;; Save state of special buffers before restart
(defun my/save-special-buffers-state ()
  "Save which special buffers were open."
  (setq my/special-buffers-state nil)
  
  ;; Debug: List all buffers for debugging
  (message "=== DEBUG: All current buffers ===")
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*.*\\*" (buffer-name buf))
      (message "Buffer: '%s'" (buffer-name buf))))
  
  ;; Check Planning Global buffer
  (let ((planning-buffer (get-buffer "*Planning Global*")))
    (message "Checking '*Planning Global*': %s" planning-buffer)
    (when planning-buffer
      (push 'planning-global my/special-buffers-state)
      (message "✓ Planning Global buffer detected for restart")))
  
  ;; Alternative check using variable if loaded
  (when (boundp 'daily-journal-global-planning-buffer)
    (message "Variable daily-journal-global-planning-buffer: '%s'" daily-journal-global-planning-buffer)
    (let ((planning-buffer-var (get-buffer daily-journal-global-planning-buffer)))
      (message "Checking buffer via variable: %s" planning-buffer-var)
      (when planning-buffer-var
        (unless (memq 'planning-global my/special-buffers-state)
          (push 'planning-global my/special-buffers-state))
        (message "✓ Planning Global buffer (via variable) detected for restart"))))
  
  ;; Check org-agenda buffer
  (when (get-buffer "*Org Agenda*")
    (push 'org-agenda my/special-buffers-state)
    (message "✓ Org Agenda buffer detected for restart"))
  
  (message "Final state to save: %s" my/special-buffers-state))

;; Helper function to restore Planning Global without disrupting window layout
(defun my/restore-planning-global-silently ()
  "Restore Planning Global buffer without switching to it or disrupting layout."
  (when (fboundp 'daily-journal-scan-all-tasks)
    (daily-journal-scan-all-tasks))
  (when (boundp 'daily-journal-global-planning-buffer)
    (let ((buffer (get-buffer-create daily-journal-global-planning-buffer)))
      (with-current-buffer buffer
        (when (fboundp 'daily-journal-global-planning-mode)
          (daily-journal-global-planning-mode))
        (when (fboundp 'daily-journal-update-global-planning-buffer)
          (daily-journal-update-global-planning-buffer)))
      ;; Create vertical split manually (like C-x 3) without changing focus
      (let ((existing-window (get-buffer-window buffer)))
        (unless existing-window
          ;; Create a new vertical split and set the buffer
          (let ((current-window (selected-window)))
            (let ((new-window (split-window-right)))
              (set-window-buffer new-window buffer)
              ;; Return focus to original window
              (select-window current-window)))))
      (message "📋 Planning Global restored silently"))))

;; Restore special buffers after desktop load
(defun my/restore-special-buffers ()
  "Restore special buffers that were open before restart."
  (message "🔄 === DEBUG: Starting buffer restoration ===")
  (message "my/special-buffers-state bound: %s" (boundp 'my/special-buffers-state))
  (when (boundp 'my/special-buffers-state)
    (message "my/special-buffers-state value: %s" my/special-buffers-state)
    (when (memq 'planning-global my/special-buffers-state)
      (message "✅ Planning Global should be restored")
      (message "daily-journal-open-global-planning available: %s" (fboundp 'daily-journal-open-global-planning))
      (if (fboundp 'daily-journal-open-global-planning)
          (progn
            (message "⏰ Scheduling Planning Global restoration in 2 seconds...")
            (run-with-timer 2 nil (lambda () 
                                    (message "🚀 Opening Planning Global now!")
                                    (my/restore-planning-global-silently))))
        (message "⚠️ Warning: daily-journal-open-global-planning not available for restore")))
    (when (memq 'org-agenda my/special-buffers-state)
      (message "⏰ Scheduling Org Agenda restoration in 1 second...")
      (run-with-timer 1 nil #'org-agenda-list)))
  (message "🔄 === DEBUG: End buffer restoration ==="))

;; Make sure the variable is saved in desktop session
(add-to-list 'desktop-globals-to-save 'my/special-buffers-state)

;; Add hooks for special buffers
(add-hook 'desktop-save-hook #'my/save-special-buffers-state)
(add-hook 'desktop-after-read-hook #'my/restore-special-buffers)
;; Also try with other hooks in case desktop-after-read-hook doesn't fire
(add-hook 'emacs-startup-hook #'my/restore-special-buffers)
(add-hook 'after-init-hook #'my/restore-special-buffers)

(defun workspace/restart-with-state ()
  "Save current state and restart Emacs cleanly."
  (interactive)
  (message "Saving current session...")
  
  ;; 1. Save all file buffers
  (save-some-buffers t)
  (message "✓ Files saved")
  
  ;; 2. Save frequency statistics before restart
  (when (fboundp 'my/frequency-save-on-exit)
    (my/frequency-save-on-exit))
  (message "✓ Frequency stats saved")
  
  ;; 3. Clear caches
  (when (fboundp 'kill/clear-all-caches)
    (kill/clear-all-caches))
  (message "✓ Caches cleared")
  
  ;; 4. Force save desktop session
  (desktop-save desktop-dirname t)
  (message "✓ Session saved")
  
  ;; 5. Restart Emacs
  (run-with-timer 0.5 nil 
    (lambda ()
      (message "Restarting Emacs...")
      (if (display-graphic-p)
          (progn
            ;; Mark all buffers as unmodified to avoid prompts
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (set-buffer-modified-p nil)))
            ;; Spawn new Emacs process before killing current one
            (if (eq system-type 'windows-nt)
                (call-process "runemacs.exe" nil 0 nil)
              (call-process "emacs" nil 0 nil))
            ;; Kill current process
            (kill-emacs))
        ;; Terminal mode
        (save-buffers-kill-terminal)))))

;; Enable desktop mode for automatic session restoration
(desktop-save-mode 1)

;; Debug function to test buffer detection
(defun workspace/test-buffer-detection ()
  "Test if special buffers are detected correctly."
  (interactive)
  (message "=== Testing buffer detection ===")
  (my/save-special-buffers-state)
  (message "=== End test ==="))

;; Debug function to check restoration state
(defun workspace/check-restore-state ()
  "Check the state of restoration variables."
  (interactive)
  (message "=== Checking restoration state ===")
  (message "my/special-buffers-state bound: %s" (boundp 'my/special-buffers-state))
  (when (boundp 'my/special-buffers-state)
    (message "my/special-buffers-state value: %s" my/special-buffers-state))
  (message "daily-journal-open-global-planning available: %s" (fboundp 'daily-journal-open-global-planning))
  (message "=== End check ==="))

;; Debug function to manually test restoration
(defun workspace/manual-restore ()
  "Manually trigger buffer restoration for testing."
  (interactive)
  (message "=== Manual restoration test ===")
  (my/restore-special-buffers)
  (message "=== Manual restoration complete ==="))

;; Keybinding
;; CONFLIT: C-c q r défini dans init.el avec perspective.el
;; (global-set-key (kbd "C-c q r") #'workspace/restart-with-state)  ; DÉSACTIVÉ
(global-set-key (kbd "C-c q d") #'workspace/restart-with-state)     ; Alternative: C-c q d (desktop)
(global-set-key (kbd "C-c q t") #'workspace/test-buffer-detection)
(global-set-key (kbd "C-c q c") #'workspace/check-restore-state)
(global-set-key (kbd "C-c q m") #'workspace/manual-restore)

(provide 'workspace)
;;; workspace.el ends here