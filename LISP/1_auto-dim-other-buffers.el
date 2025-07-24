;;; auto-dim-other-buffers.el --- Visually dim inactive buffers -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, faces, windows

;;; Commentary:
;; This package provides a minor mode that dims the faces in inactive buffers,
;; making the current buffer more prominent. This helps reduce visual clutter
;; and improve focus when working with multiple windows.
;;
;; Usage:
;; (require 'auto-dim-other-buffers)
;; (auto-dim-other-buffers-mode 1)

;;; Code:

(require 'face-remap)
(require 'cl-lib)

;; --------------------------------------------------
;; Customization
;; --------------------------------------------------
(defgroup auto-dim-other-buffers nil
  "Visually dim inactive buffers."
  :group 'convenience
  :prefix "auto-dim-other-buffers-")

(defcustom auto-dim-other-buffers-dim-factor 0.4
  "How much to dim inactive buffers.
A factor of 0.0 means completely black, 1.0 means no dimming."
  :type 'float
  :group 'auto-dim-other-buffers)

(defcustom auto-dim-other-buffers-affected-faces 
  '(default mode-line mode-line-inactive header-line)
  "List of faces to dim in inactive buffers."
  :type '(repeat face)
  :group 'auto-dim-other-buffers)

(defcustom auto-dim-other-buffers-never-dim-buffer-functions
  '(minibufferp)
  "List of functions to test if a buffer should never be dimmed.
Each function is called with one argument (the buffer) and should return
non-nil if the buffer should never be dimmed."
  :type '(repeat function)
  :group 'auto-dim-other-buffers)

(defcustom auto-dim-other-buffers-never-dim-buffer-names
  '("*Echo Area 0*" "*Echo Area 1*" "*Minibuf-0*" "*Minibuf-1*")
  "List of buffer names that should never be dimmed."
  :type '(repeat string)
  :group 'auto-dim-other-buffers)

;; --------------------------------------------------
;; Internal variables
;; --------------------------------------------------
(defvar auto-dim-other-buffers--last-buffer nil
  "Last buffer that was active.")

(defvar auto-dim-other-buffers--dim-face nil
  "Face used for dimming.")

(defvar auto-dim-other-buffers--cookie-list nil
  "List of face-remap cookies for dimmed buffers.")

;; --------------------------------------------------
;; Utility functions
;; --------------------------------------------------
(defun auto-dim-other-buffers--color-blend (color1 color2 factor)
  "Blend COLOR1 and COLOR2 with FACTOR.
FACTOR of 0.0 returns COLOR1, 1.0 returns COLOR2."
  (let* ((rgb1 (or (color-name-to-rgb color1) '(0.0 0.0 0.0)))
         (rgb2 (or (color-name-to-rgb color2) '(1.0 1.0 1.0)))
         (r (+ (* (nth 0 rgb1) (- 1.0 factor)) (* (nth 0 rgb2) factor)))
         (g (+ (* (nth 1 rgb1) (- 1.0 factor)) (* (nth 1 rgb2) factor)))
         (b (+ (* (nth 2 rgb1) (- 1.0 factor)) (* (nth 2 rgb2) factor))))
    (color-rgb-to-hex r g b)))

(defun auto-dim-other-buffers--create-dim-face ()
  "Create a face for dimming based on current theme."
  (let* ((default-bg (face-background 'default nil t))
         (default-fg (face-foreground 'default nil t))
         (dimmed-fg (auto-dim-other-buffers--color-blend 
                     default-fg default-bg 
                     auto-dim-other-buffers-dim-factor)))
    
    (unless auto-dim-other-buffers--dim-face
      (setq auto-dim-other-buffers--dim-face 
            (make-face 'auto-dim-other-buffers-dim-face)))
    
    (set-face-attribute auto-dim-other-buffers--dim-face nil
                        :foreground dimmed-fg
                        :background default-bg)))

(defun auto-dim-other-buffers--never-dim-p (buffer)
  "Return t if BUFFER should never be dimmed."
  (or (member (buffer-name buffer) auto-dim-other-buffers-never-dim-buffer-names)
      (cl-some (lambda (func) (funcall func buffer)) 
               auto-dim-other-buffers-never-dim-buffer-functions)))

(defun auto-dim-other-buffers--dim-buffer (buffer)
  "Dim the faces in BUFFER."
  (when (and (buffer-live-p buffer)
             (not (auto-dim-other-buffers--never-dim-p buffer)))
    (with-current-buffer buffer
      (unless (assq buffer auto-dim-other-buffers--cookie-list)
        (let ((cookies '()))
          (dolist (face auto-dim-other-buffers-affected-faces)
            (push (face-remap-add-relative face auto-dim-other-buffers--dim-face)
                  cookies))
          (push (cons buffer cookies) auto-dim-other-buffers--cookie-list))))))

(defun auto-dim-other-buffers--undim-buffer (buffer)
  "Remove dimming from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((entry (assq buffer auto-dim-other-buffers--cookie-list)))
        (dolist (cookie (cdr entry))
          (face-remap-remove-relative cookie))
        (setq auto-dim-other-buffers--cookie-list
              (delq entry auto-dim-other-buffers--cookie-list))))))

(defun auto-dim-other-buffers--undim-all-buffers ()
  "Remove dimming from all buffers."
  (dolist (entry auto-dim-other-buffers--cookie-list)
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (dolist (cookie (cdr entry))
          (face-remap-remove-relative cookie)))))
  (setq auto-dim-other-buffers--cookie-list nil))

;; --------------------------------------------------
;; Core functions
;; --------------------------------------------------
(defun auto-dim-other-buffers--buffer-list-update ()
  "Update dimming when buffer list changes."
  (when auto-dim-other-buffers-mode
    (let ((current-buffer (current-buffer)))
      (unless (eq current-buffer auto-dim-other-buffers--last-buffer)
        ;; Undim the previously active buffer if it's not the current one
        (when (and auto-dim-other-buffers--last-buffer
                   (buffer-live-p auto-dim-other-buffers--last-buffer)
                   (not (eq auto-dim-other-buffers--last-buffer current-buffer)))
          (auto-dim-other-buffers--dim-buffer auto-dim-other-buffers--last-buffer))
        
        ;; Undim the current buffer
        (auto-dim-other-buffers--undim-buffer current-buffer)
        
        ;; Update last buffer
        (setq auto-dim-other-buffers--last-buffer current-buffer)))))

(defun auto-dim-other-buffers--focus-in ()
  "Handle focus-in events."
  (when auto-dim-other-buffers-mode
    (auto-dim-other-buffers--buffer-list-update)))

(defun auto-dim-other-buffers--focus-out ()
  "Handle focus-out events."
  ;; We could dim all buffers when Emacs loses focus, but that might be annoying
  )

;; --------------------------------------------------
;; Mode definition
;; --------------------------------------------------
;;;###autoload
(define-minor-mode auto-dim-other-buffers-mode
  "Visually dim inactive buffers."
  :global t
  :group 'auto-dim-other-buffers
  :lighter " Dim"
  (if auto-dim-other-buffers-mode
      (progn
        ;; Enable mode
        (auto-dim-other-buffers--create-dim-face)
        
        ;; Initial setup: dim all buffers except current
        (let ((current-buffer (current-buffer)))
          (dolist (buffer (buffer-list))
            (unless (eq buffer current-buffer)
              (auto-dim-other-buffers--dim-buffer buffer)))
          (setq auto-dim-other-buffers--last-buffer current-buffer))
        
        ;; Add hooks
        (add-hook 'buffer-list-update-hook #'auto-dim-other-buffers--buffer-list-update)
        (add-hook 'focus-in-hook #'auto-dim-other-buffers--focus-in)
        (add-hook 'focus-out-hook #'auto-dim-other-buffers--focus-out)
        
        (message "Auto-dim-other-buffers mode enabled"))
    
    ;; Disable mode
    (auto-dim-other-buffers--undim-all-buffers)
    (setq auto-dim-other-buffers--last-buffer nil)
    
    ;; Remove hooks
    (remove-hook 'buffer-list-update-hook #'auto-dim-other-buffers--buffer-list-update)
    (remove-hook 'focus-in-hook #'auto-dim-other-buffers--focus-in)
    (remove-hook 'focus-out-hook #'auto-dim-other-buffers--focus-out)
    
    (message "Auto-dim-other-buffers mode disabled")))

;; --------------------------------------------------
;; Interactive functions
;; --------------------------------------------------
(defun auto-dim-other-buffers-refresh ()
  "Refresh dimming for all buffers."
  (interactive)
  (when auto-dim-other-buffers-mode
    (auto-dim-other-buffers--undim-all-buffers)
    (auto-dim-other-buffers--create-dim-face)
    (auto-dim-other-buffers--buffer-list-update)
    (message "Auto-dim-other-buffers refreshed")))

(defun auto-dim-other-buffers-toggle-buffer ()
  "Toggle dimming for the current buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (assq buffer auto-dim-other-buffers--cookie-list)
        (progn
          (auto-dim-other-buffers--undim-buffer buffer)
          (message "Dimming disabled for %s" (buffer-name buffer)))
      (auto-dim-other-buffers--dim-buffer buffer)
      (message "Dimming enabled for %s" (buffer-name buffer)))))

(defun auto-dim-other-buffers-adjust-dim-factor (factor)
  "Adjust the dim factor to FACTOR and refresh."
  (interactive "nNew dim factor (0.0-1.0): ")
  (if (and (numberp factor) (<= 0.0 factor) (<= factor 1.0))
      (progn
        (setq auto-dim-other-buffers-dim-factor factor)
        (when auto-dim-other-buffers-mode
          (auto-dim-other-buffers-refresh))
        (message "Dim factor set to %.2f" factor))
    (user-error "Dim factor must be between 0.0 and 1.0")))

;; --------------------------------------------------
;; Debug functions
;; --------------------------------------------------
(defun auto-dim-other-buffers-debug-info ()
  "Show debug information about dimmed buffers."
  (interactive)
  (with-current-buffer (get-buffer-create "*Auto-Dim Debug*")
    (erase-buffer)
    (insert "=== AUTO-DIM-OTHER-BUFFERS DEBUG INFO ===\n\n")
    (insert (format "Mode enabled: %s\n" auto-dim-other-buffers-mode))
    (insert (format "Dim factor: %.2f\n" auto-dim-other-buffers-dim-factor))
    (insert (format "Last buffer: %s\n" 
                     (if auto-dim-other-buffers--last-buffer
                         (buffer-name auto-dim-other-buffers--last-buffer)
                       "None")))
    (insert (format "Current buffer: %s\n" (buffer-name (current-buffer))))
    (insert (format "Dimmed buffers: %d\n\n" (length auto-dim-other-buffers--cookie-list)))
    
    (insert "=== DIMMED BUFFERS ===\n")
    (dolist (entry auto-dim-other-buffers--cookie-list)
      (let ((buffer (car entry))
            (cookies (cdr entry)))
        (insert (format "• %s (%d cookies)\n" 
                         (if (buffer-live-p buffer)
                             (buffer-name buffer)
                           "[DEAD BUFFER]")
                         (length cookies)))))
    
    (insert "\n=== AFFECTED FACES ===\n")
    (dolist (face auto-dim-other-buffers-affected-faces)
      (insert (format "• %s\n" face)))
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; --------------------------------------------------
;; Theme integration
;; --------------------------------------------------
(defun auto-dim-other-buffers--theme-changed ()
  "Handle theme changes by refreshing the dim face."
  (when auto-dim-other-buffers-mode
    (run-with-timer 0.1 nil #'auto-dim-other-buffers-refresh)))

;; Hook into theme changes
(add-hook 'after-enable-theme-hook #'auto-dim-other-buffers--theme-changed)

;; --------------------------------------------------
;; Keybindings (optional)
;; --------------------------------------------------
;; Uncomment these if you want default keybindings
;; (global-set-key (kbd "C-c d m") #'auto-dim-other-buffers-mode)
;; (global-set-key (kbd "C-c d r") #'auto-dim-other-buffers-refresh)
;; (global-set-key (kbd "C-c d t") #'auto-dim-other-buffers-toggle-buffer)
;; (global-set-key (kbd "C-c d f") #'auto-dim-other-buffers-adjust-dim-factor)
;; (global-set-key (kbd "C-c d i") #'auto-dim-other-buffers-debug-info)


;; --------------------------------------------------
;; Auto-activation
;; --------------------------------------------------
;; Activer automatiquement le mode au chargement
;; (auto-dim-other-buffers-mode 1) ; Désactivé temporairement pour debug

(provide 'auto-dim-other-buffers)
;;; auto-dim-other-buffers.el ends here