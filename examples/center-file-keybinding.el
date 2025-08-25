;;; center-file-keybinding.el --- Center file keybinding setup for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This file provides keybinding setup for enkan-repl center file mode.
;; It automatically activates when the center file is opened.

;;; Commentary:

;; This file provides automatic keybinding activation for center file mode:
;; - Detects when the center file is opened
;; - Activates center file specific keybindings
;; - Maintains consistency with other enkan-repl window layout modes
;;
;; Features:
;; - Auto-activation based on center file path
;; - Integration with existing keybinding system
;; - Consistent keybinding overrides following the established pattern
;;
;; Usage:
;; 1. Load this file after setting enkan-repl-center-file:
;;    (setq enkan-repl-center-file "~/center-file.org")
;;    (load-file "center-file-keybinding.el")
;;
;; 2. Open the center file - keybindings will activate automatically

;;; Code:

(require 'enkan-repl)

;; Load keybinding definitions and base keymap
(let ((constants-file (expand-file-name "keybinding-constants.el"
                                         (file-name-directory load-file-name)))
      (keybinding-file (expand-file-name "keybinding.el"
                                          (file-name-directory load-file-name))))
  (when (file-exists-p constants-file)
    (condition-case err
        (load constants-file)
      (error (message "Warning: Failed to load keybinding-constants.el: %s" err))))
  (when (file-exists-p keybinding-file)
    (condition-case err
        (progn
          (load keybinding-file)
          ;; Install base keybindings as default
          (when (fboundp 'enkan-install-base-keymap)
            (enkan-install-base-keymap)))
      (error (message "Warning: Failed to load keybinding.el: %s" err)))))

;;; ========================================
;;; Center File Mode Definition
;;; ========================================

(defvar enkan-center-file-mode-map nil
  "Keymap for center file mode commands.")

(defvar enkan-center-file-cheat-sheet-candidates
  (if (boundp 'enkan-center-file-command-definitions)
      ;; Use centralized definitions with keybinding hints
      (mapcar (lambda (def)
                (let* ((command (nth 0 def))
                       (description (nth 1 def))
                       (command-name (symbol-name command))
                       ;; Find keybinding if exists
                       (key (when (boundp 'enkan-center-file-keybinding-overrides)
                              (car (seq-find (lambda (binding)
                                               (eq (nth 1 binding) command))
                                             enkan-center-file-keybinding-overrides)))))
                  (cons command-name
                        (if key
                            (format "%s (%s)" description key)
                          description))))
              enkan-center-file-command-definitions)
    ;; Fallback definitions
    '(("enkan-repl-center-setup" . "Setup center file window layout")
      ("enkan-repl-center-reset" . "Reset center file layout")
      ("enkan-repl-center-other-window" . "Switch between center/work/reserve windows (C-M-t)")
      ("enkan-repl-center-file-help" . "Show center file help")
      ("enkan-repl-center-list-sessions" . "List registered sessions")
      ("enkan-repl-center-clear-sessions" . "Clear all sessions")))
  "Additional commands for center file mode to add to cheat sheet.")

;;; ========================================
;;; Keymap Definition
;;; ========================================

(setq enkan-center-file-mode-map
      (if (boundp 'enkan-center-file-keybinding-overrides)
          ;; Use centralized definitions if available
          (enkan-keybinding-make-keymap enkan-center-file-keybinding-overrides)
        ;; Fallback to manual definition
        (let ((map (make-sparse-keymap)))
          ;; Override base keybindings for center file mode
          (define-key map (kbd "C-x g") 'enkan-repl-center-magit)
          (define-key map (kbd "M-<return>") 'enkan-repl-center-send-region)
          (define-key map (kbd "C-M-e") 'enkan-repl-center-send-enter)
          (define-key map (kbd "C-M-i") 'enkan-repl-center-send-line)
          (define-key map (kbd "C-M-t") 'enkan-repl-center-other-window)
          (define-key map (kbd "C-M-b") 'enkan-repl-center-recenter-bottom)
          map)))

;;; ========================================
;;; Minor Mode Definition
;;; ========================================

(define-minor-mode enkan-center-file-mode
  "Minor mode for center file with enkan-repl multi-buffer access."
  :lighter " CenterFile"
  :keymap enkan-center-file-mode-map
  :global nil
  (if enkan-center-file-mode
      (progn
        ;; Ensure keymap has higher priority by setting it as minor mode keymap
        (when (current-local-map)
          (use-local-map (make-composed-keymap enkan-center-file-mode-map
                                               (current-local-map))))
        (enkan-center-file-setup-cheat-sheet-advice)
        (message "enkan-center-file-mode enabled. C-M-t to switch windows, M-1-4 for session access."))
    (progn
      (enkan-center-file-remove-cheat-sheet-advice)
      (message "enkan-center-file-mode disabled."))))

;;; ========================================
;;; Auto-activation Functions
;;; ========================================

(defun enkan-center-file-check-and-activate ()
  "Check if current buffer is the center file and activate mode if needed."
  (when (and enkan-repl-center-file
             (buffer-file-name)
             (string= (expand-file-name (buffer-file-name))
                     (expand-file-name enkan-repl-center-file)))
    (enkan-center-file-mode 1)))

(defun enkan-center-file-deactivate-if-not-center ()
  "Deactivate center file mode if current buffer is not the center file."
  (when (and enkan-center-file-mode
             enkan-repl-center-file
             (buffer-file-name)
             (not (string= (expand-file-name (buffer-file-name))
                          (expand-file-name enkan-repl-center-file))))
    (enkan-center-file-mode -1)))

;;; ========================================
;;; Hook Setup
;;; ========================================

;; Auto-activate center file mode when opening the center file
(add-hook 'find-file-hook 'enkan-center-file-check-and-activate)
(add-hook 'buffer-list-update-hook 'enkan-center-file-deactivate-if-not-center)

;;; ========================================
;;; Helper Functions
;;; ========================================

(defun enkan-center-file-describe-keybindings ()
  "Display keybindings active in center file mode."
  (interactive)
  (with-output-to-temp-buffer "*Enkan Center File Keybindings*"
    (princ "Enkan Center File Mode Keybindings\n")
    (princ "==================================\n\n")
    ;; Show base keybindings
    (princ "Base Keybindings (inherited):\n")
    (princ "------------------------------\n")
    (when (boundp 'enkan-keybinding-definitions)
      ;; Show non-overridden base bindings
      (let ((overridden-keys (when (boundp 'enkan-center-file-keybinding-overrides)
                               (mapcar #'car enkan-center-file-keybinding-overrides))))
        (dolist (def enkan-keybinding-definitions)
          (unless (member (car def) overridden-keys)
            (princ (format "  %-15s - %s\n" (nth 0 def) (nth 2 def)))))))
    (princ "\n")
    ;; Show center file specific overrides
    (princ "Center File Mode Overrides:\n")
    (princ "----------------------------\n")
    (if (boundp 'enkan-center-file-keybinding-overrides)
        (princ (enkan-keybinding-format-description enkan-center-file-keybinding-overrides))
      ;; Fallback if constants not loaded
      (princ "  C-M-t       - Switch between center/work/reserve windows\n")
      (princ "  C-c s r     - Register current session\n")
      (princ "  C-c s l     - List registered sessions\n")
      (princ "  C-c s c     - Clear all sessions\n"))
    (princ "\nCenter File Specific Features:\n")
    (princ "-------------------------------\n")
    (princ "  M-1         - Send line to session 1\n")
    (princ "  M-2         - Send line to session 2\n")
    (princ "  M-3         - Send line to session 3\n")
    (princ "  M-4         - Send line to session 4\n")
    (princ "  C-M-b       - Recenter all enkan sessions at bottom\n")
    (princ "  C-c w 2     - Setup 2-session layout\n")
    (princ "  C-c w 3     - Setup 3-session layout\n")
    (princ "  C-c w 4     - Setup 4-session layout\n")
    (princ "\nNote: Center file mode activates automatically when opening the designated center file.\n")))

;;; ========================================
;;; Cheat Sheet Integration
;;; ========================================

(defun enkan-center-file-cheat-sheet-advice (orig-fun)
  "Advice to add center file commands to enkan-repl cheat sheet."
  (if enkan-center-file-mode
      ;; When center file mode is active, show combined cheat sheet
      (progn
        (unless (featurep 'enkan-repl-constants)
          (require 'enkan-repl-constants nil t))
        (let* ((original-candidates
                (if (boundp 'enkan-repl-cheat-sheet-candidates)
                    enkan-repl-cheat-sheet-candidates
                  '()))
               ;; Combine original and center file candidates
               (candidates (append original-candidates
                                 enkan-center-file-cheat-sheet-candidates)))
          (let ((completion-extra-properties
                 `(:annotation-function
                   (lambda (candidate)
                     (let ((description (alist-get candidate ',candidates
                                                  nil nil #'string=)))
                       (when description
                         (format " — %s" description)))))))
            (let ((selected-command
                   (completing-read "enkan-repl & center file commands: " candidates)))
              (when selected-command
                (call-interactively (intern selected-command)))))))
    ;; When center file mode is not active, use original function
    (funcall orig-fun)))

(defun enkan-center-file-setup-cheat-sheet-advice ()
  "Setup advice for cheat sheet integration."
  (advice-add 'enkan-repl-cheat-sheet :around
              #'enkan-center-file-cheat-sheet-advice))

(defun enkan-center-file-remove-cheat-sheet-advice ()
  "Remove advice for cheat sheet integration."
  (advice-remove 'enkan-repl-cheat-sheet
                 #'enkan-center-file-cheat-sheet-advice))

;;; ========================================
;;; Setup Function
;;; ========================================

(defun enkan-center-file-setup-keybindings ()
  "Setup center file keybindings.
This function can be called manually if auto-activation doesn't work."
  (interactive)
  (if enkan-repl-center-file
      (progn
        (message "Center file keybinding setup completed for: %s" enkan-repl-center-file)
        (when (and (buffer-file-name)
                   (string= (expand-file-name (buffer-file-name))
                           (expand-file-name enkan-repl-center-file)))
          (enkan-center-file-mode 1)))
    (message "enkan-repl-center-file is not set. Please set it first.")))

;;; ========================================
;;; Provide
;;; ========================================

(provide 'center-file-keybinding)

;;; center-file-keybinding.el ends here
