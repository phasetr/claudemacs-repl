;;; keybinding-constants.el --- Keybinding definitions for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This file provides centralized keybinding definitions for enkan-repl.
;; All keybinding configurations should reference these constants.

;;; Commentary:

;; Centralized keybinding definitions to avoid duplication and ensure
;; consistency across all enkan-repl components.
;;
;; Structure:
;; - Each binding is defined as (KEY COMMAND DESCRIPTION CATEGORY)
;; - Categories group related bindings
;; - Mode-specific overrides are also defined here

;;; Code:

;;; ========================================
;;; Base Keybinding Definitions
;;; ========================================

(defconst enkan-keybinding-definitions
  '(;; Window Navigation
     ("C-M-l" enkan-repl-setup "Setup window layout" window-navigation)
     ("C-c 1" enkan-repl-goto-window-1 "Go to window 1" window-navigation)
     ("C-c 2" enkan-repl-goto-window-2 "Go to window 2" window-navigation)
     ("C-c 3" enkan-repl-goto-window-3 "Go to window 3" window-navigation)
     ("C-c 4" enkan-repl-goto-window-4 "Go to window 4" window-navigation)
     ("C-c 5" enkan-repl-goto-window-5 "Go to window 5" window-navigation)
     ("C-c 6" enkan-repl-goto-window-6 "Go to window 6" window-navigation)
     ("C-c 7" enkan-repl-goto-window-7 "Go to window 7" window-navigation)
     ;; Text Sending
     ("C-M-<return>" enkan-repl-send-region "Send region to REPL" text-sending)
     ("C-M-i" enkan-repl-send-line "Send current line to REPL" text-sending)
     ("C-M-e" enkan-repl-send-enter "Send enter to REPL" text-sending)
     ;; Quick Actions
     ("<escape>" enkan-repl-send-escape "Send ESC to REPL" quick-actions)
     ("C-M-1" enkan-repl-send-1 "Send 1 to REPL" quick-actions)
     ("C-M-2" enkan-repl-send-2 "Send 2 to REPL" quick-actions)
     ("C-M-3" enkan-repl-send-3 "Send 3 to REPL" quick-actions)
     ("C-M-4" enkan-repl-send-4 "Send 4 to REPL" quick-actions)
     ("C-M-5" enkan-repl-send-5 "Send 5 to REPL" quick-actions)
     ("C-M-b" enkan-repl-recenter-bottom "Recenter at bottom" quick-actions)
     ;; Multi-buffer Layout
     ("C-c w 2" enkan-repl-center-setup-2session-layout "Setup 2-session layout" multi-buffer)
     ("C-c w 3" enkan-repl-center-setup-3session-layout "Setup 3-session layout" multi-buffer)
     ("C-c w 4" enkan-repl-center-setup-4session-layout "Setup 4-session layout" multi-buffer)
     ;; Center File Multi-buffer Access
     ("M-1" enkan-repl-send-line-to-session-1 "Send line to session 1" center-file)
     ("M-2" enkan-repl-send-line-to-session-2 "Send line to session 2" center-file)
     ("M-3" enkan-repl-send-line-to-session-3 "Send line to session 3" center-file)
     ("M-4" enkan-repl-send-line-to-session-4 "Send line to session 4" center-file)
     ;; Project Management
     ("C-M-s" enkan-repl-start-eat "Start eat session" project-management)
     ("C-M-f" enkan-repl-finish-eat "Finish eat session" project-management)
     ;; Help and Documentation
     ("C-M-c" enkan-repl-cheat-sheet "Enkan cheat sheet" help))
  "Base keybinding definitions for enkan-repl.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-keybinding-categories
  '((window-navigation . "Window Navigation")
     (text-sending . "Text Sending")
     (quick-actions . "Quick Actions")
     (multi-buffer . "Multi-buffer Layout")
     (center-file . "Center File Multi-buffer Access")
     (project-management . "Project Management")
     (help . "Help and Documentation"))
  "Alist of category symbols to display names.")

;;; ========================================
;;; Mode-specific Override Definitions
;;; ========================================

(defconst enkan-simple-3pane-keybinding-overrides
  '(("C-M-t" enkan-simple-3pane-other-window "Switch between input/misc windows" window-navigation))
  "Keybinding overrides for 3pane mode.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-simple-3pane-command-definitions
  '((enkan-simple-3pane-setup "Setup 3-pane window layout")
    (enkan-simple-3pane-reset "Reset 3-pane layout")
    (enkan-simple-3pane-describe-keybindings "Show 3pane keybindings")
    (enkan-simple-3pane-other-window "Switch between input/misc windows")
    (enkan-simple-3pane-lock-windows "Lock input and eat windows")
    (enkan-simple-3pane-unlock-windows "Unlock input and eat windows"))
  "Command definitions for 3pane mode cheat sheet.
Each entry is (COMMAND DESCRIPTION).")

(defconst enkan-dual-task-keybinding-overrides
  '(("C-M-t" enkan-dual-task-other-window "Switch between input windows" window-navigation))
  "Keybinding overrides for dual task mode.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-dual-task-command-definitions
  '((enkan-dual-task-setup "Setup dual task window layout")
    (enkan-dual-task-reset "Reset dual task layout")
    (enkan-dual-task-describe-layout "Show dual task layout status")
    (enkan-dual-task-other-window "Switch between input windows"))
  "Command definitions for dual task mode.
Each entry is (COMMAND DESCRIPTION).")

(defconst enkan-center-file-keybinding-overrides
  '(("<escape>" enkan-repl-center-send-escape "Send ESC to REPL (center)" quick-actions)
    ("C-M-e" enkan-repl-center-send-enter "Send enter to REPL (center)" text-sending)
    ("C-M-1" enkan-repl-center-send-1 "Send 1 to REPL (center)" quick-actions)
    ("C-M-2" enkan-repl-center-send-2 "Send 2 to REPL (center)" quick-actions)
    ("C-M-3" enkan-repl-center-send-3 "Send 3 to REPL (center)" quick-actions)
    ("C-M-4" enkan-repl-center-send-4 "Send 4 to REPL (center)" quick-actions)
    ("C-M-5" enkan-repl-center-send-5 "Send 5 to REPL (center)" quick-actions)
    ("C-M-i" enkan-repl-center-send-line "Send line for the center file" window-navigation)
    ("C-M-e" enkan-repl-center-send-enter "Send enter for the center file" quick-actions)
    ("C-M-t" enkan-repl-center-other-window "Switch between center/work/reserve windows" window-navigation)
    ("C-M-b" enkan-repl-center-recenter-bottom "Recenter at bottom (center)" quick-actions)
    ("C-M-o" enkan-repl-center-open-file "Recenter at bottom (center)" quick-actions)
    ("C-M-s" enkan-repl-center-auto-setup "Auto setup sessions using multi-project layout" session-management)
    ("C-M-f" enkan-repl-center-finish-all-sessions "Terminate all registered center sessions" session-management)
    ("C-c C-f" enkan-toggle-center-file-global-mode "Toggle center file global mode" global-mode))
  "Keybinding overrides for center file multi-buffer mode.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-center-file-command-definitions
  '((enkan-repl-center-setup "Setup center file multi-buffer window layout")
    (enkan-repl-center-reset "Reset center file multi-buffer layout")
    (enkan-repl-center-send-enter "Send enter to REPL (center)")
    (enkan-repl-center-other-window "Switch between center/work/reserve windows")
    (enkan-repl-center-setup-2session-layout "Setup 2-session layout")
    (enkan-repl-center-setup-3session-layout "Setup 3-session layout")
    (enkan-repl-center-setup-4session-layout "Setup 4-session layout")
    (enkan-repl-center-send-escape "Send ESC to REPL (center)")
    (enkan-repl-center-send-1 "Send 1 to REPL (center)")
    (enkan-repl-center-send-2 "Send 2 to REPL (center)")
    (enkan-repl-center-send-3 "Send 3 to REPL (center)")
    (enkan-repl-center-send-4 "Send 4 to REPL (center)")
    (enkan-repl-center-send-5 "Send 5 to REPL (center)")
    (enkan-repl-center-send-line-to-session-1 "Send line to session 1 (center)")
    (enkan-repl-center-send-line-to-session-2 "Send line to session 2 (center)")
    (enkan-repl-center-send-line-to-session-3 "Send line to session 3 (center)")
    (enkan-repl-center-send-line-to-session-4 "Send line to session 4 (center)")
    (enkan-repl-center-send-region-to-session-1 "Send region to session 1 (center)")
    (enkan-repl-center-send-region-to-session-2 "Send region to session 2 (center)")
    (enkan-repl-center-send-region-to-session-3 "Send region to session 3 (center)")
    (enkan-repl-center-send-region-to-session-4 "Send region to session 4 (center)")
    (enkan-repl-center-send-buffer-to-session-1 "Send buffer to session 1 (center)")
    (enkan-repl-center-send-buffer-to-session-2 "Send buffer to session 2 (center)")
    (enkan-repl-center-send-buffer-to-session-3 "Send buffer to session 3 (center)")
    (enkan-repl-center-send-buffer-to-session-4 "Send buffer to session 4 (center)")
    (enkan-repl-center-register-current-session "Register current session")
    (enkan-repl-center-list-sessions "List registered sessions")
    (enkan-repl-center-clear-sessions "Clear all sessions"))
  "Command definitions for center file multi-buffer mode.
Each entry is (COMMAND DESCRIPTION).")

(defconst enkan-mode-keybinding-overrides
  `((enkan-simple-3pane-mode . ,enkan-simple-3pane-keybinding-overrides)
    (enkan-dual-task-mode . ,enkan-dual-task-keybinding-overrides)
    (enkan-center-file-mode . ,enkan-center-file-keybinding-overrides))
  "Alist of modes to their keybinding override definitions.")

;;; ========================================
;;; Utility Functions
;;; ========================================

(defun enkan-keybinding-get-by-category (category &optional definitions)
  "Get all keybindings in CATEGORY from DEFINITIONS.
If DEFINITIONS is nil, use `enkan-keybinding-definitions'."
  (let ((defs (or definitions enkan-keybinding-definitions)))
    (seq-filter (lambda (def)
                  (eq (nth 3 def) category))
      defs)))

(defun enkan-keybinding-get-categories (&optional definitions)
  "Get all unique categories from DEFINITIONS.
If DEFINITIONS is nil, use `enkan-keybinding-definitions'."
  (let ((defs (or definitions enkan-keybinding-definitions)))
    (seq-uniq (mapcar (lambda (def) (nth 3 def)) defs))))

(defun enkan-keybinding-make-keymap (definitions)
  "Create a keymap from DEFINITIONS."
  (let ((map (make-sparse-keymap))
        (prefix-keys '()))
    ;; First pass: identify prefix keys
    (dolist (def definitions)
      (let ((key-string (nth 0 def)))
        (when (string-match "^\\(.+\\) [0-9]$" key-string)
          (let ((prefix (match-string 1 key-string)))
            (unless (member prefix prefix-keys)
              (push prefix prefix-keys))))))
    ;; Second pass: define prefix keys
    (dolist (prefix prefix-keys)
      (define-key map (kbd prefix) (make-sparse-keymap)))
    ;; Third pass: define all keys
    (dolist (def definitions)
      (let ((key (nth 0 def))
             (command (nth 1 def)))
        (define-key map (kbd key) command)))
    map))

(defun enkan-keybinding-format-description (definitions)
  "Format DEFINITIONS as a human-readable string."
  (let ((categories (enkan-keybinding-get-categories definitions))
         (result ""))
    (dolist (cat categories)
      (let ((cat-name (or (cdr (assq cat enkan-keybinding-categories))
                        (symbol-name cat)))
             (bindings (enkan-keybinding-get-by-category cat definitions)))
        (setq result (concat result cat-name ":\n"))
        (dolist (binding bindings)
          (setq result (concat result
                         (format "  %-15s - %s\n"
                           (nth 0 binding)
                           (nth 2 binding)))))
        (setq result (concat result "\n"))))
    result))

;;; ========================================
;;; Provide
;;; ========================================

(provide 'keybinding-constants)

;;; keybinding-constants.el ends here
