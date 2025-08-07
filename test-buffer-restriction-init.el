;;; test-buffer-restriction-init.el --- Test initialization for buffer restriction mode -*- lexical-binding: t -*-

;;; Commentary:

;; Test initialization file for enkan-repl-buffer-restriction-mode.
;; Usage: emacs -Q -l test-buffer-restriction-init.el

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load required packages
(require 'enkan-repl-buffer-restriction-mode)

;; Create test buffers
(defun test-init-create-buffers ()
  "Create test buffers for demonstration."
  (get-buffer-create "*enkan-eat-session1*")
  (get-buffer-create "*enkan-eat-session2*")
  (get-buffer-create "*claudemacs:project1*")
  (get-buffer-create "*claudemacs:project2*")
  (get-buffer-create "*normal-buffer-1*")
  (get-buffer-create "*normal-buffer-2*")
  (get-buffer-create "*scratch*")
  (message "Test buffers created"))

;; Test switching function
(defun test-switch-to-restricted ()
  "Try to switch to a restricted buffer."
  (interactive)
  (condition-case err
      (switch-to-buffer "*enkan-eat-session1*")
    (error (message "Error: %s" err))))

(defun test-switch-to-normal ()
  "Try to switch to a normal buffer."
  (interactive)
  (switch-to-buffer "*normal-buffer-1*"))

;; Demo function
(defun demo-buffer-restriction ()
  "Demonstrate buffer restriction mode."
  (interactive)
  (test-init-create-buffers)
  (message "=== Buffer Restriction Mode Demo ===")
  (sit-for 1)
  
  ;; Show initial state
  (message "Initial mode state: %s" 
           (if enkan-repl-buffer-restriction-mode "ON" "OFF"))
  (sit-for 1)
  
  ;; Enable mode
  (message "Enabling buffer restriction mode...")
  (enkan-repl-buffer-restriction-mode-on)
  (sit-for 1)
  
  ;; Try to switch to restricted buffer
  (message "Switching to *enkan-eat-session1* (will open then adjust layout)...")
  (test-switch-to-restricted)
  (sit-for 2)
  (message "Notice: Buffer opened but layout was adjusted")
  (sit-for 1)
  
  ;; Try to switch to normal buffer
  (message "Attempting to switch to *normal-buffer-1* (should work normally)...")
  (test-switch-to-normal)
  (sit-for 2)
  
  ;; Show buffer list
  (message "Showing buffer list...")
  (list-buffers)
  (sit-for 2)
  
  ;; Disable mode
  (message "Disabling buffer restriction mode...")
  (enkan-repl-buffer-restriction-mode-off)
  (sit-for 1)
  
  ;; Try again with mode off
  (message "Switching to *enkan-eat-session1* (normal behavior)...")
  (switch-to-buffer "*enkan-eat-session1*")
  (sit-for 2)
  
  (message "Demo complete!"))

;; Key bindings for testing
(global-set-key (kbd "C-c t c") 'test-init-create-buffers)
(global-set-key (kbd "C-c t r") 'test-switch-to-restricted)
(global-set-key (kbd "C-c t n") 'test-switch-to-normal)
(global-set-key (kbd "C-c t d") 'demo-buffer-restriction)
(global-set-key (kbd "C-c b o") 'enkan-repl-buffer-restriction-mode-on)
(global-set-key (kbd "C-c b f") 'enkan-repl-buffer-restriction-mode-off)
(global-set-key (kbd "C-c b s") 'enkan-repl-buffer-restriction-mode-status)

;; Display help
(defun test-show-help ()
  "Show help for test commands."
  (interactive)
  (with-output-to-temp-buffer "*Buffer Restriction Test Help*"
    (princ "=== Buffer Restriction Mode Test Commands ===\n\n")
    (princ "Setup:\n")
    (princ "  C-c t c : Create test buffers\n")
    (princ "  C-c t d : Run full demo\n\n")
    (princ "Mode Control:\n")
    (princ "  C-c b o : Turn restriction mode ON\n")
    (princ "  C-c b f : Turn restriction mode OFF\n")
    (princ "  C-c b s : Show mode status\n\n")
    (princ "Test Navigation:\n")
    (princ "  C-c t r : Try to switch to restricted buffer\n")
    (princ "  C-c t n : Try to switch to normal buffer\n\n")
    (princ "Manual Testing:\n")
    (princ "  1. Run C-c t c to create test buffers\n")
    (princ "  2. Run C-c b o to enable restriction mode\n")
    (princ "  3. Try C-x b to switch buffers manually\n")
    (princ "  4. Notice that *enkan-eat-* and *claudemacs:* buffers open\n")
    (princ "     but window layout is immediately adjusted\n")
    (princ "  5. Run C-c b f to disable restriction mode\n")
    (princ "  6. Try switching again - normal behavior restored\n")))

(global-set-key (kbd "C-c t h") 'test-show-help)
(global-set-key (kbd "C-c t ?") 'test-show-help)

;; Initial setup
(test-init-create-buffers)
(test-show-help)
(message "Buffer restriction mode test environment loaded. Press C-c t ? for help.")

(provide 'test-buffer-restriction-init)

;;; test-buffer-restriction-init.el ends here