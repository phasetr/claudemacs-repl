;;; enkan-center-auto-setup-integration-test.el --- Tests for center auto setup integration -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Integration tests for enkan-repl-center-auto-setup with project alias generation.

;;; Code:

(require 'ert)

;; Load the main package
(unless (featurep 'enkan-repl)
  (condition-case nil
      (let ((main-file (expand-file-name "../enkan-repl.el" 
                                         (file-name-directory (or load-file-name buffer-file-name)))))
        (when (file-exists-p main-file)
          (load main-file)))
    (error "Could not load enkan-repl.el")))

(ert-deftest test-center-auto-setup-generates-aliases ()
  "Test that center-auto-setup generates project aliases."
  (let ((enkan-repl-center-multi-project-layouts '(("test-layout" . ("pr" "er"))))
        (enkan-repl-project-aliases nil)
        (enkan-repl-center-project-registry '(("pr" . ("project1" . "/tmp/pr")) 
                                               ("er" . ("enkan-repl" . "/tmp/er"))))
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-multi-project-layout nil)
        (enkan-center-file-global-mode nil))
    
    ;; Mock functions to avoid side effects
    (cl-letf (((symbol-function 'enkan-center-file-global-mode) 
               (lambda (&rest args) (setq enkan-center-file-global-mode t)))
              ((symbol-function 'enkan-repl-start-eat) 
               (lambda () nil))
              ((symbol-function 'message) 
               (lambda (&rest args) nil))
              ((symbol-function 'expand-file-name) 
               (lambda (path) path)))
      
      ;; Call the function
      (enkan-repl-center-auto-setup "test-layout")
      
      ;; Verify project aliases were set
      (should (equal enkan-repl-project-aliases '(("pr" . "pr") ("er" . "er"))))
      
      ;; Verify other state changes
      (should (equal enkan-repl--current-multi-project-layout "test-layout"))
      (should enkan-center-file-global-mode))))

(ert-deftest test-center-auto-setup-aliases-with-duplicates ()
  "Test that center-auto-setup handles duplicate aliases correctly."
  (let ((enkan-repl-center-multi-project-layouts '(("dup-layout" . ("pr" "pr" "er"))))
        (enkan-repl-project-aliases nil)
        (enkan-repl-center-project-registry '(("pr" . ("project1" . "/tmp/pr")) 
                                               ("er" . ("enkan-repl" . "/tmp/er"))))
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-multi-project-layout nil)
        (enkan-center-file-global-mode nil))
    
    ;; Mock functions
    (cl-letf (((symbol-function 'enkan-center-file-global-mode) 
               (lambda (&rest args) (setq enkan-center-file-global-mode t)))
              ((symbol-function 'enkan-repl-start-eat) 
               (lambda () nil))
              ((symbol-function 'message) 
               (lambda (&rest args) nil))
              ((symbol-function 'expand-file-name) 
               (lambda (path) path)))
      
      ;; Call the function
      (enkan-repl-center-auto-setup "dup-layout")
      
      ;; Verify unique aliases were set (duplicates removed)
      (should (equal enkan-repl-project-aliases '(("pr" . "pr") ("er" . "er")))))))

(provide 'enkan-center-auto-setup-integration-test)
;;; enkan-center-auto-setup-integration-test.el ends here