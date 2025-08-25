;;; enkan-repl-center-window-test.el --- Tests for center window navigation -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for pure functions in center-window-navigation.el

;;; Code:

(require 'ert)
(load-file "enkan-repl.el")
(load-file "examples/center-window-navigation.el")

;;;; Pure function tests

(ert-deftest enkan-repl-test--get-session-project-path-pure ()
  "Test pure function for getting session project path."
  (let* ((temp-dir (file-name-as-directory temporary-file-directory))
         (session-list '((4 . "pt-tools") (5 . "enkan-repl") (6 . "claude-code")))
         (project-registry `(("pt" . ("pt-tools" . ,(concat temp-dir "pt-tools")))
                             ("er" . ("enkan-repl" . ,(concat temp-dir "enkan-repl")))
                             ("cc" . ("claude-code" . ,(concat temp-dir "claude-code"))))))
    ;; Valid session numbers - should find project by project name
    (should (string= (concat temp-dir "pt-tools")
                     (enkan-repl--get-session-project-path-pure 4 session-list project-registry)))
    (should (string= (concat temp-dir "enkan-repl")
                     (enkan-repl--get-session-project-path-pure 5 session-list project-registry)))
    (should (string= (concat temp-dir "claude-code")
                     (enkan-repl--get-session-project-path-pure 6 session-list project-registry)))
    
    ;; Invalid session number
    (should (null (enkan-repl--get-session-project-path-pure 99 session-list project-registry)))
    
    ;; Empty lists
    (should (null (enkan-repl--get-session-project-path-pure 4 '() project-registry)))
    (should (null (enkan-repl--get-session-project-path-pure 4 session-list '())))
    
    ;; Project not in registry
    (let ((missing-project-session-list '((4 . "missing-project"))))
      (should (null (enkan-repl--get-session-project-path-pure 4 missing-project-session-list project-registry))))))

(ert-deftest enkan-repl-test--setup-window-dired-pure ()
  "Test pure function for window dired setup."
  (let* ((temp-dir (file-name-as-directory temporary-file-directory))
         (session-list '((4 . "pt-tools")))
         (project-registry `(("pt" . ("pt-tools" . ,temp-dir))))
         (mock-window 'test-window))
    
    ;; Create temp directory for test
    (make-directory temp-dir t)
    
    ;; Valid setup
    (let ((result (enkan-repl--setup-window-dired-pure 
                   mock-window 4 session-list project-registry)))
      (should (equal (car result) mock-window))
      (should (string= (cdr result) temp-dir)))
    
    ;; Invalid session number
    (should (null (enkan-repl--setup-window-dired-pure 
                   mock-window 99 session-list project-registry)))
    
    ;; Non-existent directory
    (let ((bad-registry `(("pt" . ("pt-tools" . "/non-existent-directory")))))
      (should (null (enkan-repl--setup-window-dired-pure 
                     mock-window 4 session-list bad-registry))))))

(ert-deftest enkan-repl-test--setup-window-eat-buffer-pure ()
  "Test pure function for window eat buffer setup."
  (let* ((temp-dir (file-name-as-directory (make-temp-file "enkan-repl-test-" t)))
         (session-list '((4 . "pt-tools") (5 . "enkan-repl")))
         (project-registry `(("pt" . ("pt-tools" . ,(concat temp-dir "pt-tools")))
                             ("enkan" . ("enkan-repl" . ,(concat temp-dir "enkan-repl")))))
         (mock-window 'test-window))
    
    ;; Valid session number with existing project path
    (let ((result (enkan-repl--setup-window-eat-buffer-pure
                   mock-window 4 session-list project-registry)))
      (should (equal (car result) mock-window))
      (should (string= (cdr result) (format "*enkan:%s*" (expand-file-name (concat temp-dir "pt-tools"))))))
    
    (let ((result (enkan-repl--setup-window-eat-buffer-pure
                   mock-window 5 session-list project-registry)))
      (should (equal (car result) mock-window))
      (should (string= (cdr result) (format "*enkan:%s*" (expand-file-name (concat temp-dir "enkan-repl"))))))
    
    ;; Invalid session number
    (should (null (enkan-repl--setup-window-eat-buffer-pure
                   mock-window 99 session-list project-registry)))
    
    ;; Empty session list
    (should (null (enkan-repl--setup-window-eat-buffer-pure
                   mock-window 4 '() project-registry)))
    
    ;; Project not found in registry
    (let ((missing-project-session-list '((4 . "nonexistent-project"))))
      (should (null (enkan-repl--setup-window-eat-buffer-pure
                     mock-window 4 missing-project-session-list project-registry))))
    
    (delete-directory temp-dir t)))

;;;; Error handling tests

(ert-deftest enkan-repl-test--2session-layout-error-handling ()
  "Test error handling in 2-session layout setup."
  (cl-letf (((symbol-function 'delete-other-windows) (lambda ()))
            ((symbol-function 'split-window-right) (lambda (&optional size) 'window-2))
            ((symbol-function 'split-window-below) (lambda (&optional size) 'window-3))
            ((symbol-function 'other-window) (lambda (n)))
            ((symbol-function 'balance-windows) (lambda ()))
            ((symbol-function 'window-list) (lambda () '(window-1 window-2 window-3 window-4 window-5)))
            ((symbol-function 'window-width) (lambda () 100))
            ((symbol-function 'window-height) (lambda () 30))
            ((symbol-function 'floor) (lambda (n) (truncate n)))
            ((symbol-function 'select-window) (lambda (window)))
            ((symbol-function 'dired) (lambda (directory))))
    
    ;; Test error when variables are not bound
    (should-error
     (let ((enkan-repl-session-list)
           (enkan-repl-center-project-registry))
       (makunbound 'enkan-repl-session-list)
       (makunbound 'enkan-repl-center-project-registry)
       (enkan-repl-setup-2session-layout))
     :type 'error)
    
    ;; Test error when session list is nil
    (should-error
     (let ((enkan-repl-session-list nil)
           (enkan-repl-center-project-registry '(("pt" . ("pt-tools" . "/tmp")))))
       (enkan-repl-setup-2session-layout))
     :type 'error)
    
    ;; Test error when project registry is nil
    (should-error
     (let ((enkan-repl-session-list '((4 . "pt-tools")))
           (enkan-repl-center-project-registry nil))
       (enkan-repl-setup-2session-layout))
     :type 'error)))

;;;; Project Path Registry Tests

(ert-deftest test-project-path-from-registry-lookup ()
  "Test that enkan-repl--get-project-path-from-registry works correctly."
  (let ((test-registry '(("alias1" . ("Project One" . "/path/to/project1"))
                         ("alias2" . ("Project Two" . "/path/to/project2"))
                         ("alias3" . ("Another Project" . "/path/to/project3")))))
    
    ;; Test successful lookup
    (should (string= "/path/to/project1"
                     (enkan-repl--get-project-path-from-registry "Project One" test-registry)))
    (should (string= "/path/to/project2"
                     (enkan-repl--get-project-path-from-registry "Project Two" test-registry)))
    (should (string= "/path/to/project3"
                     (enkan-repl--get-project-path-from-registry "Another Project" test-registry)))
    
    ;; Test failed lookup
    (should (null (enkan-repl--get-project-path-from-registry "Nonexistent Project" test-registry)))))

(ert-deftest test-center-finish-project-path-resolution ()
  "Test that center finish function correctly resolves project paths from project names."
  (let ((enkan-repl-center-project-registry 
         '(("er" . ("enkan-repl" . "/home/user/enkan-repl"))
           ("pt" . ("pt-tools" . "/home/user/pt-tools"))))
        (enkan-repl-session-list '((1 . "enkan-repl") (2 . "pt-tools")))
        (resolved-paths '()))
    
    ;; Mock enkan-repl--get-buffer-for-directory to capture resolved paths
    (cl-letf (((symbol-function 'enkan-repl--get-buffer-for-directory)
               (lambda (directory)
                 (push directory resolved-paths)
                 ;; Return mock buffer
                 (get-buffer-create (format "*mock-%s*" directory))))
              ((symbol-function 'kill-buffer)
               (lambda (buffer) t))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t)))
      
      ;; Execute the function
      (enkan-repl-center-finish-all-sessions)
      
      ;; Verify correct project paths were resolved and used
      (should (member "/home/user/enkan-repl" resolved-paths))
      (should (member "/home/user/pt-tools" resolved-paths))
      (should (= 2 (length resolved-paths))))))

(provide 'center-project-path-test)

;;; center-project-path-test.el ends here