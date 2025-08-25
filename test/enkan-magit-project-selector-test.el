;;; enkan-magit-project-selector-test.el --- TDD tests for magit project selector command -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for magit project selector with UI for choosing project directory.

;;; Code:

(require 'ert)

;; Pure function to get project list for magit selection
(defun enkan-repl--get-magit-project-list-pure (project-registry)
  "Get list of projects for magit selection from PROJECT-REGISTRY.
Returns list of (project-name . project-path) pairs."
  (unless project-registry
    (error "Project registry is empty"))
  (mapcar (lambda (entry)
            (let ((project-name (car entry))
                  (project-path (if (stringp (cdr entry))
                                    (cdr entry)
                                  ;; Handle nested cons structure
                                  (if (consp (cdr entry))
                                      (cdr (cdr entry))
                                    (cdr entry)))))
              (cons project-name (expand-file-name project-path))))
          project-registry))

;; Pure function to validate project path for magit
(defun enkan-repl--validate-magit-project-path-pure (project-path)
  "Validate PROJECT-PATH for magit operation.
Returns plist with :valid, :message."
  (cond
   ((null project-path)
    (list :valid nil :message "Project path is null"))
   ((not (stringp project-path))
    (list :valid nil :message "Project path must be a string"))
   ((not (file-directory-p project-path))
    (list :valid nil :message "Project path does not exist or is not a directory"))
   (t
    (list :valid t :message "Valid project path"))))

;; Pure function to create completion list for UI
(defun enkan-repl--create-magit-completion-list-pure (project-list)
  "Create completion list for magit project selection from PROJECT-LIST.
Returns list of strings for completing-read."
  (unless project-list
    (error "Project list is empty"))
  (mapcar (lambda (entry)
            (format "%s (%s)" (car entry) (cdr entry)))
          project-list))

;; Pure function to parse selected project from completion
(defun enkan-repl--parse-selected-magit-project-pure (selected-string project-list)
  "Parse SELECTED-STRING to get project info from PROJECT-LIST.
Returns plist with :project-name, :project-path."
  (let ((matched-entry (cl-find-if
                        (lambda (entry)
                          (string= selected-string
                                   (format "%s (%s)" (car entry) (cdr entry))))
                        project-list)))
    (if matched-entry
        (list :project-name (car matched-entry)
              :project-path (cdr matched-entry))
      (list :project-name nil :project-path nil))))

;; Test project list creation
(ert-deftest test-enkan-magit-project-list-creation ()
  "Test creation of project list for magit selection."
  (let ((registry '(("pt-tools" . "/tmp/pt-tools")
                   ("enkan-repl" . "/tmp/enkan-repl"))))
    (let ((result (enkan-repl--get-magit-project-list-pure registry)))
      (should (= (length result) 2))
      (should (equal (car result) (cons "pt-tools" (expand-file-name "/tmp/pt-tools"))))
      (should (equal (cadr result) (cons "enkan-repl" (expand-file-name "/tmp/enkan-repl"))))))
  
  ;; Test empty registry error
  (should-error (enkan-repl--get-magit-project-list-pure nil)))

;; Test project path validation
(ert-deftest test-enkan-magit-project-path-validation ()
  "Test validation of project paths for magit."
  ;; Valid path (assuming current directory exists)
  (let ((result (enkan-repl--validate-magit-project-path-pure default-directory)))
    (should (plist-get result :valid)))
  
  ;; Invalid cases
  (let ((result (enkan-repl--validate-magit-project-path-pure nil)))
    (should-not (plist-get result :valid))
    (should (string-match-p "null" (plist-get result :message))))
  
  (let ((result (enkan-repl--validate-magit-project-path-pure 123)))
    (should-not (plist-get result :valid))
    (should (string-match-p "string" (plist-get result :message))))
  
  (let ((result (enkan-repl--validate-magit-project-path-pure "/nonexistent/path")))
    (should-not (plist-get result :valid))
    (should (string-match-p "not exist" (plist-get result :message)))))

;; Test completion list creation
(ert-deftest test-enkan-magit-completion-list-creation ()
  "Test creation of completion list for UI."
  (let ((project-list '(("pt-tools" . "/tmp/pt-tools")
                       ("enkan-repl" . "/tmp/enkan-repl"))))
    (let ((result (enkan-repl--create-magit-completion-list-pure project-list)))
      (should (= (length result) 2))
      (should (equal (nth 0 result) "pt-tools (/tmp/pt-tools)"))
      (should (equal (nth 1 result) "enkan-repl (/tmp/enkan-repl)"))))
  
  ;; Test empty list error
  (should-error (enkan-repl--create-magit-completion-list-pure nil)))

;; Test project selection parsing
(ert-deftest test-enkan-magit-project-selection-parsing ()
  "Test parsing of selected project from completion."
  (let ((project-list '(("pt-tools" . "/tmp/pt-tools")
                       ("enkan-repl" . "/tmp/enkan-repl"))))
    ;; Valid selection
    (let ((result (enkan-repl--parse-selected-magit-project-pure 
                   "pt-tools (/tmp/pt-tools)" project-list)))
      (should (equal (plist-get result :project-name) "pt-tools"))
      (should (equal (plist-get result :project-path) "/tmp/pt-tools")))
    
    ;; Invalid selection
    (let ((result (enkan-repl--parse-selected-magit-project-pure 
                   "nonexistent" project-list)))
      (should (null (plist-get result :project-name)))
      (should (null (plist-get result :project-path))))))

;; Test nested registry format handling
(ert-deftest test-enkan-magit-nested-registry-format ()
  "Test handling of nested registry format that previously caused cons error."
  ;; Test nested structure handling
  (let ((nested-registry '(("pt-tools" . ("pt-tools" . "~/pt-tools")))))
    (let ((project-list (enkan-repl--get-magit-project-list-pure nested-registry)))
      (should (= (length project-list) 1))
      (should (equal (car project-list) (cons "pt-tools" (expand-file-name "~/pt-tools"))))
      ;; Should not error when creating completion list
      (let ((completion-list (enkan-repl--create-magit-completion-list-pure project-list)))
        (should (= (length completion-list) 1))
        (should (stringp (car completion-list)))))))

(provide 'enkan-magit-project-selector-test)
;;; enkan-magit-project-selector-test.el ends here