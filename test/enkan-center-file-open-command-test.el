;;; enkan-center-file-open-command-test.el --- TDD tests for center file open command -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for opening center file command with TDD and pure functions.

;;; Code:

(require 'ert)

;; Pure function to validate center file path
(defun enkan-center-file-validate-path-pure (file-path)
  "Validate center FILE-PATH for opening.
Returns plist with :valid, :message."
  (cond
   ((null file-path)
    (list :valid nil :message "Center file path not configured"))
   ((not (stringp file-path))
    (list :valid nil :message "Center file path must be a string"))
   ((string-empty-p file-path)
    (list :valid nil :message "Center file path is empty"))
   (t
    (list :valid t :message "Valid center file path"))))

;; Pure function to check if center file exists
(defun enkan-center-file-check-exists-pure (file-path)
  "Check if center FILE-PATH exists.
Returns plist with :exists, :action."
  (if (file-exists-p file-path)
      (list :exists t :action "open")
    (list :exists nil :action "create")))

;; Pure function to determine open action
(defun enkan-center-file-determine-action-pure (file-path)
  "Determine action to take for center FILE-PATH.
Returns plist with :valid, :action, :message."
  (let ((validation (enkan-center-file-validate-path-pure file-path)))
    (if (plist-get validation :valid)
        (let ((exists-check (enkan-center-file-check-exists-pure file-path)))
          (list :valid t
                :action (plist-get exists-check :action)
                :message (if (plist-get exists-check :exists)
                            "Opening existing center file"
                          "Creating new center file")))
      validation)))

;; Test path validation
(ert-deftest test-enkan-center-file-validate-path ()
  "Test center file path validation."
  (should-not (plist-get (enkan-center-file-validate-path-pure nil) :valid))
  (should-not (plist-get (enkan-center-file-validate-path-pure 123) :valid))
  (should-not (plist-get (enkan-center-file-validate-path-pure "") :valid))
  (should (plist-get (enkan-center-file-validate-path-pure "/valid/path.org") :valid)))

;; Test file existence check
(ert-deftest test-enkan-center-file-exists-check ()
  "Test center file existence checking."
  (let ((temp-file (make-temp-file "center-test" nil ".org")))
    (unwind-protect
        (progn
          (should (plist-get (enkan-center-file-check-exists-pure temp-file) :exists))
          (should (equal (plist-get (enkan-center-file-check-exists-pure temp-file) :action) "open")))
      (when (file-exists-p temp-file)
        (delete-file temp-file))))
  
  (should-not (plist-get (enkan-center-file-check-exists-pure "/nonexistent/file.org") :exists))
  (should (equal (plist-get (enkan-center-file-check-exists-pure "/nonexistent/file.org") :action) "create")))

;; Test action determination
(ert-deftest test-enkan-center-file-determine-action ()
  "Test center file action determination."
  ;; Invalid paths
  (should-not (plist-get (enkan-center-file-determine-action-pure nil) :valid))
  (should-not (plist-get (enkan-center-file-determine-action-pure "") :valid))
  
  ;; Valid path for existing file
  (let ((temp-file (make-temp-file "center-test" nil ".org")))
    (unwind-protect
        (let ((result (enkan-center-file-determine-action-pure temp-file)))
          (should (plist-get result :valid))
          (should (equal (plist-get result :action) "open")))
      (when (file-exists-p temp-file)
        (delete-file temp-file))))
  
  ;; Valid path for nonexistent file
  (let ((result (enkan-center-file-determine-action-pure "/nonexistent/test.org")))
    (should (plist-get result :valid))
    (should (equal (plist-get result :action) "create"))))

;; Test error cases
(ert-deftest test-enkan-center-file-error-cases ()
  "Test error cases for center file operations."
  (let ((result (enkan-center-file-validate-path-pure nil)))
    (should-not (plist-get result :valid))
    (should (string-match-p "not configured" (plist-get result :message))))
  
  (let ((result (enkan-center-file-validate-path-pure "")))
    (should-not (plist-get result :valid))
    (should (string-match-p "empty" (plist-get result :message)))))

(provide 'enkan-center-file-open-command-test)
;;; enkan-center-file-open-command-test.el ends here