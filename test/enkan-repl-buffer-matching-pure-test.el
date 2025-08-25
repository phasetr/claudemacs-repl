;;; enkan-repl-buffer-matching-pure-test.el --- Tests for buffer matching pure function -*- lexical-binding: t -*-

;;; Commentary:
;; Tests specifically for the actual enkan-repl--buffer-matches-directory-pure function
;; which was not covered by existing tests due to function duplication issues

;;; Code:

(require 'ert)

;; Load the main package to access the actual implementation
(unless (featurep 'enkan-repl)
  (condition-case nil
      (let ((main-file (expand-file-name "../enkan-repl.el" 
                                         (file-name-directory (or load-file-name buffer-file-name)))))
        (when (file-exists-p main-file)
          (load main-file)))
    (error "Could not load enkan-repl.el")))

(ert-deftest test-enkan-repl--buffer-matches-directory-pure-actual-implementation ()
  "Test the actual implementation of enkan-repl--buffer-matches-directory-pure."
  ;; Test non-string buffer-name
  (should (null (enkan-repl--buffer-matches-directory-pure nil "/test")))
  (should (null (enkan-repl--buffer-matches-directory-pure 123 "/test")))
  (should (null (enkan-repl--buffer-matches-directory-pure (list "test") "/test")))
  
  ;; Test non-string target-directory
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkan:/test*" nil)))
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkan:/test*" 123)))
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkan:/test*" (list "/test"))))
  
  ;; Test buffer name that doesn't start with "*enkan:"
  (should (null (enkan-repl--buffer-matches-directory-pure "*eat:/test*" "/test")))
  (should (null (enkan-repl--buffer-matches-directory-pure "enkan:/test*" "/test")))
  (should (null (enkan-repl--buffer-matches-directory-pure "*shell:/test*" "/test")))
  (should (null (enkan-repl--buffer-matches-directory-pure "regular-buffer" "/test")))
  
  ;; Test successful prefix matching
  (let ((test-dir (expand-file-name "/Users/test/project")))
    (should (enkan-repl--buffer-matches-directory-pure
             (format "*enkan:%s*" test-dir)
             "/Users/test/project")))
  
  ;; Test successful prefix matching with tilde expansion
  (let ((home-dir (expand-file-name "~")))
    (should (enkan-repl--buffer-matches-directory-pure
             (format "*enkan:%s/project*" home-dir)
             "~/project")))
  
  ;; Test failed prefix matching
  (should (null (enkan-repl--buffer-matches-directory-pure
                 "*enkan:/Users/test/project1*"
                 "/Users/test/project2")))
  
  ;; Test edge case: exact directory name in buffer but different path
  (should (null (enkan-repl--buffer-matches-directory-pure
                 "*enkan:/different/project*"
                 "/other/project")))
  
  ;; Test complex paths
  (let ((complex-path (expand-file-name "/complex/path/with spaces/project")))
    (should (enkan-repl--buffer-matches-directory-pure
             (format "*enkan:%s*" complex-path)
             "/complex/path/with spaces/project"))))

(ert-deftest test-enkan-repl--buffer-matches-directory-pure-edge-cases ()
  "Test edge cases for the actual buffer matching function."
  ;; Empty strings (should fail due to non-matching pattern)
  (should (null (enkan-repl--buffer-matches-directory-pure "" "")))
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkan:*" "")))
  (should (null (enkan-repl--buffer-matches-directory-pure "" "/path")))
  
  ;; Malformed enkan buffer names
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkan:" "/test")))
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkan/test*" "/test")))
  (should (null (enkan-repl--buffer-matches-directory-pure "enkan:/test*" "/test")))
  
  ;; Buffer names that start with enkan but are not exact pattern
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkan-other:/test*" "/test")))
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkanrepl:/test*" "/test"))))

;;; enkan-repl-buffer-matching-pure-test.el ends here