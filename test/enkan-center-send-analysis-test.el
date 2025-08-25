;;; enkan-center-send-analysis-test.el --- Tests for center send content analysis functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for enkan-repl--analyze-center-send-content-pure function.

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

;;;; Tests for enkan-repl--analyze-center-send-content-pure

(ert-deftest test-analyze-center-send-content-prefix-number ()
  "Test analysis of content with numeric prefix argument."
  (let ((result (enkan-repl--analyze-center-send-content-pure "some content" 2)))
    (should (eq 'prefix-number (plist-get result :action)))
    (should (= 2 (plist-get result :data)))))

(ert-deftest test-analyze-center-send-content-escape-directly ()
  "Test analysis of :esc only content."
  (let ((result (enkan-repl--analyze-center-send-content-pure ":esc" nil)))
    (should (eq 'escape-directly (plist-get result :action)))))

(ert-deftest test-analyze-center-send-content-escape-with-whitespace ()
  "Test analysis of :esc with whitespace."
  (let ((result (enkan-repl--analyze-center-send-content-pure "  :esc  " nil)))
    (should (eq 'escape-directly (plist-get result :action)))))

(ert-deftest test-analyze-center-send-content-alias-esc ()
  "Test analysis of :alias esc command."
  (let ((result (enkan-repl--analyze-center-send-content-pure ":er esc" nil)))
    (should (eq 'alias-command (plist-get result :action)))
    (should (equal ":er esc" (plist-get result :data)))))

(ert-deftest test-analyze-center-send-content-alias-ret ()
  "Test analysis of :alias :ret command."
  (let ((result (enkan-repl--analyze-center-send-content-pure ":test :ret" nil)))
    (should (eq 'alias-command (plist-get result :action)))
    (should (equal ":test :ret" (plist-get result :data)))))

(ert-deftest test-analyze-center-send-content-alias-with-whitespace ()
  "Test analysis of alias command with whitespace."
  (let ((result (enkan-repl--analyze-center-send-content-pure "  :myalias esc  " nil)))
    (should (eq 'alias-command (plist-get result :action)))
    (should (equal ":myalias esc" (plist-get result :data)))))

(ert-deftest test-analyze-center-send-content-default-send ()
  "Test analysis of regular text content."
  (let ((result (enkan-repl--analyze-center-send-content-pure "regular text" nil)))
    (should (eq 'default-send (plist-get result :action)))
    (should (equal "regular text" (plist-get result :data)))))

(ert-deftest test-analyze-center-send-content-prefix-takes-priority ()
  "Test that numeric prefix takes priority over :esc."
  (let ((result (enkan-repl--analyze-center-send-content-pure ":esc" 3)))
    (should (eq 'prefix-number (plist-get result :action)))
    (should (= 3 (plist-get result :data)))))

(ert-deftest test-analyze-center-send-content-invalid-alias-format ()
  "Test that invalid alias format falls back to default-send."
  (let ((result (enkan-repl--analyze-center-send-content-pure ":esc some other text" nil)))
    (should (eq 'default-send (plist-get result :action)))))

(ert-deftest test-analyze-center-send-content-complex-alias ()
  "Test analysis with complex alias names."
  (let ((result (enkan-repl--analyze-center-send-content-pure ":my-complex.alias_123 esc" nil)))
    (should (eq 'alias-command (plist-get result :action)))
    (should (equal ":my-complex.alias_123 esc" (plist-get result :data)))))

(ert-deftest test-analyze-center-send-content-function-exists ()
  "Test that enkan-repl--analyze-center-send-content-pure function exists."
  (should (fboundp 'enkan-repl--analyze-center-send-content-pure)))

(provide 'enkan-center-send-analysis-test)
;;; enkan-center-send-analysis-test.el ends here