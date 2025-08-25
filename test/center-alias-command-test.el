;;; center-alias-command-test.el --- Tests for center file alias command parsing -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for center file alias command functionality.
;; These tests verify parsing of ":er alias esc" format commands.

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-center-parse-alias-command-simple-alias-esc ()
  "Test parsing of simple alias with esc command."
  (let ((result (enkan-repl-center--parse-alias-command-pure ":er esc")))
    (should (equal (plist-get result :valid) t))
    (should (equal (plist-get result :alias) "er"))
    (should (equal (plist-get result :command) :esc))
    (should (equal (plist-get result :text) nil))))

(ert-deftest test-center-parse-alias-command-alias-with-text ()
  "Test parsing of alias with text to send."
  (let ((result (enkan-repl-center--parse-alias-command-pure ":er hello world")))
    (should (equal (plist-get result :valid) t))
    (should (equal (plist-get result :alias) "er"))
    (should (equal (plist-get result :command) :text))
    (should (equal (plist-get result :text) "hello world"))))

(ert-deftest test-center-parse-alias-command-alias-ret ()
  "Test parsing of alias with return command."
  (let ((result (enkan-repl-center--parse-alias-command-pure ":test :ret")))
    (should (equal (plist-get result :valid) t))
    (should (equal (plist-get result :alias) "test"))
    (should (equal (plist-get result :command) :ret))
    (should (equal (plist-get result :text) nil))))

(ert-deftest test-center-parse-alias-command-no-colon-prefix ()
  "Test parsing without : prefix should fail."
  (let ((result (enkan-repl-center--parse-alias-command-pure "er esc")))
    (should (equal (plist-get result :valid) nil))
    (should (string-match-p "Must start with" (plist-get result :message)))))

(ert-deftest test-center-parse-alias-command-only-alias ()
  "Test parsing with only alias (no command or text)."
  (let ((result (enkan-repl-center--parse-alias-command-pure ":er")))
    (should (equal (plist-get result :valid) t))
    (should (equal (plist-get result :alias) "er"))
    (should (equal (plist-get result :command) :empty))
    (should (equal (plist-get result :text) ""))))

(ert-deftest test-center-parse-alias-command-complex-aliases ()
  "Test parsing with complex alias names."
  (let ((result1 (enkan-repl-center--parse-alias-command-pure ":my-complex_alias123 esc"))
        (result2 (enkan-repl-center--parse-alias-command-pure ":test.server :ret")))
    (should (equal (plist-get result1 :valid) t))
    (should (equal (plist-get result1 :alias) "my-complex_alias123"))
    (should (equal (plist-get result2 :valid) t))
    (should (equal (plist-get result2 :alias) "test.server"))))

(ert-deftest test-center-parse-alias-command-empty-input ()
  "Test parsing with empty input."
  (let ((result (enkan-repl-center--parse-alias-command-pure "")))
    (should (equal (plist-get result :valid) nil))
    (should (string-match-p "Must start with" (plist-get result :message)))))

(ert-deftest test-center-parse-alias-command-invalid-format ()
  "Test parsing with invalid format."
  (let ((result (enkan-repl-center--parse-alias-command-pure ":")))
    (should (equal (plist-get result :valid) nil))
    (should (string-match-p "Alias cannot be empty" (plist-get result :message)))))

(provide 'center-alias-command-test)

;;; center-alias-command-test.el ends here