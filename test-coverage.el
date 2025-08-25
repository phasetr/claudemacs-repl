;;; test-coverage.el --- Coverage measurement for enkan-repl -*- lexical-binding: t -*-

;; Initialize package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Load undercover first
(require 'undercover)

;; Configure undercover for enkan-repl.el coverage
(undercover "enkan-repl.el" (:send-report nil) (:report-file "coverage.json"))

;; Load main package
(require 'enkan-repl (expand-file-name "enkan-repl.el"))

;; Load and run all tests
(load-file "test/enkan-repl-test.el")
(load-file "test/session-management-test.el")
(load-file "test/window-management-test.el")
(load-file "test/center-unified-send-test.el")
(load-file "test/center-alias-command-test.el")
(load-file "test/center-send-line-test.el")
(load-file "test/center-keybinding-test.el")

;; Run tests and generate coverage report
(ert-run-tests-batch-and-exit)

;;; test-coverage.el ends here