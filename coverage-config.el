;;; coverage-config.el --- Undercover.el configuration for enkan-repl -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for undercover.el coverage measurement of enkan-repl project

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'undercover)

;; Force coverage collection
(setq undercover-force-coverage t)

;; Add current directory to load-path to ensure enkan-repl.el is found
(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))

;; Configure undercover for enkan-repl project
(undercover "enkan-repl.el"
            (:send-report nil)
            (:report-file "coverage-report.json")
            (:report-format 'simplecov))

;; Load the main library (now that it's on load-path and undercover is configured)
(require 'enkan-repl)

;; Load all test files from test directory automatically
(let* ((test-dir (expand-file-name "test"))
       (test-files (directory-files test-dir t "\\.el$" t)))
  (message "Loading test files from: %s" test-dir)
  (dolist (file test-files)
    (when (file-regular-p file)
      (message "Loading test file: %s" (file-name-nondirectory file))
      (condition-case err
          (load-file file)
        (error (message "Warning: Failed to load %s: %s" 
                       (file-name-nondirectory file) err))))))

;; Run tests and generate coverage report
(ert-run-tests-batch)
(undercover-report)

;;; coverage-config.el ends here