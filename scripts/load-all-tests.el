;;; load-all-tests.el --- Load all Emacs Lisp test files in the 'test' directory -*- lexical-binding: t; -*-

;; Copyright (C) 2025  phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: tests

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This script is used to dynamically load all Emacs Lisp test files
;; located in the 'test' directory and then run ERT tests.
;;
;;; Code:

(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../test" (file-name-directory load-file-name)))

;; Load the main package file for context
(require 'enkan-repl)

(let* ((test-dir (expand-file-name "../test" (file-name-directory load-file-name)))
       (test-files (directory-files test-dir t "\\.el$" t))
       (excluded-patterns '("center-project-path-test\\.el"))) ; Exclude files that load examples with syntax errors
  (message "Loading test files from: %s" test-dir)
  (dolist (file test-files)
    (when (and (file-regular-p file)
               (not (cl-some (lambda (pattern)
                              (string-match-p pattern (file-name-nondirectory file)))
                            excluded-patterns)))
      (message "Loading test file: %s" (file-name-nondirectory file))
      (condition-case err
          (load-file file)
        (error (message "Warning: Failed to load %s: %s" 
                       (file-name-nondirectory file) err))))))

;; Run all ERT tests
(require 'ert)
(ert-run-tests-batch-and-exit)

(provide 'load-all-tests)
;;; load-all-tests.el ends here