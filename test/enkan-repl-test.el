;;; enkan-repl-test.el --- Tests for enkan-repl pure functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code <noreply@anthropic.com>
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; Comprehensive tests for enkan-repl pure functions using ERT.
;; These tests aim for 100% code coverage of all pure functions.

;;; Code:

(require 'ert)

;; Load the main package and define helper function
(defvar enkan-repl-test-package-dir
  (let ((package-dir (or (and load-file-name
                              (file-name-directory load-file-name))
                         (file-name-directory (locate-library "enkan-repl-test"))
                         default-directory)))
    (expand-file-name "../" package-dir)))

(load (expand-file-name "enkan-repl.el" enkan-repl-test-package-dir))


