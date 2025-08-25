;;; center-send-line-esc-test.el --- Tests for center send line ESC functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for the enhanced center-send-line functionality that handles
;; ":esc" commands by calling enkan-repl--send-escape-directly.

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

;;;; Tests for :esc command detection and execution

(ert-deftest test-center-send-line-esc-only-command ()
  "Test that line containing only ':esc' calls enkan-repl--send-escape-directly."
  (let ((escape-directly-called nil)
        (send-buffer-content-called nil))
    ;; Mock the functions
    (cl-letf (((symbol-function 'enkan-repl--send-escape-directly)
               (lambda () (setq escape-directly-called t)))
              ((symbol-function 'enkan-repl--send-buffer-content)
               (lambda (start end desc) (setq send-buffer-content-called t))))
      ;; Test with buffer containing only ":esc"
      (with-temp-buffer
        (insert ":esc")
        (goto-char (point-min))
        (enkan-repl-center-send-line))
      ;; Verify the correct function was called
      (should escape-directly-called)
      (should-not send-buffer-content-called))))

(ert-deftest test-center-send-line-esc-with-whitespace ()
  "Test that line with ':esc' and whitespace still calls enkan-repl--send-escape-directly."
  (let ((escape-directly-called nil))
    (cl-letf (((symbol-function 'enkan-repl--send-escape-directly)
               (lambda () (setq escape-directly-called t))))
      ;; Test with whitespace
      (with-temp-buffer
        (insert "  :esc  ")
        (goto-char (point-min))
        (enkan-repl-center-send-line))
      (should escape-directly-called))))

(ert-deftest test-center-send-line-normal-text-not-esc ()
  "Test that normal text does not call enkan-repl--send-escape-directly."
  (let ((escape-directly-called nil)
        (send-buffer-content-called nil))
    (cl-letf (((symbol-function 'enkan-repl--send-escape-directly)
               (lambda () (setq escape-directly-called t)))
              ((symbol-function 'enkan-repl--send-buffer-content)
               (lambda (start end desc) (setq send-buffer-content-called t))))
      ;; Test with normal text
      (with-temp-buffer
        (insert "normal text")
        (goto-char (point-min))
        (enkan-repl-center-send-line))
      (should-not escape-directly-called)
      (should send-buffer-content-called))))

(ert-deftest test-center-send-line-esc-with-other-text ()
  "Test that ':esc' mixed with other text does not trigger escape behavior."
  (let ((escape-directly-called nil)
        (send-buffer-content-called nil))
    (cl-letf (((symbol-function 'enkan-repl--send-escape-directly)
               (lambda () (setq escape-directly-called t)))
              ((symbol-function 'enkan-repl--send-buffer-content)
               (lambda (start end desc) (setq send-buffer-content-called t))))
      ;; Test with :esc mixed with other text
      (with-temp-buffer
        (insert ":esc some other text")
        (goto-char (point-min))
        (enkan-repl-center-send-line))
      (should-not escape-directly-called)
      (should send-buffer-content-called))))

(ert-deftest test-center-send-line-prefix-arg-overrides-esc ()
  "Test that numeric prefix argument overrides :esc behavior."
  (let ((escape-directly-called nil)
        (send-region-with-prefix-called nil)
        (prefix-used nil))
    (cl-letf (((symbol-function 'enkan-repl--send-escape-directly)
               (lambda () (setq escape-directly-called t)))
              ((symbol-function 'enkan-repl--send-region-with-prefix)
               (lambda (start end prefix)
                 (setq send-region-with-prefix-called t)
                 (setq prefix-used prefix))))

      ;; Test with prefix argument 1
      (with-temp-buffer
        (insert ":esc")
        (goto-char (point-min))
        (enkan-repl-center-send-line 1))

      (should-not escape-directly-called)
      (should send-region-with-prefix-called)
      (should (= prefix-used 1)))))

;;;; Tests for enkan-repl--send-escape-directly function (to be implemented)

(ert-deftest test-send-escape-directly-function-exists ()
  "Test that enkan-repl--send-escape-directly function exists after implementation."
  ;; This test will fail initially, then pass after implementation
  (should (fboundp 'enkan-repl--send-escape-directly)))

(ert-deftest test-center-send-line-alias-esc-delegation ()
  "Test that ':alias :esc' line calls enkan-repl-center-send-region."
  (let ((center-send-region-called nil)
        (region-start nil)
        (region-end nil)
        (action-string nil))
    (cl-letf (((symbol-function 'enkan-repl-center-send-region)
               (lambda (start end action)
                 (setq center-send-region-called t)
                 (setq region-start start)
                 (setq region-end end)
                 (setq action-string action))))

      ;; Test with :er esc line (er is an alias)
      (with-temp-buffer
        (insert ":er esc")
        (goto-char (point-min))
        (enkan-repl-center-send-line))

      ;; Verify the correct delegation occurred
      (should center-send-region-called)
      (should (= region-start 1))
      (should (= region-end 8))  ; Length of ":er esc" + 1
      (should (equal action-string ":er esc")))))

(provide 'center-send-line-esc-test)

;;; center-send-line-esc-test.el ends here
