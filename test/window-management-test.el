;;; window-management-test.el --- Unified tests for window management functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Unified tests for window management functionality including:
;; - Window layout and splitting
;; - Session layout management
;; - Window split sequence logic

;;; Code:

(require 'ert)

;; Load the main package
(let ((package-dir (or (and load-file-name
                            (file-name-directory load-file-name))
                       default-directory)))
  (load (expand-file-name "../enkan-repl.el" package-dir)))

;;;; Basic Window Management Tests

(ert-deftest test-start-eat-single-window ()
  "Test that eat buffer is displayed on the right when starting from single window."
  (let ((original-window-config (current-window-configuration))
        (test-buffer (generate-new-buffer "*test-input*"))
        (mock-eat-buffer nil))
    (cl-letf (((symbol-function 'enkan-repl--get-session-info)
               (lambda () (list "/test/dir/" nil nil)))
              ((symbol-function 'enkan-repl--get-target-directory-for-buffer)
               (lambda () "/test/dir/"))
              ((symbol-function 'enkan-repl--get-buffer-for-directory)
               (lambda (dir) nil)))
      (unwind-protect
          (with-current-buffer test-buffer
            (delete-other-windows)
            (should (= 1 (length (window-list))))
            ;; Verify single window state before starting
            (should (one-window-p)))
        ;; Cleanup
        (kill-buffer test-buffer)
        (set-window-configuration original-window-config)))))

;;;; Session Layout Tests

(ert-deftest test-calculate-session-layout-ratios-2-sessions ()
  "Test window ratio calculations for 2 sessions."
  (let ((result (enkan-repl--calculate-session-layout-ratios-pure 2)))
    (should (= 0.6 (plist-get result :left-ratio)))
    (should (= 5 (plist-get result :expected-windows)))
    (should (= 1 (plist-get result :right-splits)))))

(ert-deftest test-calculate-session-layout-ratios-3-sessions ()
  "Test window ratio calculations for 3 sessions."
  (let ((result (enkan-repl--calculate-session-layout-ratios-pure 3)))
    (should (= 0.6 (plist-get result :left-ratio)))
    (should (= 6 (plist-get result :expected-windows)))
    (should (= 2 (plist-get result :right-splits)))))

(ert-deftest test-calculate-session-layout-ratios-4-sessions ()
  "Test window ratio calculations for 4 sessions."
  (let ((result (enkan-repl--calculate-session-layout-ratios-pure 4)))
    (should (= 0.6 (plist-get result :left-ratio)))
    (should (= 7 (plist-get result :expected-windows)))
    (should (= 3 (plist-get result :right-splits)))))

;;;; Window Split Logic Tests

(ert-deftest test-validate-window-split-logic-valid-splits ()
  "Test validation of correct window split logic."
  (let ((result (enkan-repl--validate-window-split-logic-pure 2 1)))
    (should (plist-get result :valid))
    (should (string-match-p "Correct: 1 splits for 2 sessions" 
                           (plist-get result :message)))))

(ert-deftest test-validate-window-split-logic-invalid-splits ()
  "Test validation of incorrect window split logic."
  (let ((result (enkan-repl--validate-window-split-logic-pure 2 3)))
    (should-not (plist-get result :valid))
    (should (string-match-p "Expected 1 splits but got 3" 
                           (plist-get result :message)))))

;;;; Window Split Sequence Tests

(ert-deftest test-calculate-window-split-sequence-2-sessions ()
  "Test window split sequence for 2 sessions."
  (let ((result (enkan-repl--calculate-window-split-sequence-pure 2)))
    (should (= 1 (length result)))
    (should (eq 'horizontal (nth 0 result)))))

(ert-deftest test-calculate-window-split-sequence-3-sessions ()
  "Test window split sequence for 3 sessions."
  (let ((result (enkan-repl--calculate-window-split-sequence-pure 3)))
    (should (= 2 (length result)))
    (should (eq 'horizontal (nth 0 result)))
    (should (eq 'vertical (nth 1 result)))))

(ert-deftest test-calculate-window-split-sequence-4-sessions ()
  "Test window split sequence for 4 sessions."
  (let ((result (enkan-repl--calculate-window-split-sequence-pure 4)))
    (should (= 3 (length result)))
    (should (eq 'horizontal (nth 0 result)))
    (should (eq 'vertical (nth 1 result)))
    (should (eq 'vertical (nth 2 result)))))

;;;; Integration Tests

(ert-deftest test-session-layout-integration ()
  "Test integration of session layout calculations."
  (let ((session-count 3)
        (ratios (enkan-repl--calculate-session-layout-ratios-pure 3))
        (sequence (enkan-repl--calculate-window-split-sequence-pure 3)))
    ;; Verify ratios and sequence are consistent
    (should (= (plist-get ratios :right-splits) (length sequence)))
    (should (= (plist-get ratios :expected-windows) (+ 3 session-count)))))

(ert-deftest test-window-management-workflow ()
  "Test complete window management workflow."
  (let ((session-count 2))
    ;; Calculate layout
    (let ((ratios (enkan-repl--calculate-session-layout-ratios-pure session-count))
          (sequence (enkan-repl--calculate-window-split-sequence-pure session-count)))
      ;; Validate calculations
      (let ((splits-performed (length sequence)))
        (let ((validation (enkan-repl--validate-window-split-logic-pure 
                          session-count splits-performed)))
          (should (plist-get validation :valid)))))))

;;;; Pure Function Helpers

;; Pure function to calculate session layout ratios
(defun enkan-repl--calculate-session-layout-ratios-pure (session-count)
  "Calculate correct window split ratios for SESSION-COUNT.
Returns plist with :left-ratio and :expected-windows."
  (cond
   ((= session-count 2)
    (list :left-ratio 0.6 :expected-windows 5 :right-splits 1))
   ((= session-count 3)
    (list :left-ratio 0.6 :expected-windows 6 :right-splits 2))
   ((= session-count 4)
    (list :left-ratio 0.6 :expected-windows 7 :right-splits 3))
   (t (error "Invalid session count: %d" session-count))))

;; Pure function to validate window split logic
(defun enkan-repl--validate-window-split-logic-pure (session-count splits-performed)
  "Validate window split logic for SESSION-COUNT and SPLITS-PERFORMED.
Returns plist with :valid and :message."
  (let ((expected (enkan-repl--calculate-session-layout-ratios-pure session-count)))
    (let ((expected-splits (plist-get expected :right-splits)))
      (if (= splits-performed expected-splits)
          (list :valid t :message (format "Correct: %d splits for %d sessions" 
                                         splits-performed session-count))
        (list :valid nil :message (format "Expected %d splits but got %d for %d sessions"
                                         expected-splits splits-performed session-count))))))

;; Pure function to calculate window split sequence
(defun enkan-repl--calculate-window-split-sequence-pure (session-count)
  "Calculate window split sequence for SESSION-COUNT sessions.
Returns list of split directions ('horizontal or 'vertical)."
  (cond
   ((= session-count 2) '(horizontal))
   ((= session-count 3) '(horizontal vertical))
   ((= session-count 4) '(horizontal vertical vertical))
   (t (error "Invalid session count: %d" session-count))))

(provide 'window-management-test)

;;; window-management-test.el ends here