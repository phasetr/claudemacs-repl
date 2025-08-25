;;; enkan-filter-valid-buffers-test.el --- Tests for valid buffer filtering functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for enkan-repl--filter-valid-buffers-pure function.

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

;;;; Tests for enkan-repl--filter-valid-buffers-pure

(ert-deftest test-filter-valid-buffers-pure-with-active-process ()
  "Test filtering buffers with active eat processes."
  (let ((test-buffer (generate-new-buffer "*test-eat-buffer*"))
        (normal-buffer (generate-new-buffer "*normal-buffer*")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-live-p)
                   (lambda (proc) (eq proc 'mock-process))))
          ;; Mock active eat process for test-buffer
          (with-current-buffer test-buffer
            (setq-local eat--process 'mock-process))
          
          ;; Test filtering
          (let ((result (enkan-repl--filter-valid-buffers-pure (list test-buffer normal-buffer))))
            (should (= 1 (length result)))
            (should (eq test-buffer (car result)))))
      
      ;; Cleanup
      (kill-buffer test-buffer)
      (kill-buffer normal-buffer))))

(ert-deftest test-filter-valid-buffers-pure-empty-list ()
  "Test filtering empty buffer list."
  (let ((result (enkan-repl--filter-valid-buffers-pure '())))
    (should (null result))))

(ert-deftest test-filter-valid-buffers-pure-no-valid-buffers ()
  "Test filtering when no buffers have eat processes."
  (let ((buffer1 (generate-new-buffer "*test1*"))
        (buffer2 (generate-new-buffer "*test2*")))
    (unwind-protect
        (let ((result (enkan-repl--filter-valid-buffers-pure (list buffer1 buffer2))))
          (should (null result)))
      ;; Cleanup
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest test-filter-valid-buffers-pure-dead-process ()
  "Test filtering buffer with dead eat process."
  (let ((test-buffer (generate-new-buffer "*test-dead-process*")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-live-p)
                   (lambda (proc) nil)))  ; Mock all processes as dead
          ;; Mock dead eat process for test-buffer
          (with-current-buffer test-buffer
            (setq-local eat--process 'dead-mock-process))
          
          ;; Test filtering - should not include buffer with dead process
          (let ((result (enkan-repl--filter-valid-buffers-pure (list test-buffer))))
            (should (null result))))
      
      ;; Cleanup
      (kill-buffer test-buffer))))

(ert-deftest test-filter-valid-buffers-pure-function-exists ()
  "Test that enkan-repl--filter-valid-buffers-pure function exists."
  (should (fboundp 'enkan-repl--filter-valid-buffers-pure)))

(provide 'enkan-filter-valid-buffers-test)
;;; enkan-filter-valid-buffers-test.el ends here