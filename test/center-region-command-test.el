;;; center-region-command-test.el --- Tests for center file region sending commands -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for center file region sending functionality.
;; These tests verify the integration of alias command parsing with region sending.

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-center-send-region-with-alias-esc ()
  "Test center-send-region with alias esc command."
  (let ((test-buffer (generate-new-buffer "*enkan:test*"))
        (region-text "test region content")
        (escape-sent nil)
        (process-obj nil))
    (with-current-buffer test-buffer
      ;; Set up mock process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process))
    
    (with-temp-buffer
      (insert region-text)
      (let ((start (point-min))
            (end (point-max)))
        ;; Mock functions
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () (list test-buffer)))
                  ((symbol-function 'eat--send-string)
                   (lambda (process string)
                     (when (string= string "\e")
                       (setq escape-sent test-buffer))))
                  ((symbol-function 'process-live-p) 
                   (lambda (proc) t)))
          (let ((enkan-repl-project-aliases '(("test" . "test"))))
            ;; Test the function
            (enkan-repl-center-send-region start end ":test esc")
            (should (eq escape-sent test-buffer))))))
    
    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(ert-deftest test-center-send-region-with-alias-text ()
  "Test center-send-region with alias and text sending."
  (let ((test-buffer (generate-new-buffer "*enkan:test*"))
        (region-text "test region content")
        (text-sent nil)
        (process-obj nil))
    (with-current-buffer test-buffer
      ;; Set up mock process  
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process))
    
    (with-temp-buffer
      (insert region-text)
      (let ((start (point-min))
            (end (point-max)))
        ;; Mock functions
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () (list test-buffer)))
                  ((symbol-function 'enkan-repl--center-send-text-to-buffer)
                   (lambda (text buffer)
                     (setq text-sent text)
                     t))
                  ((symbol-function 'process-live-p) 
                   (lambda (proc) t)))
          (let ((enkan-repl-project-aliases '(("er" . "test"))))
            ;; Test the function
            (enkan-repl-center-send-region start end ":er")
            (should (equal text-sent region-text))))))
    
    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(ert-deftest test-center-send-region-with-alias-ret ()
  "Test center-send-region with alias return command."
  (let ((test-buffer (generate-new-buffer "*enkan:test*"))
        (region-text "test region content")
        (return-sent nil)
        (process-obj nil))
    (with-current-buffer test-buffer
      ;; Set up mock process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process))
    
    (with-temp-buffer
      (insert region-text)
      (let ((start (point-min))
            (end (point-max)))
        ;; Mock functions
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () (list test-buffer)))
                  ((symbol-function 'enkan-repl--center-send-text-to-buffer)
                   (lambda (text buffer)
                     (setq return-sent text)
                     t))
                  ((symbol-function 'process-live-p) 
                   (lambda (proc) t)))
          (let ((enkan-repl-project-aliases '(("er" . "test"))))
            ;; Test the function
            (enkan-repl-center-send-region start end ":er :ret")
            (should (equal return-sent "\r"))))))
    
    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(ert-deftest test-center-send-region-no-buffers ()
  "Test center-send-region with no enkan buffers available."
  (let ((region-text "test region content")
        (message-shown nil))
    (with-temp-buffer
      (insert region-text)
      (let ((start (point-min))
            (end (point-max)))
        ;; Mock functions
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () '()))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq message-shown (apply #'format format-string args)))))
          
          ;; Test the function
          (enkan-repl-center-send-region start end "")
          (should (string-match-p "No active enkan sessions found" message-shown)))))))

(ert-deftest test-center-send-region-invalid-alias ()
  "Test center-send-region with invalid alias format."
  (let ((region-text "test region content")
        (message-shown nil))
    (with-temp-buffer
      (insert region-text)
      (let ((start (point-min))
            (end (point-max)))
        ;; Mock functions
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq message-shown (apply #'format format-string args)))))
          
          ;; Test the function
          (enkan-repl-center-send-region start end "invalid format")
          (should (string-match-p "Invalid action format" message-shown)))))))

(provide 'center-region-command-test)

;;; center-region-command-test.el ends here