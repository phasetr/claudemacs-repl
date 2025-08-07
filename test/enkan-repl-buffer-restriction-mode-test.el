;;; enkan-repl-buffer-restriction-mode-test.el --- Tests for buffer restriction mode -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for enkan-repl-buffer-restriction-mode.
;; Tests pattern matching, buffer detection, and mode activation.

;;; Code:

(require 'ert)
(require 'enkan-repl-buffer-restriction-mode)

;;;; Pattern Matching Tests

(ert-deftest test-enkan-repl-buffer-restriction--match-pattern-p ()
  "Test pattern matching for buffer names."
  ;; Test matching *enkan-eat* patterns
  (should (enkan-repl-buffer-restriction--match-pattern-p
           "*enkan-eat*"
           enkan-repl-buffer-restriction-patterns))
  (should (enkan-repl-buffer-restriction--match-pattern-p
           "*enkan-eat-session1*"
           enkan-repl-buffer-restriction-patterns))
  (should (enkan-repl-buffer-restriction--match-pattern-p
           "*enkan-eat-project-name*"
           enkan-repl-buffer-restriction-patterns))
  
  ;; Test matching *claudemacs:* patterns
  (should (enkan-repl-buffer-restriction--match-pattern-p
           "*claudemacs:test*"
           enkan-repl-buffer-restriction-patterns))
  (should (enkan-repl-buffer-restriction--match-pattern-p
           "*claudemacs:/Users/test/project*"
           enkan-repl-buffer-restriction-patterns))
  
  ;; Test non-matching patterns
  (should-not (enkan-repl-buffer-restriction--match-pattern-p
               "*eat*"
               enkan-repl-buffer-restriction-patterns))
  (should-not (enkan-repl-buffer-restriction--match-pattern-p
               "*scratch*"
               enkan-repl-buffer-restriction-patterns))
  (should-not (enkan-repl-buffer-restriction--match-pattern-p
               "normal-buffer.el"
               enkan-repl-buffer-restriction-patterns)))

;;;; Buffer Detection Tests

(ert-deftest test-enkan-repl-buffer-restriction--is-restricted-buffer-p ()
  "Test restricted buffer detection."
  (let ((test-buffers
         '(("*enkan-eat*" . t)
           ("*enkan-eat-test*" . t)
           ("*claudemacs:dir*" . t)
           ("*eat*" . nil)
           ("*scratch*" . nil))))
    (dolist (test test-buffers)
      (let* ((buffer-name (car test))
             (expected (cdr test))
             (buffer (get-buffer-create buffer-name)))
        (unwind-protect
            (if expected
                (should (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer))
              (should-not (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer)))
          (kill-buffer buffer))))))

;;;; Mode Activation Tests

(ert-deftest test-enkan-repl-buffer-restriction-mode-activation ()
  "Test mode activation and deactivation."
  ;; Start with mode off
  (enkan-repl-buffer-restriction-mode-off)
  (should-not enkan-repl-buffer-restriction-mode)
  
  ;; Turn mode on
  (enkan-repl-buffer-restriction-mode-on)
  (should enkan-repl-buffer-restriction-mode)
  
  ;; Turn mode on again (should be idempotent)
  (enkan-repl-buffer-restriction-mode-on)
  (should enkan-repl-buffer-restriction-mode)
  
  ;; Turn mode off
  (enkan-repl-buffer-restriction-mode-off)
  (should-not enkan-repl-buffer-restriction-mode)
  
  ;; Turn mode off again (should be idempotent)
  (enkan-repl-buffer-restriction-mode-off)
  (should-not enkan-repl-buffer-restriction-mode))

;;;; Advice Tests

(ert-deftest test-enkan-repl-buffer-restriction-advice ()
  "Test that advice is properly added and removed."
  ;; Start with mode off
  (enkan-repl-buffer-restriction-mode-off)
  
  ;; Check no advice initially
  (should-not (advice-member-p #'enkan-repl-buffer-restriction--check-switch
                               'switch-to-buffer))
  (should-not (advice-member-p #'enkan-repl-buffer-restriction--check-display
                               'display-buffer))
  (should-not (advice-member-p #'enkan-repl-buffer-restriction--check-pop
                               'pop-to-buffer))
  
  ;; Enable mode
  (enkan-repl-buffer-restriction-mode-on)
  
  ;; Check advice is added
  (should (advice-member-p #'enkan-repl-buffer-restriction--check-switch
                          'switch-to-buffer))
  (should (advice-member-p #'enkan-repl-buffer-restriction--check-display
                          'display-buffer))
  (should (advice-member-p #'enkan-repl-buffer-restriction--check-pop
                          'pop-to-buffer))
  
  ;; Disable mode
  (enkan-repl-buffer-restriction-mode-off)
  
  ;; Check advice is removed
  (should-not (advice-member-p #'enkan-repl-buffer-restriction--check-switch
                               'switch-to-buffer))
  (should-not (advice-member-p #'enkan-repl-buffer-restriction--check-display
                               'display-buffer))
  (should-not (advice-member-p #'enkan-repl-buffer-restriction--check-pop
                               'pop-to-buffer)))

;;;; Integration Tests

(ert-deftest test-enkan-repl-buffer-restriction-integration ()
  "Test buffer switching restriction in practice."
  (let ((restricted-buffer (get-buffer-create "*enkan-eat-test*"))
        (normal-buffer (get-buffer-create "*test-normal*"))
        (original-buffer (current-buffer)))
    (unwind-protect
        (progn
          ;; Enable restriction mode
          (enkan-repl-buffer-restriction-mode-on)
          
          ;; Try to switch to normal buffer (should work)
          (switch-to-buffer normal-buffer)
          (should (eq (current-buffer) normal-buffer))
          
          ;; Try to switch to restricted buffer (should be blocked)
          ;; Note: In actual use, this would redirect. For testing,
          ;; we just verify the buffer is detected as restricted
          (should (enkan-repl-buffer-restriction--is-restricted-buffer-p
                  restricted-buffer))
          
          ;; Disable mode
          (enkan-repl-buffer-restriction-mode-off)
          
          ;; Now switching should work
          (switch-to-buffer restricted-buffer)
          (should (eq (current-buffer) restricted-buffer)))
      ;; Cleanup
      (switch-to-buffer original-buffer)
      (kill-buffer restricted-buffer)
      (kill-buffer normal-buffer)
      (enkan-repl-buffer-restriction-mode-off))))

;;;; Custom Pattern Tests

(ert-deftest test-enkan-repl-buffer-restriction-custom-patterns ()
  "Test customization of restriction patterns."
  (let ((original-patterns enkan-repl-buffer-restriction-patterns))
    (unwind-protect
        (progn
          ;; Add custom pattern
          (setq enkan-repl-buffer-restriction-patterns
                '("^\\*enkan-eat" "^\\*claudemacs:" "^\\*custom-"))
          
          ;; Test new pattern works
          (should (enkan-repl-buffer-restriction--match-pattern-p
                  "*custom-test*"
                  enkan-repl-buffer-restriction-patterns))
          
          ;; Original patterns still work
          (should (enkan-repl-buffer-restriction--match-pattern-p
                  "*enkan-eat*"
                  enkan-repl-buffer-restriction-patterns)))
      ;; Restore original patterns
      (setq enkan-repl-buffer-restriction-patterns original-patterns))))

(provide 'enkan-repl-buffer-restriction-mode-test)

;;; enkan-repl-buffer-restriction-mode-test.el ends here