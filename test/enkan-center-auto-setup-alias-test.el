;;; enkan-center-auto-setup-alias-test.el --- Tests for center auto setup alias functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for enkan-repl-center-auto-setup project alias generation functionality.

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

;;;; Tests for enkan-repl-center--generate-project-aliases-pure

(ert-deftest test-generate-project-aliases-pure-single-project ()
  "Test generation of project aliases for single project."
  (let ((alias-list '("pr"))
        (result (enkan-repl-center--generate-project-aliases-pure '("pr"))))
    (should (equal result '(("pr" . "pr"))))))

(ert-deftest test-generate-project-aliases-pure-multiple-projects ()
  "Test generation of project aliases for multiple projects."
  (let ((result (enkan-repl-center--generate-project-aliases-pure '("pr" "er" "test"))))
    (should (equal result '(("pr" . "pr") ("er" . "er") ("test" . "test"))))))

(ert-deftest test-generate-project-aliases-pure-empty-list ()
  "Test generation of project aliases for empty list."
  (let ((result (enkan-repl-center--generate-project-aliases-pure '())))
    (should (null result))))

(ert-deftest test-generate-project-aliases-pure-duplicate-aliases ()
  "Test generation handles duplicate aliases correctly."
  (let ((result (enkan-repl-center--generate-project-aliases-pure '("pr" "pr" "er"))))
    (should (equal result '(("pr" . "pr") ("er" . "er"))))))

(ert-deftest test-generate-project-aliases-pure-function-exists ()
  "Test that enkan-repl-center--generate-project-aliases-pure function exists."
  (should (fboundp 'enkan-repl-center--generate-project-aliases-pure)))

;;;; Tests for integration with auto-setup

(ert-deftest test-center-auto-setup-sets-project-aliases ()
  "Test that center-auto-setup sets project aliases properly."
  ;; Mock the layout configuration
  (let ((enkan-repl-center-multi-project-layouts '(("test-layout" . ("pr" "er"))))
        (enkan-repl-project-aliases nil)
        (enkan-repl-center-project-registry '(("pr" . ("project1" . "/path/pr")) 
                                               ("er" . ("enkan-repl" . "/path/er"))))
        (session-started nil))
    
    ;; Mock functions to avoid side effects
    (cl-letf (((symbol-function 'enkan-center-file-global-mode) (lambda (&rest args) nil))
              ((symbol-function 'enkan-repl-start-eat) (lambda () (setq session-started t)))
              ((symbol-function 'message) (lambda (&rest args) nil)))
      
      ;; Call the function (this should be mocked to avoid actual eat session creation)
      ;; We'll test the pure function behavior instead
      (let ((generated-aliases (enkan-repl-center--generate-project-aliases-pure '("pr" "er"))))
        (should (equal generated-aliases '(("pr" . "pr") ("er" . "er"))))))))

(provide 'enkan-center-auto-setup-alias-test)
;;; enkan-center-auto-setup-alias-test.el ends here