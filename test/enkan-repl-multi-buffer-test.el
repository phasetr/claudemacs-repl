;;; enkan-repl-multi-buffer-test.el --- Tests for multi-buffer access -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;;; Commentary:
;; Tests for enkan-repl multi-buffer access functionality

;;; Code:

(require 'ert)
(require 'enkan-repl)

;;;; Project name extraction tests

(ert-deftest enkan-repl-test--extract-project-name ()
  "Test project name extraction from buffer names."
  (let ((temp-dir (file-name-as-directory temporary-file-directory)))
    (should (string= "pt-tools"
                     (enkan-repl--extract-project-name (format "*enkan:%spt-tools/*" temp-dir))))
    (should (string= "enkan-repl"
                     (enkan-repl--extract-project-name (format "*enkan:%senkan-repl/*" temp-dir))))
    (should (string= "project"
                     (enkan-repl--extract-project-name "/path/to/project/")))
    (should (string= "project"
                     (enkan-repl--extract-project-name "/path/to/project")))))

;;;; Alias resolution tests

(ert-deftest enkan-repl-test--resolve-project-name ()
  "Test project name alias resolution."
  (let ((enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl"))))
    (should (string= "pt-tools"
                     (enkan-repl--resolve-project-name "pt")))
    (should (string= "enkan-repl"
                     (enkan-repl--resolve-project-name "er")))
    (should (string= "unknown"
                     (enkan-repl--resolve-project-name "unknown")))
    (should (string= "full-name"
                     (enkan-repl--resolve-project-name "full-name")))))

;;;; Session management tests

(ert-deftest enkan-repl-test--register-session ()
  "Test session registration functionality."
  (let ((enkan-repl-session-list nil))
    ;; Register new session
    (enkan-repl--register-session 4 "pt-tools")
    (should (equal '((4 . "pt-tools")) enkan-repl-session-list))

    ;; Register another session
    (enkan-repl--register-session 5 "enkan-repl")
    (should (equal '((4 . "pt-tools") (5 . "enkan-repl")) enkan-repl-session-list))

    ;; Update existing session
    (enkan-repl--register-session 4 "new-project")
    (should (equal '((4 . "new-project") (5 . "enkan-repl")) enkan-repl-session-list))))

(ert-deftest enkan-repl-test--get-session-by-user-number ()
  "Test getting session by user number (1-4)."
  (let ((enkan-repl-session-list '((4 . "pt-tools") (5 . "enkan-repl") (6 . "claude-code") (7 . "web-app"))))
    (should (string= "pt-tools" (enkan-repl--get-session-by-user-number 1)))
    (should (string= "enkan-repl" (enkan-repl--get-session-by-user-number 2)))
    (should (string= "claude-code" (enkan-repl--get-session-by-user-number 3)))
    (should (string= "web-app" (enkan-repl--get-session-by-user-number 4)))
    (should (null (enkan-repl--get-session-by-user-number 5)))))

;;;; Prefix notation parsing tests

(ert-deftest enkan-repl-test--parse-prefix-notation ()
  "Test prefix notation parsing."
  ;; With prefix
  (should (equal '("pt-tools" . "npm test")
                 (enkan-repl--parse-prefix-notation ":pt-tools npm test")))
  (should (equal '("1" . "ls -la")
                 (enkan-repl--parse-prefix-notation ":1 ls -la")))
  (should (equal '("project" . "echo hello")
                 (enkan-repl--parse-prefix-notation ":project echo hello")))

  ;; Without prefix
  (should (equal '(nil . "npm test")
                 (enkan-repl--parse-prefix-notation "npm test")))
  (should (equal '(nil . "ls -la")
                 (enkan-repl--parse-prefix-notation "ls -la")))

  ;; Edge cases
  (should (equal '(nil . ":invalid")
                 (enkan-repl--parse-prefix-notation ":invalid")))
  (should (equal '(nil . "")
                 (enkan-repl--parse-prefix-notation ""))))

;;;; Center file alias resolution tests

(ert-deftest enkan-repl-test--center-resolve-project-name ()
  "Test center file alias resolution."
  ;; Setup test aliases
  (let ((enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl"))))
    ;; Test alias resolution
    (should (string= "pt-tools" (enkan-repl--center-resolve-project-name "pt")))
    (should (string= "enkan-repl" (enkan-repl--center-resolve-project-name "er")))
    ;; Test non-existent alias (should return original)
    (should (string= "unknown" (enkan-repl--center-resolve-project-name "unknown")))
    ;; Test empty string
    (should (string= "" (enkan-repl--center-resolve-project-name "")))))

(ert-deftest enkan-repl-test--resolve-target-to-directory ()
  "Test target resolution to directory path."
  ;; Mock project aliases
  (let ((enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl"))))
    ;; Mock buffer list to simulate project directories
    (with-temp-buffer
      (rename-buffer "*enkan:/path/to/pt-tools/*")
      (should (string= "/path/to/pt-tools/"
                       (enkan-repl--resolve-target-to-directory "pt"))))))

(ert-deftest enkan-repl-test--resolve-target-to-directory-failure ()
  "Test target resolution failure when no matching buffer exists."
  ;; Setup aliases but no matching buffers
  (let ((enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl"))))
    ;; No matching buffer exists - should return nil
    (should (null (enkan-repl--resolve-target-to-directory "pt")))))

(ert-deftest enkan-repl-test--find-directory-by-project-name ()
  "Test finding directory by project name from buffer names."
  ;; Test with matching buffer
  (with-temp-buffer
    (rename-buffer "*enkan:/Users/test/pt-tools/*")
    (should (string= "/Users/test/pt-tools/"
                     (enkan-repl--find-directory-by-project-name "pt-tools"))))

  ;; Test with no matching buffer
  (should (null (enkan-repl--find-directory-by-project-name "nonexistent-project")))

  ;; Test with complex path
  (with-temp-buffer
    (rename-buffer "*enkan:/complex/path/with spaces/project-name/*")
    (should (string= "/complex/path/with spaces/project-name/"
                     (enkan-repl--find-directory-by-project-name "project-name"))))

(ert-deftest enkan-repl-test--prefix-no-buffer-should-auto-start ()
  "Test that prefix notation auto-starts session when project directory exists."
  (let ((enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl")))
        (enkan-repl-center-file "/tmp/center.org")
        (enkan-repl-center-project-registry '(("pt" . ("pt-tools" . "/existing/pt-tools"))
                                              ("er" . ("enkan-repl" . "/existing/enkan-repl"))))
        (started-sessions '())
        (sent-commands '()))

    ;; Mock auto-start function
    (cl-letf (((symbol-function 'enkan-repl-start-eat)
               (lambda ()
                 (let ((project-name (file-name-nondirectory
                                     (directory-file-name default-directory))))
                   (push project-name started-sessions))))
              ((symbol-function 'enkan-repl--send-text)
               (lambda (text directory)
                 (push (list text directory) sent-commands)
                 t))
              ((symbol-function 'file-directory-p)
               (lambda (dir)
                 (string-match-p "/existing/" dir))))

      (with-temp-buffer
        (let ((buffer-file-name "/tmp/center.org")
              (default-directory "/existing/pt-tools/"))
          (insert ":pt ls")

          ;; Should auto-start session and then send command
          (enkan-repl--send-buffer-content 1 (point-max) "Line" t)

          ;; Verify session was started and command sent
          (should (member "pt-tools" started-sessions))
          (should (equal '(("ls" "/existing/pt-tools/")) sent-commands)))))))

(ert-deftest enkan-repl-test--prefix-no-buffer-invalid-project ()
  "Test that prefix notation shows error when project directory doesn't exist."
  (let ((enkan-repl-project-aliases '(("invalid" . "invalid-project")))
        (enkan-repl-center-file "/tmp/center.org")
        (enkan-repl-center-project-registry '(("invalid" . ("invalid-project" . "/nonexistent/path"))))
        (error-message nil))

    ;; Mock message function to capture error
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq error-message (apply #'format format-string args))))
              ((symbol-function 'file-directory-p)
               (lambda (dir) nil)))  ; No directories exist

      (with-temp-buffer
        (let ((buffer-file-name "/tmp/center.org"))
          (insert ":invalid command")

          ;; Should show error message
          (enkan-repl--send-buffer-content 1 (point-max) "Line" t)

          ;; Verify error message was shown
          (should (string-match-p "Project.*not found" error-message)))))))

(ert-deftest enkan-repl-test--send-line-with-prefix-no-buffer-fallback ()
  "Test sending line with prefix falls back to current directory when target buffer doesn't exist."
  (let ((enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl")))
        (enkan-repl-center-file "/tmp/center.org")
        (buffer-file-name "/tmp/center.org")
        (default-directory "/fallback/directory/")
        (sent-text nil)
        (sent-directory nil))

    ;; Mock enkan-repl--send-text to capture what gets sent
    (cl-letf (((symbol-function 'enkan-repl--send-text)
               (lambda (text directory)
                 (setq sent-text text
                       sent-directory directory)
                 t)))

      (with-temp-buffer
        (let ((buffer-file-name "/tmp/center.org")
              (default-directory "/fallback/directory/"))
          (insert ":pt ls")

          ;; Should fall back to current directory when target buffer doesn't exist
          (enkan-repl--send-buffer-content 1 (point-max) "Line" t)

          ;; Verify command sent (without prefix) to fallback directory
          (should (string= "ls" sent-text))
          (should (string= "/fallback/directory/" sent-directory))))))))

(ert-deftest enkan-repl-test--send-buffer-content-with-prefix ()
  "Test send-buffer-content with prefix notation on center file."
  ;; Setup test environment
  (let ((enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl")))
        (enkan-repl-center-file "/tmp/center.org")
        (buffer-file-name "/tmp/center.org")
        (test-called nil)
        (sent-text nil)
        (sent-directory nil))

    ;; Mock enkan-repl--send-text to capture what gets sent
    (cl-letf (((symbol-function 'enkan-repl--send-text)
               (lambda (text directory)
                 (setq test-called t
                       sent-text text
                       sent-directory directory)
                 t))
              ((symbol-function 'enkan-repl--resolve-target-to-directory)
               (lambda (target)
                 (cond ((string= target "pt") "/path/to/pt-tools/")
                       ((string= target "er") "/path/to/enkan-repl/")
                       (t nil))))
              ((symbol-function 'enkan-repl--get-target-directory-for-buffer)
               (lambda () "/default/directory/")))

      (with-temp-buffer
        (let ((buffer-file-name "/tmp/center.org"))
          (insert ":pt ls -la")

          ;; Test with prefix notation
          (enkan-repl--send-buffer-content 1 (point-max) "Test" t)

          ;; Verify correct text was sent (without :pt prefix)
          (should test-called)
          (should (string= "ls -la" sent-text))
          (should (string= "/path/to/pt-tools/" sent-directory))

          ;; Verify buffer content remains unchanged
          (should (string= ":pt ls -la" (buffer-string))))))))

;;;; Auto registration tests

(ert-deftest enkan-repl-test--auto-register-session ()
  "Test automatic session registration."
  (let ((enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl-project-aliases '(("pt" . "pt-tools"))))

    ;; First registration
    (enkan-repl--auto-register-session "pt-tools")
    (should (equal '((4 . "pt-tools")) enkan-repl-session-list))
    (should (= 1 enkan-repl--session-counter))

    ;; Second registration
    (enkan-repl--auto-register-session "enkan-repl")
    (should (equal '((4 . "pt-tools") (5 . "enkan-repl")) enkan-repl-session-list))
    (should (= 2 enkan-repl--session-counter))

    ;; Duplicate registration (should not register again)
    (enkan-repl--auto-register-session "pt-tools")
    (should (equal '((4 . "pt-tools") (5 . "enkan-repl")) enkan-repl-session-list))
    (should (= 2 enkan-repl--session-counter))

    ;; Registration with alias
    (enkan-repl--auto-register-session "pt")
    (should (equal '((4 . "pt-tools") (5 . "enkan-repl")) enkan-repl-session-list))
    (should (= 2 enkan-repl--session-counter))))

;;;; Session lookup tests

(ert-deftest enkan-repl-test--get-session-by-name-or-alias ()
  "Test session lookup by name or alias."
  (let ((enkan-repl-session-list '((4 . "pt-tools") (5 . "enkan-repl")))
        (enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl"))))

    ;; By project name
    (should (= 4 (enkan-repl--get-session-by-name-or-alias "pt-tools")))
    (should (= 5 (enkan-repl--get-session-by-name-or-alias "enkan-repl")))

    ;; By alias
    (should (= 4 (enkan-repl--get-session-by-name-or-alias "pt")))
    (should (= 5 (enkan-repl--get-session-by-name-or-alias "er")))

    ;; Non-existent
    (should (null (enkan-repl--get-session-by-name-or-alias "unknown")))))

;;;; Project registry tests

(ert-deftest enkan-repl-test--get-project-info-from-registry ()
  "Test project info retrieval from registry."
  (let* ((temp-dir (file-name-as-directory temporary-file-directory))
         (enkan-repl-center-project-registry
          `(("pt" . ("pt-tools" . ,(concat temp-dir "pt-tools")))
            ("er" . ("enkan-repl" . ,(concat temp-dir "enkan-repl")))
            ("cc" . ("claude-code" . ,(concat temp-dir "claude-code"))))))
    ;; Valid aliases
    (should (equal `("pt-tools" . ,(concat temp-dir "pt-tools"))
                   (enkan-repl--get-project-info-from-registry "pt")))
    (should (equal `("enkan-repl" . ,(concat temp-dir "enkan-repl"))
                   (enkan-repl--get-project-info-from-registry "er")))

    ;; Non-existent alias
    (should (null (enkan-repl--get-project-info-from-registry "unknown")))))

(ert-deftest enkan-repl-test--setup-project-session ()
  "Test project session setup function."
  (let* ((temp-dir (file-name-as-directory temporary-file-directory))
         (enkan-repl-center-project-registry
          `(("pt" . ("pt-tools" . ,(concat temp-dir "pt-tools")))
            ("er" . ("enkan-repl" . ,(concat temp-dir "enkan-repl"))))))
    ;; Valid setup
    (should (equal `("pt-tools" . ,(concat temp-dir "pt-tools"))
                   (enkan-repl--setup-project-session "pt" 4)))
    (should (equal `("enkan-repl" . ,(concat temp-dir "enkan-repl"))
                   (enkan-repl--setup-project-session "er" 5)))

    ;; Invalid alias should signal error
    (should-error (enkan-repl--setup-project-session "unknown" 4)
                  :type 'error)))

;;;; Pure function tests for 2-session layout

(ert-deftest enkan-repl-test--get-session-project-name ()
  "Test getting project name from session number."
  (let ((session-list '((4 . "pt-tools") (5 . "enkan-repl") (6 . "claude-code"))))
    (should (string= "pt-tools" (enkan-repl--get-session-project-name 4 session-list)))
    (should (string= "enkan-repl" (enkan-repl--get-session-project-name 5 session-list)))
    (should (string= "claude-code" (enkan-repl--get-session-project-name 6 session-list)))
    (should (null (enkan-repl--get-session-project-name 7 session-list)))
    (should (null (enkan-repl--get-session-project-name 1 session-list)))))

(ert-deftest enkan-repl-test--get-project-path-from-registry ()
  "Test getting project path from registry by project name."
  (let ((project-registry '(("pt" . ("pt-tools" . "/Users/test/pt-tools"))
                            ("er" . ("enkan-repl" . "/Users/test/enkan-repl"))
                            ("cc" . ("claude-code" . "/Users/test/claude-code")))))
    (should (string= "/Users/test/pt-tools"
                     (enkan-repl--get-project-path-from-registry "pt-tools" project-registry)))
    (should (string= "/Users/test/enkan-repl"
                     (enkan-repl--get-project-path-from-registry "enkan-repl" project-registry)))
    (should (string= "/Users/test/claude-code"
                     (enkan-repl--get-project-path-from-registry "claude-code" project-registry)))
    (should (null (enkan-repl--get-project-path-from-registry "nonexistent" project-registry)))
    (should (null (enkan-repl--get-project-path-from-registry "" project-registry)))))

(ert-deftest enkan-repl-test--get-session-project-paths ()
  "Test getting multiple session project paths."
  (let ((session-list '((4 . "pt-tools") (5 . "enkan-repl") (6 . "missing-project")))
        (project-registry '(("pt" . ("pt-tools" . "/Users/test/pt-tools"))
                            ("er" . ("enkan-repl" . "/Users/test/enkan-repl"))
                            ("cc" . ("claude-code" . "/Users/test/claude-code")))))
    ;; Test with sessions 4 and 5 (both should be found)
    (let ((result (enkan-repl--get-session-project-paths '(4 5) session-list project-registry)))
      (should (= 2 (length result)))
      (should (equal '(4 . "/Users/test/pt-tools") (assoc 4 result)))
      (should (equal '(5 . "/Users/test/enkan-repl") (assoc 5 result))))

    ;; Test with session 6 (missing-project not in registry)
    (let ((result (enkan-repl--get-session-project-paths '(6) session-list project-registry)))
      (should (= 0 (length result))))

    ;; Test with non-existent session
    (let ((result (enkan-repl--get-session-project-paths '(7) session-list project-registry)))
      (should (= 0 (length result))))

    ;; Test with mixed valid and invalid sessions
    (let ((result (enkan-repl--get-session-project-paths '(4 6 7) session-list project-registry)))
      (should (= 1 (length result)))
      (should (equal '(4 . "/Users/test/pt-tools") (assoc 4 result))))))

(ert-deftest enkan-repl-test--extract-directory-from-buffer-name-pure ()
  "Test pure function for extracting expanded directory path from buffer name."
  ;; Valid enkan buffer names with tilde expansion
  (should (string= (expand-file-name "~/dev/self/enkan-repl/")
                   (enkan-repl--extract-directory-from-buffer-name-pure "*enkan:~/dev/self/enkan-repl*")))

  (should (string= (expand-file-name "~/pt-tools/")
                   (enkan-repl--extract-directory-from-buffer-name-pure "*enkan:~/pt-tools*")))

  ;; Absolute paths should also work
  (should (string= "/Users/test/project/"
                   (enkan-repl--extract-directory-from-buffer-name-pure "*enkan:/Users/test/project*")))

  ;; Complex paths with spaces
  (should (string= "/complex/path/with spaces/project/"
                   (enkan-repl--extract-directory-from-buffer-name-pure "*enkan:/complex/path/with spaces/project*")))

  ;; Invalid buffer names should return nil
  (should (null (enkan-repl--extract-directory-from-buffer-name-pure "not-enkan-buffer")))
  (should (null (enkan-repl--extract-directory-from-buffer-name-pure "*eat-something*")))
  (should (null (enkan-repl--extract-directory-from-buffer-name-pure nil)))
  (should (null (enkan-repl--extract-directory-from-buffer-name-pure "")))

  ;; Malformed enkan buffer names
  (should (null (enkan-repl--extract-directory-from-buffer-name-pure "*enkan:no-closing-star")))
  (should (null (enkan-repl--extract-directory-from-buffer-name-pure "enkan:/path/no-stars"))))

(ert-deftest enkan-repl-test--buffer-matches-directory-pure ()
  "Test pure function for buffer name and directory matching."
  ;; Matching cases with tilde expansion
  (should (enkan-repl--buffer-matches-directory-pure
           (format "*enkan:%s*" (expand-file-name "~/pt-tools"))
           "~/pt-tools"))
  
  (should (enkan-repl--buffer-matches-directory-pure
           (format "*enkan:%s*" (expand-file-name "~/dev/self/enkan-repl"))
           "~/dev/self/enkan-repl"))
  
  ;; Absolute paths should match
  (should (enkan-repl--buffer-matches-directory-pure
           "*enkan:/Users/test/project*"
           "/Users/test/project"))
  
  ;; Non-matching cases
  (should (null (enkan-repl--buffer-matches-directory-pure
                 "*enkan:/Users/test/other-project*"
                 "~/pt-tools")))
  
  ;; Invalid buffer names
  (should (null (enkan-repl--buffer-matches-directory-pure
                 "*eat-not-enkan*"
                 "~/pt-tools")))
  
  (should (null (enkan-repl--buffer-matches-directory-pure
                 "regular-buffer"
                 "~/pt-tools")))
  
  ;; Invalid input types
  (should (null (enkan-repl--buffer-matches-directory-pure nil "~/pt-tools")))
  (should (null (enkan-repl--buffer-matches-directory-pure "*enkan:/path*" nil))))


;;; enkan-repl-multi-buffer-test.el ends here
