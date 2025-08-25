;;; enkan-repl-utils.el --- Utility functions for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions for enkan-repl package.
;; This file provides shared functionality for documentation generation
;; and dynamic function listing that can be used by both the main package
;; and build/documentation scripts.

;;; Code:

(require 'cl-lib)

;;;; Session List Pure Functions

(defun enkan-repl--extract-session-info-pure (buffer-name buffer-live-p has-eat-process process-live-p)
  "Pure function to extract session info from buffer properties.
BUFFER-NAME is the name of the buffer.
BUFFER-LIVE-P indicates if buffer is live.
HAS-EAT-PROCESS indicates if buffer has eat--process variable.
PROCESS-LIVE-P indicates if the process is alive.
Returns a plist with :name, :directory, and :status, or nil if not a session."
  (when (and buffer-name
             buffer-live-p
             (string-match "^\\*enkan:\\(.*?\\)\\*$" buffer-name))
    (let ((dir (match-string 1 buffer-name))
          (status (if (and has-eat-process process-live-p)
                      'alive
                    'dead)))
      (list :name buffer-name
            :directory dir
            :status status))))

(defun enkan-repl--collect-sessions-pure (buffer-info-list)
  "Pure function to collect session info from buffer info list.
BUFFER-INFO-LIST is a list of plists with buffer properties.
Each plist should have :name, :live-p, :has-eat-process, and :process-live-p.
Returns a list of session info plists."
  (let ((sessions '()))
    (dolist (buffer-info buffer-info-list)
      (let ((session-info
             (enkan-repl--extract-session-info-pure
              (plist-get buffer-info :name)
              (plist-get buffer-info :live-p)
              (plist-get buffer-info :has-eat-process)
              (plist-get buffer-info :process-live-p))))
        (when session-info
          (push session-info sessions))))
    (nreverse sessions)))

(defun enkan-repl--format-sessions-pure (sessions)
  "Pure function to format sessions for display.
SESSIONS is a list of session info plists.
Returns a formatted string for display."
  (if (null sessions)
      "No active sessions found\n"
    (concat "Active enkan-repl sessions:\n"
            "Press 'd' to delete a session, 'q' to quit\n"
            "─────────────────────────────────────────\n\n"
            (mapconcat
             (lambda (session)
               (format "  %s\n    Directory: %s\n    Status: %s\n\n"
                       (plist-get session :name)
                       (plist-get session :directory)
                       (plist-get session :status)))
             sessions
             ""))))

(defun enkan-repl--find-deletion-bounds-pure (lines current-line)
  "Pure function to find bounds for deletion.
LINES is a list of strings (buffer lines).
CURRENT-LINE is the current line number (0-indexed).
Returns a plist with :start-line and :end-line for deletion bounds,
or nil if not on a session entry."
  (when (and (>= current-line 0)
             (< current-line (length lines)))
    (let ((line (nth current-line lines)))
      ;; Check if we're on or near a session entry
      (cond
       ;; On session name line
       ((string-match "^  \\*enkan:" line)
        (list :start-line current-line
              :end-line (min (+ current-line 4) (length lines))))
       ;; On directory line
       ((and (> current-line 0)
             (string-match "^    Directory:" line)
             (string-match "^  \\*enkan:" (nth (1- current-line) lines)))
        (list :start-line (1- current-line)
              :end-line (min (+ current-line 3) (length lines))))
       ;; On status line
       ((and (> current-line 1)
             (string-match "^    Status:" line)
             (string-match "^  \\*enkan:" (nth (- current-line 2) lines)))
        (list :start-line (- current-line 2)
              :end-line (min (+ current-line 2) (length lines))))
       (t nil)))))

(defun enkan-repl--format-numbered-sessions-pure (sessions)
  "Pure function to format numbered session list.
SESSIONS is a list of session info plists.
Returns a formatted string with numbered sessions."
  (let ((result "")
        (index 1))
    (dolist (session sessions)
      (setq result
            (concat result
                    (format "%d. %s — Directory: %s, Status: %s\n"
                            index
                            (plist-get session :name)
                            (plist-get session :directory)
                            (plist-get session :status))))
      (setq index (1+ index)))
    result))

(defun enkan-repl--format-minibuffer-sessions-pure (sessions)
  "Pure function to format sessions for minibuffer display.
SESSIONS is a list of session info plists.
Returns a list of formatted strings, one per session."
  (let ((index 1)
        (result '()))
    (dolist (session sessions)
      (push (format "%d. %s — Directory: %s, Status: %s"
                    index
                    (plist-get session :name)
                    (plist-get session :directory)
                    (plist-get session :status))
            result)
      (setq index (1+ index)))
    (nreverse result)))

(defun enkan-repl--prepare-session-candidates-pure (sessions)
  "Pure function to prepare candidates for completing-read.
SESSIONS is a list of session info plists.
Returns an alist of (NAME . DESCRIPTION)."
  (mapcar
   (lambda (session)
     (cons (plist-get session :name)
           (format "Directory: %s, Status: %s"
                   (plist-get session :directory)
                   (plist-get session :status))))
   sessions))

(defun enkan-repl--find-session-buffer-pure (selected-name buffer-info-list)
  "Pure function to find buffer for selected session.
SELECTED-NAME is the selected session name.
BUFFER-INFO-LIST is the list of buffer info plists.
Returns the buffer object or nil."
  (let ((buf-info (cl-find-if
                   (lambda (info)
                     (equal (plist-get info :name) selected-name))
                   buffer-info-list)))
    (and buf-info (plist-get buf-info :buffer))))

(defun enkan-repl--session-action-pure (action selected-name)
  "Pure function to determine session action result.
ACTION is the action character (?s, ?d, ?c).
SELECTED-NAME is the selected session name.
Returns a plist with :type and :message."
  (cl-case action
    (?s (list :type 'switch
              :message (format "Switched to session: %s" selected-name)))
    (?d (list :type 'delete
              :message (format "Session deleted: %s" selected-name)))
    (?c (list :type 'cancel
              :message "Cancelled"))
    (t (list :type 'unknown
             :message "Unknown action"))))

(defun enkan-repl-utils--extract-function-info (file-path)
  "Extract public function information from an Emacs Lisp file.
Returns a list of plists, each containing :name, :args, :docstring,
:interactive, and :autoload status."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let ((functions-info '())
          lines
          (current-line 0))
      ;; Split into lines for easier processing
      (setq lines (split-string (buffer-string) "\n"))
      (while (< current-line (length lines))
        (let ((line (nth current-line lines))
              (autoload-p nil))
          ;; Check for autoload marker
          (when (string-match "^;;;###autoload" line)
            (setq autoload-p t)
            (setq current-line (1+ current-line))
            (setq line (nth current-line lines)))
          ;; Check for defun
          (when (and line (string-match "^(defun\\s-+\\([^ \t\n(]+\\)\\s-+\\((.*)\\)" line))
            (let* ((func-name (match-string 1 line))
                   (args-str (match-string 2 line))
                   (args nil)
                   (docstring "")
                   (interactive-p nil))
              ;; Parse arguments safely
              (condition-case nil
                  (setq args (read args-str))
                (error (setq args nil)))
              ;; Look for docstring in next lines (support multi-line)
              (let ((next-line-idx (1+ current-line))
                    (in-docstring nil)
                    (docstring-lines '()))
                (while (and (< next-line-idx (length lines))
                            (< next-line-idx (+ current-line 10))) ; reasonable limit
                  (let ((next-line (nth next-line-idx lines)))
                    (cond
                     ;; Start of docstring
                     ((and (not in-docstring)
                           (string-match "^\\s-*\"\\(.*\\)\"\\s-*$" next-line))
                      ;; Single line docstring
                      (setq docstring (match-string 1 next-line))
                      (setq next-line-idx (length lines))) ; break
                     ((and (not in-docstring)
                           (string-match "^\\s-*\"\\(.*\\)" next-line))
                      ;; Start of multi-line docstring
                      (setq in-docstring t)
                      (push (match-string 1 next-line) docstring-lines))
                     ;; End of multi-line docstring
                     ((and in-docstring
                           (string-match "\\(.*\\)\"\\s-*$" next-line))
                      (push (match-string 1 next-line) docstring-lines)
                      (setq docstring (mapconcat 'identity (nreverse docstring-lines) " "))
                      (setq next-line-idx (length lines))) ; break
                     ;; Middle of multi-line docstring
                     (in-docstring
                      (push (string-trim next-line) docstring-lines))
                     ;; Not a docstring line, break
                     (t (setq next-line-idx (length lines)))))
                  (setq next-line-idx (1+ next-line-idx))))
              ;; Look for (interactive) in the function body
              ;; Only search until we find another defun or end of reasonable function scope
              (let ((body-idx (1+ current-line))
                    (paren-count 0)
                    (in-function t))
                (while (and (< body-idx (length lines))
                            (not interactive-p)
                            in-function
                            (< body-idx (+ current-line 20))) ; reasonable limit
                  (let ((body-line (nth body-idx lines)))
                    (when body-line
                      ;; Stop if we encounter another defun
                      (when (string-match "^(defun\\|^;;;###autoload" body-line)
                        (setq in-function nil))
                      ;; Look for interactive only if still in this function
                      (when (and in-function (string-match "(interactive" body-line))
                        (setq interactive-p t))))
                  (setq body-idx (1+ body-idx))))
              ;; Add to results if public
              (when (or interactive-p autoload-p)
                (push (list :name func-name
                            :args args
                            :docstring docstring
                            :interactive interactive-p
                            :autoload autoload-p)
                      functions-info))))
          (setq current-line (1+ current-line))))
      (nreverse functions-info))))

(defun enkan-repl-utils--generate-function-list-flat (functions)
  "Generate flat function list in org-mode format.
FUNCTIONS should be a list of function info plists from
`enkan-repl-utils--extract-function-info'."
  (let ((result ""))
    (dolist (func functions)
      (let* ((name (plist-get func :name))
             (docstring (plist-get func :docstring))
             (description (if (and docstring (not (zerop (length docstring))))
                              docstring
                            "Send command")))
        (setq result (concat result (format "- ~M-x %s~ - %s\n" name description)))))
    result))

(defun enkan-repl-utils--get-all-public-functions (file-path)
  "Get all public functions from FILE-PATH as flat org-mode list.
Returns a string containing org-mode formatted function list."
  (let* ((functions (enkan-repl-utils--extract-function-info file-path))
         (interactive-functions (cl-remove-if-not
                                (lambda (f) (plist-get f :interactive))
                                functions)))
    (enkan-repl-utils--generate-function-list-flat interactive-functions)))

(defun enkan-repl-utils--extract-category-from-docstring (docstring)
  "Extract category information from DOCSTRING.
Returns category string or nil if not found."
  (when (and docstring (stringp docstring))
    (if (string-match "Category: \\(.*\\)$" docstring)
        (match-string 1 docstring)
      nil)))

(defun enkan-repl-utils--get-categorized-functions (file-path)
  "Get functions organized by category from FILE-PATH.
Returns an alist where each element is (CATEGORY . FUNCTIONS-LIST).
FUNCTIONS-LIST contains plists with :name, :docstring, :category."
  (let ((functions-info (enkan-repl-utils--extract-function-info file-path))
        (categorized-functions '())
        (categories '("Command Palette" "Text Sender" "Session Controller" "Utilities")))
    ;; Extract categories from docstrings and group functions
    (dolist (func functions-info)
      (when (plist-get func :interactive)
        (let* ((docstring (plist-get func :docstring))
               (category (enkan-repl-utils--extract-category-from-docstring docstring))
               (clean-docstring (if category
                                   (replace-regexp-in-string "  Category: .*$" "" docstring)
                                 docstring))
               (func-info `(:name ,(plist-get func :name)
                           :docstring ,clean-docstring
                           :category ,(or category "Uncategorized"))))
          ;; Add to appropriate category list
          (let ((category-entry (assoc (or category "Uncategorized") categorized-functions)))
            (if category-entry
                (setcdr category-entry (append (cdr category-entry) (list func-info)))
              (push (cons (or category "Uncategorized") (list func-info)) categorized-functions))))))
    ;; Sort by predefined category order
    (let ((sorted-result '()))
      (dolist (category categories)
        (let ((category-functions (cdr (assoc category categorized-functions))))
          (when category-functions
            (push (cons category category-functions) sorted-result))))
      ;; Add any uncategorized functions at the end
      (let ((uncategorized (cdr (assoc "Uncategorized" categorized-functions))))
        (when uncategorized
          (push (cons "Uncategorized" uncategorized) sorted-result)))
      (nreverse sorted-result))))

(defun enkan-repl-utils--generate-org-section (category functions header-level)
  "Generate org-mode section for CATEGORY with FUNCTIONS at HEADER-LEVEL.
HEADER-LEVEL should be a number (1 for *, 2 for **, etc.)."
  (let ((header-prefix (make-string header-level ?*))
        (result ""))
    (setq result (concat result (format "%s %s\n\n" header-prefix category)))
    (dolist (func functions)
      (let ((name (plist-get func :name))
            (docstring (plist-get func :docstring)))
        (setq result (concat result (format "- ~M-x %s~ - %s\n" name (or docstring "No description"))))))
    (concat result "\n")))

(defun enkan-repl-utils--generate-categorized-documentation (file-path header-level)
  "Generate categorized documentation from FILE-PATH with HEADER-LEVEL.
Returns org-mode formatted string with functions organized by category."
  (let ((categorized-functions (enkan-repl-utils--get-categorized-functions file-path))
        (result ""))
    (dolist (category-entry categorized-functions)
      (let ((category (car category-entry))
            (functions (cdr category-entry)))
        (setq result (concat result (enkan-repl-utils--generate-org-section category functions header-level)))))
    result))

;; Additional pure functions for session management tests

(defun enkan-repl--validate-session-selection-pure (selection candidates)
  "Pure function to validate session selection against candidates.
SELECTION is the selected string.
CANDIDATES is a list of valid candidate strings.
Returns non-nil if selection is valid."
  (member selection candidates))

(defun enkan-repl--format-session-list-pure (sessions)
  "Pure function to format session list for display.
SESSIONS is a list of plists with :name, :directory, :status.
Returns formatted string."
  (mapconcat
   (lambda (session)
     (let ((name (plist-get session :name))
           (status (plist-get session :status)))
       (format "%s [%s]" 
               (if (string-match "\\*enkan:\\(.*?\\)\\*" name)
                   (match-string 1 name)
                 name)
               (upcase (symbol-name status)))))
   sessions
   "\n"))

(provide 'enkan-repl-utils)

;;; enkan-repl-utils.el ends here
