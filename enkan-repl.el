;;; enkan-repl.el --- Enhanced repl utilities for enkan -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.1") (claudemacs "0.1.0"))
;; Keywords: enkan ai tools convenience
;; URL: https://github.com/phasetr/enkan-repl
;; License: MIT

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; enkan-repl revives interactive development culture for the AI era.
;; Send thoughts with `send-region`, receive Claude's response, and deepen
;; your thinking further - a new REPL experience woven by Emacs and AI.
;;
;; Core Features:
;; - Text sending capabilities (region, buffer, line, etc.)
;; - Persistent org-mode input files with template system
;; - Directory-based Claude session targeting
;; - M-x driven workflow with no default keybindings
;; - File path support for Claude direct reading
;;
;; Quick Start:
;;   M-x enkan-repl-start-claudemacs
;;   M-x enkan-repl-open-project-input-file
;;
;; This package builds upon claudemacs and eat.  We extend our deepest
;; gratitude to their authors and contributors.

;;; Code:

(require 'cl-lib)

;; Load utility functions (require for template generation and cheatsheet)
(when (locate-library "enkan-repl-utils")
  (require 'enkan-repl-utils))

;; Declare external functions to avoid byte-compile warnings
(declare-function enkan-repl-utils--extract-function-info "enkan-repl-utils" (file-path))

;; Compatibility check
(when (locate-library "claudemacs")
  (require 'claudemacs))

;; Declare external functions to avoid byte-compiler warnings
(declare-function eat--send-string "eat" (process string))
(defvar eat--process)
(defvar eat-mode)

;;;; Custom Variables

(defgroup enkan-repl nil
  "Enhanced REPL utilities for enkan."
  :group 'tools
  :prefix "enkan-repl-")

;;;; Constants

(defconst enkan-repl-path-separator "--"
  "String to replace path separators (/) in filenames.
This is a fixed part of the enkan-repl filename encoding scheme
and should not be changed to maintain compatibility and security.")

(defconst enkan-repl-default-template-filename "default.org"
  "Default template filename for enkan-repl input files.
This file should be located in the package directory and contains
the standard template structure for new project input files.")

(defconst enkan-repl-file-prefix "enkan"
  "Prefix for enkan-repl encoded filenames.
This is a fixed part of the filename encoding scheme and should not be changed
to maintain compatibility with existing project files.")

(defconst enkan-repl-command-categories
  '("Command Palette"
    "Text Sender"
    "Session Controller"
    "Utilities")
  "Command category names for enkan-repl package.
Command Palette is positioned first as the primary interface.
This structure is used for documentation generation and organization.")

;;;; Internal Variables

(defvar enkan-repl-debug-mode nil
  "When non-nil, enable debug messages for send operations.")

;; Declare external variable from constants file
(defvar enkan-repl-cheatsheet-candidates nil
  "Precompiled list of cheatsheet candidates from constants file.")

(defun enkan-repl--find-template-directory ()
  "Find directory containing default template file.
Returns the directory path where default.org is located."
  (or
   ;; Try load-file-name first (when loading with load command)
   (and load-file-name
        (let ((dir (file-name-directory load-file-name)))
          (when (file-exists-p (expand-file-name enkan-repl-default-template-filename dir))
            dir)))
   ;; Try buffer-file-name (when evaluating in buffer)
   (and buffer-file-name
        (let ((dir (file-name-directory buffer-file-name)))
          (when (file-exists-p (expand-file-name enkan-repl-default-template-filename dir))
            dir)))
   ;; Try locate-library result directory
   (and (locate-library "enkan-repl")
        (let ((dir (file-name-directory (locate-library "enkan-repl"))))
          (when (file-exists-p (expand-file-name enkan-repl-default-template-filename dir))
            dir)))
   ;; For straight.el: try repos directory if build directory doesn't have template
   (and (locate-library "enkan-repl")
        (let* ((build-dir (file-name-directory (locate-library "enkan-repl")))
               (straight-build-p (string-match-p "/straight/build/" build-dir))
               (repos-dir (and straight-build-p
                               (replace-regexp-in-string "/straight/build/" "/straight/repos/" build-dir))))
          (when (and repos-dir (file-exists-p (expand-file-name enkan-repl-default-template-filename repos-dir)))
            repos-dir)))
   ;; Search for default.org in load-path directories
   (cl-some
    (lambda (dir)
      (let ((expanded-dir (expand-file-name dir))
            (default-org-path (expand-file-name enkan-repl-default-template-filename (expand-file-name dir))))
        (when (file-exists-p default-org-path)
          expanded-dir)))
    load-path)
   ;; Final fallback to current directory
   default-directory))

(defvar enkan-repl--package-directory
  (enkan-repl--find-template-directory)
  "Package directory determined at load time.")

(defcustom enkan-repl-template-file nil
  "Template file path for enkan-repl input files.
When nil, uses the default template (default.org).
When set to a file path, uses that file as the template."
  :type
  '(choice
    (const :tag "Use default template" nil)
    (file :tag "Custom template file"))
  :group
  'enkan-repl)

;;;; Core Functions

;;;; File Naming and Path Management

(defun enkan-repl--encode-full-path (path)
  "Encode PATH by replacing forward slashes with the configured separator.
Example: \\='/Users/phasetr/project1/\\=' -> \\='enkan--Users--phasetr--project1\\='"
  (enkan-repl--encode-full-path-pure
   path
   enkan-repl-file-prefix
   enkan-repl-path-separator))

(defun enkan-repl--decode-full-path (encoded-name)
  "Decode ENCODED-NAME back to original path.
Example: \\='enkan--Users--phasetr--project1\\=' -> \\='/Users/phasetr/project1/\\='"
  (enkan-repl--decode-full-path-pure
   encoded-name
   enkan-repl-file-prefix
   enkan-repl-path-separator))

(defun enkan-repl--get-project-file-path (&optional directory)
  "Get the full path for the project file based on DIRECTORY.
If DIRECTORY is nil, use the current `default-directory'."
  (let*
      ((target-dir (or directory default-directory))
       (encoded-name (enkan-repl--encode-full-path target-dir))
       (filename (concat encoded-name ".org")))
    (expand-file-name filename target-dir)))

;;;; Template Loading Functions

(defun enkan-repl--get-package-directory ()
  "Get package directory for template files.
Returns directory path where default templates are located."
  (or (enkan-repl--find-template-directory)
      enkan-repl--package-directory
      default-directory))

(defvar enkan-repl--testing-mode nil
  "When non-nil, disable interactive prompts for testing.")

(defun enkan-repl--handle-missing-template (template-path)
  "Handle missing template file with interactive user choices.
TEMPLATE-PATH is the path to the missing template file.
Returns the template path to use, or nil to use default template."
  ;; In testing mode, just return nil to use default template
  (if
      enkan-repl--testing-mode
      nil
    (let
        ((choice
          (read-char-choice
           (format "Template file not found: %s\n\nChoose action:\n(d) Use default template\n(s) Select different file\n(c) Create new template file\n(q) Quit\n\nYour choice: "
                   template-path)
           '(?d ?s ?c ?q))))
      (cl-case choice
        (?d
         (message "Using default template instead")
         nil)  ; Use default template
        (?s
         (let
             ((selected-file
               (read-file-name "Select template file: "
                               user-emacs-directory nil t nil
                               (lambda (name) (string-suffix-p ".org" name)))))
           (if
               (file-exists-p selected-file)
               selected-file
             (progn
               (message "Selected file does not exist. Using default template.")
               nil))))
        (?c
         (when
             (y-or-n-p
              (format "Create new template file at %s? " template-path))
           (let
               ((dir (file-name-directory template-path)))
             ;; Ensure directory exists
             (unless
                 (file-exists-p dir)
               (make-directory dir t))
             ;; Create template with dynamically generated command list
             (with-temp-file template-path
               (insert "* Quick Start\n\n")
               (insert "** Custom Template\n")
               (insert "This is your custom template.\n")
               (insert "Edit this content to match your needs.\n\n")
               (insert "** Available Commands\n\n")
               ;; Insert static function list
               (insert "- ~M-x enkan-repl-send-region~ - Send selected text to Claude\n")
               (insert "- ~M-x enkan-repl-send-buffer~ - Send entire buffer content\n")
               (insert "- ~M-x enkan-repl-send-rest-of-buffer~ - Send from cursor position to end\n")
               (insert "- ~M-x enkan-repl-send-line~ - Send current line\n")
               (insert "- ~M-x enkan-repl-send-enter~ - Send enter key for prompts\n")
               (insert "- ~M-x enkan-repl-send-1~ - Send '1' for numbered choice prompts\n")
               (insert "- ~M-x enkan-repl-send-2~ - Send '2' for numbered choice prompts\n")
               (insert "- ~M-x enkan-repl-send-3~ - Send '3' for numbered choice prompts\n")
               (insert "- ~M-x enkan-repl-send-escape~ - Send ESC key to interrupt operations\n")
               (insert "- ~M-x enkan-repl-open-project-input-file~ - Open or create project input file\n")
               (insert "- ~M-x enkan-repl-start-claudemacs~ - Start claudemacs session\n")
               (insert "- ~M-x enkan-repl-setup-window-layout~ - Set up convenient window layout\n")
               (insert "- ~M-x enkan-repl-output-template~ - Export template for customization\n")
               (insert "- ~M-x enkan-repl-status~ - Show diagnostic information\n")
               (insert "- ~M-x enkan-repl-toggle-debug-mode~ - Toggle debug mode\n")
               (insert "- ~M-x enkan-repl-enable-debug-mode~ - Enable debug mode\n")
               (insert "- ~M-x enkan-repl-disable-debug-mode~ - Disable debug mode\n")
               (insert "\n** Working Notes\n")
               (insert "Write your thoughts and notes here.\n")
               (insert "Send specific parts to Claude Code using the commands above.\n"))
             (message "Created new template file: %s" template-path)
             template-path)))
        (?q
         (user-error "Operation cancelled by user"))))))

(defun enkan-repl--load-template ()
  "Load template content based on enkan-repl-template-file setting.
Returns template content as string, using embedded template as fallback."
  (let
      ((template-path
        (cond
         ;; If custom template file is specified, check if it exists
         (enkan-repl-template-file
          (let ((custom-path (expand-file-name enkan-repl-template-file)))
            (if (file-exists-p custom-path)
                custom-path
              ;; Handle missing custom template interactively
              (or (enkan-repl--handle-missing-template custom-path)
                  ;; Fall back to default if user chooses default
                  (expand-file-name enkan-repl-default-template-filename
                                    (enkan-repl--get-package-directory))))))
         ;; Otherwise, look for default template in package directory
         (t (expand-file-name enkan-repl-default-template-filename
                              (enkan-repl--get-package-directory))))))
    (if (and template-path (file-exists-p template-path))
        (with-temp-buffer
          (insert-file-contents template-path)
          (buffer-string))
      ;; Fall back to embedded template
      (enkan-repl--get-embedded-template))))

(defun enkan-repl--get-categorized-functions ()
  "Get categorized function list from constants file.
Returns categorized functions as string, or falls back to static list."
  (condition-case nil
      (progn
        ;; Try to load constants file if not already loaded
        (unless (featurep 'enkan-repl-constants)
          (require 'enkan-repl-constants))
        ;; Group functions by category
        (let ((categories (make-hash-table :test 'equal))
              (category-order '("Command Palette" "Text Sender" "Session Controller" "Utilities")))
          ;; Group functions by category
          (dolist (candidate enkan-repl-cheatsheet-candidates)
            (let* ((func-name (car candidate))
                   (description (cdr candidate))
                   (category (if (string-match "Category: \\([^\"]+\\)" description)
                                 (match-string 1 description)
                               "Other")))
              (unless (gethash category categories)
                (puthash category nil categories))
              (puthash category
                       (cons (cons func-name description) (gethash category categories))
                       categories)))
          ;; Generate categorized output
          (let ((result ""))
            (dolist (category category-order)
              (when (gethash category categories)
                (setq result (concat result "** " category "\n\n"))
                (dolist (func (reverse (gethash category categories)))
                  (let* ((func-name (car func))
                         (description (cdr func))
                         (clean-desc (replace-regexp-in-string "  Category: [^\"]*" "" description)))
                    (setq result (concat result "- ~M-x " func-name "~ - " clean-desc "\n"))))
                (setq result (concat result "\n"))))
            result)))
    ;; Fallback to static list if constants file is unavailable
    (error (enkan-repl--get-static-functions))))

(defun enkan-repl--get-static-functions ()
  "Return static function list as fallback when constants unavailable."
  (concat "** Command Palette\n\n"
          "- ~M-x enkan-repl-cheatsheet~ - Display interactive cheatsheet for enkan-repl commands.\n\n"
          "** Text Sender\n\n"
          "- ~M-x enkan-repl-send-region~ - Send the text in region from START to END to claudemacs.\n"
          "- ~M-x enkan-repl-send-buffer~ - Send the entire current buffer to claudemacs.\n"
          "- ~M-x enkan-repl-send-rest-of-buffer~ - Send rest of buffer from cursor position to end to claudemacs.\n"
          "- ~M-x enkan-repl-send-line~ - Send the current line to claudemacs.\n"
          "- ~M-x enkan-repl-send-enter~ - Send enter key to claudemacs buffer.\n"
          "- ~M-x enkan-repl-send-1~ - Send \\='1\\=' to claudemacs buffer for numbered choice prompts.\n"
          "- ~M-x enkan-repl-send-2~ - Send \\='2\\=' to claudemacs buffer for numbered choice prompts.\n"
          "- ~M-x enkan-repl-send-3~ - Send \\='3\\=' to claudemacs buffer for numbered choice prompts.\n"
          "- ~M-x enkan-repl-send-escape~ - Send ESC key to claudemacs buffer.\n\n"
          "** Session Controller\n\n"
          "- ~M-x enkan-repl-start-claudemacs~ - Start claudemacs and change to appropriate directory.\n"
          "- ~M-x enkan-repl-setup-window-layout~ - Set up window layout with org file on left and claudemacs on right.\n\n"
          "** Utilities\n\n"
          "- ~M-x enkan-repl-open-project-input-file~ - Open or create project input file for current directory.\n"
          "- ~M-x enkan-repl-output-template~ - Output current template content to a new buffer for customization.\n"
          "- ~M-x enkan-repl-status~ - Show detailed diagnostic information for troubleshooting connection issues.\n"
          "- ~M-x enkan-repl-toggle-debug-mode~ - Toggle debug mode for enkan-repl operations.\n"
          "- ~M-x enkan-repl-enable-debug-mode~ - Enable debug mode for enkan-repl operations.\n"
          "- ~M-x enkan-repl-disable-debug-mode~ - Disable debug mode for enkan-repl operations.\n\n"))

(defun enkan-repl--get-embedded-template ()
  "Return embedded default template content as string."
  (concat "#+TITLE: Claude Input File\n\n"
          "This is the default project template for enkan-repl.\n\n"
          "* Quick Start\n\n"
          "Describe your main objective here.\n\n"
          "* Context\n\n"
          "Provide relevant background information, constraints, or requirements.\n\n"
          "* Approach\n\n"
          "Outline your planned approach or methodology.\n\n"
          "* Functions/Commands\n\n"
          "We open the following functions/commands.\n\n"
          (enkan-repl--get-categorized-functions)
          "* Notes\n\n"
          "Add your thoughts, observations, or important points here.\n\n"
          "* Next Steps\n\n"
          "- [ ] Item 1\n"
          "- [ ] Item 2\n"
          "- [ ] Item 3\n"))

;;;; Pure Functions for Directory/Buffer Detection

(defun enkan-repl--encode-full-path-pure (path prefix separator)
  "Pure function: Encode PATH with PREFIX and SEPARATOR.
PATH: Directory path to encode.
PREFIX: Prefix string to add (e.g., \\='enkan\\=').
SEPARATOR: String to replace '/' with (e.g., '--').
Example: \='/Users/project\=' + \='enkan\=' + \='--\=' -> \='enkan--Users--project\='"
  (let*
      ((expanded-path (expand-file-name path))
       (cleaned-path
        (if
            (string-suffix-p "/" expanded-path)
            (substring expanded-path 0 -1)
          expanded-path)))
    (concat prefix (replace-regexp-in-string "/" separator cleaned-path))))

(defun enkan-repl--decode-full-path-pure (encoded-name prefix separator)
  "Pure function: Decode ENCODED-NAME with PREFIX and SEPARATOR.
ENCODED-NAME: Encoded filename to decode.
PREFIX: Expected prefix string (e.g., \\='enkan\\=').
SEPARATOR: String to replace with '/' (e.g., '--').
Example: \='enkan--Users--project\=' + \='enkan\=' + \='--\=' -> \='/Users/project/\='"
  (when (string-prefix-p prefix encoded-name)
    (let
        ((path-part (substring encoded-name (length prefix))))  ; Remove prefix
      (concat (replace-regexp-in-string (regexp-quote separator) "/" path-part) "/"))))

;;;; Debug Functions

(defun enkan-repl--debug-message (format-string &rest args)
  "Print debug message if debug mode is enabled.
FORMAT-STRING is the format string.  ARGS are the arguments."
  (when enkan-repl-debug-mode
    (apply #'message (concat "[CLAUDEMACS-REPL-DEBUG] " format-string) args)))

(defun enkan-repl--sanitize-content (content)
  "Sanitize CONTENT to ensure it can be safely sent to claudemacs.
This function handles edge cases with special characters and ensures
proper formatting for terminal input.  Also addresses Claude Code
interpretation issues and Mac region selection problems."
  (when content
    (let ((sanitized content))
      (enkan-repl--debug-message "Input content: %S" content)
      (enkan-repl--debug-message "Input content length: %d" (length content))
      ;; Step 1: Remove all carriage return characters (\r) immediately
      ;; This is the most likely cause of "extra newlines" on Mac
      (setq sanitized (replace-regexp-in-string "\r" "" sanitized))
      (enkan-repl--debug-message "After \\r removal: %S" sanitized)
      ;; Step 2: Normalize all newline variations to single LF
      ;; Handles \r\n, \n\r, and other combinations that might remain
      (setq sanitized (replace-regexp-in-string "\\(\r\n\\|\n\r\\)" "\n" sanitized))
      (enkan-repl--debug-message "After newline normalization: %S" sanitized)
      ;; Step 3: Collapse consecutive newlines to single newlines
      ;; This prevents sending empty lines that might confuse Claude Code
      (setq sanitized (replace-regexp-in-string "\n\n+" "\n" sanitized))
      (enkan-repl--debug-message "After consecutive newline collapse: %S" sanitized)
      ;; Step 4: Remove other problematic control characters
      ;; Keep only \n (newline) and \t (tab) among control characters
      (setq
       sanitized
       (replace-regexp-in-string
        "[[:cntrl:]]"
        (lambda (match)
          (cond
           ;; Keep normal newlines and tabs
           ((or (string= match "\n") (string= match "\t")) match)
           ;; Remove other control characters
           (t "")))
        sanitized))
      (enkan-repl--debug-message "After control char cleanup: %S" sanitized);
      ;; Step 5: Remove Unicode line separators that might cause issues
      ;; U+0085 (NEL), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR)
      (setq sanitized (replace-regexp-in-string "[\u0085\u2028\u2029]" "" sanitized))
      (enkan-repl--debug-message "After Unicode separator removal: %S" sanitized)
      ;; Step 6: Final cleanup of any remaining problematic characters at end
      (setq sanitized (replace-regexp-in-string "[\x0B\x0C\x0E-\x1F]+\\'" "" sanitized))
      (enkan-repl--debug-message "After final cleanup: %S" sanitized)
      ;; Step 7: File path interpretation workaround (existing logic)
      (enkan-repl--debug-message
       "File path pattern match: %s"
       (if (string-match-p "~/[^[:space:]]*\\.[a-zA-Z0-9]+\\'" sanitized) "YES" "NO"))
      (enkan-repl--debug-message
       "Punctuation pattern match: %s"
       (if (string-match-p "[.!?。！？]\\'" sanitized) "YES" "NO"))
      (when
          (and
           (string-match-p "~/[^[:space:]]*\\.[a-zA-Z0-9]+\\'" sanitized)
           (not (string-match-p "[.!?。！？]\\'" sanitized)))
        (enkan-repl--debug-message "Adding end marker to prevent file path interpretation")
        (setq sanitized (concat sanitized "\n(This text is added by enkan-repl as a workaround for Claude Code's special interpretation of file paths)")))
      ;; Step 8: Final trim and validation
      (setq sanitized (string-trim sanitized))
      (enkan-repl--debug-message "Final sanitized content: %S" sanitized)
      (enkan-repl--debug-message "Final content length: %d" (length sanitized))
      sanitized)))

;;;; Target Directory Detection Functions

(defun enkan-repl--get-target-directory-for-buffer ()
  "Get the target directory for current buffer.
For persistent files, extract directory from filename.
For other buffers, use current `default-directory'."
  (if
      (and
       buffer-file-name
       (string-match-p "enkan-.+\\.org$" (file-name-nondirectory buffer-file-name)))
      ;; This is a persistent file, extract directory from filename
      (enkan-repl--decode-full-path
       (file-name-base (file-name-nondirectory buffer-file-name)))
    ;; Use current directory
    default-directory))

(defun enkan-repl--get-buffer-for-directory (&optional directory)
  "Get the claudemacs buffer for DIRECTORY if it exists and is live.
If DIRECTORY is nil, use current `default-directory'."
  (let
      ((target-dir (or directory default-directory))
       (matching-buffer nil))
    (cl-block search-buffers
      (dolist (buf (buffer-list))
        (let
            ((name (buffer-name buf))
             (default-dir
               (with-current-buffer buf
                 (when (boundp 'default-directory)
                   default-directory)))
             (eat-mode
              (with-current-buffer buf
                (and (boundp 'eat-mode) eat-mode))))
          (when
              (and (buffer-live-p buf)
                   name     ; Ensure name is not nil
                   ;; Check for directory-specific claudemacs buffer
                   (or
                    (and
                     (string-match-p "^\\*claudemacs:" name)
                     (string-prefix-p (concat "*claudemacs:" target-dir) name))
                    ;; Fallback to generic buffers only if they match directory
                    (and
                     (or
                      (string= name "*claude*")
                      (string= name "*claudemacs*"))
                     (when default-dir
                       (string=
                        (file-truename default-dir)
                        (file-truename target-dir))))
                    ;; Check for eat-mode buffers with claude in name
                    (and eat-mode
                         (string-match-p "claude" name)
                         (when default-dir
                           (string=
                            (file-truename default-dir)
                            (file-truename target-dir))))))
            (setq matching-buffer buf)
            (cl-return-from search-buffers)))))
    matching-buffer))

(defun enkan-repl--buffer-matches-directory (buffer target-dir)
  "Check if BUFFER's working directory matches TARGET-DIR."
  (with-current-buffer buffer
    (when (boundp 'default-directory)
      (string=
       (file-truename default-directory)
       (file-truename target-dir)))))

;;;; Send Functions - Internal Helpers

(defun enkan-repl--can-send-text (&optional directory)
  "Check if text can actually be sent to claudemacs (strict check).
If DIRECTORY is provided, check for claudemacs in that directory.
Otherwise, use current `default-directory'."
  (let ((claude-buffer (enkan-repl--get-buffer-for-directory directory)))
    (when claude-buffer
      (with-current-buffer claude-buffer
        (and
         (boundp 'eat--process)
         eat--process
         (process-live-p eat--process))))))

(defun enkan-repl--send-text (text &optional directory)
  "Send TEXT to claudemacs buffer.
If DIRECTORY is provided, send to claudemacs in that directory.
Otherwise, use current `default-directory'."
  (let
      ((claude-buffer
        (enkan-repl--get-buffer-for-directory directory)))
    (enkan-repl--debug-message
     "send-text called with text length: %d, buffer: %s"
     (length text) (if claude-buffer (buffer-name claude-buffer) "nil"))
    (when
        (and claude-buffer
             (with-current-buffer claude-buffer
               (and (boundp 'eat--process)
                    eat--process
                    (process-live-p eat--process))))
      (enkan-repl--debug-message
       "Sending text to claude buffer: %S"
       (substring text 0 (min 50 (length text))))
      (with-current-buffer claude-buffer
        (eat--send-string eat--process text)
        (eat--send-string eat--process "\r")
        (enkan-repl--debug-message "Text sent successfully")
        t))))

(defun enkan-repl--send-numbered-choice (number)
  "Send NUMBER as string to claudemacs buffer for numbered choice prompt.
NUMBER should be a string (e.g., \\='1\\=', \\='2\\=', \\='3\\=') or empty string for enter."
  (let
      ((target-dir (enkan-repl--get-target-directory-for-buffer)))
    (if
        (enkan-repl--can-send-text target-dir)
        (progn
          (enkan-repl--send-text number target-dir)
          (if
              (= (length number) 0)
              (message "Sent enter to Claude")
            (message "Sent '%s' to Claude" number)))
      (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))))

(defun enkan-repl--send-escape-directly ()
  "Send ESC key to claudemacs buffer directly."
  (let
      ((claude-buffer
        (enkan-repl--get-buffer-for-directory
         (enkan-repl--get-target-directory-for-buffer))))
    (if
        (and claude-buffer
             (with-current-buffer claude-buffer
               (and (boundp 'eat--process)
                    eat--process
                    (process-live-p eat--process))))
        (progn
          (enkan-repl--debug-message "Sending ESC key to claude buffer")
          (with-current-buffer claude-buffer
            (eat--send-string eat--process "\e")
            (enkan-repl--debug-message "ESC key sent successfully"))
          (message "Sent ESC to Claude"))
      (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))))

(defun enkan-repl--send-buffer-content (start end content-description &optional skip-empty-check)
  "Send buffer content from START to END with CONTENT-DESCRIPTION.
START and END define the region to send.
CONTENT-DESCRIPTION is used in success message.
If SKIP-EMPTY-CHECK is non-nil, send content even if empty."
  (let*
      ((raw-content
        (buffer-substring-no-properties start end))
       (content
        (enkan-repl--sanitize-content (string-trim raw-content)))
       (target-dir
        (enkan-repl--get-target-directory-for-buffer)))
    (enkan-repl--debug-message
     "Raw content length: %d, trimmed: %d"
     (length raw-content)
     (length content))
    (enkan-repl--debug-message
     "Content empty?: %s, target-dir: %s"
     (= (length content) 0)
     target-dir)
    (if
        (or skip-empty-check (and content (not (= (length content) 0))))
        (progn
          (enkan-repl--debug-message "Attempting to send content")
          (if (enkan-repl--send-text content target-dir)
              (message "%s sent to Claude (%d characters)"
                       content-description (length content))
            (message "❌ Cannot send - no matching claudemacs buffer found for this directory")))
      (message "No content to send (empty or whitespace only)"))))

;;;; Public API - Send Functions

;;;###autoload
(defun enkan-repl-send-region (start end)
  "Send the text in region from START to END to claudemacs.

Category: Text Sender"
  (interactive "r")
  (when (use-region-p)
    (enkan-repl--send-buffer-content start end "Region")))

;;;###autoload
(defun enkan-repl-send-buffer ()
  "Send the entire current buffer to claudemacs.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-buffer-content
   (point-min) (point-max)
   (format "File %s" (buffer-name)) t))

;;;###autoload
(defun enkan-repl-send-rest-of-buffer ()
  "Send rest of buffer from cursor position to end to claudemacs.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-buffer-content (point) (point-max) "Rest of buffer"))

;;;###autoload
(defun enkan-repl-send-line ()
  "Send the current line to claudemacs.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-buffer-content
   (line-beginning-position) (line-end-position) "Line"))

;;;###autoload
(defun enkan-repl-send-enter ()
  "Send enter key to claudemacs buffer.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice ""))

;;;###autoload
(defun enkan-repl-send-1 ()
  "Send \\='1\\=' to claudemacs buffer for numbered choice prompt.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice "1"))

;;;###autoload
(defun enkan-repl-send-2 ()
  "Send \\='2\\=' to claudemacs buffer for numbered choice prompt.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice "2"))

;;;###autoload
(defun enkan-repl-send-3 ()
  "Send \\='3\\=' to claudemacs buffer for numbered choice prompt.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice "3"))

;;;###autoload
(defun enkan-repl-send-escape ()
  "Send ESC key to claudemacs buffer.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-escape-directly))

(defun enkan-repl--create-project-input-file (target-directory)
  "Create project input file for TARGET-DIRECTORY from template.
Returns the created file path."
  (let*
      ((file-path
        (enkan-repl--get-project-file-path target-directory))
       (template-content
        (enkan-repl--load-template)))
    ;; Ensure target directory exists
    (unless (file-exists-p (file-name-directory file-path))
      (make-directory (file-name-directory file-path) t))
    ;; Write template content to target location
    (with-temp-file file-path
      (insert template-content))
    file-path))

;;;###autoload
(defun enkan-repl-open-project-input-file (&optional directory)
  "Open or create project input file for DIRECTORY.
If DIRECTORY is nil, use current `default-directory'.
If project input file exists, open it directly.
If not exists, create from template then open.

Category: Utilities"
  (interactive)
  (let*
      ((target-dir
        (or directory default-directory))
       (file-path
        (enkan-repl--get-project-file-path target-dir)))
    ;; Create file if it doesn't exist
    (unless
        (file-exists-p file-path)
      (enkan-repl--create-project-input-file target-dir))
    ;; Open the file
    (let ((buffer (find-file file-path)))
      (with-current-buffer buffer
        ;; Set up org mode
        (when (fboundp 'org-mode)
          (unless (eq major-mode 'org-mode)
            (let ((org-mode-hook nil))
              (ignore org-mode-hook)  ; Suppress unused variable warning
              (org-mode)))))
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (message "Project input file ready: %s" (file-name-nondirectory file-path)))))

(defun enkan-repl--get-session-info ()
  "Get session information for current buffer.
Returns (target-dir existing-buffer can-send) as a list."
  (let ((target-dir (enkan-repl--get-target-directory-for-buffer)))
    (let ((existing-buffer (enkan-repl--get-buffer-for-directory target-dir)))
      (let ((can-send (enkan-repl--can-send-text target-dir)))
        (list target-dir existing-buffer can-send)))))

(defun enkan-repl--execute-claudemacs-command (command-symbol success-message)
  "Execute a claudemacs command with proper setup and error handling.
This function temporarily changes the working directory to the target directory,
executes the command, and restores the original directory afterward.

COMMAND-SYMBOL is the command to execute (e.g., \\='claudemacs-transient-menu).
SUCCESS-MESSAGE is the message format string for successful execution."
  (let ((target-dir (car (enkan-repl--get-session-info)))
        (original-default-directory default-directory))
    (unwind-protect
        (progn
          (cd target-dir)
          ;; Ensure claudemacs is loaded and command is available
          (unless (require 'claudemacs nil t)
            (error "Claudemacs package not found or failed to load.  Please install and configure claudemacs first"))
          (unless (fboundp command-symbol)
            (error "%s not available after loading claudemacs package" command-symbol))
          ;; Execute the command
          (funcall command-symbol)
          (message success-message target-dir))
      ;; Always restore original directory
      (cd original-default-directory))))

(defun enkan-repl--handle-dead-session (existing-buffer target-dir prompt-format restart-func)
  "Handle dead session with user confirmation.
EXISTING-BUFFER is the dead buffer to handle.
TARGET-DIR is the target directory.
PROMPT-FORMAT is the format string for `y-or-n-p' prompt.
RESTART-FUNC is a zero-argument function to call for restart.
  When nil, only cleanup is performed (buffer is killed).
  When provided, the function is called after cleanup for restart."
  (when (y-or-n-p (format prompt-format target-dir))
    (kill-buffer existing-buffer)
    (if restart-func
        (funcall restart-func)
      (message "Removed dead claudemacs session buffer in: %s" target-dir))))

;;;###autoload
(defun enkan-repl-start-claudemacs ()
  "Start claudemacs and change to appropriate directory.
Determines directory from current buffer filename if it's a persistent file.
Checks for existing sessions to prevent double startup.

Category: Session Controller"
  (interactive)
  (let* ((session-info (enkan-repl--get-session-info))
         (target-dir (nth 0 session-info))
         (existing-buffer (nth 1 session-info))
         (can-send (nth 2 session-info)))
    (cond
     ;; Active session already exists
     (can-send
      (message "Claudemacs session already running in: %s (buffer: %s)"
               target-dir (buffer-name existing-buffer)))
     ;; Dead session exists - offer to restart
     (existing-buffer
      (enkan-repl--handle-dead-session
       existing-buffer target-dir
       "Dead claudemacs session found in %s. Restart? "
       #'enkan-repl-start-claudemacs))
     ;; No existing session - start new one
     (t
      (enkan-repl--execute-claudemacs-command
       'claudemacs-transient-menu
       "Started claudemacs in: %s")
      ;; Verify startup succeeded
      (unless (enkan-repl--can-send-text target-dir)
        (error "Failed to start claudemacs session in: %s" target-dir))))))

;;;###autoload
(defun enkan-repl-finish-claudemacs ()
  "Terminate claudemacs session and close its buffer.
Determines directory from current buffer filename if it's a persistent file.
Calls claudemacs-kill for proper session termination.

Category: Session Controller"
  (interactive)
  (let* ((session-info (enkan-repl--get-session-info))
         (target-dir (nth 0 session-info))
         (existing-buffer (nth 1 session-info))
         (can-send (nth 2 session-info)))
    (cond
     ;; No session found
     ((not existing-buffer)
      (message "No claudemacs session found for directory: %s" target-dir))
     ;; Active session - terminate using claudemacs-kill
     (can-send
      (when (y-or-n-p (format "Terminate active claudemacs session in %s? " target-dir))
        (enkan-repl--execute-claudemacs-command
         'claudemacs-kill
         "Terminated claudemacs in: %s")))
     ;; Dead session exists - offer to clean up
     (existing-buffer
      (enkan-repl--handle-dead-session
       existing-buffer target-dir
       "Clean up dead claudemacs session in %s? "
       nil)))))

;;;###autoload
(defun enkan-repl-setup-window-layout ()
  "Set up window layout with org file on left and claudemacs on right.
This is the author's preference - customize as needed.

Category: Session Controller"
  (interactive)
  (let* ((target-dir (enkan-repl--get-target-directory-for-buffer))
         (input-file-path (enkan-repl--get-project-file-path target-dir))
         (input-buffer (when (file-exists-p input-file-path)
                         (find-file-noselect input-file-path))))
    (delete-other-windows)
    ;; Set up left window with input file
    (if input-buffer
        (switch-to-buffer input-buffer)
      ;; If no input file exists, create it
      (enkan-repl-open-project-input-file target-dir))
    ;; Set up right window with claudemacs
    (split-window-right)
    (other-window 1)
    (let ((claudemacs-buf (enkan-repl--get-buffer-for-directory target-dir)))
      (if claudemacs-buf
          (switch-to-buffer claudemacs-buf)
        (message "claudemacs buffer not found. Run (enkan-repl-start-claudemacs) first.")))
    ;; Return to left window (input file)
    (other-window -1)
    (message "Window layout setup complete")))

;;;###autoload
(defun enkan-repl-output-template ()
  "Output current template content to a new buffer for customization.

Category: Utilities"
  (interactive)
  (let*
      ((template-content
        (enkan-repl--load-template))
       (template-file
        (or enkan-repl-template-file "default"))
       (buffer-name
        (format "*enkan-repl-template-%s*"
                (file-name-base template-file))))
    (if template-content
        (progn
          (with-output-to-temp-buffer buffer-name
            (princ template-content))
          (with-current-buffer buffer-name
            (when (fboundp 'org-mode)
              (let ((org-mode-hook nil))
                (ignore org-mode-hook)  ; Suppress unused variable warning
                (org-mode))))
          (message "Template output to buffer: %s" buffer-name)
          t)
      (message "Template not found: %s" template-file)
      nil)))

;;; Debug and Utility Functions

;; These functions are primarily for debugging and troubleshooting.
;; They can be safely removed in production environments if needed.

;;;###autoload
(defun enkan-repl-status ()
  "Show detailed diagnostic information for troubleshooting connection issues.

Category: Utilities"
  (interactive)
  (let*
      ((target-dir (enkan-repl--get-target-directory-for-buffer))
       (claude-buffer (enkan-repl--get-buffer-for-directory target-dir))
       (can-send (enkan-repl--can-send-text target-dir))
       (expected-session (concat "*claudemacs:" target-dir "*"))
       (all-buffers (buffer-list))
       (claudemacs-sessions
        (seq-filter
         (lambda (buf)
           (string-match-p "^\\*claudemacs:" (buffer-name buf)))
         all-buffers))
       (other-claude-buffers
        (seq-filter
         (lambda (buf)
           (let
               ((name (buffer-name buf)))
             (or (string= name "*claude*")
                 (string= name "*claudemacs*")
                 (and (with-current-buffer buf
                        (and (boundp 'eat-mode) eat-mode))
                      (string-match-p "claude" name)))))
         all-buffers)))
    (with-output-to-temp-buffer
        "*enkan-repl-diagnostic*"
      (princ "═══ enkan-repl Diagnostic Report ═══\n\n")
      ;; Basic information
      (princ (format "Target directory: %s\n" target-dir))
      (princ (format "Current buffer: %s\n\n" (buffer-name)))
      ;; Session status
      (if
          can-send
          (princ
           (format "[✅] Session status: CONNECTED\n  Active session: %s\n\n"
                   (buffer-name claude-buffer)))
        (princ "[❌] Session status: NO MATCHING SESSION FOUND\n\n"))
      ;; Target directory analysis
      (princ "Target directory analysis:\n")
      (princ (format "  Expected session: %s\n" expected-session))
      (if
          (get-buffer expected-session)
          (let
              ((process-status
                (with-current-buffer expected-session
                  (if
                      (and (boundp 'eat--process)
                           eat--process
                           (process-live-p eat--process))
                      "alive" "dead"))))
            (princ (format "  → Session exists (process: %s)\n\n" process-status)))
        (princ "  → This session does not exist\n\n"))
      ;; Path encoding check
      (if
          (and
           (buffer-file-name)
           (string-match-p "enkan-.+\\.org$" (file-name-nondirectory (buffer-file-name))))
          (let*
              ((encoded-name (file-name-base (file-name-nondirectory (buffer-file-name))))
               (decoded-path (enkan-repl--decode-full-path encoded-name))
               (re-encoded (enkan-repl--encode-full-path decoded-path)))
            (princ "[✅] Path encoding: OK\n")
            (princ (format "  File: %s\n" (file-name-nondirectory (buffer-file-name))))
            (princ (format "  → Decoded to: %s\n" decoded-path))
            (if (string= encoded-name re-encoded)
                (princ "  → Re-encoding: ✓ Consistent\n")
              (princ (format "  → Re-encoding: ⚠️  Inconsistent (%s)\n" re-encoded)))
            (when (string-match-p "--.*--" (file-name-nondirectory (buffer-file-name)))
              (princ "  ⚠️  Warning: Path contains multiple '--' which may cause parsing issues\n"))
            (princ "\n"))
        (princ "[ℹ️] Path encoding: Not applicable (not a persistent file)\n\n"))
      ;; Found sessions
      (princ "Found claudemacs sessions:\n")
      (if claudemacs-sessions
          (dolist (buf claudemacs-sessions)
            (let*
                ((name (buffer-name buf))
                 (process-status
                  (with-current-buffer buf
                    (if
                        (and (boundp 'eat--process)
                             eat--process
                             (process-live-p eat--process))
                        "alive" "dead"))))
              (princ (format "  - %s (process: %s)\n" name process-status))))
        (princ "  (no claudemacs sessions found)\n"))
      ;; Other Claude buffers
      (when other-claude-buffers
        (princ "\nOther Claude-related buffers:\n")
        (dolist (buf other-claude-buffers)
          (let*
              ((name (buffer-name buf))
               (dir
                (with-current-buffer buf
                  (when (boundp 'default-directory)
                    default-directory)))
               (process-status
                (with-current-buffer buf
                  (if
                      (and (boundp 'eat--process)
                           eat--process
                           (process-live-p eat--process))
                      "alive" "dead"))))
            (princ (format "  - %s in %s (process: %s)\n" name (or dir "unknown") process-status)))))
      (princ "\n")
      ;; Recommended actions
      (princ "Recommended actions:\n")
      (cond
       (can-send
        (princ "✓ Connection is working. You can send text to Claude.\n"))
       ((not claudemacs-sessions)
        (princ "1. Start claudemacs in target directory: (enkan-repl-start-claudemacs)\n")
        (princ "2. Or run: M-x enkan-repl-start-claudemacs\n"))
       (t
        (princ "1. Start claudemacs for target directory: (enkan-repl-start-claudemacs)\n")
        (princ "2. Or switch to existing session directory:\n")
        (dolist (buf claudemacs-sessions)
          (let
              ((session-path
                (replace-regexp-in-string
                 "^\\*claudemacs:\\(.*\\)\\*$" "\\1"
                 (buffer-name buf))))
            (princ (format "   - %s\n" session-path))))))
      (princ "\nFor more help: M-x enkan-repl-open-project-input-file\n"))))

;;;###autoload
(defun enkan-repl-toggle-debug-mode ()
  "Toggle debug mode for enkan-repl operations.

Category: Utilities"
  (interactive)
  (setq
   enkan-repl-debug-mode
   (not enkan-repl-debug-mode))
  (message
   "enkan-repl debug mode: %s"
   (if enkan-repl-debug-mode "ENABLED" "DISABLED")))

;;;###autoload
(defun enkan-repl-enable-debug-mode ()
  "Enable debug mode for enkan-repl operations.

Category: Utilities"
  (interactive)
  (setq enkan-repl-debug-mode t)
  (message "enkan-repl debug mode: ENABLED"))

;;;###autoload
(defun enkan-repl-disable-debug-mode ()
  "Disable debug mode for enkan-repl operations.

Category: Utilities"
  (interactive)
  (setq enkan-repl-debug-mode nil)
  (message "enkan-repl debug mode: DISABLED"))

;;; Interactive Cheatsheet Feature

;;;###autoload
(defun enkan-repl-cheatsheet ()
  "Display interactive cheatsheet for enkan-repl commands.

Category: Command Palette"
  (interactive)
  (unless (featurep 'enkan-repl-constants)
    (require 'enkan-repl-constants))
  (let ((candidates enkan-repl-cheatsheet-candidates))
    (let ((completion-extra-properties
           `(:annotation-function
             (lambda (candidate)
               (let ((description (alist-get candidate ',candidates nil nil #'string=)))
                 (when description
                   (format " — %s" description)))))))
      (let ((selected-command (completing-read "enkan-repl commands: " candidates)))
        (when selected-command
          (call-interactively (intern selected-command)))))))

(provide 'enkan-repl)

;;; enkan-repl.el ends here
