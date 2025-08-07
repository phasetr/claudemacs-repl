;;; enkan-repl-buffer-restriction-mode.el --- Buffer navigation restriction for enkan-repl -*- lexical-binding: t -*-

;;; Commentary:

;; This mode restricts navigation to specific buffers (eat and claudemacs).
;; When enabled, attempting to switch to restricted buffers will redirect
;; to the appropriate input file or setup window layout.
;;
;; Main features:
;; - Global mode to restrict buffer access
;; - Explicit on/off functions (not toggle)
;; - Mode line indication with soft red background when active
;; - Redirection to input files when access is blocked

;;; Code:

(require 'cl-lib)

;;;; Custom Variables

(defgroup enkan-repl-buffer-restriction nil
  "Buffer navigation restrictions for enkan-repl."
  :group 'enkan-repl
  :prefix "enkan-repl-buffer-restriction-")

(defcustom enkan-repl-buffer-restriction-patterns
  '("^\\*enkan-eat" "^\\*claudemacs:")
  "List of regexp patterns for buffers to restrict access to.
Default includes *enkan-eat* and *claudemacs:* buffers."
  :type '(repeat regexp)
  :group 'enkan-repl-buffer-restriction)

;;;; Variables

(defvar enkan-repl-buffer-restriction-mode nil
  "Non-nil when buffer restriction mode is active.")

(defvar enkan-repl-buffer-restriction--redirecting nil
  "Flag to prevent recursive redirection.")

(defvar enkan-repl-buffer-restriction--pending-timer nil
  "Timer for pending layout adjustment.")

;;;; Pure Functions

(defun enkan-repl-buffer-restriction--match-pattern-p (buffer-name patterns)
  "Check if BUFFER-NAME matches any of the PATTERNS.
PATTERNS is a list of regexp strings."
  (cl-some (lambda (pattern)
             (string-match-p pattern buffer-name))
           patterns))

;;;; Buffer Detection

(defun enkan-repl-buffer-restriction--is-restricted-buffer-p (buffer)
  "Check if BUFFER should be restricted.
Returns t if the buffer name matches restricted patterns."
  (when (bufferp buffer)
    (let ((buffer-name (buffer-name buffer)))
      (enkan-repl-buffer-restriction--match-pattern-p
       buffer-name
       enkan-repl-buffer-restriction-patterns))))

;;;; Redirection Functions

(defun enkan-repl-buffer-restriction--find-input-file ()
  "Find the appropriate input file for the current directory.
Returns the buffer if found, nil otherwise."
  (when (fboundp 'enkan-repl--get-project-file-path)
    (let* ((file-path (enkan-repl--get-project-file-path))
           (buffer (when (file-exists-p file-path)
                    (find-file-noselect file-path))))
      buffer)))

(defun enkan-repl-buffer-restriction--redirect-from-restricted ()
  "After opening restricted buffer, set up window layout.
The buffer is displayed but immediately followed by window layout setup."
  (unless enkan-repl-buffer-restriction--redirecting
    ;; Set flag immediately to prevent multiple invocations
    (setq enkan-repl-buffer-restriction--redirecting t)
    ;; Cancel any pending timer
    (when (timerp enkan-repl-buffer-restriction--pending-timer)
      (cancel-timer enkan-repl-buffer-restriction--pending-timer))
    ;; Set new timer
    (setq enkan-repl-buffer-restriction--pending-timer
          (run-at-time 0.1 nil
                       (lambda ()
                         (when (and (fboundp 'enkan-repl-setup-window-layout)
                                    enkan-repl-buffer-restriction-mode) ; Check mode is still on
                           ;; Temporarily disable advice during layout setup
                           (let ((enkan-repl-buffer-restriction--redirecting t))
                             (enkan-repl-setup-window-layout)
                             (message "Window layout adjusted (buffer restriction active)"))
                           ;; Reset flag after a delay to allow normal operation
                           (run-at-time 1.0 nil
                                        (lambda ()
                                          (setq enkan-repl-buffer-restriction--redirecting nil)))))))))

;;;; Advice Functions

(defun enkan-repl-buffer-restriction--check-switch (orig-fun buffer-or-name &rest args)
  "Advice for `switch-to-buffer' to check restrictions.
ORIG-FUN is the original function.
BUFFER-OR-NAME is the buffer to switch to.
ARGS are additional arguments."
  (let ((result (apply orig-fun buffer-or-name args)))
    ;; After switching, check if we need to adjust layout
    (when (and enkan-repl-buffer-restriction-mode
               (not enkan-repl-buffer-restriction--redirecting))
      (let ((buffer (get-buffer buffer-or-name)))
        (when (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer)
          (enkan-repl-buffer-restriction--redirect-from-restricted))))
    result))

(defun enkan-repl-buffer-restriction--check-display (orig-fun buffer-or-name &rest args)
  "Advice for `display-buffer' to check restrictions.
ORIG-FUN is the original function.
BUFFER-OR-NAME is the buffer to display.
ARGS are additional arguments."
  (let ((result (apply orig-fun buffer-or-name args)))
    ;; After displaying, check if we need to adjust layout
    (when (and enkan-repl-buffer-restriction-mode
               (not enkan-repl-buffer-restriction--redirecting))
      (let ((buffer (get-buffer buffer-or-name)))
        (when (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer)
          (enkan-repl-buffer-restriction--redirect-from-restricted))))
    result))

(defun enkan-repl-buffer-restriction--check-pop (orig-fun buffer-or-name &rest args)
  "Advice for `pop-to-buffer' to check restrictions.
ORIG-FUN is the original function.
BUFFER-OR-NAME is the buffer to pop to.
ARGS are additional arguments."
  (let ((result (apply orig-fun buffer-or-name args)))
    ;; After popping, check if we need to adjust layout
    (when (and enkan-repl-buffer-restriction-mode
               (not enkan-repl-buffer-restriction--redirecting))
      (let ((buffer (get-buffer buffer-or-name)))
        (when (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer)
          (enkan-repl-buffer-restriction--redirect-from-restricted))))
    result))

;;;; Mode Line Functions

(defun enkan-repl-buffer-restriction--mode-line-indicator ()
  "Return mode line indicator for buffer restriction status."
  (when (and enkan-repl-buffer-restriction-mode
             (string-match-p "enkan--" (buffer-name)))
    (propertize " [BR] "
                'face '(:foreground "#ff6600" :weight bold)
                'help-echo "Buffer Restriction ON: claudemacs/eat navigation triggers layout adjustment")))

(defun enkan-repl-buffer-restriction--setup-mode-line ()
  "Set up mode line indicator for buffer restriction mode."
  ;; Initialize mode-line-misc-info if it's nil or not a list
  (unless (and (boundp 'mode-line-misc-info)
               (listp mode-line-misc-info))
    (setq mode-line-misc-info '("")))
  
  ;; Ensure mode-line-misc-info is in mode-line-format
  (unless (and (boundp 'mode-line-format)
               (or (memq 'mode-line-misc-info mode-line-format)
                   (member '("" mode-line-misc-info) mode-line-format)))
    ;; Add mode-line-misc-info to mode-line-format if not present
    (when (listp mode-line-format)
      (setq mode-line-format
            (append mode-line-format '(mode-line-misc-info)))))
  
  ;; Add our indicator to mode-line-misc-info
  (add-to-list 'mode-line-misc-info
               '(:eval (when (and enkan-repl-buffer-restriction-mode
                                  (string-match-p "enkan--" (buffer-name)))
                         (propertize " [BR]"
                                     'face '(:foreground "#ff6600" :weight bold)
                                     'help-echo "Buffer Restriction Mode is ON")))
               t)
  ;; Force mode line update
  (force-mode-line-update t))

(defun enkan-repl-buffer-restriction--remove-mode-line ()
  "Remove mode line indicator for buffer restriction mode."
  ;; Remove our indicator from mode-line-misc-info
  (setq mode-line-misc-info
        (delete '(:eval (when (and enkan-repl-buffer-restriction-mode
                                   (string-match-p "enkan--" (buffer-name)))
                          (propertize " [BR]"
                                      'face '(:foreground "#ff6600" :weight bold)
                                      'help-echo "Buffer Restriction Mode is ON")))
                mode-line-misc-info)))


;;;; Public API

;;;###autoload
(defun enkan-repl-buffer-restriction-mode-on ()
  "Enable buffer restriction mode.
This prevents navigation to *enkan-eat* and *claudemacs:* buffers."
  (interactive)
  (unless enkan-repl-buffer-restriction-mode
    ;; Enable mode
    (setq enkan-repl-buffer-restriction-mode t)
    
    ;; Add advice to buffer switching functions
    (advice-add 'switch-to-buffer :around
                #'enkan-repl-buffer-restriction--check-switch)
    (advice-add 'display-buffer :around
                #'enkan-repl-buffer-restriction--check-display)
    (advice-add 'pop-to-buffer :around
                #'enkan-repl-buffer-restriction--check-pop)
    
    ;; Set up mode line indicator
    (enkan-repl-buffer-restriction--setup-mode-line)
    (force-mode-line-update t)
    
    ;; Check all visible windows for restricted buffers
    (let ((restricted-window-found nil))
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer)
            (setq restricted-window-found t))))
      ;; If any restricted buffer is visible, adjust layout
      (when restricted-window-found
        (enkan-repl-buffer-restriction--redirect-from-restricted)))
    
    (message "Buffer restriction mode enabled")))

;;;###autoload
(defun enkan-repl-buffer-restriction-mode-off ()
  "Disable buffer restriction mode."
  (interactive)
  (when enkan-repl-buffer-restriction-mode
    ;; Disable mode
    (setq enkan-repl-buffer-restriction-mode nil)
    
    ;; Cancel any pending timer
    (when (timerp enkan-repl-buffer-restriction--pending-timer)
      (cancel-timer enkan-repl-buffer-restriction--pending-timer)
      (setq enkan-repl-buffer-restriction--pending-timer nil))
    
    ;; Reset flags
    (setq enkan-repl-buffer-restriction--redirecting nil)
    
    ;; Remove mode line indicator
    (enkan-repl-buffer-restriction--remove-mode-line)
    (force-mode-line-update t)
    
    ;; Remove advice
    (advice-remove 'switch-to-buffer
                   #'enkan-repl-buffer-restriction--check-switch)
    (advice-remove 'display-buffer
                   #'enkan-repl-buffer-restriction--check-display)
    (advice-remove 'pop-to-buffer
                   #'enkan-repl-buffer-restriction--check-pop)
    
    (message "Buffer restriction mode disabled")))

;;;###autoload
(defun enkan-repl-buffer-restriction-mode-status ()
  "Show current status of buffer restriction mode."
  (interactive)
  (let ((mode-status (if enkan-repl-buffer-restriction-mode "ON" "OFF"))
        (buffer-match (string-match-p "enkan--" (buffer-name)))
        (in-misc-info (member '(:eval (when (and enkan-repl-buffer-restriction-mode
                                                  (string-match-p "enkan--" (buffer-name)))
                                         (propertize " [BR]"
                                                     'face '(:foreground "#ff6600" :weight bold)
                                                     'help-echo "Buffer Restriction Mode is ON")))
                              mode-line-misc-info)))
    (message "Buffer restriction: %s | Current buffer: %s | In mode-line: %s | mode-line-misc-info: %S" 
             mode-status
             (if buffer-match "enkan input file" "other buffer")
             (if in-misc-info "yes" "no")
             mode-line-misc-info)))

;;;; Testing Functions

(defun enkan-repl-buffer-restriction--test-create-buffers ()
  "Create test buffers for testing restriction mode."
  (interactive)
  ;; Create test buffers that should be restricted
  (get-buffer-create "*enkan-eat-test*")
  (get-buffer-create "*claudemacs:test*")
  ;; Create normal buffer that should not be restricted
  (get-buffer-create "*normal-buffer*")
  (message "Test buffers created"))

(defun enkan-repl-buffer-restriction--test-patterns ()
  "Test pattern matching functionality."
  (interactive)
  (let ((test-cases '(("*enkan-eat*" . t)
                     ("*enkan-eat-session1*" . t)
                     ("*claudemacs:dir*" . t)
                     ("*eat*" . nil)
                     ("*scratch*" . nil))))
    (dolist (test test-cases)
      (let* ((buffer-name (car test))
             (expected (cdr test))
             (result (enkan-repl-buffer-restriction--match-pattern-p
                     buffer-name
                     enkan-repl-buffer-restriction-patterns)))
        (message "Test %s: %s (expected %s)"
                buffer-name
                (if (eq result expected) "PASS" "FAIL")
                expected)))))

(provide 'enkan-repl-buffer-restriction-mode)

;;; enkan-repl-buffer-restriction-mode.el ends here