;;; center-window-navigation.el --- Center file window layout and navigation for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2024 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Version: 1.0.0

;;; Commentary:

;; Center file window layout and navigation utilities extracted from enkan-repl.el
;; This file provides complete window layout management and navigation functions
;; for center file multi-buffer access pattern.

;;; Code:

;;;; Pure Functions for Project Path Resolution

(defun enkan-repl--get-session-project-path-pure (session-number session-list project-registry)
  "Get project path for SESSION-NUMBER from SESSION-LIST and PROJECT-REGISTRY.
Pure function that returns project path string or nil.
SESSION-LIST format: ((session-number . project-name) ...)
PROJECT-REGISTRY format: ((alias . (project-name . project-path)) ...)"
  (let* ((session-project (cdr (assoc session-number session-list)))
         (project-info (when session-project
                         (cl-find-if (lambda (entry)
                                       (string= (car (cdr entry)) session-project))
                                     project-registry)))
         (project-path (when project-info (cdr (cdr project-info)))))
    project-path))

(defun enkan-repl--setup-window-dired-pure (window session-number session-list project-registry)
  "Pure function to determine dired setup for WINDOW and SESSION-NUMBER.
Returns cons (window . project-path) or nil if invalid."
  (let ((project-path (enkan-repl--get-session-project-path-pure
                       session-number session-list project-registry)))
    (when (and project-path (file-directory-p (expand-file-name project-path)))
      (cons window project-path))))

(defun enkan-repl--setup-window-eat-buffer-pure (window session-number session-list project-registry)
  "Pure function to determine eat buffer setup for WINDOW and SESSION-NUMBER.
Returns cons (window . buffer-name) or nil if session not registered."
  (let ((project-name (cdr (assoc session-number session-list))))
    (when project-name
      (let ((project-path (enkan-repl--get-project-path-from-registry project-name project-registry)))
        (when project-path
          (let* ((expanded-path (expand-file-name project-path))
                 (buffer-name (format "*enkan:%s*" expanded-path)))
            (cons window buffer-name)))))))

;;;; Window Layout Variables

(defvar enkan-repl--window-1 nil
  "Window 1 (center file) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-2 nil
  "Window 2 (work area) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-3 nil
  "Window 3 (reserve area) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-4 nil
  "Window 4 (session 1) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-5 nil
  "Window 5 (session 2) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-6 nil
  "Window 6 (session 3) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-7 nil
  "Window 7 (session 4) for enkan-repl multi-buffer layout.")

;;;; Window Layout Functions

;; Helper function for setting up eat buffer in window
(defun enkan-repl--setup-session-eat-buffer (window session-number)
  "Setup eat buffer for SESSION-NUMBER in WINDOW."
  (let ((eat-setup (enkan-repl--setup-window-eat-buffer-pure
                     window session-number enkan-repl-session-list enkan-repl-center-project-registry)))
    (if eat-setup
      (let ((buffer (get-buffer (cdr eat-setup)))
            (project-name (cdr (assoc session-number enkan-repl-session-list))))
        (if buffer
          (progn
            (select-window (car eat-setup))
            ;; Set correct default-directory before switching buffer
            (when project-name
              (let ((project-path (enkan-repl--get-project-path-from-registry 
                                  project-name enkan-repl-center-project-registry)))
                (when project-path
                  (setq default-directory (expand-file-name project-path)))))
            (switch-to-buffer buffer)
            (message "✅ Window %d: Opened eat buffer %s in %s" 
                    session-number (cdr eat-setup) default-directory))
          (message "❌ Window %d: Eat buffer %s not found. Run C-M-s to start sessions."
            session-number (cdr eat-setup))))
      (message "❌ Window %d: No session registered for slot %d (internal number %d)."
        session-number (- session-number 3) session-number))))

;;;###autoload
(defun enkan-repl-setup-2session-layout ()
  "Setup window layout for 2-session management.
  +----------+---+---+
  |    1     | 4 | 5 |
  | center   |   |   |
  |  file    |   |   |
  +-----+----|   |   |
  | 2   | 3  |   |   |
  |work |rsv |   |   |
  +-----+----+---+---+

  1: Center file (command source)
  2: Work area (misc files/buffers)
  3: Reserve area (temporary buffers like magit commit)
  4, 5: enkan-repl sessions

Category: Utilities"
  (interactive)
  (message "Starting enkan-repl-setup-2session-layout")
  (message "enkan-repl-center-file: %s" enkan-repl-center-file)
  (message "enkan-repl-session-list: %s" (if (boundp 'enkan-repl-session-list) enkan-repl-session-list 'unbound))
  (message "enkan-repl-center-project-registry: %s" (if (boundp 'enkan-repl-center-project-registry) enkan-repl-center-project-registry 'unbound))
  (delete-other-windows)
  ;; Create 2 columns on the right
  (split-window-right (floor (* (window-width) 0.6)))
  (other-window 1)
  (split-window-right)
  ;; Split bottom left
  (other-window 2)
  (split-window-below (floor (* (window-height) 0.65)))
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.5)))
  (other-window 4)
  (balance-windows)
  ;; Set window variables for center file layout
  (let ((windows (window-list)))
    (setq enkan-repl--window-1 (nth 0 windows))  ; Center file
    (setq enkan-repl--window-2 (nth 1 windows))  ; Work area
    (setq enkan-repl--window-3 (nth 2 windows))  ; Reserve area
    (setq enkan-repl--window-4 (nth 3 windows))  ; Session 1
    (setq enkan-repl--window-5 (nth 4 windows))) ; Session 2
  ;; Open project root dired in work area windows (2 and 3)
  (cond
    ((not (and (boundp 'enkan-repl-session-list) (boundp 'enkan-repl-center-project-registry)))
      (error "❌ enkan-repl-session-list or enkan-repl-center-project-registry is not defined. Run C-M-s first to setup sessions."))
    ((or (null enkan-repl-session-list) (null enkan-repl-center-project-registry))
      (error "❌ Session list or project registry is empty. Run C-M-s to setup multi-project layout first."))
    (t
      ;; Window 2: Session 1's project dired
      (let ((dired-setup-2 (enkan-repl--setup-window-dired-pure
                             enkan-repl--window-2 4
                             enkan-repl-session-list enkan-repl-center-project-registry)))
        (if dired-setup-2
          (progn
            (select-window (car dired-setup-2))
            (dired (expand-file-name (cdr dired-setup-2)))
            (message "✅ Window 2: Opened dired for %s" (cdr dired-setup-2)))
          (let ((session-name (cdr (assoc 4 enkan-repl-session-list))))
            (if session-name
              (message "❌ Window 2: Project directory not found for session '%s'. Check enkan-repl-center-project-registry." session-name)
              (message "❌ Window 2: No session registered for slot 1 (internal number 4). Run C-M-s to setup sessions.")))))
      ;; Window 3: Session 2's project dired
      (let ((dired-setup-3 (enkan-repl--setup-window-dired-pure
                             enkan-repl--window-3 5
                             enkan-repl-session-list enkan-repl-center-project-registry)))
        (if dired-setup-3
          (progn
            (select-window (car dired-setup-3))
            (dired (expand-file-name (cdr dired-setup-3)))
            (message "✅ Window 3: Opened dired for %s" (cdr dired-setup-3)))
          (let ((session-name (cdr (assoc 5 enkan-repl-session-list))))
            (if session-name
              (message "❌ Window 3: Project directory not found for session '%s'. Check enkan-repl-center-project-registry." session-name)
              (message "❌ Window 3: No session registered for slot 2 (internal number 5). Run C-M-s to setup sessions.")))))))
  ;; Setup eat buffers in session windows (4 and 5)
  (when (and (boundp 'enkan-repl-session-list) enkan-repl-session-list)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-4 4)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-5 5))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;###autoload
(defun enkan-repl-setup-3session-layout ()
  "Setup window layout for 3-session management.
  +--------+---+---+---+
  |    1   | 4 | 5 | 6 |
  | center |   |   |   |
  |  file  |   |   |   |
  +--------+---+---+---+

  4, 5, 6: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; Create left column for center file (30%) and right section (70%)
  (split-window-right (floor (* (window-width) 0.3)))
  ;; Move to right section and split into 3 columns
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.4)))
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.5)))
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (other-window 2)
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-3 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-4 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup eat buffers in session windows (4, 5, 6 in multi-project order)
  (when (and (boundp 'enkan-repl-session-list) enkan-repl-session-list)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-2 4)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-3 5)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-4 6))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;###autoload
(defun enkan-repl-setup-4session-layout ()
  "Setup window layout for 4-session management.
  +--------+---+---+---+---+
  |    1   | 4 | 5 | 6 | 7 |
  | center |   |   |   |   |
  |  file  |   |   |   |   |
  +--------+---+---+---+---+

  4, 5, 6, 7: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; Create left column for center file (30%) and right section (70%)
  (split-window-right (floor (* (window-width) 0.3)))
  ;; Move to right section and split into 4 columns
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.3)))
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.4)))
  (other-window 1)
  (split-window-right)
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (other-window 2)
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-3 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-4 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-5 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup eat buffers in session windows (4, 5, 6 in multi-project order)
  (when (and (boundp 'enkan-repl-session-list) enkan-repl-session-list)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-2 4)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-3 5)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-4 6)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-5 7))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;; Window Navigation Functions

(defun enkan-repl-goto-window (window-number)
  "Move to the specified window number.
window-number: Integer 1-7

Category: Utilities"
  (interactive "nWindow number (1-7): ")
  (unless (<= 1 window-number 7)
    (user-error "Window number must be 1-7"))

  (let ((target-window (nth (1- window-number) (window-list nil 'no-minibuf))))
    (if target-window
        (select-window target-window)
      (user-error "Window %d does not exist" window-number))))

;; Individual window navigation functions for keybindings
;;;###autoload
(defun enkan-repl-goto-window-1 () "Go to window 1" (interactive) (enkan-repl-goto-window 1))
;;;###autoload
(defun enkan-repl-goto-window-2 () "Go to window 2" (interactive) (enkan-repl-goto-window 2))
;;;###autoload
(defun enkan-repl-goto-window-3 () "Go to window 3" (interactive) (enkan-repl-goto-window 3))
;;;###autoload
(defun enkan-repl-goto-window-4 () "Go to window 4" (interactive) (enkan-repl-goto-window 4))
;;;###autoload
(defun enkan-repl-goto-window-5 () "Go to window 5" (interactive) (enkan-repl-goto-window 5))
;;;###autoload
(defun enkan-repl-goto-window-6 () "Go to window 6" (interactive) (enkan-repl-goto-window 6))
;;;###autoload
(defun enkan-repl-goto-window-7 () "Go to window 7" (interactive) (enkan-repl-goto-window 7))

;;;; Center file multi-buffer window navigation
(defvar enkan-repl-center--current-window 1
  "Current window number for center file layout cycling.")

;;;###autoload
(defun enkan-repl-center-other-window ()
  "Switch between windows 1-3 in center file layout.
Window 1: Center file, Window 2: Work area, Window 3: Reserve area.

Category: Center File Multi-buffer Access"
  (interactive)
  (let* ((window-list (window-list nil 'no-minibuf))
         (max-window (min 3 (length window-list))))
    (when (> max-window 0)
      (setq enkan-repl-center--current-window
            (1+ (mod enkan-repl-center--current-window max-window)))
      (when (> enkan-repl-center--current-window max-window)
        (setq enkan-repl-center--current-window 1))
      (let ((target-window (nth (1- enkan-repl-center--current-window) window-list)))
        (when target-window
          (select-window target-window)
          (message "Switched to window %d" enkan-repl-center--current-window))))))

(provide 'center-window-navigation)
;;; center-window-navigation.el ends here
