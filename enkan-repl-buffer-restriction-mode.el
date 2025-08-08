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
  '("^\\*enkan-eat" "^\\*claudemacs" "^\\*eat\\*")
  "List of regexp patterns for buffers to restrict access to.
Default includes *enkan-eat*, *claudemacs* (with or without colon), and *eat* buffers."
  :type '(repeat regexp)
  :group 'enkan-repl-buffer-restriction)

;;;; Variables

(defvar enkan-repl-buffer-restriction-mode nil
  "Non-nil when buffer restriction mode is active.")

(defvar enkan-repl-buffer-restriction--redirecting nil
  "Flag to prevent recursive redirection.")


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

(defun enkan-repl-buffer-restriction--find-claudemacs-directory ()
  "Find the directory associated with existing claudemacs buffer."
  (let ((claudemacs-buffers (cl-remove-if-not
                             (lambda (buf)
                               (string-match-p "^\\*claudemacs" (buffer-name buf)))
                             (buffer-list))))
    (when claudemacs-buffers
      ;; Get directory from first claudemacs buffer
      (with-current-buffer (car claudemacs-buffers)
        default-directory))))

(defun enkan-repl-buffer-restriction--get-target-directory ()
  "現在のバッファから適切なターゲットディレクトリを取得する."
  ;; 現在のバッファがenkan入力ファイルの場合、そのディレクトリを使用
  (if (string-match-p "^enkan--" (buffer-name))
      ;; バッファ名からディレクトリを復元
      ;; enkan--Users--sekine--.emacs.d.org -> /Users/sekine/.emacs.d/
      (let ((buf-name (buffer-name)))
        (if (string-match "^enkan--\\(.+\\)\\.org" buf-name)
            (let ((path-part (match-string 1 buf-name)))
              ;; -- を / に置換
              (setq path-part (replace-regexp-in-string "--" "/" path-part))
              (concat "/" path-part "/"))
          default-directory))
    default-directory))

(defun enkan-repl-buffer-restriction--redirect-from-restricted ()
  "無関係なファイルから制限バッファを開こうとした場合、入力ファイルと制限バッファをレイアウト表示する."
  (unless enkan-repl-buffer-restriction--redirecting
    (when enkan-repl-buffer-restriction-mode
      (setq enkan-repl-buffer-restriction--redirecting t)
      
      ;; 現在のバッファから適切なディレクトリを取得
      (let* ((target-dir (enkan-repl-buffer-restriction--get-target-directory))
             ;; 対応するclaudemacsバッファを探す
             (claudemacs-buf (cl-find-if
                              (lambda (buf)
                                (and (string-match-p "^\\*claudemacs" (buffer-name buf))
                                     (with-current-buffer buf
                                       (string= (expand-file-name default-directory)
                                                (expand-file-name target-dir)))))
                              (buffer-list))))
        
        ;; レイアウトを設定: 左に入力ファイル、右に制限バッファ
        (delete-other-windows)
        
        ;; 左ウィンドウに入力ファイルを設定
        ;; まず既存のenkan入力ファイルバッファを探す
        (let* ((expected-buffer-pattern
                ;; /Users/sekine/.emacs.d/ -> enkan--Users--sekine--.emacs.d
                (let ((clean-dir (directory-file-name target-dir)))
                  (concat "^enkan--"
                          (replace-regexp-in-string "/" "--" (substring clean-dir 1))
                          "\\.org")))
               (input-buffer
                (or
                 ;; 既存のenkan入力バッファを探す
                 (cl-find-if
                  (lambda (buf)
                    (string-match-p expected-buffer-pattern (buffer-name buf)))
                  (buffer-list))
                 ;; enkan-replで作成を試みる
                 (when (and (require 'enkan-repl nil t)
                            (fboundp 'enkan-repl--get-project-file-path))
                   (let ((input-file-path (enkan-repl--get-project-file-path target-dir)))
                     (or (get-file-buffer input-file-path)
                         (when (file-exists-p input-file-path)
                           (find-file-noselect input-file-path))
                         (when (fboundp 'enkan-repl-open-project-input-file)
                           (save-window-excursion
                             (enkan-repl-open-project-input-file target-dir)
                             (current-buffer)))))))))
          (if input-buffer
              (switch-to-buffer input-buffer)
            ;; 入力ファイルが見つからない場合は、新規作成を試みる
            (if (and (require 'enkan-repl nil t)
                     (fboundp 'enkan-repl-open-project-input-file))
                (enkan-repl-open-project-input-file target-dir)
              ;; それもできない場合は現在のバッファに留まる
              (message "Cannot find or create input file for %s" target-dir))))
        
        ;; 右ウィンドウに制限バッファ（claudemacs）を設定
        (split-window-right)
        (other-window 1)
        (if claudemacs-buf
            (switch-to-buffer claudemacs-buf)
          ;; claudemacsバッファがなければエラーメッセージ
          (message "Claudemacs buffer not found for %s" target-dir))
        
        ;; 左ウィンドウ（入力ファイル）に戻る
        (other-window -1)
        (message "Window layout setup complete: input file (left) + restricted buffer (right)"))
      
      (setq enkan-repl-buffer-restriction--redirecting nil))))

;;;; Advice Functions

(defun enkan-repl-buffer-restriction--check-other-window (orig-fun count &rest args)
  "Advice for `other-window' to check restrictions.
ORIG-FUN is the original function.
COUNT is the window count to move.
ARGS are additional arguments."
  ;; Check if target window has restricted buffer
  (if (and enkan-repl-buffer-restriction-mode
           (not enkan-repl-buffer-restriction--redirecting))
      (let* ((current-win (selected-window))
             ;; Find the target window based on count
             (target-window (if (> count 0)
                                ;; Forward: get next window count times
                                (let ((win current-win))
                                  (dotimes (_ (abs count))
                                    (setq win (next-window win)))
                                  win)
                              ;; Backward: get previous window count times
                              (let ((win current-win))
                                (dotimes (_ (abs count))
                                  (setq win (previous-window win)))
                                win)))
             (target-buffer (window-buffer target-window)))
        (if (enkan-repl-buffer-restriction--is-restricted-buffer-p target-buffer)
            ;; Don't move to restricted window
            (progn
              (message "Cannot switch to window with restricted buffer %s" (buffer-name target-buffer))
              (selected-window))  ; Stay in current window
          ;; OK to move
          (apply orig-fun count args)))
    ;; Mode off or redirecting - proceed normally
    (apply orig-fun count args)))

(defun enkan-repl-buffer-restriction--check-select-window (orig-fun window &rest args)
  "Advice for `select-window' to check restrictions.
ORIG-FUN is the original function.
WINDOW is the window to select.
ARGS are additional arguments."
  ;; Check BEFORE selecting
  (if (and enkan-repl-buffer-restriction-mode
           (not enkan-repl-buffer-restriction--redirecting))
      (let ((buffer (window-buffer window)))
        (if (and buffer
                 (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer))
            ;; Don't select restricted window
            (progn
              (message "Cannot select window with restricted buffer %s" (buffer-name buffer))
              (selected-window))  ; Return current window
          ;; OK to select
          (apply orig-fun window args)))
    ;; Mode off or redirecting - proceed normally
    (apply orig-fun window args)))

(defun enkan-repl-buffer-restriction--check-switch (orig-fun buffer-or-name &rest args)
  "Advice for `switch-to-buffer' to check restrictions.
ORIG-FUN is the original function.
BUFFER-OR-NAME is the buffer to switch to.
ARGS are additional arguments."
  ;; Check BEFORE switching
  (if (and enkan-repl-buffer-restriction-mode
           (not enkan-repl-buffer-restriction--redirecting))
      (let ((buffer (get-buffer buffer-or-name)))
        (if (and buffer
                 (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer))
            ;; Restricted buffer - redirect and don't call original function
            (progn
              (message "Buffer %s is restricted. Redirecting..." (buffer-name buffer))
              (enkan-repl-buffer-restriction--redirect-from-restricted))
          ;; Not restricted - proceed normally
          (apply orig-fun buffer-or-name args)))
    ;; Mode off or redirecting - proceed normally
    (apply orig-fun buffer-or-name args)))

(defun enkan-repl-buffer-restriction--check-display (orig-fun buffer-or-name &rest args)
  "Advice for `display-buffer' to check restrictions.
ORIG-FUN is the original function.
BUFFER-OR-NAME is the buffer to display.
ARGS are additional arguments."
  ;; Check BEFORE displaying
  (if (and enkan-repl-buffer-restriction-mode
           (not enkan-repl-buffer-restriction--redirecting))
      (let ((buffer (get-buffer buffer-or-name)))
        (if (and buffer
                 (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer))
            ;; Restricted buffer - redirect and don't display
            (progn
              (message "Buffer %s is restricted. Redirecting..." (buffer-name buffer))
              (enkan-repl-buffer-restriction--redirect-from-restricted)
              nil)  ; Return nil to indicate no window was created
          ;; Not restricted - proceed normally
          (apply orig-fun buffer-or-name args)))
    ;; Mode off or redirecting - proceed normally
    (apply orig-fun buffer-or-name args)))

(defun enkan-repl-buffer-restriction--check-pop (orig-fun buffer-or-name &rest args)
  "Advice for `pop-to-buffer' to check restrictions.
ORIG-FUN is the original function.
BUFFER-OR-NAME is the buffer to pop to.
ARGS are additional arguments."
  ;; Check BEFORE popping
  (if (and enkan-repl-buffer-restriction-mode
           (not enkan-repl-buffer-restriction--redirecting))
      (let ((buffer (get-buffer buffer-or-name)))
        (if (and buffer
                 (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer))
            ;; Restricted buffer - redirect and don't pop
            (progn
              (message "Buffer %s is restricted. Redirecting..." (buffer-name buffer))
              (enkan-repl-buffer-restriction--redirect-from-restricted))
          ;; Not restricted - proceed normally
          (apply orig-fun buffer-or-name args)))
    ;; Mode off or redirecting - proceed normally
    (apply orig-fun buffer-or-name args)))

;;;; Mode Line Functions

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
    (advice-add 'other-window :around
                #'enkan-repl-buffer-restriction--check-other-window)
    (advice-add 'select-window :around
                #'enkan-repl-buffer-restriction--check-select-window)
    
    ;; Set up mode line indicator
    (enkan-repl-buffer-restriction--setup-mode-line)
    (force-mode-line-update t)
    
    ;; Check all visible windows for restricted buffers
    (let ((restricted-window-found nil))
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (enkan-repl-buffer-restriction--is-restricted-buffer-p buffer)
            (setq restricted-window-found t))))
      ;; If any restricted buffer is visible, adjust layout immediately
      (when restricted-window-found
        ;; Don't use the timer-based redirect, do it immediately
        (when (fboundp 'enkan-repl-setup-window-layout)
          (enkan-repl-setup-window-layout))))
    
    (message "Buffer restriction mode enabled")))

;;;###autoload
(defun enkan-repl-buffer-restriction-mode-off ()
  "Disable buffer restriction mode."
  (interactive)
  (when enkan-repl-buffer-restriction-mode
    ;; Disable mode
    (setq enkan-repl-buffer-restriction-mode nil)
    
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
    (advice-remove 'other-window
                   #'enkan-repl-buffer-restriction--check-other-window)
    (advice-remove 'select-window
                   #'enkan-repl-buffer-restriction--check-select-window)
    
    (message "Buffer restriction mode disabled")))

;;;###autoload
(defun enkan-repl-buffer-restriction-mode-status ()
  "Show current status of buffer restriction mode."
  (interactive)
  (message "Buffer restriction mode: %s"
           (if enkan-repl-buffer-restriction-mode "ON" "OFF")))


(provide 'enkan-repl-buffer-restriction-mode)

;;; enkan-repl-buffer-restriction-mode.el ends here