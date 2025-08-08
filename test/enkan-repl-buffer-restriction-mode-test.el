;;; enkan-repl-buffer-restriction-mode-test.el --- Tests for buffer restriction mode -*- lexical-binding: t -*-

;;; Commentary:

;; TDD test suite for enkan-repl-buffer-restriction-mode.
;; 期待する動作:
;; 1. 制限モードON時、*claudemacs*や*enkan-eat*バッファへの移動を防ぐ
;; 2. other-windowでも制限バッファへの移動を防ぐ
;; 3. 制限バッファを開こうとしたらレイアウト設定（入力ファイル左、claudemacs右）
;; 4. 重複バッファを作らない
;; 5. モードOFF時は通常通り移動可能

;;; Code:

(require 'ert)
(require 'enkan-repl-buffer-restriction-mode)

;;;; Test Helpers

(defmacro with-test-buffers (&rest body)
  "テスト用バッファを作成して、テスト後にクリーンアップする."
  `(let ((test-claudemacs (get-buffer-create "*claudemacs*"))
         (test-eat (get-buffer-create "*enkan-eat*"))
         (test-input (get-buffer-create "enkan--test-project.org"))
         (test-normal (get-buffer-create "normal.txt")))
     (unwind-protect
         (progn
           ;; モードを確実にOFFから開始
           (when enkan-repl-buffer-restriction-mode
             (enkan-repl-buffer-restriction-mode-off))
           ,@body)
       ;; クリーンアップ
       (when enkan-repl-buffer-restriction-mode
         (enkan-repl-buffer-restriction-mode-off))
       (kill-buffer test-claudemacs)
       (kill-buffer test-eat)
       (kill-buffer test-input)
       (kill-buffer test-normal))))

;;;; 期待動作1: 制限モードON時、制限バッファへの移動を防ぐ

(ert-deftest test-switch-to-restricted-buffer-blocked ()
  "制限モードON時、*claudemacs*への切り替えがブロックされる."
  (with-test-buffers
   (switch-to-buffer "normal.txt")
   (enkan-repl-buffer-restriction-mode-on)
   
   ;; claudemacsへの切り替えを試みる
   (switch-to-buffer "*claudemacs*")
   
   ;; claudemacsに移動していないことを確認
   (should-not (string= (buffer-name) "*claudemacs*"))
   ;; 制限されていないバッファにいることを確認
   (should-not (enkan-repl-buffer-restriction--is-restricted-buffer-p
                (current-buffer)))))

(ert-deftest test-switch-to-eat-buffer-blocked ()
  "制限モードON時、*enkan-eat*への切り替えがブロックされる."
  (with-test-buffers
   (switch-to-buffer "normal.txt")
   (enkan-repl-buffer-restriction-mode-on)
   
   ;; enkan-eatへの切り替えを試みる
   (switch-to-buffer "*enkan-eat*")
   
   ;; enkan-eatに移動していないことを確認
   (should-not (string= (buffer-name) "*enkan-eat*"))
   ;; 制限されていないバッファにいることを確認
   (should-not (enkan-repl-buffer-restriction--is-restricted-buffer-p
                (current-buffer)))))

;;;; 期待動作2: other-windowでも制限バッファへの移動を防ぐ

(ert-deftest test-other-window-to-restricted-blocked ()
  "other-windowで制限バッファへの移動がブロックされる."
  (with-test-buffers
   ;; 2つのウィンドウを設定
   (delete-other-windows)
   (switch-to-buffer "normal.txt")
   (split-window-right)
   (other-window 1)
   (switch-to-buffer "*claudemacs*")
   (other-window -1)
   
   ;; 制限モードを有効化
   (enkan-repl-buffer-restriction-mode-on)
   
   ;; 左ウィンドウにいることを確認
   (should (string= (buffer-name) "normal.txt"))
   
   ;; other-windowで移動を試みる
   (other-window 1)
   
   ;; claudemacsに移動していないことを確認
   (should-not (string= (buffer-name) "*claudemacs*"))
   ;; 制限されていないバッファにいることを確認  
   (should-not (enkan-repl-buffer-restriction--is-restricted-buffer-p
                (current-buffer)))))

;;;; 期待動作3: 制限バッファを開こうとしたらレイアウト設定

(ert-deftest test-single-window-triggers-layout ()
  "無関係なファイルから制限バッファを開くと入力ファイルと制限バッファのレイアウトが設定される."
  (with-test-buffers
   (delete-other-windows)
   (switch-to-buffer "normal.txt")
   (enkan-repl-buffer-restriction-mode-on)
   
   ;; 単一ウィンドウであることを確認
   (should (= (length (window-list)) 1))
   
   ;; 制限バッファを開こうとする（作業開始の意図）
   (switch-to-buffer "*claudemacs*")
   
   ;; 2つのウィンドウになっていることを確認（左:入力ファイル、右:claudemacs）
   (should (= (length (window-list)) 2))
   
   ;; 左ウィンドウ（入力ファイル）にいることを確認
   ;; 入力ファイルは enkan-- で始まるか、*scratch* などの非制限バッファ
   (should-not (enkan-repl-buffer-restriction--is-restricted-buffer-p
                (current-buffer)))
   
   ;; 右ウィンドウが存在する場合の確認（claudemacsがない場合は元のバッファのまま）
   (when (> (length (window-list)) 1)
     (other-window 1)
     ;; claudemacsバッファまたは元のバッファ
     (should (or (string-match-p "\\*claudemacs\\*" (buffer-name))
                 (string= (buffer-name) "normal.txt")))
     (other-window -1))))

;;;; 期待動作4: 重複バッファを作らない

(ert-deftest test-no-duplicate-input-buffers ()
  "同じ入力ファイルバッファを重複作成しない."
  (with-test-buffers
   (delete-other-windows)
   (switch-to-buffer "normal.txt")
   (enkan-repl-buffer-restriction-mode-on)
   
   ;; 最初のenkan--バッファ数を記録
   (let ((initial-enkan-count
          (cl-count-if (lambda (buf)
                         (string-match-p "^enkan--" (buffer-name buf)))
                       (buffer-list))))
     
     ;; 複数回制限バッファを開こうとする
     (switch-to-buffer "*claudemacs*")
     (switch-to-buffer "*claudemacs*")
     (switch-to-buffer "*enkan-eat*")
     
     ;; enkan--バッファが増えていないことを確認
     (let ((final-enkan-count
            (cl-count-if (lambda (buf)
                           (string-match-p "^enkan--" (buffer-name buf)))
                         (buffer-list))))
       ;; 増えていないか、1つだけ増えた（初回作成）
       (should (<= (- final-enkan-count initial-enkan-count) 1))))))

;;;; 期待動作5: モードOFF時は通常通り移動可能

(ert-deftest test-mode-off-allows-all-navigation ()
  "モードOFF時はすべてのバッファへ移動可能."
  (with-test-buffers
   ;; モードをOFFにする
   (enkan-repl-buffer-restriction-mode-off)
   
   ;; claudemacsへ移動可能
   (switch-to-buffer "*claudemacs*")
   (should (string= (buffer-name) "*claudemacs*"))
   
   ;; enkan-eatへ移動可能
   (switch-to-buffer "*enkan-eat*")
   (should (string= (buffer-name) "*enkan-eat*"))
   
   ;; 通常バッファへも移動可能
   (switch-to-buffer "normal.txt")
   (should (string= (buffer-name) "normal.txt"))))

(ert-deftest test-mode-off-allows-other-window ()
  "モードOFF時はother-windowで制限バッファへ移動可能."
  (with-test-buffers
   ;; 2ウィンドウ設定
   (delete-other-windows)
   (switch-to-buffer "normal.txt")
   (split-window-right)
   (other-window 1)
   (switch-to-buffer "*claudemacs*")
   (other-window -1)
   
   ;; モードOFF
   (enkan-repl-buffer-restriction-mode-off)
   
   ;; other-windowで移動可能
   (other-window 1)
   (should (string= (buffer-name) "*claudemacs*"))))

;;;; その他のテスト

(ert-deftest test-normal-buffer-navigation-allowed ()
  "制限モードON時でも通常バッファへは移動可能."
  (with-test-buffers
   (switch-to-buffer "normal.txt")
   (enkan-repl-buffer-restriction-mode-on)
   
   ;; 入力ファイルへ移動可能
   (switch-to-buffer "enkan--test-project.org")
   (should (string= (buffer-name) "enkan--test-project.org"))
   
   ;; 通常ファイルへ戻れる
   (switch-to-buffer "normal.txt")
   (should (string= (buffer-name) "normal.txt"))))

(ert-deftest test-pattern-matching ()
  "パターンマッチングが正しく動作する."
  (with-test-buffers
   ;; 制限対象
   (should (enkan-repl-buffer-restriction--is-restricted-buffer-p
            (get-buffer "*claudemacs*")))
   (should (enkan-repl-buffer-restriction--is-restricted-buffer-p
            (get-buffer "*enkan-eat*")))
   
   ;; 制限対象外
   (should-not (enkan-repl-buffer-restriction--is-restricted-buffer-p
                (get-buffer "normal.txt")))
   (should-not (enkan-repl-buffer-restriction--is-restricted-buffer-p
                (get-buffer "enkan--test-project.org")))))

(ert-deftest test-correct-directory-claudemacs-buffer ()
  "正しいディレクトリのclaudemacsバッファが開かれる.
enkan--Users--sekine--.emacs.d.orgから対応する*claudemacs*を開く."
  (with-test-buffers
   ;; .emacs.dディレクトリ用の入力ファイルとclaudemacsを作成
   (let ((emacs-input (get-buffer-create "enkan--Users--sekine--.emacs.d.org"))
         (emacs-claudemacs (get-buffer-create "*claudemacs:/Users/sekine/.emacs.d*"))
         ;; 別ディレクトリのバッファも作成（間違って開かれないことを確認）
         (repl-input (get-buffer-create "enkan--Users--sekine--dev--self--enkan-repl.org"))
         (repl-claudemacs (get-buffer-create "*claudemacs:/Users/sekine/dev/self/enkan-repl*")))
     
     (unwind-protect
         (progn
           ;; .emacs.d用バッファのディレクトリを設定
           (with-current-buffer emacs-claudemacs
             (setq default-directory "/Users/sekine/.emacs.d/"))
           (with-current-buffer repl-claudemacs
             (setq default-directory "/Users/sekine/dev/self/enkan-repl/"))
           
           ;; .emacs.dの入力ファイルから開始
           (delete-other-windows)
           (switch-to-buffer emacs-input)
           (enkan-repl-buffer-restriction-mode-on)
           
           ;; claudemacsを開こうとする
           (switch-to-buffer "*claudemacs:/Users/sekine/.emacs.d*")
           
           ;; レイアウトが設定されたことを確認
           (should (= (length (window-list)) 2))
           
           ;; 左ウィンドウが正しい入力ファイル
           (should (string-match-p "enkan--Users--sekine--.emacs.d" (buffer-name)))
           
           ;; 右ウィンドウが正しいclaudemacs
           ;; 制限モードをOFFにしてから確認
           (enkan-repl-buffer-restriction-mode-off)
           (other-window 1)
           (should (string= (buffer-name) "*claudemacs:/Users/sekine/.emacs.d*"))
           (other-window -1)
           (enkan-repl-buffer-restriction-mode-on))
       
       ;; クリーンアップ
       (kill-buffer emacs-input)
       (kill-buffer emacs-claudemacs)
       (kill-buffer repl-input)
       (kill-buffer repl-claudemacs)))))

(ert-deftest test-no-scratch-buffer-opened ()
  "*scratch*バッファが開かれないことを確認.
入力ファイルが見つからない場合でも*scratch*を開かない."
  (with-test-buffers
   ;; 存在しないディレクトリのclaudemacsバッファを作成
   (let ((nonexistent-claudemacs (get-buffer-create "*claudemacs:/nonexistent/path*")))
     (unwind-protect
         (progn
           (with-current-buffer nonexistent-claudemacs
             (setq default-directory "/nonexistent/path/"))
           
           ;; 無関係なファイルから開始
           (delete-other-windows)
           (switch-to-buffer "normal.txt")
           (enkan-repl-buffer-restriction-mode-on)
           
           ;; claudemacsを開こうとする
           (switch-to-buffer "*claudemacs:/nonexistent/path*")
           
           ;; *scratch*が開かれていないことを確認
           (should-not (string= (buffer-name) "*scratch*"))
           
           ;; どのウィンドウにも*scratch*がないことを確認
           (dolist (win (window-list))
             (should-not (string= (buffer-name (window-buffer win)) "*scratch*"))))
       
       ;; クリーンアップ
       (kill-buffer nonexistent-claudemacs)))))

(provide 'enkan-repl-buffer-restriction-mode-test)

;;; enkan-repl-buffer-restriction-mode-test.el ends here