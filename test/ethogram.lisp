(defpackage :ethogram.test
  (:use :cl
        :ethogram))
(in-package :ethogram.test)

;; # DSL example
;;
;; ;; 引数は&bodyにしてエディタのインデントを減らす
;; (defmacro examples (type &body body))
;;
;; (defentry "a function to check number's oddness"
;;   :subject #'oddp
;;
;;   (examples :function
;;     :returns t :for (1))
;;
;;   (examples :function
;;     :about "first argument is an odd number"
;;     :returns nil :for (0)
;;     :returns nil :for (2)
;;     :returns nil :for (10))
;;
;;   (examples :group
;;     :about "first argument is not an odd number"
;;     (examples :function
;;       :about "non-zero numbers"
;;       :returns t
;;       :for (1)
;;       :for (3)
;;       :for (1001))
;;     (examples :function
;;       :about "a zero"
;;       :returns t
;;       :for (0))))
;;
;; # for sideeffects
;;
;; (defun make-adder (n)
;;   (values (lambda () n)
;;           (lambda () (incf n))))
;;
;; (defentry "a function increase its internal integer value"
;;   ; subjectはそのまま使われるとは限らない
;;   ; 中の:subjectで上書きされることもある
;;   ; でも主題はこいつ、という明示
;;   ; そうすると、descよりsubjectが上では...？ -> そうかも
;;   :subject #'make-adder
;;
;;   (examples :sideeffect
;;     ; :letは毎回初期化されないほうがいいとも思う…が…どうだろ…
;;     :let (getter adder) := (make-adder 0)
;;     :pre (zerop (getter))
;;     :do (adder)
;;     :post (= (getter) 1)))
;;
;;   ;;もうちょっと複雑な例考える (0で止まる引くやつ?)

;; # TODO (上からやる)
;;
;; - [x] 検査を実行する
;; - [x] 検査の前に:prepareを実行する
;; - [x] 検査の後に:disposeを実行する
;; - [x] 検査がコンディションを投げても:disposeを実行する
;; - [x] 検査のタイトルをentry-descで取得できる
;; - [x] `(examples :function ...)`で検査内容を定義できる
;; - [x] defentryの中身をパースできる
;; - [x] examplesマクロで定義した検査を実行できる
;; - [x] 検査の結果をstdioに即時出力する
;; - [x] 複数の検査を実行する (`(examples :function ...)`の中が複数)
;; - [x] `(examples :function ...)`で返り値が多値のケースを考慮する: `(values ...)`と書く
;; - [x] `(examples :function ...)`で引数が複数のケースを考慮する: `:args (...)`と書く
;; - [x] `(examples :function ...)`でむしろ:for (1引数のみのケース) をやめる
;; - [x] `(examples :funcion ...)`に書く引数や返り値の値を評価する
;;     - つまり`(examples :function :for (1+ ) :returns 1)`が成功すること
;; - [x] defentryで定義した検査を保持・一覧取得・クリアできる
;; - [x] entryはsubjectで分類される
;;     - cataloguesリストの要素はsubject->entryリストのハッシュテーブル
;;     - cataloguesリストはなにかをキーにする必要はない…？ パッケージとか…？
;; - [ ] 検査の結果を収集する
;; - [ ] 収集した検査結果をstdioに書き出す
;; - [ ] 複数の検査を実行する (`(examples  ...)`自体が複数)
;; - [ ] `:let NAME := INITFORM`で名前を束縛できる (初期化はオプショナル);;
;; - [ ] `(examples :sideeffect ...)`で副作用を検査できる
;; - [ ] リネーム: `defsec`マクロ -> `catalog`マクロ
;; - [ ] このテスト自身をethogramのもので置き換える

(defun spec.check-flow ()
  (let ((logs ()))
    (labels ((push-log (name) (push name logs))
             (prepare () (push-log :prepare))
             (dispose () (push-log :dispose))
             (subject ()
               (push-log :check)
               (signal "signal a condition")))
      (let ((spec (defentry "check flow is: preparation, checking and disposing"
                    :subject #'subject
                    :prepare (prepare)
                    :dispose (dispose)
                    (examples :function
                      :returns nil :for ()))))
        (check spec)
        (assert (equal '(:prepare :check :dispose) (reverse logs)))))))

(defun spec.entry-desc ()
  (let ((spec (defentry "spec description"
                :subject #'oddp
                (examples :function
                  :returns t :for (1)))))
    (assert (string= (entry-desc spec) "spec description"))))

(defun verify-malformed-examples-error-reason (body reason)
  (block check
    (flet ((check-reason (c)
             (assert (string= (slot-value c 'reason) reason))
             (return-from check)))
      (handler-bind ((malformed-examples-error #'check-reason))
        (parse-function-examples body)))))

(defun spec.parse-function-exampels.malformed.empty ()
  (let ((body '())
        (reason "empty"))
    (verify-malformed-examples-error-reason body reason)))

(defun spec.parse-function-exampels.parse-io-pair ()
  (let ((body '(:returns t :for (1))))
    (assert (equal (parse-function-examples body)
                   '(((1) t))))))

(defun spec.parse-function-exapmles.allow-values-for-output ()
  (let ((body '(:returns (values t nil) :for (1))))
    (assert (equal (parse-function-examples body)
                   '(((1) (values t nil)))))))

(defun spec.parse-function-exampels.malformed.incomplete-io-pair ()
  (let ((body '(:returns t))
        (reason "there is no :FOR ARGLIST"))
    (verify-malformed-examples-error-reason body reason))
  (let ((body '(:for (1)))
        (reason "there is no :RETURNS VALUES"))
    (verify-malformed-examples-error-reason body reason)))

(defun spec.parse-function-examples.multiple-pairs ()
  (let ((body '(:returns t :for (1)
                :returns nil :for (2)
                :returns t :for (3))))
    (assert (equal (parse-function-examples body)
                   '(((1) t) ((2) nil) ((3) t))))))

(defun spec.define-example ()
  "define a function example"
  (let ((examples (examples :function
                    :returns 42 :for ('(6 9)))))
    (assert (typep examples 'list))
    (let ((example (elt examples 0)))
      (assert (typep example 'function-examples))
      (let ((input (function-examples-input example)))
        (assert (typep input 'list))
        (assert (= (length input) 1))
        (let ((arg1 (elt input 0)))
          (assert (typep arg1 'list))
          (assert (= (elt arg1 0) 6))
          (assert (= (elt arg1 1) 9))))
      (let ((output (function-examples-output example)))
        (assert (typep output 'list))
        (assert (= (length output) 1))
        (let ((ret1 (elt output 0)))
          (assert (typep ret1 'number))
          (assert (= ret1 42))))))
  (let ((examples (examples :function
                    :returns (1 2 3) :for ('(1 2 3)))))
    (assert (typep examples 'list))o
    (let ((example (elt examples 0)))
      (assert (typep example 'function-examples))
      (let ((input (function-examples-input example)))
        (assert (typep input 'list))
        (assert (= (length input) 1))
        (let ((arg1 (elt input 0)))
          (assert (= (elt arg1 0) 1))
          (assert (= (elt arg1 1) 2))
          (assert (= (elt arg1 2) 3))))
      (let ((output (function-examples-output example)))
        (assert (typep output 'list))
        (assert (= (length output) 1))
        (let ((ret1 (elt output 0)))
          (assert (typep ret1 'list))
          (assert (= (elt ret1 0) 1))
          (assert (= (elt ret1 1) 2))
          (assert (= (elt ret1 2) 3)))))))

(defun spec.define-example.evaluate-io-values ()
  (let* ((examples (examples :function
                     :returns (values (1+ 0) 2)
                     :for (1 (+ 1 1))))
         (example (elt examples 0))
         (input (function-examples-input example))
         (output (function-examples-output example)))
    (assert (equal input '(1 2)))
    (assert (equal output '(1 2)))))

(defun spec.parse-entry-body.empty-body-signaled-error ()
  (let ((body '()))
    (block check
      (flet ((check-reason (c)
               (assert (string= (slot-value c 'reason) "empty"))
               (return-from check)))
        (handler-bind ((malformed-entry-error #'check-reason))
          (parse-entry body)
          (assert nil))))))

(defun spec.parse-entry-body.parse-subject ()
  (let ((body '(:subject #'oddp)))
    (assert (eq (parse-entry body) '#'oddp))))

(defun spec.parse-entry-body.parse-prepare ()
  (let ((body '(:subject #'oddp
                :prepare (identity 42))))
    (assert (equal (multiple-value-list (parse-entry body))
                   '(#'oddp
                     (identity 42)
                     nil
                     nil)))))

(defun spec.parse-entry-body.parse-dispose ()
  (let ((body '(:subject #'oddp
                :prepare (identity 42)
                :dispose (identity 45))))
    (assert (equal (multiple-value-list (parse-entry body))
                   '(#'oddp
                     (identity 42)
                     (identity 45)
                     nil)))))

(defun spec.parse-entry-body.subject-is-required ()
  (let ((body `(:prepare '(identity 42))))
    (block check
      (flet ((check-reason (c)
               (assert (string= (slot-value c 'reason) ":SUBJECT is required"))
               (return-from check)))
        (handler-bind ((malformed-entry-error #'check-reason))
          (parse-entry body))))))

(defun spec.parse-entry-body.parse-examples ()
  (let ((body '(:subject #'oddp
                :prepare (identity 42)
                (examples :function :returns t :for (1)))))
    (assert (tree-equal (multiple-value-list (parse-entry body))
                        '(#'oddp
                          (identity 42)
                          nil
                          (examples :function :returns t :for (1)))))))

(defun spec.define-spec ()
  (let ((spec (defentry "defining spec"
                :subject #'oddp
                :prepare (print :ln)
                (examples :function
                  :returns t :for (1)))))
    (assert (string= (entry-desc spec) "defining spec"))
    (assert (eq (entry-subject spec) #'oddp))
    (assert (not (null (entry-check spec))))))

(defun spec.check-spec-succeeds ()
  (let ((spec (defentry "spec will succeed"
                :subject #'oddp
                (examples :function
                  :returns t :for (1)))))
    (assert (equal (check spec) '(t)))))

(defun spec.check-spec-fails ()
  (let ((spec (defentry "spec will fail"
                :subject #'oddp
                (examples :function
                  :returns t :for (2)))))
    (assert (equal (check spec) '(nil)))))

(defun spec.check-spec.with-multiple-io-pairs-succeeds ()
  (let ((spec (defentry "all examples in spec will succeed"
                :subject #'oddp
                (examples :function
                  :returns t :for (1)
                  :returns nil :for (2)
                  :returns t :for (3)))))
    (assert (equal (check spec) '(t t t)))))

(defun spec.check-spec.with-multiple-values ()
  (flet ((odd-even (n)
           (let ((odd (oddp n)))
             (values odd (not odd)))))
    (let ((spec (defentry "this will succeed"
                  :subject #'odd-even
                  (examples :function
                    :returns (values t nil) :for (1)))))
      (assert (equal (check spec) '(t))))
    (let ((spec (defentry "this will fail"
                  :subject #'odd-even
                  (examples :function
                    :returns (values t nil) :for (2)))))
      (assert (equal (check spec) '(nil))))
    (let ((spec (defentry "this also will fail"
                  :subject #'odd-even
                  (examples :function
                    :returns (t nil) :for (2)))))
      (assert (equal (check spec) '(nil))))))

(defun spec.output-spec-succeeded-result ()
  (let ((spec (defentry "ODDP"
                :subject #'oddp
                (examples :function
                  :returns t :for (1)))))
    (assert (string= (with-output-to-string (*standard-output*)
                       (check spec))
                     (format nil "a spec \"ODDP\" is succeeded~%")))))

(defun spec.output-spec-failed-result ()
  (let ((spec (defentry "ODDP"
                :subject #'oddp
                (examples :function
                  :returns t :for (2)))))
    (assert (string= (with-output-to-string (*standard-output*)
                       (check spec))
                     (format nil "a spec \"ODDP\" is failed~%")))))

(defun spec.store-and-clear-catalogues ()
  (let ((spec (defentry "IDENTITY"
                :subject #'identity
                (examples :function
                  :returns t :for (t)))))
    (assert (not (zerop (hash-table-count (all-catalogues)))))
    (clear-catalogues)
    (assert (zerop (hash-table-count (all-catalogues))))))

(defun spec.get-entry-with-subject ()
  (clear-catalogues)
  (let ((spec1 (defentry "test 1 for #'oddp"
                 :subject #'oddp))
        (spec2 (defentry "test 2 for #'oddp"
                 :subject #'oddp)))
    (assert (typep (all-catalogues) 'hash-table))
    (assert (typep (gethash #'oddp (all-catalogues)) 'list))
    (let ((oddp-entries (gethash #'oddp (all-catalogues))))
      (assert (string= (entry-desc (elt oddp-entries 0)) "test 1 for #'oddp"))
      (assert (string= (entry-desc (elt oddp-entries 1)) "test 2 for #'oddp")))))

(spec.check-flow)

(spec.entry-desc)

(spec.parse-function-exampels.malformed.empty)
(spec.parse-function-exampels.parse-io-pair)
(spec.parse-function-exapmles.allow-values-for-output)
(spec.parse-function-exampels.malformed.incomplete-io-pair)
(spec.parse-function-examples.multiple-pairs)

(spec.define-example)
(spec.define-example.evaluate-io-values)

(spec.parse-entry-body.empty-body-signaled-error)
(spec.parse-entry-body.parse-subject)
(spec.parse-entry-body.subject-is-required)
(spec.parse-entry-body.parse-prepare)
(spec.parse-entry-body.parse-dispose)
(spec.parse-entry-body.parse-examples)

(spec.define-spec)

(spec.check-spec-succeeds)
(spec.check-spec-fails)
(spec.check-spec.with-multiple-io-pairs-succeeds)
(spec.check-spec.with-multiple-values)

(spec.output-spec-succeeded-result)
(spec.output-spec-failed-result)

(spec.store-and-clear-catalogues)
(spec.get-entry-with-subject)
