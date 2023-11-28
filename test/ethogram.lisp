(defpackage :ethogram.spec
  (:use :cl
        :ethogram))
(in-package :ethogram.spec)

;; # DSL example
;;
;; ;; 引数は&bodyにしてエディタのインデントを減らす
;; (defmacro examples (type &body body))
;;
;; (defspec "a function to check number's oddness"
;;   :subject #'oddp
;;
;;   (examples :function
;;     :returns t :for 1)
;;
;;   (examples :function
;;     :about "first argument is an odd number"
;;     :returns nil :for 0
;;     :returns nil :for 2
;;     :returns nil :for 10)
;;
;;   (examples :group
;;     :about "first argument is not an odd number"
;;     (examples :function
;;       :about "non-zero numbers"
;;       :returns t
;;       :for 1
;;       :for 3
;;       :for 1001)
;;     (examples :function
;;       :about "a zero"
;;       :returns t
;;       :for 0)))

;; # TODO (上からやる)
;;
;; - [x] 検査を実行する
;; - [x] 検査の前に:prepareを実行する
;; - [x] 検査の後に:disposeを実行する
;; - [x] 検査がコンディションを投げても:disposeを実行する
;; - [x] 検査のタイトルをspec-descで取得できる
;; - [x] `(examples :function ...)`で検査内容を定義できる
;; - [ ] defspecの中身をパースできる
;; - [ ] examplesマクロで定義した検査を実行できる
;; - [ ] 検査の結果をstdioに即時出力する
;; - [ ] defspecによって、それが書かれた環境で評価されるようなチェックコードができる (*1)
;;     - あきらかにデカいので分解する
;; - [ ] :prepareを式で渡せる (*1)
;; - [ ] :disposeを式で渡せる (*1)
;; - [ ] 複数の検査を実行する
;; - [ ] 検査の結果を収集する
;;
;; -----
;;
;; *1 後にまわす系の既知の課題。
;;    defspec内に現れる各式はdefspecが定義された環境で評価されるべき。
;;    でないとdefspecの外のdefparameterやdefunを触れなくなる。
;;    defspecの内部なら宣言を以下の形式でのみ許可して限定できるとする:
;;      `:let NAME := INITFORM` (初期化はオプショナル)
;;    でも書いた環境で評価されることを保証せねばいろいろ直観的でなくなっちゃう。
;;    例: defspecではspecに式を保持するだけでcheck時に式を評価すると、
;;        環境が違うので意図しない値が評価結果で出てきて泣く。
;;    どうしよ...
;;
;; ```lisp
;; (defclass counter ()
;;   (count :initform 0
;;          :reader counter-n))
;;
;; (defgeneric countup (counter))
;; (defmethod countup ((c counter))
;;   (incf (counter-n c)))
;;
;; (defspec "prepareで保持してテストで使ってdisposeで束縛ごと後始末したい"
;;   :subject #'countup
;;
;;   :let counter
;;   :prepare (setf counter (make-instance 'counter))
;;   (examples :sideeffect     ; ちょっと大仰...
;;     :run (subject counter)  ; doじゃないこうメインの処理感がほしい...
;;     :expect 1))
;; ```
;;

(defun spec.check-flow ()
  (let ((logs ()))
    (labels ((push-log (name) (push name logs))
             (prepare () (push-log :prepare))
             (dispose () (push-log :dispose))
             (subject ()
               (push-log :check)
               (signal "signal a condition")))
      (let ((spec (defspec "check flow is: preparation, checking and disposing"
                    :subject #'subject
                    :prepare (prepare)
                    :dispose (dispose)
                    (examples :function
                      :returns nil :for nil))))
        (check spec)
        (assert (equal '(:prepare :check :dispose) (reverse logs)))))))

(defun spec.spec-desc ()
  (let ((spec (defspec "spec description"
                :subject #'oddp
                (examples :function
                  :returns t :for 1))))
    (assert (string= (spec-desc spec) "spec description"))))

(defun spec.define-example ()
  "define a function example"
  (let ((example (examples :function
                   :returns 42 :for (6 9))))
    (assert (typep example 'function-examples))
    (let ((input (function-examples-input example)))
      (assert (typep input 'list))
      (assert (= (length input) 2))
      (assert (= (elt input 0) 6))
      (assert (= (elt input 1) 9)))
    (let ((output (function-examples-output example)))
      (assert (typep output 'list))
      (assert (= (elt output 0) 42))))
  (let ((example (examples :function
                   :returns (1 2 3) :for (1 2 3))))
    (assert (typep example 'function-examples))
    (let ((input (function-examples-input example)))
      (assert (typep input 'list))
      (assert (= (elt input 0) 1))
      (assert (= (elt input 1) 2))
      (assert (= (elt input 2) 3)))
    (let ((output (function-examples-output example)))
      (assert (typep output 'list))
      (assert (= (elt output 0) 1))
      (assert (= (elt output 1) 2))
      (assert (= (elt output 2) 3)))))

(defun spec.parse-spec-body.empty-body-signaled-error ()
  (let ((body '()))
    (block check
      (flet ((check-reason (c)
               (assert (string= (slot-value c 'reason) "empty"))
               (return-from check)))
        (handler-bind ((malformed-spec-error #'check-reason))
          (parse-spec body)
          (assert nil))))))

(defun spec.parse-spec-body.parse-subject ()
  (let ((body '(:subject #'oddp)))
    (assert (eq (parse-spec body) '#'oddp))))

(defun spec.parse-spec-body.parse-prepare ()
  (let ((body '(:subject #'oddp
                :prepare (identity 42))))
    (assert (equal (multiple-value-list (parse-spec body))
                   '(#'oddp
                     (identity 42)
                     nil
                     nil)))))

(defun spec.parse-spec-body.parse-dispose ()
  (let ((body '(:subject #'oddp
                :prepare (identity 42)
                :dispose (identity 45))))
    (assert (equal (multiple-value-list (parse-spec body))
                   '(#'oddp
                     (identity 42)
                     (identity 45)
                     nil)))))

(defun spec.parse-spec-body.subject-is-required ()
  (let ((body `(:prepare '(identity 42))))
    (block check
      (flet ((check-reason (c)
               (assert (string= (slot-value c 'reason) ":SUBJECT is required"))
               (return-from check)))
        (handler-bind ((malformed-spec-error #'check-reason))
          (parse-spec body))))))

(defun spec.parse-spec-body.parse-examples ()
  (let ((body '(:subject #'oddp
                :prepare (identity 42)
                (examples :function :returns t :for 1))))
    (assert (tree-equal (multiple-value-list (parse-spec body))
                        '(#'oddp
                          (identity 42)
                          nil
                          (examples :function :returns t :for 1))))))

(defun spec.define-spec ()
  (let ((spec (defspec "spec"
                :subject #'oddp
                :prepare (print :ln)
                (examples :function
                  :returns t :for 1))))
    (assert (string= (spec-desc spec) "spec"))
    (assert (eq (spec-subject spec) #'oddp))
    (assert (not (null (spec-check spec))))))

(defun spec.check-spec-succeeds ()
  (let ((spec (defspec "spec"
                :subject #'oddp
                (examples :function
                  :returns t :for 1))))
    (assert (check spec))))

(defun spec.check-spec-fails ()
  (let ((spec (defspec "spec"
                :subject #'oddp
                (examples :function
                  :returns t :for 2))))
    (assert (not (check spec)))))

(spec.check-flow)
(spec.spec-desc)
(spec.define-example)
(spec.parse-spec-body.empty-body-signaled-error)
(spec.parse-spec-body.parse-subject)
(spec.parse-spec-body.subject-is-required)
(spec.parse-spec-body.parse-prepare)
(spec.parse-spec-body.parse-dispose)
(spec.parse-spec-body.parse-examples)
(spec.define-spec)
(spec.check-spec-succeeds)
(spec.check-spec-fails)