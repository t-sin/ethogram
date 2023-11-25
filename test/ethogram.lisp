(defpackage :ethogram.test
  (:use :cl
        :ethogram))
(in-package :ethogram.test)

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
;; - [ ] `(examples :function ...)`で定義した内容で検査を実行できる
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
;;   :let counter
;;   :prepare (setf counter (make-instance 'counter))
;;   :do (subject counter)  ; doじゃないこうメインの処理感がほしい...
;;   :expect 1)
;; ```
;;

(defun test.checked? ()
  (flet ((subject ()))
    (let ((spec (defspec "check if did spec preparation")))
      (assert (null (checked? spec)))
      (check spec)
      (assert (not (null (checked? spec)))))))

(defun test.check-flow ()
  (let ((logs ()))
    (labels ((push-log (name) (push name logs))
             (prepare () (push-log :prepare))
             (dispose () (push-log :dispose))
             (subject ()
               (push-log :check)
               (signal "signal a condition")))
      (let ((spec (defspec "check flow is: preparation, checking and disposing"
                    :subject #'subject
                    :prepare #'prepare
                    :dispose #'dispose)))
        (check spec)
        (assert (equal '(:prepare :check :dispose) (reverse logs)))))))

(defun test.spec-desc ()
  (let ((spec (defspec "spec description")))
    (assert (string= (spec-desc spec) "spec description"))))

(test.checked?)
(test.check-flow)
(test.spec-desc)
