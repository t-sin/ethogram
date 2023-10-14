(defsystem "ethogram-test"
  :depends-on ("ethogram")
  :components ((:module "test"
                :serial t
                :components ((:file "context-function")))))
