(defsystem "ethogram-test"
  :depends-on ("ethogram")
  :serial t
  :components ((:module "test"
                :components ((:file "ethogram")))))
