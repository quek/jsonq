(asdf:defsystem :jsonq.test
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "test"))
  :depends-on (:jsonq
               :fiasco))
