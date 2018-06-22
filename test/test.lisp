(in-package :jsonq.test)

(fiasco:deftest basic-test ()
  (fiasco:is (string= (princ-to-string (jsonq:obj))
                      "{}"))
  (fiasco:is (string= (princ-to-string (jsonq:obj :a "b"))
                      "{\"a\": \"b\"}"))
  (fiasco:is (string= (princ-to-string (jsonq:obj :a "b" :cc-cc "d" "ee-ee" "f"))
                      "{\"a\": \"b\", \"ccCc\": \"d\", \"ee-ee\": \"f\"}"))
  (fiasco:is (string= (princ-to-string (jsonq:obj :a 1 :b 2 :a 10))
                      "{\"a\": 10, \"b\": 2}"))
  (fiasco:is (string= (princ-to-string (jsonq:obj :a 1 :b 2 * (jsonq:obj :a 10 :c 3)))
                      "{\"a\": 10, \"b\": 2, \"c\": 3}")))

(run-package-tests :interactive t)
