(ns ep2.core-test
  (:require [clojure.test :refer :all]
            [ep2.core :refer :all]
    )
)

(deftest GetGramRuleTest []
  (let [
      rules_test [
        [ "S" ["a", "A", "S"]  ]
        [ "A" ["a"]            ]
      ]

      value_result (ep2.core/GetGramRule rules_test 0)

      value_result1 (ep2.core/GetGramRule rules_test 1)
    ]
  
    (testing "Testando a função GetGramRuleTest"
      (is (= value_result {"simbol" "S", "value" ["a", "A", "S"] }))
      (is (= (value_result "simbol") "S" ))
      (is (= (value_result "value" ) ["a", "A", "S"] ))
      (is (= value_result1 {"simbol" "A", "value" ["a"]} ))
    )
  )
)


(deftest GetApplyRuleInElementTest []
  (let [
      rules_test0 [
        [ "S" ["a", "A", "S"]  ]
        [ "S" ["a"]            ]
        [ "A" ["b" "a"]        ]
      ]

      value_result0 (ep2.core/GetApplyRuleInElement rules_test0 "S")
      value_result1 (ep2.core/GetApplyRuleInElement rules_test0 "A")
      value_result2 (ep2.core/GetApplyRuleInElement rules_test0 "a")
    ]
  
    (testing "Testando a função GetGramRuleTest"
      (is (= value_result0 [["a", "A", "S"] ["a"]] ))
      (is (= value_result1 [["b" "a"]] ))
      (is (= value_result2 [] ))
    )
  )
)

(deftest GetApplyRulesInChainTest []
  (let [
      rules_test0 [
        [ "S" ["a", "A", "S"]  ]
        [ "S" ["a"]            ]
        [ "A" ["b" "a"]        ]
      ]

      rules_test1 [
        [ "S" ["a", "A", "S"]  ]
        [ "S" ["a"]            ]
      ]

      value_result0 (ep2.core/GetApplyRulesInChain rules_test0 ["S", "A"])
      value_result1 (ep2.core/GetApplyRulesInChain rules_test1 ["a", "A", "S"])
    ]
  
    (testing "Testando a função GetGramRuleTest"
      (is (= value_result0 [["a", "A", "S", "A"], ["a", "A"], ["S", "b", "a"]]  ))
      (is (= value_result1 [["a", "A", "a", "A", "S"], ["a", "A", "a"]]  ))
    )
  )
)