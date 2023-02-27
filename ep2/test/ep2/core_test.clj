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
  
    (testing "Testando a função GetGramRule"
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
  
    (testing "Testando a função GetApplyRuleInElement"
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
  
    (testing "Testando a função GetApplyRulesInChain"
      (is (= value_result0 [["a", "A", "S", "A"], ["a", "A"], ["S", "b", "a"]]  ))
      (is (= value_result1 [["a", "A", "a", "A", "S"], ["a", "A", "a"]]  ))
    )
  )
)

(deftest GenerateAllPossibleChainsTest []
  (let [
      rules_test0 [
        [ "S" ["a"]            ]
      ]

      rules_test1 [
        [ "S" ["a", "A", "S"]  ]
        [ "S" ["a"]            ]
      ]

      value_result0 (ep2.core/GenerateAllPossibleChains rules_test0 2)
      value_result1 (ep2.core/GenerateAllPossibleChains rules_test1 3)
    ]
  
    (testing "Testando a função GenerateAllPossibleChains"
      (is (= value_result0 [["S"], ["a"]]  ))
      (is (= value_result1 [["S"], ["a", "A", "S"], ["a"], ["a", "A", "a"]]  ))
    )
  )
)

(deftest CheckIfChainIsAccepedTest []
  (let [
      rules_test0 [
        [ "S" ["a", "A", "S"]  ]
        [ "S" ["a"]            ]
        [ "S" ["S", "S"]       ]
        [ "A" ["b", "a"]       ]
        [ "A" ["S", "S"]       ]
      ]

      chain_to_be_recognized0 ["a", "a"] 
      chain_to_be_recognized1 ["a", "a","a","a"] 
      chain_to_be_recognized2 ["b", "b"] 

      value_result0 (ep2.core/CheckIfChainIsAcceped rules_test0 chain_to_be_recognized0)
      value_result1 (ep2.core/CheckIfChainIsAcceped rules_test0 chain_to_be_recognized1)
      value_result2 (ep2.core/CheckIfChainIsAcceped rules_test0 chain_to_be_recognized2)
    ]
  
    (testing "Testando a função CheckIfChainIsAcceped"
      (is value_result0)
      (is value_result1)
      (is (not value_result2))
    )
  )
)