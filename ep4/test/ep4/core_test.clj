(ns ep4.core-test
  (:require [clojure.test :refer :all]
            [ep4.core :refer :all]))

;; functions tests --------------------
(deftest GetAllPossibleSymbolsTest []
  (let
    [
      grammar [["S" ["A", "S", "A"]]
               ["S" ["a", "B"]]
               ["A" ["B"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]]
                   
      result (ep4.core/GetAllPossibleSymbols grammar)
    ]

    (testing "Testando a função GetAllPossibleSymbols"
      (is (= result ["S" "A" "B" ]))
    )
  )
)

(deftest IsRuleWithTwoVariableTest []
  (let
    [
      grammar [["S" ["A", "S", "A"]]
               ["S" ["a", "B"]]
               ["A" ["B" "A"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]]
      
      start_symbol "S"

      end_symbols ["a" "b"]
                   
      result0 (ep4.core/IsRuleWithTwoVariable grammar start_symbol end_symbols 0)
      result1 (ep4.core/IsRuleWithTwoVariable grammar start_symbol end_symbols 1)
      result2 (ep4.core/IsRuleWithTwoVariable grammar start_symbol end_symbols 2)
      result3 (ep4.core/IsRuleWithTwoVariable grammar start_symbol end_symbols 3)
      result4 (ep4.core/IsRuleWithTwoVariable grammar start_symbol end_symbols 4)
    ]

    (testing "Testando a função IsRuleWithTwoVariable"
      (is (not result0) )
      (is (not result1) )
      (is result2 )
      (is (not result3) )
      (is (not result4) )
    )
  )
)

(deftest IsRuleWithOneTerminalTest []
  (let
    [
      grammar [["S" ["A", "S", "A"]]
               ["S" ["a", "B", "a"]]
               ["A" ["B" "A"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]]
      
      start_symbol "S"

      end_symbols ["a" "b"]
                   
      result0 (ep4.core/IsRuleWithOneTerminal grammar start_symbol end_symbols 0)
      result1 (ep4.core/IsRuleWithOneTerminal grammar start_symbol end_symbols 1)
      result2 (ep4.core/IsRuleWithOneTerminal grammar start_symbol end_symbols 2)
      result3 (ep4.core/IsRuleWithOneTerminal grammar start_symbol end_symbols 3)
      result4 (ep4.core/IsRuleWithOneTerminal grammar start_symbol end_symbols 4)
    ]

    (testing "Testando a função IsRuleWithTwoVariable"
      (is (not result0) )
      (is (not result1) )
      (is (not result2) )
      (is (not result3) )
      (is result4 )
    )
  )
)