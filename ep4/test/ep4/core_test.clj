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

(deftest GetNumberOfVariableTest []
  (let
    [
      grammar [["S" ["A", "S", "A"]]
               ["S" ["a", "B"]]
               ["A" ["B" "A"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]]
      
      start_symbol "S"

      end_symbols ["a" "b"]
                   
      result0 (ep4.core/GetNumberOfVariables grammar start_symbol end_symbols 0)
      result1 (ep4.core/GetNumberOfVariables grammar start_symbol end_symbols 1)
      result2 (ep4.core/GetNumberOfVariables grammar start_symbol end_symbols 2)
      result3 (ep4.core/GetNumberOfVariables grammar start_symbol end_symbols 3)
      result4 (ep4.core/GetNumberOfVariables grammar start_symbol end_symbols 4)
    ]

    (testing "Testando a função GetNumberOfVariable"
      (is (= result0 3) )
      (is (= result1 1) )
      (is (= result2 2) )
      (is (= result3 1) )
      (is (= result4 0) )
    )
  )
)

(deftest GetNumberOfTerminalsTest []
  (let
    [
      grammar [["S" ["A", "S", "A"]]
               ["S" ["a", "B", "a"]]
               ["A" ["B" "A"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]]
      
      start_symbol "S"

      end_symbols ["a" "b"]
                   
      result0 (ep4.core/GetNumberOfTerminals grammar start_symbol end_symbols 0)
      result1 (ep4.core/GetNumberOfTerminals grammar start_symbol end_symbols 1)
      result2 (ep4.core/GetNumberOfTerminals grammar start_symbol end_symbols 2)
      result3 (ep4.core/GetNumberOfTerminals grammar start_symbol end_symbols 3)
      result4 (ep4.core/GetNumberOfTerminals grammar start_symbol end_symbols 4)
    ]

    (testing "Testando a função IsRuleWithTwoVariable"
      (is (= result0 0) )
      (is (= result1 2) )
      (is (= result2 0) )
      (is (= result3 0) )
      (is (= result4 1) )
    )
  )
)

(deftest GetNumberEmptyVariablesTest
  (let 
    [
      grammar [["S" ["A", "S", "A"]]
               ["S" ["a", "B", "a"]]
               ["A" ["B" "A"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]
               ["S" ["a", "ε"]]]
      
      start_symbol "S"

      end_symbols ["a" "b"]

      result0 (ep4.core/GetNumberEmptyVariables grammar start_symbol end_symbols 0)
      result1 (ep4.core/GetNumberEmptyVariables grammar start_symbol end_symbols 1)
      result2 (ep4.core/GetNumberEmptyVariables grammar start_symbol end_symbols 2)
      result3 (ep4.core/GetNumberEmptyVariables grammar start_symbol end_symbols 3)
      result4 (ep4.core/GetNumberEmptyVariables grammar start_symbol end_symbols 4)

      result5 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 0)
      result6 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 1)
      result7 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 2)
      result8 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 3)
      result9 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 4)
      result10 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 5)
    ]

    (testing "Testando a função IsRuleWithTwoVariable"
      (is (= result0 0) )
      (is (= result1 0) )
      (is (= result2 0) )
      (is (= result3 0) )
      (is (= result4 1) )
      (is result5 )
      (is result6 )
      (is result7 )
      (is result8 )
      (is (not result9) )
      (is result10 )
    )
  )
)

(deftest IsThatGrammerInChomskyNormalFormTest []
  (let 
    [ 
      start_symbol0 "S"
      end_symbols0 ["a" "b"]

      grammar0 [["S" ["A", "S", "A"]]
               ["S" ["a", "B"]]

               ["A" ["B" "A"]]
               ["A" ["S"]]
               
               ["B" ["b"]]
               ["B" ["ε"]]]

      start_symbol1 "S0"
      end_symbols1 ["a" "b"]
      grammar1 [
                ["S0" ["A" "Z"]]
                ["S0" ["U" "B"]]
                ["S0" ["a"]]
                ["S0" ["S" "A"]]
                ["S0" ["A" "S"]]

                ["S" ["A" "Z"]]
                ["S" ["U" "B"]]
                ["S" ["a"]]
                ["S" ["S" "A"]]
                ["S" ["A" "S"]]

                ["A" ["b"]]
                ["A" ["A" "Z"]]
                ["A" ["U" "B"]]
                ["A" ["a"]]
                ["A" ["S" "A"]]
                ["A" ["A" "S"]]

                ["Z" ["S" "A"]]

                ["U" ["a"]]

                ["B" ["b"]]
              ]

      result0 (ep4.core/IsThatGrammerInChomskyNormalForm grammar0 start_symbol0 end_symbols0)
      result1 (ep4.core/IsThatGrammerInChomskyNormalForm grammar1 start_symbol1 end_symbols1)
    ]
    
    (testing "Testando a função IsThatGrammerInChomskyNormalForm"
      (is (not result0))
      (is result1)
    )
  )
)

(deftest RemoveInitalSymbolFromRightSideTest
  (let 
    [
      grammar [["S" ["A", "S", "A"]]
               ["S" ["a", "B"]]
               ["A" ["B" "A"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]
               ["S" ["a", "ε"]]]
      
      start_symbol "S"

      end_symbols ["a" "b"]

      result0 (ep4.core/RemoveInitalSymbolFromRightSide grammar start_symbol end_symbols)

      expectec_result0 [
        [
          ["S0" ["S"]]
          ["S" ["A", "S", "A"]]
          ["S" ["a", "B"]]
          ["A" ["B" "A"]]
          ["A" ["S"]]
          ["B" ["b", "ε"]]
          ["S" ["a", "ε"]]
        ]

        "S0"]
    ]
    (testing "Testando a função RemoveInitalSymbolFromRightSide"
      (is (= expectec_result0 result0))
    )
  )
)

(deftest AddRuleInGrammerTest
  (let
    [
      grammar [ 
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["B" ["b"]]
        ["B" ["ε"]]
      ]

      elements [["A" "S"] ["S" "A"] ["S"]]

      symbol0 "S"
      symbol1 "B"

      result0 (AddRuleInGrammer grammar elements symbol0)
      result1 (AddRuleInGrammer grammar elements symbol1)
      result2 (AddRuleInGrammer grammar [] symbol1)

      expectec_result0 [
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]

        ["S" ["A" "S"]]
        ["S" ["S" "A"]]
        ["S" ["S"]]

        ["A" ["B"]]
        ["A" ["S"]]
        ["B" ["b"]]
        ["B" ["ε"]]
      ]

      expectec_result1 [
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]

        ["A" ["B"]]
        ["A" ["S"]]

        ["B" ["b"]]
        ["B" ["ε"]]

        ["B" ["A" "S"]]
        ["B" ["S" "A"]]
        ["B" ["S"] ]
      ]

    ]

    (testing "Testando a função AddRuleInGrammer"
      (is (= result0 expectec_result0))
      (is (= result1 expectec_result1))
      (is (= result2 grammar))
    )
  )
)

(deftest GenerateAllCombinationsByRemoveElementTest
  (let
    [
      element0 ["A", "S", "A"]
      element1 ["a", "B"]
      element2 ["A", "S"]
      element3 ["A"]

      symbol "A"

      result0 (ep4.core/GenerateAllCombinationsByRemovedElement element0 symbol)
      result1 (ep4.core/GenerateAllCombinationsByRemovedElement element1 symbol)
      result2 (ep4.core/GenerateAllCombinationsByRemovedElement element2 symbol)
      result3 (ep4.core/GenerateAllCombinationsByRemovedElement element3 symbol)

      expectec_result0 [
        ["S"]
        ["A", "S"]
        ["S", "A"]
      ]

      expectec_result1 []

      expectec_result2 [
        ["S"]
      ]

      expectec_result3 [
        ["ε"]
      ]


    ]
    (testing "Testando a função GenerateAllCombinationsByRemoveElement"
      (is (= result0 expectec_result0))
      (is (= result1 expectec_result1))
      (is (= result2 expectec_result2))
      (is (= result3 expectec_result3))
    )
  )
)

(deftest ApplyEmptySymbolInGrammarTest
  (let 
    [
      start_symbol "S0"

      end_symbols ["a" "b"]

      grammar0 [ 
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["B" ["b"]]
        ["B" ["ε"]]
      ]
      index0 0
      result0 (ep4.core/ApplyEmptySymbolInGrammar grammar0 start_symbol end_symbols index0)
      expectec_result0 [
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["A" ["ε"]]
        ["B" ["b"]]
      ]

      index1 1
      result1 (ep4.core/ApplyEmptySymbolInGrammar expectec_result0 start_symbol end_symbols index1)
      expectec_result1 [
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S"]]
        ["S" ["A", "S"]]
        ["S" ["S", "A"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["B" ["b"]]
      ]

    ]
    (testing "Testando a função RemoveEmptyValues"
      (is (= result0 expectec_result0))
      (is (= result1 expectec_result1))
    )
  )
)

(deftest RemoveAllPossibleEmptyValuesTest
  (let
    [
      grammar [ ["S0" ["S"]]
                ["S" ["A", "S", "A"]]
                ["S" ["a", "B"]]
                ["A" ["B"]]
                ["A" ["S"]]
                ["B" ["b"]]
                ["B" ["ε"]]]
      
      start_symbol "S0"

      end_symbols ["a" "b"]

      result0 (ep4.core/RemoveAllPossibleEmptyValues grammar start_symbol end_symbols)

      expectec_result0 [
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S"]]
        ["S" ["A", "S"]]
        ["S" ["S", "A"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["B" ["b"]]
      ]
    ]
    (testing "Testando a função RemoveAllPossibleEmptyValues"
      (is (= result0 expectec_result0))
    )
  )
)