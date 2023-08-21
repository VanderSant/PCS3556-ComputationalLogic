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

(deftest IsThatGrammarInChomskyNormalFormTest []
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

      result0 (ep4.core/IsThatGrammarInChomskyNormalForm grammar0 start_symbol0 end_symbols0)
      result1 (ep4.core/IsThatGrammarInChomskyNormalForm grammar1 start_symbol1 end_symbols1)
    ]
    
    (testing "Testando a função IsThatGrammarInChomskyNormalForm"
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

(deftest AddRuleInGrammarTest
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

      result0 (ep4.core/AddRuleInGrammar grammar elements symbol0)
      result1 (ep4.core/AddRuleInGrammar grammar elements symbol1)
      result2 (ep4.core/AddRuleInGrammar grammar [] symbol1)

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

    (testing "Testando a função AddRuleInGrammar"
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
    (testing "Testando a função GenerateAllCombinationsByRemovedElement"
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
    (testing "Testando a função ApplyEmptySymbolInGrammar"
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

(deftest RemoveRedundantValuesTest
  (let
    [
      grammar0 [ 
        ["S" ["a"]]
        ["S" ["S"]]
      ]

      grammar1 [ 
        ["S" ["a"]]
        ["S" ["B"]]
        ["S" ["B"]]
      ]

      grammar2 [ 
        ["S0" ["S"]]
        ["S0" ["A" "S"]] 
        ["S0" ["S" "A"]] 
        ["S0" ["A" "S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S"]]
        ["S" ["a", "B"]]
        ["S" ["A", "S"]]
        ["S" ["S", "A"]]
        ["S" ["S", "A"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["A" ["S"]]
        ["B" ["b"]]
      ]

      result0 (ep4.core/RemoveRedundantValues grammar0)
      result1 (ep4.core/RemoveRedundantValues grammar1)
      result2 (ep4.core/RemoveRedundantValues grammar2)

      expectec_result0 [
        ["S" ["a"]]
      ]

      expectec_result1 [
        ["S" ["a"]]
        ["S" ["B"]]
      ]

      expectec_result2 [
        ["S0" ["S"]]
        ["S0" ["A" "S"]] 
        ["S0" ["S" "A"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["A", "S"]]
        ["S" ["S", "A"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["B" ["b"]]
      ]
    ]
    (testing "Testando a função RemoveRedundantValues"
      (is (= result0 expectec_result0))
      (is (= result1 expectec_result1))
      (is (= result2 expectec_result2))
    )
  )
)

(deftest FindFirstAndLastIndexOfSymbolTest
  (let
    [

      grammar [ 
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S"]]
        ["S" ["A", "S"]]
        ["S" ["S", "A"]]
        ["S" ["S", "A"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["A" ["S"]]
        ["B" ["b"]]
      ]

      symbol0 "S0"
      symbol1 "S"
      symbol2 "A"
      symbol3 "B"
      symbol4 "C"

      result_first_0 (ep4.core/FindFirstIndexOfSymbol grammar symbol0)
      result_last_0 (ep4.core/FindLastIndexOfSymbol grammar symbol0)
      result_first_1 (ep4.core/FindFirstIndexOfSymbol grammar symbol1)
      result_last_1 (ep4.core/FindLastIndexOfSymbol grammar symbol1)
      result_first_2 (ep4.core/FindFirstIndexOfSymbol grammar symbol2)
      result_last_2 (ep4.core/FindLastIndexOfSymbol grammar symbol2)
      result_first_3 (ep4.core/FindFirstIndexOfSymbol grammar symbol3)
      result_last_3 (ep4.core/FindLastIndexOfSymbol grammar symbol3)
      result_first_4 (ep4.core/FindFirstIndexOfSymbol grammar symbol4)
      result_last_4 (ep4.core/FindLastIndexOfSymbol grammar symbol4)

      expectec_result_first_0 0
      expectec_result_last_0 0
      expectec_result_first_1 1
      expectec_result_last_1 7
      expectec_result_first_2 8
      expectec_result_last_2 10
      expectec_result_first_3 11
      expectec_result_last_3 11
      expectec_result_first_4 nil
      expectec_result_last_4 nil

    ]
    (testing "Testando a função FindFirstAndLastIndexOfSymbol"
      (is (= result_first_0 expectec_result_first_0))
      (is (= result_last_0 expectec_result_last_0))
      (is (= result_first_1 expectec_result_first_1))
      (is (= result_last_1 expectec_result_last_1))
      (is (= result_first_2 expectec_result_first_2))
      (is (= result_last_2 expectec_result_last_2))
      (is (= result_first_3 expectec_result_first_3))
      (is (= result_last_3 expectec_result_last_3))
      (is (= result_first_4 expectec_result_first_4))
      (is (= result_last_4 expectec_result_last_4))
    )
  )
)

(deftest RemoveAllUnitValuesTest
  (let
    [

      grammar [ 
        ["S0" ["S"] ]
        ["S0" ["A", "S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["A", "S"]]
        ["S" ["S", "A"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["B" ["b"]]
      ]
      
      start_symbol "S0"

      end_symbols ["a" "b"]

      result_0 (ep4.core/RemoveAllUnitValues grammar start_symbol end_symbols)

      expectec_result_0 [
        ["S0" ["A", "S", "A"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["A", "S"]]
        ["S0" ["S", "A"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["A", "S"]]
        ["S" ["S", "A"]]
        ["A" ["b"]]
        ["A" ["A", "S", "A"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["A", "S"]]
        ["A" ["S", "A"]]
        ["B" ["b"]]
      ]

    ]
    (testing "Testando a função RemoveAllUnitValues"
      (is (= result_0 expectec_result_0))
    )
  )
)