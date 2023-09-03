(ns ep4.core-test
  (:require [clojure.test :refer :all]
            [ep4.core :refer :all]))

;; --------------------------------------
;; General functions tests
;; --------------------------------------

(deftest GetAllPossibleSymbolsTest []
  (let
    [
      grammar0 [["S" ["A", "S", "A"]]
               ["S" ["a", "B"]]
               ["A" ["B"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]]

      grammar1 [
        ["S0" ["A", "S", "A"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]
        ["A" ["b"]]
        ["A" ["A", "S", "A"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]
        ["B" ["b"]]
      ]

      result0 (ep4.core/GetAllPossibleSymbols grammar0)
      result1 (ep4.core/GetAllPossibleSymbols grammar1)
    ]

    (testing "Testando a função GetAllPossibleSymbols"
      (is (= result0 ["S" "A" "B" ]))
      (is (= result1 ["S0" "S" "A" "B" ]))
    )
  )
)

(deftest GetNumberOfVariablesTest []
  (let
    [
      grammar [["S" ["A", "S", "A"]]
               ["S" ["a", "B"]]
               ["A" ["B" "A"]]
               ["A" ["S"]]
               ["B" ["b", "ε"]]]
      
      start_symbol "S"

      end_symbols ["a" "b"]
                   
      result0 (ep4.core/GetNumberOfVariablesByIndex grammar start_symbol end_symbols 0)
      result1 (ep4.core/GetNumberOfVariablesByIndex grammar start_symbol end_symbols 1)
      result2 (ep4.core/GetNumberOfVariablesByIndex grammar start_symbol end_symbols 2)
      result3 (ep4.core/GetNumberOfVariablesByIndex grammar start_symbol end_symbols 3)
      result4 (ep4.core/GetNumberOfVariablesByIndex grammar start_symbol end_symbols 4)

      result5 (ep4.core/GetNumberOfVariablesByElement grammar start_symbol end_symbols (get (get grammar 0) 1) )
      result6 (ep4.core/GetNumberOfVariablesByElement grammar start_symbol end_symbols (get (get grammar 1) 1) )
      result7 (ep4.core/GetNumberOfVariablesByElement grammar start_symbol end_symbols (get (get grammar 2) 1) )
      result8 (ep4.core/GetNumberOfVariablesByElement grammar start_symbol end_symbols (get (get grammar 3) 1) )
      result9 (ep4.core/GetNumberOfVariablesByElement grammar start_symbol end_symbols (get (get grammar 4) 1) )
    ]

    (testing "Testando a função GetNumberOfVariable"
      (is (= result0 3) )
      (is (= result1 1) )
      (is (= result2 2) )
      (is (= result3 1) )
      (is (= result4 0) )
      (is (= result5 3) )
      (is (= result6 1) )
      (is (= result7 2) )
      (is (= result8 1) )
      (is (= result9 0) )
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
                   
      result0 (ep4.core/GetNumberOfTerminalsByIndex grammar start_symbol end_symbols 0)
      result1 (ep4.core/GetNumberOfTerminalsByIndex grammar start_symbol end_symbols 1)
      result2 (ep4.core/GetNumberOfTerminalsByIndex grammar start_symbol end_symbols 2)
      result3 (ep4.core/GetNumberOfTerminalsByIndex grammar start_symbol end_symbols 3)
      result4 (ep4.core/GetNumberOfTerminalsByIndex grammar start_symbol end_symbols 4)
    
      result5 (ep4.core/GetNumberOfTerminalsByElement grammar start_symbol end_symbols (get (get grammar 0) 1) )
      result6 (ep4.core/GetNumberOfTerminalsByElement grammar start_symbol end_symbols (get (get grammar 1) 1) )
      result7 (ep4.core/GetNumberOfTerminalsByElement grammar start_symbol end_symbols (get (get grammar 2) 1) )
      result8 (ep4.core/GetNumberOfTerminalsByElement grammar start_symbol end_symbols (get (get grammar 3) 1) )
      result9 (ep4.core/GetNumberOfTerminalsByElement grammar start_symbol end_symbols (get (get grammar 4) 1) )
    ]

    (testing "Testando a função GetNumberOfTerminalsByIndex"
      (is (= result0 0) )
      (is (= result1 2) )
      (is (= result2 0) )
      (is (= result3 0) )
      (is (= result4 1) )
      (is (= result5 0) )
      (is (= result6 2) )
      (is (= result7 0) )
      (is (= result8 0) )
      (is (= result9 1) )
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

      result0 (ep4.core/GetNumberEmptyVariablesByIndex grammar start_symbol end_symbols 0)
      result1 (ep4.core/GetNumberEmptyVariablesByIndex grammar start_symbol end_symbols 1)
      result2 (ep4.core/GetNumberEmptyVariablesByIndex grammar start_symbol end_symbols 2)
      result3 (ep4.core/GetNumberEmptyVariablesByIndex grammar start_symbol end_symbols 3)
      result4 (ep4.core/GetNumberEmptyVariablesByIndex grammar start_symbol end_symbols 4)

      result5 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 0)
      result6 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 1)
      result7 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 2)
      result8 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 3)
      result9 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 4)
      result10 (ep4.core/EmptyVariableValidation grammar start_symbol end_symbols 5)
    
      result11 (ep4.core/GetNumberEmptyVariablesByElement grammar start_symbol end_symbols (get (get grammar 0) 1) )
      result12 (ep4.core/GetNumberEmptyVariablesByElement grammar start_symbol end_symbols (get (get grammar 1) 1) )
      result13 (ep4.core/GetNumberEmptyVariablesByElement grammar start_symbol end_symbols (get (get grammar 2) 1) )
      result14 (ep4.core/GetNumberEmptyVariablesByElement grammar start_symbol end_symbols (get (get grammar 3) 1) )
      result15 (ep4.core/GetNumberEmptyVariablesByElement grammar start_symbol end_symbols (get (get grammar 4) 1) )
    ]

    (testing "Testando a função GetNumberEmptyVariablesByIndex"
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
      (is (= result11 0) )
      (is (= result12 0) )
      (is (= result13 0) )
      (is (= result14 0) )
      (is (= result15 1) )
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
      symbol2 "B"
      symbol3 "C"

      result0 (ep4.core/AddRuleInGrammar grammar elements symbol0)
      result1 (ep4.core/AddRuleInGrammar grammar elements symbol1)
      result2 (ep4.core/AddRuleInGrammar grammar [] symbol2)
      result3 (ep4.core/AddRuleInGrammar grammar elements symbol3)

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

      expectec_result3 [
        ["S0" ["S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]

        ["A" ["B"]]
        ["A" ["S"]]

        ["B" ["b"]]
        ["B" ["ε"]]

        ["C" ["A" "S"]]
        ["C" ["S" "A"]]
        ["C" ["S"] ]
      ]

    ]

    (testing "Testando a função AddRuleInGrammar"
      (is (= result0 expectec_result0))
      (is (= result1 expectec_result1))
      (is (= result2 grammar))
      (is (= result3 expectec_result3))
    )
  )
)

;; -------------------------------------- 
;; Chomsky normal form verification tests
;; --------------------------------------

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

;; -------------------------------------- 
;; Remove Initial Symbols from right side tests
;; --------------------------------------

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

;; --------------------------------------
;; Remove Empties values tests
;; --------------------------------------

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

;; -------------------------------------- 
;; Remove redundant values tests
;; --------------------------------------

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

;; -------------------------------------- 
;; Remove unit values tests
;; --------------------------------------

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

;; -------------------------------------- 
;; Create new variables to grammar tests
;; --------------------------------------

(deftest GrammarCorrectionVerificationTest
  (let
    [

      grammar [ 
        ["S0" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["B" ["a", "b"]]
      ]
      
      start_symbol "S0"

      end_symbols ["a" "b"]

      ;; one variable, one terminal verification
      result_0 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 0 0)
      result_1 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 1 0)
      result_2 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 2 0)
      result_3 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 3 0)
      result_4 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 4 0)

      ;; two variable verification
      result_5 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 0 1)
      result_6 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 1 1)
      result_7 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 2 1)
      result_8 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 3 1)
      result_9 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 4 1)

      ;; two terminals verification
      result_10 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 0 2)
      result_11 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 1 2)
      result_12 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 2 2)
      result_13 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 3 2)
      result_14 (ep4.core/GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols 4 2)

      ;; one variable, one terminal verification
      result_15 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 0) 1) 0)
      result_16 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 1) 1) 0)
      result_17 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 2) 1) 0)
      result_18 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 3) 1) 0)
      result_19 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 4) 1) 0)

      ;; two variable verification
      result_20 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 0) 1) 1)
      result_21 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 1) 1) 1)
      result_22 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 2) 1) 1)
      result_23 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 3) 1) 1)
      result_24 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 4) 1) 1)

      ;; two terminals verification
      result_25 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 0) 1) 2)
      result_26 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 1) 1) 2)
      result_27 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 2) 1) 2)
      result_28 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 3) 1) 2)
      result_29 (ep4.core/GrammarCorrectionVerificationByElement grammar start_symbol end_symbols (get (get grammar 4) 1) 2)

      expectec_result_0 false
      expectec_result_1 true
      expectec_result_2 false
      expectec_result_3 false
      expectec_result_4 false

      expectec_result_5 true
      expectec_result_6 false
      expectec_result_7 false
      expectec_result_8 false
      expectec_result_9 false

      expectec_result_10 false
      expectec_result_11 false
      expectec_result_12 false
      expectec_result_13 false
      expectec_result_14 true

      expectec_result_15 false
      expectec_result_16 true
      expectec_result_17 false
      expectec_result_18 false
      expectec_result_19 false
      expectec_result_20 true
      expectec_result_21 false
      expectec_result_22 false
      expectec_result_23 false
      expectec_result_24 false
      expectec_result_25 false
      expectec_result_26 false
      expectec_result_27 false
      expectec_result_28 false
      expectec_result_29 true

    ]
    (testing "Testando a função GrammarCorrectionVerification"
      (is (= result_0 expectec_result_0))
      (is (= result_1 expectec_result_1))
      (is (= result_2 expectec_result_2))
      (is (= result_3 expectec_result_3))
      (is (= result_4 expectec_result_4))
      (is (= result_5 expectec_result_5))
      (is (= result_6 expectec_result_6))
      (is (= result_7 expectec_result_7))
      (is (= result_8 expectec_result_8))
      (is (= result_9 expectec_result_9))
      (is (= result_10 expectec_result_10))
      (is (= result_11 expectec_result_11))
      (is (= result_12 expectec_result_12))
      (is (= result_13 expectec_result_13))
      (is (= result_14 expectec_result_14))

      (is (= result_15 expectec_result_15))
      (is (= result_16 expectec_result_16))
      (is (= result_17 expectec_result_17))
      (is (= result_18 expectec_result_18))
      (is (= result_19 expectec_result_19))
      (is (= result_20 expectec_result_20))
      (is (= result_21 expectec_result_21))
      (is (= result_22 expectec_result_22))
      (is (= result_23 expectec_result_23))
      (is (= result_24 expectec_result_24))
      (is (= result_25 expectec_result_25))
      (is (= result_26 expectec_result_26))
      (is (= result_27 expectec_result_27))
      (is (= result_28 expectec_result_28))
      (is (= result_29 expectec_result_29))
    )
  )
)

(deftest GetSymbolToSwapTest
  (let
    [
      grammar [
        ["S0" ["A", "S", "A"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]
        ["A" ["b"]]
        ["A" ["A", "S", "A"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]
        ["B" ["b"]]
      ]
      start_symbol "S0"
      end_symbols ["a" "b"]

      [grammar_result_0 symbol_result_0] (ep4.core/GetSymbolToSwap grammar ["A", "S"] )

      expectec_grammar_result_0 [
        ["S0" ["A", "S", "A"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]
        ["A" ["b"]]
        ["A" ["A", "S", "A"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]
        ["B" ["b"]]
        ["B1" ["A", "S"]]
      ]
      expectec_symbol_result_0 "B1"

      [grammar_result_1 symbol_result_1] (ep4.core/GetSymbolToSwap expectec_grammar_result_0 ["a"] )

      expectec_grammar_result_1 [
        ["S0" ["A", "S", "A"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]
        ["A" ["b"]]
        ["A" ["A", "S", "A"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]
        ["B" ["b"]]
        ["B1" ["A", "S"]]
        ["B11" ["a"]]
      ]
      expectec_symbol_result_1 "B11"

      [grammar_result_2 symbol_result_2] (ep4.core/GetSymbolToSwap expectec_grammar_result_0 ["A", "S"] )
    ]
    (testing "Testando a função GetSymbolToSwap"
      (is (= grammar_result_0 expectec_grammar_result_0))
      (is (= symbol_result_0 expectec_symbol_result_0))
      (is (= grammar_result_1 expectec_grammar_result_1))
      (is (= symbol_result_1 expectec_symbol_result_1))
      (is (= grammar_result_2 expectec_grammar_result_0))
      (is (= symbol_result_2 expectec_symbol_result_0))
    )
  )
)

(deftest TwoVariablesCorrectionTest
  (let
    [

      grammar [
        ["S0" ["A", "S", "A"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]
        ["A" ["b"]]
        ["A" ["A", "S", "A"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]
        ["B" ["b"]]
      ]
      
      start_symbol "S0"

      end_symbols ["a" "b"]

      result_0 (ep4.core/TwoVariablesCorrection grammar start_symbol end_symbols 0)

      expectec_result_0 [
        ["S0" ["A", "B1"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]

        ["S" ["A", "B1"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]

        ["A" ["b"]]
        ["A" ["A", "B1"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]

        ["B" ["b"]]
        
        ["B1" ["S", "A"] ]
      ]

    ]
    (testing "Testando a função TerminalVariableCorrection"
      (is (= result_0 expectec_result_0))
    )
  )
)

(deftest TerminalVariableCorrectionTest
  (let
    [

      grammar [
        ["S0" ["A", "B1"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]
        ["S" ["A", "B1"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]
        ["A" ["b"]]
        ["A" ["A", "B1"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]
        ["B" ["b"]]        
        ["B1" ["S", "A"] ]
      ]
      
      start_symbol "S0"

      end_symbols ["a" "b"]

      result_0 (ep4.core/TerminalVariableCorrection grammar start_symbol end_symbols 1)

      expectec_result_0 [
        ["S0" ["A", "B1"]]
        ["S0" ["B11", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]
        ["S" ["A", "B1"]]
        ["S" ["B11", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]
        ["A" ["b"]]
        ["A" ["A", "B1"]]
        ["A" ["B11", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]
        ["B" ["b"]]        
        ["B1" ["S", "A"] ]
        ["B11" ["a"] ]
      ]

    ]
    (testing "Testando a função TerminalVariableCorrection"
      (is (= result_0 expectec_result_0))
    )
  )
)

(deftest AddNewVariablesToGrammarTest
  (let
    [

      grammar [ 
        ["S0" ["A", "S", "A"]]
        ["S0" ["a", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]
        ["A" ["b"]]
        ["A" ["A", "S", "A"]]
        ["A" ["a", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]
        ["B" ["b"]]
      ]
      
      start_symbol "S0"

      end_symbols ["a" "b"]

      result_0 (ep4.core/AddNewVariablesToGrammar grammar start_symbol end_symbols)

      expectec_result_0 [
        ["S0" ["A", "B1"]]
        ["S0" ["B11", "B"]]
        ["S0" ["a"]]
        ["S0" ["S", "A"]]
        ["S0" ["A", "S"]]

        ["S" ["A", "B1"]]
        ["S" ["B11", "B"]]
        ["S" ["a"]]
        ["S" ["S", "A"]]
        ["S" ["A", "S"]]

        ["A" ["b"]]
        ["A" ["A", "B1"]]
        ["A" ["B11", "B"]]
        ["A" ["a"]]
        ["A" ["S", "A"]]
        ["A" ["A", "S"]]

        ["B" ["b"]]

        ["B1" ["S" "A"]]

        ["B11" ["a"]]
      ]

    ]
    (testing "Testando a função AddNewVariablesToGrammar"
      (is (= result_0 expectec_result_0))
    )
  )
)

;; --------------------------------------
;; Chomsky Normalization
;; --------------------------------------

(deftest PerformeChomskyNormalizationTest
  (let 
    [
      grammar0 [
        ["S" ["A", "S", "A"]]
        ["S" ["a", "B"]]
        ["A" ["B"]]
        ["A" ["S"]]
        ["B" ["b"]]
        ["B" ["ε"]]
      ]
      start_symbol0 "S"
      end_symbols0 ["a" "b"]
      result0 (ep4.core/PerformeChomskyNormalization grammar0 start_symbol0 end_symbols0)
      expectec_grammar_result0 [
          ["S0" ["A", "B1"]]
          ["S0" ["B11", "B"]]
          ["S0" ["a"]]
          ["S0" ["A", "S"]]
          ["S0" ["S", "A"]]
          ["S" ["A", "B1"]]
          ["S" ["B11", "B"]]
          ["S" ["a"]]
          ["S" ["A", "S"]]
          ["S" ["S", "A"]]
          ["A" ["b"]]
          ["A" ["A", "B1"]]
          ["A" ["B11", "B"]]
          ["A" ["a"]]
          ["A" ["A", "S"]]
          ["A" ["S", "A"]]
          ["B" ["b"]]
          ["B1" ["S" "A"]]
          ["B11" ["a"]]
        ]

      grammar1 [
        ["S" ["B", "a", "d"]]
        ["B" ["b"]]
      ]
      start_symbol1 "S"
      end_symbols1 ["a" "b" "d"]
      result1 (ep4.core/PerformeChomskyNormalization grammar1 start_symbol1 end_symbols1)
      expectec_grammar_result1 [
          ["S" ["B", "B111"]]
          ["B" ["b"]]
          ["B1" ["a"]]
          ["B11" ["d"]]
          ["B111" ["B1", "B11"] ] 
        ]
    ]
    (testing "Testando a função PerformeChomskyNormalization"
      (is (= true (get result0 0)))
      (is (= expectec_grammar_result0 (get result0 1)) )
      (is (= true (get result1 0)))
      (is (= expectec_grammar_result1 (get result1 1)) )
    )
  )
)