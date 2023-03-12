(ns ep3.core-test
  (:require [clojure.test :refer :all]
            [ep3.core :refer :all]
    )
)

(deftest GenDeterministicFiniteAutomatonStatesTest []
  (let [
      states0 ["Q1" "Q2" "Q3"]
      actions0 [["a"] ["a"] ["a"]]
      next_state0 [["Q1"] ["Q2"] ["Q2"]]
      result0 (ep3.core/GenDeterministicFiniteAutomatonStates states0 actions0 next_state0)

      states1 ["Q1" "Q2" "Q3"]
      actions1 [["a" "b" "c"] ["a" "c"] ["a" "d"]]
      next_state1 [["Q1" "Q1" "Q2"] ["Q2" "Q3"] ["Q2" "Q1"]]
      result1 (ep3.core/GenDeterministicFiniteAutomatonStates states1 actions1 next_state1)
    ]
  
    (testing "Testando a função CreateR"
      (is (= result0 {"Q3" {"a" "Q2"}, "Q2" {"a" "Q2"}, "Q1" {"a" "Q1"}} ) )
      (is (= result1 {"Q3" {"a" "Q2" "d" "Q1"}, "Q2" {"a" "Q2", "c" "Q3"}, "Q1" {"a" "Q1", "b" "Q1", "c" "Q2"}}))
    )
  )
)

(deftest GetDeterministicNextStateTest []
  (let [
      matrix {"Q3" {"d" "Q3", "c" "Q3", "b" "Q3", "a" "Q2"}, "Q2" {"c" "Q3", "b" "Q2", "a" "Q2"}, "Q1" {"d" "Q3", "c" "Q2", "b" "Q2", "a" "Q1"}}

      result0 (ep3.core/GetDeterministicNextState matrix "Q3" "a")
      result1 (ep3.core/GetDeterministicNextState matrix "Q3" "b")
      result2 (ep3.core/GetDeterministicNextState matrix "Q1" "a")
    ]
  
    (testing "Testando a função CreateR"
      (is (= result0 "Q2"))
      (is (= result1 "Q3"))
      (is (= result2 "Q1"))
    )
  )
)

(deftest GetResultStateTest []
  (let [
      matrix {"Q3" {"d" "Q3", "c" "Q3", "b" "Q3", "a" "Q2"}, "Q2" {"c" "Q3", "b" "Q2", "a" "Q2"}, "Q1" {"d" "Q3", "c" "Q2", "b" "Q2", "a" "Q1"}}

      input0 ["a" "b" "c"]
      result0 (ep3.core/GetResultState matrix input0 )

      input1 ["a" "b"]
      result1 (ep3.core/GetResultState matrix input1 )

      input2 ["a" "d"]
      result2 (ep3.core/GetResultState matrix input2 )

      input3 ["a" "a" "a"]
      result3 (ep3.core/GetResultState matrix input3 )
    ]
  
    (testing "Testando a função CreateR"
      (is (= result0 "Q3"))
      (is (= result1 "Q2"))
      (is (= result2 "Q3"))
      (is (= result3 "Q1"))
    )
  )
)

(deftest SolveDeterministicFiniteAutomatonTest []
  (let [
      states  ["Q1""Q2""Q3"]

      actions [
                ["a" "b" "c" "d"]
                ["a" "b" "c"]
                ["a" "b" "c" "d"]
              ]

      next_states [
                    ["Q1" "Q2" "Q2" "Q3"]
                    ["Q2" "Q2" "Q3"]
                    ["Q2" "Q3" "Q3" "Q3"]
                  ]

      accept_states ["Q2" "Q3"]

      TestFunction (partial ep3.core/SolveDeterministicFiniteAutomaton states actions next_states accept_states)

      input0 ["a","b","c"]
      result0 (TestFunction input0)

      input1 ["a","a","a"]
      result1 (TestFunction input1)

      input2 ["a","b"]
      result2 (TestFunction input2)
    ]
  
    (testing "Testando a função CreateR"
      (is result0)
      (is (not result1) )
      (is result2)
    )
  )
)