;; Definition of namespace -------------
(ns ep3.core
      (:gen-class)
      (:require [clojure.set :as set]))

;; Deterministic functions --------------------
(defn GenDeterministicFiniteAutomatonStates [states actions next_states]
      (let
            [
                  actions->next_state (vec (map zipmap actions next_states) )
                  state->action (zipmap states actions->next_state)
            ]

            state->action
      )
)

(defn GetDeterministicNextState [matrix state action]
      (let 
            [
                  next_state (get (get matrix state) action)
            ]
            next_state
      )
)

(defn GetResultState 
      ([matrix input] (GetResultState matrix input "Q1"))
      ([matrix input actual_state]

            (let
                  [
                        new_action (first input)
                        next_state (GetDeterministicNextState matrix actual_state new_action)
                        new_input (rest input)
                  ]
                  (if (not (empty? new_input))
                        (GetResultState matrix new_input next_state)
                        next_state
                  )
            )
      )

)

(defn SolveDeterministicFiniteAutomaton [states actions next_states accept_states input]
      (let
            [
                  states_deterministic_matrix (GenDeterministicFiniteAutomatonStates states actions next_states)
                  final_state (GetResultState states_deterministic_matrix input)
                  result (contains? (set accept_states) final_state)
            ]
            result 
      )
)

;; Main function ----------------------
(defn -main []
      (def states  ["Q1""Q2""Q3"])

      (def actions [
                  ["a" "b" "c" "d"]
                  ["a" "b" "c"]
                  ["a" "b" "c" "d"]
            ]
      )

      (def next_states 
            [
                  ["Q1" "Q2" "Q2" "Q3"]
                  ["Q2" "Q2" "Q3"]
                  ["Q2" "Q3" "Q3" "Q3"]
            ]
      )

      (def accept_states ["Q2""Q3"])

      (def input ["a","b","c"] )

      (println (SolveDeterministicFiniteAutomaton states actions next_states accept_states input) )

)