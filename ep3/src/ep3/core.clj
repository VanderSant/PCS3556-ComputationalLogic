;; Definition of namespace -------------
(ns ep3.core
      (:gen-class)
      (:require [clojure.set :as set]))

;; Generical functions --------------------
(defn GetNextState [matrix state action]
      (let 
            [
                  next_state (get (get matrix state) action)
            ]
            next_state
      )
)

;; Deterministic functions --------------------
(defn GenDeterministicTransistions [states actions next_states]
      (let
            [
                  actions->next_state (vec (map zipmap actions next_states) )
                  state->action (zipmap states actions->next_state)
            ]

            state->action
      )
)

(defn GetDeterministicResultState 
      ([matrix input] (GetDeterministicResultState matrix input "Q1"))
      ([matrix input actual_state]

            (let
                  [
                        new_action (first input)
                        next_state (GetNextState matrix actual_state new_action)
                        new_input (rest input)
                  ]
                  (if (not (empty? new_input))
                        (GetDeterministicResultState matrix new_input next_state)
                        next_state
                  )
            )
      )

)

(defn SolveDeterministicFiniteAutomaton [states actions next_states accept_states input]
      (let
            [
                  deterministic_transistions (GenDeterministicTransistions states actions next_states)
                  final_state (GetDeterministicResultState deterministic_transistions input)
                  result (contains? (set accept_states) final_state)
            ]
            result 
      )
)

(defn MainDeterministic []
      (let
            [
                  states  ["Q1" "Q2" "Q3"]

                  actions [
                        ["a" "b" "c" "d"]
                        ["a" "b" "c" "d"]
                        ["a" "b" "c" "d"]
                  ]

                  next_states [
                        ["Q1" "Q2" "Q2" "Q3"]
                        ["Q2" "Q2" "Q3" "Q1"]
                        ["Q2" "Q3" "Q3" "Q3"]
                  ]

                  accept_states ["Q2 ""Q3"]

                  input ["a" "b" "c"]

                  result (SolveDeterministicFiniteAutomaton states actions next_states accept_states input)
            ]
            (do   
                  (println "--- Deterministic Finite Automaton ---")
                  (println "states" states)
                  (println "actions" actions)
                  (println "next states" next_states)
                  (println "accept states" accept_states)
                  (println "input states" input)
                  (if result
                        (println "Automata aceita! :)")
                        (println "Automata Não foi aceita :(")
                  )

            )
            result
      )
)

;; Non Deterministic functions --------------------

(defn GenNonDeterministicTransistions [states actions next_states]
      (let
            [     
                  GroupByKey (fn [chaves valores]
                        (->>  (map vector chaves valores)
                              (group-by first)
                              (map (fn [[k vs]] [k (vec (map second vs))]))
                              (into {})
                        )
                  )
                  actions->next_state (vec (map GroupByKey actions next_states) )
                  state->action (zipmap states actions->next_state)
            ]
            state->action
      )
)

(defn GetNonDeterministicResultState 
      ([matrix input] (GetNonDeterministicResultState matrix input "Q1"))
      ([matrix input actual_state]

            (let
                  [
                        new_action (first input)
                        next_states (GetNextState matrix actual_state new_action)
                        new_input (rest input)
                  ]
                  (if (not (empty? new_input))
                        (vec (distinct (flatten (map (partial GetNonDeterministicResultState matrix new_input) next_states) )))
                        next_states
                  )
            )
      )

)

(defn SolveNonDeterministicFiniteAutomaton [states actions next_states accept_states input]
      (let
            [
                  deterministic_transistions (GenNonDeterministicTransistions states actions next_states)
                  final_states (GetNonDeterministicResultState deterministic_transistions input)
            
                  result (some #(contains? (set accept_states) %) final_states)
            ]
            result 
      )
)

(defn MainNonDeterministic []
      (let
            [
                  states  ["Q1" "Q2" "Q3"]

                  actions [
                        ["a" "a" "c" "d"]
                        ["a" "b" "c" "c"]
                        ["a" "b" "c" "d"]
                  ]

                  next_states [
                        ["Q1" "Q2" "Q2" "Q3"]
                        ["Q2" "Q2" "Q3" "Q2"]
                        ["Q2" "Q3" "Q3" "Q3"]
                  ]

                  accept_states ["Q2""Q3"]

                  input ["a","b","c"]
                  
                  result (SolveNonDeterministicFiniteAutomaton states actions next_states accept_states input)
            ]
            (do   
                  (println "--- Non Deterministic Finite Automaton ---")
                  (println "states" states)
                  (println "actions" actions)
                  (println "next states" next_states)
                  (println "accept states" accept_states)
                  (println "input states" input)
                  (if result
                        (println "Automata aceita! :)")
                        (println "Automata Não foi aceita :(")
                  )

            )
            result
      )
)

;; Main function ----------------------

(defn -main []
      (MainDeterministic)
      (MainNonDeterministic)
)