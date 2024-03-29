;; Definition of namespace -------------
(ns ep3.core
  (:gen-class)
  (:require [clojure.set :as set]))

;; Finite Automaton Functions --------------------

(defn GetNextState [matrix state action]
  (let
   [next_state (get (get matrix state) action)]
    next_state))

(defn GenTransistions [states actions next_states]
  (let
   [GroupByKey (fn [chaves valores]
                 (->>  (map vector chaves valores)
                       (group-by first)
                       (map (fn [[k vs]] [k (vec (map second vs))]))
                       (into {})))

    actions->next_state (vec (map GroupByKey actions next_states))
    state->action (zipmap states actions->next_state)]
    state->action))

(defn GetResultState
  ([matrix input] (GetResultState matrix input "Q1"))
  ([matrix input current_state]

   (let
    [new_action (first input)
     next_states (GetNextState matrix current_state new_action)
     new_input (rest input)]
     (if (not (empty? new_input))
       (vec (distinct (flatten (map (partial GetResultState matrix new_input) next_states))))
       next_states))))

(defn SolveFiniteAutomaton [states actions next_states accept_states input]
  (let
   [deterministic_transistions (GenTransistions states actions next_states)
    final_states (GetResultState deterministic_transistions input)

    result (some #(contains? (set accept_states) %) final_states)]
    [result deterministic_transistions final_states]))

;; Main functions --------------------

(defn MainDeterministic []
  (let
   [states  ["Q1" "Q2" "Q3"]

    actions [["a" "b" "c" "d"]
             ["a" "b" "c" "d"]
             ["a" "b" "c" "d"]]

    next_states [["Q1" "Q2" "Q2" "Q3"]
                 ["Q2" "Q2" "Q1" "Q1"]
                 ["Q2" "Q3" "Q3" "Q3"]]

    accept_states ["Q2" "Q3"]

    input ["a" "b" "c"]

    [result deterministic_transistions final_states] (SolveFiniteAutomaton states actions next_states accept_states input)]
    (do
      (println "--- Deterministic Finite Automaton ---")
      (println "States = " states)
      (println "Actions = " actions)
      (println "Next states = " next_states)
      (println "Accept states = " accept_states)
      (println "Input states = " input)
      (println "Transitions = " (reverse deterministic_transistions))
      (println "Final States = " final_states)
      (if result
        (println "Automata aceita! :)")
        (println "Automata Não foi aceita :(")))

    result))

(defn MainNonDeterministic []
  (let
   [states  ["Q1" "Q2" "Q3"]

    actions [["a" "a" "c" "d"]
             ["a" "b" "c" "c"]
             ["a" "b" "c" "d"]]

    next_states [["Q1" "Q2" "Q2" "Q3"]
                 ["Q2" "Q2" "Q3" "Q2"]
                 ["Q2" "Q3" "Q3" "Q3"]]

    accept_states ["Q3"]

    input ["a","a","a"]

    [result deterministic_transistions final_states] (SolveFiniteAutomaton states actions next_states accept_states input)]
    (do
      (println "--- Non Deterministic Finite Automaton ---")
      (println "States = " states)
      (println "Actions = " actions)
      (println "Next states = " next_states)
      (println "Accept states = " accept_states)
      (println "Input states = " input)
      (println "Transitions = " (reverse deterministic_transistions))
      (println "Final States = " final_states)
      (if result
        (println "Automata aceita! :)")
        (println "Automata Não foi aceita :(")))

    result))

;; Main function ----------------------

(defn -main []
  (MainDeterministic)
  (MainNonDeterministic))