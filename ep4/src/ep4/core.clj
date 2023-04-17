;; Definition of namespace -------------
(ns ep4.core
  (:gen-class)
  (:require [clojure.set :as set]
            [ep2.core :as ep2]))

;; Finite Automaton Functions --------------------

(defn GetAllPossibleSymbols [grammar]
  (let [
    GetSymbol #(nth % 0)
    get_all_symbols (vec (map GetSymbol grammar))
    all_symbols_without_duplicate (distinct get_all_symbols)
  ]
    all_symbols_without_duplicate
  )
)

(defn IsRuleWithTwoVariable
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    symbol (rule "simbol")
    values (rule "value")

    all_symbols (GetAllPossibleSymbols grammar)
    number_of_symbols_in_values (count (filter #(contains? (set all_symbols) %) values))
    is_the_right_number_of_variable (= number_of_symbols_in_values 2)
  ]
    is_the_right_number_of_variable
  )
)

(defn IsRuleWithOneTerminal
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    symbol (rule "simbol")
    values (rule "value")

    number_of_termminals_in_values (count (filter #(contains? (set end_symbols) %) values))
    is_the_right_number_of_terminal (= number_of_termminals_in_values 1)
  ]
    is_the_right_number_of_terminal
  )
)

(defn IsThatGrammerInChomskyNormalForm 
  [grammar start_symbol end_symbols]

)


;; Main function ----------------------

(defn -main []
  (def grammar [["S" ["A", "S", "A"]]
                   ["S" ["a", "B"]]
                   ["A" ["B"]]
                   ["A" ["S"]]
                   ["B" ["b", "ε"]]])
  
  (def start_symbol "S")
  (def end_symbols ["a" "b"])

  (println "Regras gramaticais: " grammar)
  (println "Simbolos iniciais: " start_symbol)
  (println "Simbolos finais: " end_symbols)

  (def chain_to_be_recognized
    ["b", "a"])
  (println "Cadeia para ser reconhecida (w): " chain_to_be_recognized)

  (def chain_is_acceped
    (ep2/CheckIfChainIsAcceped grammar chain_to_be_recognized))
)