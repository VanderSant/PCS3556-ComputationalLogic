;; Definition of namespace -------------
(ns ep4.core
  (:gen-class)






  (:require [clojure.set :as set]
            [ep2.core :as ep2]))

;; Finite Automaton Functions --------------------


;; Main functions --------------------


;; Main function ----------------------

(defn -main []
  (def gram_rules [["S" ["a", "A", "S"]]
                   ["S" ["a"]]
                   ["S" ["S", "S"]]
                   ["A" ["b", "a"]]
                   ["A" ["S", "S"]]])

  (println "Regras gramaticais: " gram_rules)

  (def chain_to_be_recognized
    ["b", "a"])
  (println "Cadeia para ser reconhecida (w): " chain_to_be_recognized)

  (def chain_is_acceped
    (ep2/CheckIfChainIsAcceped gram_rules chain_to_be_recognized))
)