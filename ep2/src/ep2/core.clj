;; Defintion of namespace -------------
(ns ep2.core
      (:gen-class)
      (:require [clojure.set :as set]))

;; Basix functions --------------------

(defn GetGramRule [rules pos]
      (def simbol 
            (get (get rules pos) 0)
      )

      (let 
            [simbol (get (get rules pos) 0)
             simbol_values (get (get rules pos) 1)
             result {"simbol" simbol, "value" simbol_values } ]

            result
      )
)

;; Chair recognition functions --------------------

(defn GetApplyRuleInElement [rules elem]
      (do
            (def values [])
            (def indexes (range (count rules)))
            (doseq [index indexes]
                  (do
                        (def rule (GetGramRule rules index))
                        (if (= (rule "simbol") elem)
                              (def values (clojure.set/union values (vector (rule "value")) )) 
                        )
                  )
            )
      )
      values
)

(defn GetApplyRulesInChair 
      ([rules, chain] (GetApplyRulesInChair rules, chain, 0, []))
      ([rules, chain, index] (GetApplyRulesInChair rules, chain, index, []))
      ([rules, chain, index, chair_applied]

            (if (= index (count chain))
                  chair_applied
                  (do
                        (def curr_elem (chain index))
                        
                        (def tranf_curr_elem (GetApplyRuleInElement rules curr_elem) )

                        (def new_transf
                              (vec 
                                    (map 
                                          #(vec (concat (subvec chain 0 index) % (subvec chain (inc index))) )
                                          tranf_curr_elem
                                    )
                              )
                        )
                        (def new_chair_applied (vec (concat chair_applied new_transf)))

                        (GetApplyRulesInChair rules chain (+ index 1) new_chair_applied)
                  )
            )
      )
)

(defn GenerateAllPossibleChains
      ([rules, max_size] (GenerateAllPossibleChains rules, max_size, 0, [["S"]]))
      ([rules, max_size, index, generated_chains]
            (if (= index (count generated_chains)) 
                  generated_chains
                  (do
                        (def chain_applied_changes
                              (GetApplyRulesInChair
                                    rules     
                                    (get generated_chains index)
                              )
                        )
                        (def new_generated_chain
                              (vec
                                    (filter 
                                          #(not (contains? (set generated_chains) %))
                                          (filter 
                                                #(<= (count %) max_size)
                                                chain_applied_changes
                                          )
                                    )
                              )     
                        )
                        (def new_possible_chair (vec (concat generated_chains new_generated_chain)))
                        (GenerateAllPossibleChains rules, max_size, (+ index 1), new_possible_chair)
                  )
            )
      )
)

(defn CheckIfChainIsAcceped [rules chain]
      (let [max_size (count chain)]
            (println "max_size (l): " max_size)
            (def all_possibilities (GenerateAllPossibleChains rules max_size))
            (def result (contains? (set all_possibilities) chain) )
      )
      result
)

;; Test function ----------------------
(defn TestGetApplyRuleInElement[rules]
      (def elem "A")
      (println (GetApplyRuleInElement rules elem))
)

(defn TestGetApplyRulesInChair [rules chain]
      (println (GetApplyRulesInChair rules chain))
)

(defn TestGenerateAllPossibleChains [rules chain]
      (println (GenerateAllPossibleChains rules 3))
)

;; Main function ----------------------
(defn -main []
      (def gram_rules [
                  [ "S" ["a", "A", "S"]  ]
                  [ "S" ["a"]            ]
                  [ "S" ["S", "S"]       ] 
                  [ "A" ["b", "a"]       ]
                  [ "A" ["S", "S"]       ]
            ]
      )
      (println "Regras gramaticais: " gram_rules)

      (def chain_to_be_recognized
            ["a", "a","a","a"]
      )
      (println "Cadeia para ser reconhecida (w): " chain_to_be_recognized)

      (def chain_is_acceped
            (CheckIfChainIsAcceped gram_rules chain_to_be_recognized)
      )
      
      (if chain_is_acceped
            (println "Cadeia (w) foi Aceita :)")
            (println "Cadeia (w) foi Rejeitada :(")
      )
)