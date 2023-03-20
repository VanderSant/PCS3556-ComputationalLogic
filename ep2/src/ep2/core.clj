;; Definition of namespace -------------
(ns ep2.core
      (:gen-class)
      (:require [clojure.set :as set]))

;; Basic functions --------------------

(defn GetGramRule [rules pos]
      (let [
                  simbol (get (get rules pos) 0)
                  simbol_values (get (get rules pos) 1)
                  result {"simbol" simbol, "value" simbol_values } 
            ]
      result
      )
)

;; Chain recognition functions --------------------

(defn GetApplyRuleInElement [rules elem]
      (let [
                  indexes (range (count rules))
                  GetRules (partial GetGramRule rules) 
                  GetNewValue (fn [rule] (if (= (rule "simbol") elem ) (rule "value") [] ) )
                  GetNewValueWithRule (fn [index] (GetNewValue (GetRules index) ) ) 
                  GetAllNewValues (remove empty? (vec (map GetNewValueWithRule indexes ) ) )
            ]
      GetAllNewValues
      )
)

(defn GetApplyRulesInChain 
      ([rules, chain] (GetApplyRulesInChain rules, chain, 0, []))
      ([rules, chain, index] (GetApplyRulesInChain rules, chain, index, []))
      ([rules, chain, index, chain_applied]

            (if (= index (count chain))
                  chain_applied
                  (let  [
                              curr_elem (chain index)
                              
                              tranf_curr_elem (GetApplyRuleInElement rules curr_elem)

                              new_transf (vec 
                                          (map 
                                                #(vec (concat (subvec chain 0 index) % (subvec chain (inc index))) )
                                                tranf_curr_elem
                                          )
                                    )
                              new_chain_applied (vec (concat chain_applied new_transf))
                        ]
                  (GetApplyRulesInChain rules chain (+ index 1) new_chain_applied)
                  )
            )
      )
)

(defn GenerateAllPossibleChains
      ([rules, max_size] (GenerateAllPossibleChains rules, max_size, 0, [["S"]]))
      ([rules, max_size, index, generated_chains]
            (if (= index (count generated_chains)) 
                  generated_chains
                  (let [
                              chain_applied_changes
                                    (GetApplyRulesInChain
                                          rules     
                                          (get generated_chains index)
                                    )
                              
                              new_generated_chain (vec
                                          (filter 
                                                #(not (contains? (set generated_chains) %))
                                                (filter 
                                                      #(<= (count %) max_size)
                                                      chain_applied_changes
                                                )
                                          )
                                    )     
                              
                              new_possible_chain (vec (concat generated_chains new_generated_chain))
                        ]      
                  (GenerateAllPossibleChains rules, max_size, (+ index 1), new_possible_chain)
                  )
            )
      )
)

(defn CheckIfChainIsAcceped [rules chain]
      (let [
                  max_size (count chain)
                  all_possibilities (GenerateAllPossibleChains rules max_size)
                  result (contains? (set all_possibilities) chain)
            ]
      result
      )
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
            ["b", "a"]
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