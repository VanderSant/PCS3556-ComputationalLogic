;; Defintion of namespace ---------------------------------------------------------------------
(ns ep1.core
      (:gen-class)
      (:require [clojure.set :as set]))

;; Basix functions ----------------------------------------------------------------------------
(defn CreateR [i_array j_array]
  (let [ result (map vector i_array j_array) ]
  result)
)

(defn CreateReflectiveR[R PossibleReflectiveValues]
  (let [return (remove #(contains? (set R) %) PossibleReflectiveValues)]
  return
  )
)

;; Transitive function without recursion -----------------------------------------------------
(defn Transitive [R]
  (reduce (fn [result x]
            (let [transitive (filter #(= (second %) x) R)]
              (if (empty? transitive)
                result
                (conj result (first (first transitive)))))) R R)
)

;; Transitive function using recursion -------------------------------------------------------
(defn acessiveis [x R]
  (map second (filter #(= x (first %)) R)))

(defn transitivo-rec [x R visitados]
  (if (contains? visitados x)
    []
    (let [proximos (set (mapcat #(transitivo-rec % R (conj visitados x)) (acessiveis x R)))]
      (conj proximos x))))

(defn TransitiveRecursive [R]
  (apply clojure.set/union (map #(transitivo-rec % R []) R)))

;; Global variables --------------------------------------------------------------------------
(def A [1 2 3 ])

(def R
  (CreateR [1, 2, 3] 
            [2, 3, 3])
)

(def PossibleReflectiveValues
  (CreateR A A) 
)

(def ReflectiveR
  ( CreateReflectiveR R PossibleReflectiveValues )
)

(def TransitiveR
  ( Transitive R )
)

(def TransitiveRecursiveR
  ( TransitiveRecursive R )
)

;; Main function -----------------------------------------------------------------------------
(defn -main []
  (println "A:" A)
  (println "R:" R)
  (println "Possible Reflective Values: " PossibleReflectiveValues)
  (println "Reflective(R):" ReflectiveR)
  (println "Transitive(R) - Without Recursive:" TransitiveR)
  (println "Transitive(R) - With    Recursive:" TransitiveRecursiveR)
)

