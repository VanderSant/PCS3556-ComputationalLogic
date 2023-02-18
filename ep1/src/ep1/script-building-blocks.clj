(ns my-namespace
  (:require [clojure.set :as set]))

(def A #{\a \b \c \d \e \f \g \h \i \j})

(println A)

(doseq [element A]
  (println element))

(def R #{[\a \b] [\a \c] [\b \d]})

(println R)

(defn self-pairs [elements]
  (into #{} (for [x elements] [x x])))

(println (self-pairs A))

(println (set/union (self-pairs A) (self-pairs A))) 


(defn transitive-closure
  "Calculates the transitive clojure of a relation given as a set of vectors, 
   e.g. (trans-closure #{[1 2] [2 3]}) yields #{[1 2] [2 3] [1 3]}"
  [e]
  (letfn [(f [x] (for [[a b] x [c d] x :when (= b c)] [a d]))]
    (let [e2 (set (f e))]
      (if (clojure.set/subset? e2 e)
        e
        (recur (clojure.set/union e e2))))))

(defn print-transitive-closure [R]
  (let [new-R (transitive-closure R)]
    (println "Original set R:" R)
    (println "Transitive closure:" new-R)))

(print-transitive-closure #{[\a \b] [\a \c] [\b \d]})


(println (transitive-closure R))

(defn transitive-closure-recur
  "Calculates the transitive closure of a relation given as a set of vectors, 
   e.g. (transitive-closure #{[1 2] [2 3]}) yields #{[1 2] [2 3] [1 3]}"
  [e]
  (letfn [(f [x] (for [[a b] x [c d] x :when (= b c)] [a d]))]
    (let [e2 (set (f e))]
      (if (clojure.set/subset? e2 e)
        e
        (transitive-closure-recur (clojure.set/union e e2))))))

(println (transitive-closure-recur R))