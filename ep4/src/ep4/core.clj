;; Definition of namespace -------------
(ns ep4.core
  (:gen-class)
  (:require [clojure.set :as set]))

;; Finite Automaton Functions --------------------


;; Main functions --------------------


;; Main function ----------------------

(defn -main []
  (MainDeterministic)
  (MainNonDeterministic))