;; Defintion of namespace ---------------------------------------------------------------------
(ns ep1.core
      (:gen-class)
      (:require [clojure.set :as set]))

;; Basix functions ----------------------------------------------------------------------------
(defn CreateR [i_array j_array]
  (let [ result (map vector i_array j_array) ]
  (vec result))
)

(defn CreateReflectiveR[R PossibleReflectiveValues]
  (def DataRemoveCondition (fn [x] (contains? (set R) x)) )
  (let [return (remove DataRemoveCondition PossibleReflectiveValues)]
  (clojure.set/union R return)
  )
)

;; Transitive function using recursion -------------------------------------------------------

(defn CreateBinaryMatrix [n coords]
  (let [matrix (vec (repeat n (vec (repeat n 0))))] ; cria matriz vazia
    (do 
      (def result matrix)
      (doseq [[x y] coords]
        (do
          (def result (assoc-in result [(- x 1) (- y 1)] 1))
        )
      )
    )
    result
  )
)

(defn MatrixMult
  [mat1 mat2]

  (let [ 
      Transpose #(apply map vector %)
      DotProduct #(reduce + (map * %1 %2)) ;; product all values and sum (produto escalar)
      RowMult (fn [mat row] (
                  map (partial DotProduct row) (Transpose mat)
                )
              )

      new_matrix (map (partial RowMult mat2) mat1 )
      result  (vec (map vec new_matrix))
      ]
    result
  )
)

(defn TranformMatrixInBinaryMatrix [matrix]
  (let [
    ChangeOneValueToBinary #(if (> % 0) 1 0)
    ChangeOneRowToBinary #(vec (map ChangeOneValueToBinary %))
    ChangeMatrixToBinary #(vec (map ChangeOneRowToBinary %))
    result (ChangeMatrixToBinary matrix)
  ]
    result
  )
)

(defn GetBinatyMatrixIndex[n matrix]
    (do
      (def result [])
      (def i 0)
      (def j 0)
      (while (< i n)
        (do
          (while (< j n)
            (do
              (if (= 1 (get-in matrix [i j]))
                (do
                  (def result (conj result [(+ i 1) (+ j 1)]))
                )
              )
              (def j (+ j 1))
            )
          )
          (def j 0)
          (def i (+ i 1))
        )
      )
  )
    result
)

(defn MatrixOr [matrix_a matrix_b]
  (let [
    TwoBitOr #(if (or (= 1 %1) (= 1 %2)) 1 0)
    TwoRowOr (fn [row1 row2] (vec (map TwoBitOr row1 row2 )) )
    GetMatrixOr (fn [mat1 mat2] (vec (map TwoRowOr mat1 mat2 )) )
    result (GetMatrixOr matrix_a matrix_b)
  ]
    result
  )
)

(defn GetTransitiveR [n R Rn]
  (def Rn_plus1 (TranformMatrixInBinaryMatrix (MatrixMult R Rn)) )
  (if (= Rn_plus1 Rn)
    Rn
    (MatrixOr Rn (GetTransitiveR n R Rn_plus1) )
  )
)

(defn Transitive [n R]
  (def R_binary (CreateBinaryMatrix n R))
  (def R_final_binary (GetTransitiveR n R_binary R_binary) )
  (def R_final (GetBinatyMatrixIndex n R_final_binary) )
  R_final
)

;; Main function -----------------------------------------------------------------------------
(defn -main []

  (def A [1 2 3])
  (println "A:" A)

  (def n (count A))

  (def R
    (CreateR [1 1 2 3] 
              [1 2 3 1])
  )
  (println "R:" R)

  (def PossibleReflectiveValues
    (CreateR A A) 
  )

  (def ReflectiveR
    ( CreateReflectiveR R PossibleReflectiveValues )
  )
  (println "Fecho Reflexivo  de R | Reflective(R):" ReflectiveR)

  (def TransitiveR
    ( Transitive n R)
  )
  (println "Fecho Transitivo de R | Transitive(R):" TransitiveR)
)

