Ao longos desses exercícios programas, fomos aprendendo ao poucos a programar melhor de maneira funcional. Aqui, vou comentar de algumas funções feitas em primeira vistas erradas(maneira tradicional) e depois corrigidas para a maneira funcional de fato.

- Maneira funcional:
```clj
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
```

- Maneira Tradicional:
```clj
(defn TranformMatrixInBinaryMatrix [n matrix]
  (do
    (def result (vec (repeat n (vec (repeat n 0)))) )
    (def i 0)
    (def j 0)
    (while (< i n)
      (do
        (while (< j n)
          (do
            (if (> (get-in matrix [i j]) 0)
              (do
                (def result (assoc-in result [i j] 1))
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
```

Fica nítido que na maneira funcional o código fica muito mais limpo e legivel!

Pode conferir outro exemplo:

- Maneira funcional:
```clj
(defn MatrixOr [n matrix_a matrix_b]
  (let [
    TwoBitOr #(if (or (= 1 %1) (= 1 %2)) 1 0)
    TwoRowOr (fn [row1 row2] (vec (map TwoBitOr row1 row2 )) )
    MatrixOr (fn [mat1 mat2] (vec (map TwoRowOr mat1 mat2 )) )
    result (MatrixOr matrix_a matrix_b)
  ]
    result
  )
)
```

- Maneira Tradicional:
```clj
(defn MatrixOr [n matrix-a matrix-b]
    (do
      (def result (vec (repeat n (vec (repeat n 0)))) )
      (def i 0)
      (def j 0)
      (def matrix2 '[[0 0 1] [0 0 1] [0 0 1]])
      (while (< i n)
        (do
          (while (< j n)
            (do
              (if (or (= 1 (get-in matrix-a [i j])) (= 1 (get-in matrix-b [i j])))
                (do
                  (def result (assoc-in result [i j] 1))
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
```