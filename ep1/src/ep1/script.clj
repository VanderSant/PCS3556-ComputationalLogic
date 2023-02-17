(defn criar-R
  [i j]
  {:i i :j j})


(defn criar-reflexivo
  [elemento]
  {:i elemento
   :j elemento})


(defn get-valores-i
  [R]
  (map :i R))


(defn get-valores-j
  [R]
  (map :j R))


(defn existe-elemento?
  [elemento R]
  (and (= elemento (:i R)) (not (= "a" (:j R)))))


(defn get-elemento
  [elemento R]
  (filter #(existe-elemento? elemento %) R))
(def A ["a" "b" "c" "d"])


(def R
  [(criar-R "a" "a")
   (criar-R "a" "b")
   (criar-R "b" "c")
   (criar-R "b" "d")])


(def R-reflexivo [(map criar-reflexivo A)])


(defn get-next-elemento
  [elemento]
  (println "next do elemento " elemento ":" (nth A (inc (.indexOf A elemento))))
  (nth A (inc (.indexOf A elemento))))


(defn next-layer
  [elemento R-completo R]
  (if (not (= (get-elemento elemento R) elemento))
    (let [next-elemento (get-next-elemento elemento)
          R-aux (merge + (criar-R elemento next-elemento) R-completo)]
      (recur next-elemento R-aux R))
    (println R-completo)))


(defn get-R
  [A R R-completo]
  (map #(next-layer % R-completo R) (distinct A)))


(defn main
  []
  (println "A:" A)
  (println "R: " R)
  (println "reflexivos:" R-reflexivo)


  (println "inicio da analise dos transitivos:")
  (get-R A R []))


(main)