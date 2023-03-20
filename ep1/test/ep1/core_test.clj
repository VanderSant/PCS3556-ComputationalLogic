(ns ep1.core-test
  (:require [clojure.test :refer :all]
            [ep1.core :refer :all]))

(deftest CreateRTest []
  (let [result (ep1.core/CreateR [1 1 2 3] [1 2 3 1])]

    (testing "Testando a função CreateR"
      (is (= result  [[1 1] [1 2] [2 3] [3 1]])))))

(deftest CreateBinaryMatrixTest []
  (let [result (ep1.core/CreateBinaryMatrix 3 [[1 1] [1 2] [2 3] [3 1]])]

    (testing "Testando a função CreateBinaryMatrix"
      (is (= result  [[1 1 0],[0 0 1],[1 0 0]])))))

(deftest MatrixMultTest []
  (let [result0 (ep1.core/MatrixMult
                 [[1 1 0],[0 0 1],[1 0 0]]
                 [[1 1 0],[0 0 1],[1 0 0]])

        result1 (ep1.core/MatrixMult
                 [[1 1 0],[0 2 1],[1 0 3]]
                 [[1 2 0],[0 1 1],[1 1 1]])]

    (testing "Testando a função MatrixMult"
      (is (= result0  [[1 1 1],[1 0 0],[1 1 0]]))
      (is (= result1  [[1 3 1],[1 3 3],[4 5 3]])))))

(deftest TranformMatrixInBinaryMatrixTest []
  (let [result0 (ep1.core/TranformMatrixInBinaryMatrix
                 [[1 1 0],[0 0 1],[1 0 0]])

        result1 (ep1.core/TranformMatrixInBinaryMatrix
                 [[1 1 0],[0 2 1],[1 0 3]])]

    (testing "Testando a função TranformMatrixInBinaryMatrix"
      (is (= result0  [[1 1 0],[0 0 1],[1 0 0]]))
      (is (= result1  [[1 1 0],[0 1 1],[1 0 1]])))))

(deftest GetBinatyMatrixIndexTest []
  (let [result0 (ep1.core/GetBinatyMatrixIndex
                 3 [[1 1 0],[0 0 1],[1 0 0]])

        result1 (ep1.core/GetBinatyMatrixIndex
                 3 [[1 1 0],[0 1 1],[1 0 1]])]

    (testing "Testando a função GetBinatyMatrixIndex"
      (is (= result0  [[1 1] [1 2] [2 3] [3 1]]))
      (is (= result1  [[1 1] [1 2] [2 2] [2 3] [3 1] [3 3]])))))

(deftest MatrixOrTest []
  (let [result0 (ep1.core/MatrixOr
                 [[1 1 0],[0 0 1],[1 0 0]] [[1 1 1],[1 0 1],[1 1 0]])]

    (testing "Testando a função MatrixOr"
      (is (= result0  [[1 1 1] [1 0 1] [1 1 0]])))))

(deftest GetTransitiveRTest []
  (let [result0 (ep1.core/GetTransitiveR
                 3 [[1 1 0],[0 0 1],[1 0 0]] [[1 1 0],[0 0 1],[1 0 0]])

        result1 (ep1.core/GetTransitiveR
                 3 [[1 0 0],[0 0 1],[1 0 0]] [[1 0 0],[0 0 1],[1 0 0]])]

    (testing "Testando a função GetTransitiveR"
      (is (= result0  [[1 1 1] [1 1 1] [1 1 1]]))
      (is (= result1  [[1 0 0] [1 0 1] [1 0 0]])))))

(deftest TransitiveTest []
  (let [result0 (ep1.core/Transitive
                 3 [[1 1] [1 2] [2 3] [3 1]])

        result1 (ep1.core/Transitive
                 3 [[1,2] [2,3] [3,3]])]

    (testing "Testando a função Transitive"
      (is (= result0  [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2] [3 3]]))
      (is (= result1  [[1 2] [1 3] [2 3] [3 3]])))))