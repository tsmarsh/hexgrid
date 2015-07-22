(ns hexgrid.core-test
  (:require [clojure.test :refer :all]
            [hexgrid.core :refer :all]
            [schema.test :as st]))

(use-fixtures :once st/validate-schemas)

(deftest arithmatic
  (testing "addition"
    (is (= [0 0] (add [0 0] [0 0])))
    (is (= [3 -3] (add [-1 -1] [2 -3] [2 1]))))

  (testing "subtraction"
    (is (= [0 0] (sub [0 0] [0 0])))
    (is (= [-5 1] (sub [-1 -1] [2 -3] [2 1]))))

  (testing "division"
    (is (= [2 2] (div [4 4] [2 2])))
    (is (= [-1/4 1/3] (div [-1 -1] [2 -3] [2 1]))))

  (testing "multiplication"
    (is (= [0 0] (multiply [0 0] [0 0])))
    (is (= [-4 3] (multiply [-1 -1] [2 -3] [2 1])))))

(deftest distance-finding
  (testing "can find the distance between two points"
    (is (= 3 (distance [0 0] [-3 0])))
    (is (= 2 (distance [0 0] [1, -2])))))

(deftest conversion
  (testing "can convert from axial to cube"
    (is (= [0 0 0] (axial-to-cube [0 0])))
    (is (= [1 2 -3] (axial-to-cube [1 -3])))
    (is (= [1 -3 2] (axial-to-cube [1 2]))))

  (testing "can convert from cube to axial"
    (is (= [0 0] (cube-to-axial [0 0 0])))
    (is (= [1 -3] (cube-to-axial [1 2 -3])))
    (is (= [1 2] (cube-to-axial [1 -3 2])))))

(deftest lines
  (testing "can build a path between two points"
    (is (= [[0 0]] (line [0 0] [0 0])))
    (is (= [[0 0] [0 1]] (line [0 0] [0 1])))
    (is (= [[-2 3] [-1 2] [0 1] [1 1] [2 0] [3 -1]]
           (line [-2 3] [3 -1])))))

(deftest area-of-effect
  (testing "can find cells surrounding a cell"
    (is (= [[-1 1] [-1 0] [0 1] [0 0] [0 -1] [1 0] [1 -1]]
           (axial-range [0 0] 1))))

  (testing "can find overlap of two areas of affect"
    (is (= [[-1 0 1] [-1 1 0] [0 -1 1] [0 0 0]]
           (cube-overlap [[0 0 0] 1] [[-1 0 1] 1])))
    (is (= [[-1 1] [-1 0] [0 1] [0 0]]
           (axial-overlap [[0 0] 1] [[-1 1] 1])))))
