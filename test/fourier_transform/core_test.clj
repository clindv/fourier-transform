(ns fourier-transform.core-test
  (:require [clojure.test :refer :all]
            [fourier-transform.core :refer :all]))
(deftest tranform
  (testing "dft"
    (is (= (apply + (dft [1 2 3 4])) 17.65685424949238))))
