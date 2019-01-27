(ns fourier-transform.core-test
  (:require [clojure.test :refer :all]
            [fourier-transform.core :refer :all]))
(deftest tranform
  (testing "dft"
    (is (= (apply + (dft [1 2 3 4])) 17.65685424949238)))
  (testing "dft-lazy"
    (is (= (apply + (dft-lazy [1 2 3 4])) 17.65685424949238))
    (pr "time of dft 512: ")
    (time (nth (dft (range 512)) 500))
    (pr "time of dft-lazy 512: ")
    (time (nth (dft-lazy (range 512)) 500))
    (pr "time of dft-lazy-memoize 512: ")
    (time (nth (dft-lazy-memoize (range 512)) 500))))
