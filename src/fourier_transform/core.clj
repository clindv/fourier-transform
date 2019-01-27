(ns fourier-transform.core
  (:gen-class))
(defn dft [v]
  (let [n (count v)]
    (for [i (range n)]
      (Math/hypot
       (apply + (map * v (map (comp #(Math/cos %) (partial * 2 i Math/PI (/ n))) (range n))))
       (apply + (map * v (map (comp #(Math/sin %) (partial * 2 i Math/PI (/ n))) (range n))))))))
(defn dft-lazy [v]
  (let [n (count v)]
    (for [i (range n)]
      (#(Math/hypot (first %) (second %))
       (apply (partial map +)
              (map #(map (partial * %1) %2) v
                   (map (juxt (fn [x] (Math/cos x)) (fn [x] (Math/sin x)))
                        (map #(* 2 i Math/PI (/ n) %)
                             (range n)))))))))
(defn dft-lazy-memoize [v]
  (let [n (count v)]
    (for [i (range n)]
      (#(Math/hypot (first %) (second %))
       (apply (partial map +)
              (map #(map (partial * %1) %2) v
                   (map (juxt (memoize (fn [x] (Math/cos x))) (memoize (fn [x] (Math/sin x))))
                        (map #(* 2 i Math/PI (/ n) %)
                             (range n)))))))))
(defn fft [v]
  (let [n (count v)]
    (vec (if (= n 1) (map (fn [s] [s 0]) v)
             (map (fn [a b] [(+ (first a) (first b)) (+ (second a) (second b))])
                  (reduce into (repeat 2 (fft (take-nth 2 v))))
                  (map-indexed (fn [b a] (let [b ((fn [x] [(Math/cos x) (Math/sin x)]) (* -2 Math/PI b (/ n)))]
                                           [(- (* (first a) (first b)) (* (second a) (second b)))
                                            (+ (* (first a) (second b)) (* (second a) (first b)))]))
                               (reduce into (repeat 2 (fft (take-nth 2 (rest v)))))))))))
(defn fftn [v]
  (map (partial apply #(Math/hypot %1 %2)) (fft v)))
(defn -main
  [& args]
  (println "Hello, World!"))
