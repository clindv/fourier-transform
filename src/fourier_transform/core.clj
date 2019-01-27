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
(defn -main
  [& args]
  (println "Hello, World!"))
